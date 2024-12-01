package slaq.ql.core

import annotation.implicitNotFound
import java.sql.{PreparedStatement, Statement}
import slaq.Fail
import slaq.ql.{
  Query, Unpackable, Unpack, Table, NamedColumn, Projection, MappedProjection
}
import slaq.session.{Session, PositionedParameters}
import slaq.util.{BatchResult, SqlBuilder, Node}
import scala.collection.immutable.IndexedSeq

final class InsertInvoker[T, U](unpackable: Unpackable[T, U], profile: Profile) {

  def insertInvoker: this.type = this

  private lazy val hasAutoInc: Boolean = hasAutoIncrement(unpackable.value)

  final lazy val insertStatement =
    profile.buildInsert(unpackable.value)

  final lazy val upsertStatement =
    profile.buildUpsert(unpackable.value)

  def insertStatementFor[TT](query: Query[TT, U]): String =
    profile.buildInsert(unpackable.value, query).sql

  def insertStatementFor[TT](c: TT)(using Unpack[TT, U]): String = insertStatementFor(Query(c))

  /**
   * Insert a row.
   */
  infix def insert[V, TT](value: V)(using ev: PackedUnpackedUnion[TT, U, V], session: Session): Int =
    ev.fold(
      u => insertValue(u, forUpsert = false),
      (t, unpack) =>
        given Unpack[TT, U] = unpack
        insert(Query(t))
    )(value)

  /**
   * Upsert a row.
   */
  infix def upsert(value: U)(using Session): Int = {
    insertValue(value, forUpsert = true)
  }

  private def insertValue(value: U, forUpsert: Boolean)(using session: Session): Int =
    val statement = if forUpsert then upsertStatement else insertStatement
    session.withPreparedStatement(statement) { st =>
      st.clearParameters()
      unpackable.linearizer.setParameter(
        profile, new PositionedParameters(st), Some(value)
      )
      st.executeUpdate()
    }

  infix def insert[TT](query: Query[TT, U])(using session: Session): Int = {
    val sbr = profile.buildInsert(unpackable.value, query)
    session.withPreparedStatement(insertStatementFor(query)) { st =>
      st.clearParameters()
      sbr.setter(new PositionedParameters(st), null)
      st.executeUpdate()
    }
  }

  /**
   * Insert multiple rows using JDBC's batch update.
   * If table defines an AutoInc column a `BatchResult` with List[Long] `generatedKeys`
   * is returned; otherwise Some(affectedRows). If any part of the
   * batch fails, an exception is thrown.
   */
  infix def insertAll(values: U*)(using session: Session): BatchResult =
    if (values.size == 0) BatchResult()
    else insertAll(values*)(forUpsert = false)

  /**
   * Upsert multiple rows using JDBC's batch update.
   * Returns `BatchResult` with Some(affectedRows), or None if the database
   * returned no row count for some part of the batch. If any part of the
   * batch fails, an exception is thrown.
   */
  infix def upsertAll(values: U*)(using session: Session): BatchResult =
    if (values.size == 0) BatchResult()
    else insertAll(values*)(forUpsert = true)

  private def insertAll
    (values: U*)
    (forUpsert: Boolean)
    (using session: Session): BatchResult = {

    session.withTransaction {
      val statement = if forUpsert then upsertStatement else insertStatement
      val withGeneratedKeys = !forUpsert && hasAutoInc
      val withStatement =
        if (!withGeneratedKeys)
          session.withPreparedStatement[BatchResult](statement)
        else
          session.withPreparedStatement[BatchResult](statement, withGeneratedKeys)

      withStatement { st =>
        st.clearParameters()
        for (value <- values) {
          unpackable.linearizer.setParameter(
            profile, new PositionedParameters(st), Some(value)
          )
          st.addBatch()
        }

        var unknown = false
        var count = 0
        val resultWithIndex = st.executeBatch().zipWithIndex
        for ((res, idx) <- resultWithIndex) res match {
          case Statement.SUCCESS_NO_INFO => unknown = true
          case Statement.EXECUTE_FAILED => Fail(s"Failed to insert row #${idx + 1}")
          case i => count += i
        }
        if (withGeneratedKeys) {
          var xs = List.empty[Long]
          val rs = st.getGeneratedKeys()
          while (rs.next()) {
            xs = xs ++ List(rs.getLong(1))
          }
          BatchResult(generatedKeys = xs)
        }
        else if (unknown) BatchResult()
        else BatchResult(affectedRows = Some(count))
      }
    }
  }

  private def hasAutoIncrement(column: Any): Boolean = {
    def f(c: Any, firstPass: Boolean): Boolean = c match {
      case p: Projection[_] =>
        import util.control.Breaks.*

        var autoInc = false
        breakable {
          for (i <- 0 until p.productArity) {
            autoInc = f(Node(p.productElement(i)), firstPass)
            if autoInc then break
          }
        }
        autoInc
      case t: Table[_]       => f(Node(t.*), firstPass)
      case n: NamedColumn[_] if !firstPass => n.options.exists(_ == ColumnOptions.AutoInc)
      case n: NamedColumn[_] =>
        (column, n.table.asInstanceOf[Table[?]].*) match
          // when product arities don't match user is selecting a subset of table fields.
          // do a second pass using target column's table to find possible AutoInc column
          case (c: Product, t: Product) if c.productArity != t.productArity =>
            f(n.table, firstPass = false)
          case (c: Product, t: MappedProjection[?, ?]) if c.productArity != t.child.productArity=>
            f(n.table, firstPass = false)
          // product arities match, check for AutoInc column
          case _ =>
            n.options.exists(_ == ColumnOptions.AutoInc)
      case _ => false
    }
    f(Node(column), firstPass = true)
  }
}

@implicitNotFound(msg = """
union type mismatch;
found: ${T}
required: ${U}
or: ${P} with evidence Unpack[${P}, ${U}]
""")
trait PackedUnpackedUnion[P, U, T] {
  def fold[R](f: U => R, g: (P, Unpack[P, U]) => R)(v: T): R
}

object PackedUnpackedUnion extends PackedUnpackedUnionLowPriority {
  given packedUnpackedUnionTypeU[P, U, T <: U]: PackedUnpackedUnion[P, U, T] =
    new PackedUnpackedUnion[P, U, T] {
      def fold[R](f: U => R, g: (P, Unpack[P, U]) => R)(v: T): R = f(v)
    }
}

class PackedUnpackedUnionLowPriority {
  given packedUnpackedUnionTypeP[P, U, T <: P](using ev: Unpack[P, U]): PackedUnpackedUnion[P, U, T] =
    new PackedUnpackedUnion[P, U, T] {
      def fold[R](f: U => R, g: (P, Unpack[P, U]) => R)(v: T): R = g(v, ev)
    }
}
