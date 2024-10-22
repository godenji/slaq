package slaq.ql.core

import annotation.implicitNotFound
import java.sql.Statement
import slaq.Fail
import slaq.ql.{Query, Unpackable, Unpack}
import slaq.session.{Session, PositionedParameters}
import scala.collection.immutable.IndexedSeq

final class InsertInvoker[T, U](unpackable: Unpackable[T, U], profile: Profile) {

  final lazy val insertStatement =
    profile.buildInsert(unpackable.value)

  def insertStatementFor[TT](query: Query[TT, U]): String =
    profile.buildInsert(unpackable.value, query).sql

  def insertStatementFor[TT](c: TT)(using Unpack[TT, U]): String = insertStatementFor(Query(c))

  def useBatchUpdates(using session: Session): Boolean =
    session.capabilities.supportsBatchUpdates

  /**
   * Insert a single row.
   */
  infix def insert[V, TT](value: V)(using ev: PackedUnpackedUnion[TT, U, V], session: Session): Int =
    ev.fold(
      u =>
        insertValue(u),
      (t, unpack) => insertExpr(t)(using unpack, session)
    )(value)

  def insertValue(value: U)(using session: Session): Int =
    session.withPreparedStatement(insertStatement) { st =>
      st.clearParameters()
      unpackable.linearizer.setParameter(
        profile, new PositionedParameters(st), Some(value)
      )
      st.executeUpdate()
    }

  def insertExpr[TT](c: TT)(using Unpack[TT, U], Session): Int =
    insert(Query(c))

  /**
   * Insert multiple rows. Uses JDBC's batch update feature if supported by
   * the JDBC driver. Returns Some(rowsAffected), or None if the database
   * returned no row count for some part of the batch. If any part of the
   * batch fails, an exception is thrown.
   */
  infix def insertAll(values: U*)(using session: Session): Option[Int] = {
    session.withTransaction {
      session.withPreparedStatement(insertStatement) { st =>
        st.clearParameters()
        for (value <- values) {
          unpackable.linearizer.setParameter(
            profile, new PositionedParameters(st), Some(value)
          )
          st.addBatch()
        }
        var unknown = false
        var count = 0
        for ((res, idx) <- st.executeBatch().zipWithIndex) res match {
          case Statement.SUCCESS_NO_INFO => unknown = true
          case Statement.EXECUTE_FAILED =>
            Fail("Failed to insert row #" + (idx + 1))
          case i => count += i
        }
        if (unknown) None else Some(count)
      }
    }
  }

  infix def insert[TT](query: Query[TT, U])(using session: Session): Int = {
    val sbr = profile.buildInsert(unpackable.value, query)
    session.withPreparedStatement(insertStatementFor(query)) { st =>
      st.clearParameters()
      sbr.setter(new PositionedParameters(st), null)
      st.executeUpdate()
    }
  }

  def insertInvoker: this.type = this
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
