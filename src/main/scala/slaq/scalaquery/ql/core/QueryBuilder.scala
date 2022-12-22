package slaq.ql.core

import slaq.Fail
import slaq.ql._
import slaq.util._
import slaq.ql.{ColumnOps => Cols}

/**
 * generic query builder (benchmarking and statement verification)
 */
final class GenericQueryBuilder[T](
  _query: Query[_, _],
  _nc: NamingContext,
  _parent: Option[QueryBuilder],
  _profile: T
)(using T =:= Profile)
  extends QueryBuilder(_query, _nc, _parent, _profile.asInstanceOf[Profile]) {

  type Self = QueryBuilder
  protected def createSubQueryBuilder(query: Query[_, _], nc: NamingContext) =
    new GenericQueryBuilder(query, nc, Some(this), profile)
}

abstract class QueryBuilder(
  val query: Query[_, _],
  protected val namingContext: NamingContext,
  val parent: Option[QueryBuilder],
  val profile: Profile
)
  extends QueryBuilderAction with QueryBuilderClause {
  import profile.sqlUtils._

  type Self <: QueryBuilder
  protected val scalarFrom: Option[String] = None
  protected val concatOperator: Option[String] = None
  private val NullColumn = ConstColumn.NULL
  private val intReg = "[0-9]+".r

  protected def expr(node: Node, b: SqlBuilder, rename: Boolean): Unit = {

    def subqueryPos() = {
      // extract last subquery column position (e.g. 2 from 't1."name" as "c2", ')
      val str = b.lastSegment.map(_.sb.reverse.take(5).toString()).mkString
      intReg.findFirstIn(str).map(
        // re-reverse string to get correct position (e.g. c12 => 21c => 12)
        _.reverse.toInt
      )
    }
    var pos = if !rename then 0 else subqueryPos() getOrElse 0
    def alias(as: => String, outer: Boolean) = {
      if (rename) b += as
      if (outer) pos = 1
    }
    def delimit(action: => Unit) = {
      if (pos != 0) b += ','
      action
      pos += 1
      alias(s" as ${quote(s"c$pos")}", outer = false)
    }
    node match {
      case p: ProductNode =>
        val xs = p.nodeChildren
        xs.zipWithIndex.foreach {
          // following two cases yield full table from subquery
          case (ta @ Table.Alias(t: Table[_]), _) if rename =>
            show(ta, b, true)
          case (j: Join, i) if rename =>
            show(p.product.productElement(i).asInstanceOf[Table[_]], b, true)
          case (_: Join, i) =>
            delimit( // delegate is Join, show parent Table
              show(p.product.productElement(i).asInstanceOf[Table[_]], b)
            )
          case (n, i) if rename && pos != 0 =>
            delimit(expr(n, b, true))
          case (n: ProductNode, _) =>
            n.nodeChildren.foreach { x =>
              delimit(expr(x, b))
            }
          case (n, _) =>
            delimit(expr(n, b))
        }
      case j: Join => // query yields a single table (i.e. non-product/projection)
        val t = query.unpackable.value.asInstanceOf[Table[_]]
        show(
          j.extractNode(t.tableName, forTableAlias = false), b
        )
      // handle subquery implicit join case (i.e. "select * from tableA a, tableB b")
      case t: Table[_] if rename => show(t, b, true)
      case n => show(n, b)
    }
    if (pos == 0) alias(s" as ${quote("c1")}", outer = true)
  }
  def expr(c: Node, b: SqlBuilder): Unit = expr(c, b, false)

  protected def show(c: Node, b: SqlBuilder): Unit = show(c, b, false)

  protected def show(c: Node, b: SqlBuilder, rename: Boolean): Unit = c match {
    case c: Query[_, _]                => show(c, b)
    case c: OperatorColumn[_]          => show(c, b)
    case c: Column[_]                  => show(c, b)
    case t: Table[_]                   => expr(Node(t.*), b, rename)
    case ta @ Table.Alias(t: Table[_]) => expr(t.mapOp(_ => ta), b, rename)
    case Table.Alias(ta: Table.Alias)  => expr(ta, b, rename) // Union
    case sq @ Subquery(_, _, _)        => b += s"${quote(tableAlias(sq))}.*"
    case SubqueryTable(xs)             => xs.zipWithIndex.foreach((x, i) =>
      show(x, b)
      if i != xs.size -1 then b += ','
    )
    case SubqueryColumn(pos, q, _, _) =>  b += s"${quote(tableAlias(q))}.${quote(s"c$pos")}"
    case SimpleLiteral(w)              => b += w
    case _ =>
      Fail(s"Unmatched node `$c` in show block")
  }

  /*
   * Query show
   */
  private final def show(c: Query[_, _], b: SqlBuilder): Unit = c match {
    case q: ForeignKeyQuery[_, _] => q.fks.foreach(show(_, b))
    case q =>
      b += '('; subQueryBuilder(q).Select.build(b, false); b += ')'
  }

  /*
   * OperatorColumn show
   */
  private final def show[T](c: OperatorColumn[T], b: SqlBuilder): Unit = c match {
    case Cols.Is(l, NullColumn) =>
      b += '('; expr(l, b); b += " IS NULL)"
    case Cols.Is(l, r) =>
      b += '('; expr(l, b); b += '='; expr(r, b); b += ')'
    case Cols.Not(Cols.Is(l, NullColumn)) =>
      b += '('; expr(l, b); b += " IS NOT NULL)"
    case Cols.Not(e) =>
      b += "(NOT "; expr(e, b); b += ')'

    case fk: ForeignKey[_, _] =>
      b += "(("; expr(fk.left, b); b += ") = ("; expr(fk.right, b); b += "))"

    case Cols.InSet(e, seq, tm, bind) =>
      if (seq.isEmpty) show(ConstColumn(false), b) else {
        b += '('; expr(e, b); b += " IN ("
        if (bind) b.sep(seq, ",")(x => b +?= { (p, _) => tm(profile).setValue(x, p) })
        else b += seq.map(tm(profile).value2SQLLiteral).mkString(",")
        b += "))"
      }
    case Cols.Between(left, start, end) =>
      expr(left, b); b += " BETWEEN "; expr(start, b); b += " AND "; expr(end, b)

    case Cols.CountDistinct(e) =>
      b += "COUNT(DISTINCT "; expr(e, b); b += ')'
    case Cols.Like(l, r, esc) =>
      b += '('; expr(l, b); b += " LIKE "; expr(r, b);
      esc.foreach { ch =>
        if (ch == '\'' || ch == '%' || ch == '_') Fail(
          s"Illegal escape character '$ch' for LIKE expression"
        )
        b += s" escape '$ch'"
      }
      b += ')'

    case EscFunction("concat", l, r) if concatOperator.isDefined =>
      b += '('; expr(l, b); b += concatOperator.get; expr(r, b); b += ')'

    case s: SimpleFunction =>
      if (s.scalar) b += "{fn "
      b += s.name += '('; b.sep(s.nodeChildren, ",")(expr(_, b)); b += ')'
      if (s.scalar) b += '}'

    case s: SimpleExpression => s.toSQL(b, this)
    case s: SimpleBinaryOperator =>
      b += '('; expr(s.left, b); b += s" ${s.name} "; expr(s.right, b); b += ')'
  }

  /*
   * Column show
   */
  private final def show[T](c: Column[T], b: SqlBuilder): Unit = c match {
    case n @ NamedColumn(t: SubqueryTable, name, _) =>
      t.cols
        .collect {
          case SubqueryColumn(pos, subquery, _, Some(col)) if col.name == name =>
            s"${quote(tableAlias(subquery))}.${quote(s"c$pos")}"
          }
        .foreach(b += _)
    case n: NamedColumn[_] =>
      // must pass self (not n.table) to tableAlias for Join check
      b += s"${quote(tableAlias(n))}.${quote(n.name)}"

    case c @ BindColumn(v) =>
      b +?= { (p, _) => c.typeMapper(profile).setValue(v, p) }

    case pc @ ParameterColumn(idx) =>
      b +?= { (p, param) =>
        val v = (
          if (idx == -1) param
          else param.asInstanceOf[Product].productElement(idx)
        )
        pc.typeMapper(profile).setValue(v.asInstanceOf[T], p)
      }

    case NullColumn         => b += "NULL"
    case c @ ConstColumn(v) => b += c.typeMapper(profile).value2SQLLiteral(v)
    case HavingColumn(_)    => expr(c, b)
    case a @ AsColumnOf(ch, name) =>
      val tn = name.getOrElse(mapTypeName(a.typeMapper(profile)))
      b += "cast("; expr(ch, b); b += s" as $tn)"

    case c: CaseColumn[_] =>
      b += "(CASE"
      c.clauses.foldRight(()) { (w, _) =>
        b += " WHEN "; expr(w.left, b); b += " THEN "; expr(w.right, b)
      }
      c.elseClause match {
        case NullColumn =>
        case n          => b += " ELSE "; expr(n, b)
      }
      b += " END)"

    // handle bizarre edge case where more than 1 If condition in a Case When statement
    // *that uses String equality* results in a WrappedColumn (with parent as OperatorColumn)
    // rather than the OperatorColumn itself
    case x: WrappedColumn[_] if x.parent.isInstanceOf[OperatorColumn[_]] => show(x.parent, b)
    case _: OperatorColumn[_] | _: WrappedColumn[_] =>
  }
}
