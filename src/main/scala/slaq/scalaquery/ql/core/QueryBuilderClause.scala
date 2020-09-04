package slaq.ql.core

import scala.reflect.ClassTag
import slaq.Fail
import slaq.ql._
import slaq.util._

trait QueryBuilderClause { self: QueryBuilder =>
  protected def appendClauses(b: SqlBuilder): Unit = {
    appendConditions(b)
    appendGroupClause(b)
    appendHavingConditions(b)
    appendOrderClause(b)
    appendLimitClause(b)
  }

  protected def queryModifiers[T <: QueryModifier](implicit m: ClassTag[T]) =
    query.modifiers.filter(m.runtimeClass.isInstance(_)).asInstanceOf[List[T]]

  protected def appendConditions(b: SqlBuilder): Unit =
    query.cond.filter { case HavingColumn(_) => false; case _ => true } match {
      case Nil =>
      case xs =>
        b += " WHERE "
        b.sep(xs, " AND ")(x => expr(Node(x), b))
    }

  protected def appendGroupClause(b: SqlBuilder): Unit =
    queryModifiers[Grouping] match {
      case Nil =>
      case xs =>
        b += " GROUP BY "
        b.sep(xs, ",")(x => expr(x.by, b, false))
    }

  protected def appendHavingConditions(b: SqlBuilder): Unit = {
    query.cond.collect { case HavingColumn(c) => c } match {
      case Nil =>
      case xs =>
        b += " HAVING "
        b.sep(xs, " AND ")(x => expr(Node(x), b))
    }
  }

  protected def appendOrderClause(b: SqlBuilder): Unit =
    queryModifiers[Ordering] match {
      case Nil =>
      case xs =>
        b += " ORDER BY "
        b.sep(xs, ",")(appendOrdering(_, b))
    }

  protected def appendOrdering(o: Ordering, b: SqlBuilder): Unit = {
    expr(o.by, b, false)
    if (o.isInstanceOf[Ordering.Desc]) b += " DESC"
    o.nullOrdering match {
      case Ordering.NullsFirst   => b += " NULLS FIRST"
      case Ordering.NullsLast    => b += " NULLS LAST"
      case Ordering.NullsDefault =>
    }
  }

  protected def appendLimitClause(b: SqlBuilder): Unit =
    queryModifiers[TakeDrop].lastOption.foreach {
      case TakeDrop(Some(t), Some(d), compareNode) =>
        val compFn = maybeLimitNode(t, d, compareNode, _: Boolean)
        appendLimitValue(b += " OFFSET ", d, compFn(true))
        appendLimitValue(b += " ROW FETCH NEXT ", t, compFn(false))
        b += " ROW ONLY"

      case TakeDrop(Some(t), None, _) =>
        appendLimitValue(b += " FETCH NEXT ", t)
        b += " ROW ONLY"

      case TakeDrop(None, Some(d), _) =>
        appendLimitValue(b += " OFFSET ", d)
        b += " ROW"
      case _ =>
    }

  /*
   * returns a potential comparison node for take, drop calculation
   */
  protected def maybeLimitNode(
    takeColumn: Column[Int],
    dropColumn: Column[Int],
    compareColumn: Option[Column[Int]], isDrop: Boolean
  ): Option[Column[Int]] = {

    if (isDrop) {
      if (Some(dropColumn) == compareColumn) None
      else Some(takeColumn)
    }
    else if (Some(takeColumn) == compareColumn) Some(dropColumn)
    else None
  }

  protected def appendLimitValue(
    b: SqlBuilder, node: Column[Int],
    compareNode: Option[Column[Int]] = None
  ): Unit = {

    def paramValue(param: Any, idx: Int): Int = (
      if (idx == -1) param
      else param.asInstanceOf[Product].productElement(idx)
    ).asInstanceOf[Int]

    def compareValue(a: Int, b: Int): Int = (
      if (a == b) a else if (a > b) a - b else (b - a) + 1
    )

    (node, compareNode) match {
      case (ConstColumn(v), None)                 => b += v
      case (ConstColumn(x), Some(ConstColumn(y))) => b += compareValue(x, y)
      case (
        x @ ParameterColumn(idx), None) => b +?= { (p, param) =>
        x.typeMapper(profile).setValue(
          paramValue(param, idx), p
        )
      }
      case (target @ ParameterColumn(idxA), Some(ParameterColumn(idxB))) =>
        b +?= { (p, param) =>
          target.typeMapper(profile).setValue(
            compareValue(
              paramValue(param, idxA), paramValue(param, idxB)
            ), p
          )
        }
      case _ => Fail(s"""
				values in a take, drop operation cannot be mixed; 
				they must be either ConstColumn[Int] or Param[Int].
				Supplied node $node and Optional compareNode $compareNode
				could not be converted to a literal value
			""")
    }
    ()
  }

}
