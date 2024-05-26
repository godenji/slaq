package slaq.ql

import slaq.util.{Node, ValueLinearizer}
import slaq.ql.{JoinType => jt}
import slaq.util.RefId

sealed abstract class Query[+P, +U] extends Node {
  override def toString = "Query"

  def reified: Node
  def linearizer: ValueLinearizer[? <: U]
  def unpackable: Unpackable[? <: P, ? <: U]
  def cond: List[Column[?]]
  def modifiers: List[QueryModifier]

  def nodeChildren = reified :: cond.map(Node.apply) ::: modifiers

  override def nodeNamedChildren =
    (reified, "select") :: cond.map((_, "where")) ::: modifiers.map((_, "modifier"))

  def flatMap[P2, U2](f: P => Query[P2, U2]): Query[P2, U2] = {
    val q = f(unpackable.value)
    new QueryWrap[P2, U2](
      q.unpackable, cond ::: q.cond, modifiers ::: q.modifiers
    )
  }
  def >>[P2, U2](q: Query[P2, U2]): Query[P2, U2] = flatMap(_ => q)

  def map[P2, U2](f: P => P2)(using Unpack[P2, U2]): Query[P2, U2] = flatMap {
    p => Query(f(p))
  }

  infix def filter[T](f: P => T)(using qc: Queryable[T]): Query[P, U] =
    new QueryWrap[P, U](
      unpackable, qc(f(unpackable.value), cond), modifiers
    )
  def withFilter[T](f: P => T)(using Queryable[T]): Query[P, U] = filter(f)

  infix def groupBy(by: Column[?]*) = new QueryWrap[P, U](
    unpackable, cond, modifiers ::: by.map(Grouping.apply).toList
  )

  infix def orderBy(by: Ordering*) = new QueryWrap[P, U](
    unpackable, cond, modifiers ::: by.toList
  )

  infix def having[T <: Column[?]](f: P => T)(using qc: Queryable[T]): Query[P, U] = {
    val c = f(unpackable.value)
    val conditions = qc(c, cond).map { x =>
      if (x == c) HavingColumn(c)(using c.typeMapper) else x
    }
    new QueryWrap[P, U](unpackable, conditions, modifiers)
  }

  def subquery[U2 >: U, R](using reify: Reify[P, R]) = {
    Subquery.query(this, None)(unpackable, reify)
  }

  infix def union[P2 >: P, U2 >: U, R](right: Query[P2, U2])(using Reify[P2, R]): Query[R, U] = union(right, false)

  infix def unionAll[P2 >: P, U2 >: U, R](right: Query[P2, U2])(using Reify[P2, R]): Query[R, U] = union(right, true)

  private def union[P2 >: P, U2 >: U, R](right: Query[P2, U2], all: Boolean)(using reify: Reify[P, R]) =
    Subquery.union(this, right, all)(unpackable, reify)

  infix def take(num: Int): Query[P, U] = take(ConstColumn(num))
  infix def drop(num: Int): Query[P, U] = drop(ConstColumn(num))
  infix def take(node: Column[Int]): Query[P, U] = takeDrop(node, isTake = true)
  infix def drop(node: Column[Int]): Query[P, U] = takeDrop(node, isTake = false)

  private def takeDrop(node: Column[Int], isTake: Boolean) = {
    val fn = if (isTake) TakeDrop.take(_, _) else TakeDrop.drop(_, _)
    val (mod, others) = fn(modifiers, node)
    new QueryWrap[P, U](unpackable, cond, mod :: others)
  }

  infix def exists =
    StdFunction[Boolean]("exists", map(_ => ConstColumn(1)))

  def asColumn(using ev: P <:< Column[?]): P =
    ev(unpackable.value).mapOp(_ => this).asInstanceOf[P]

  infix def join[P2, U2](right: Query[P2, U2]) = joinPair(right, jt.Inner)
  infix def leftJoin[P2, U2](right: Query[P2, U2]) = joinPair(right, jt.Left)
  infix def rightJoin[P2, U2](right: Query[P2, U2]) = joinPair(right, jt.Right)
  infix def outerJoin[P2, U2](right: Query[P2, U2]) = joinPair(right, jt.Outer)

  infix def join[C <: Column[?]: Queryable](on: P => C) = joinOne(on, jt.Inner)
  infix def leftJoin[C <: Column[?]: Queryable](on: P => C) = joinOne(on, jt.Left)
  infix def rightJoin[C <: Column[?]: Queryable](on: P => C) = joinOne(on, jt.Right)
  infix def outerJoin[C <: Column[?]: Queryable](on: P => C) = joinOne(on, jt.Outer)

  infix def lateral[U2 >: U, R](using reify: Reify[P, R]) = _lateral(jt.Inner)
  infix def lateralLeft[U2 >: U, R](using reify: Reify[P, R]) = _lateral(jt.Left)
  infix def lateralRight[U2 >: U, R](using reify: Reify[P, R]) = _lateral(jt.Right)
  infix def lateralOuter[U2 >: U, R](using reify: Reify[P, R]) = _lateral(jt.Outer)

  private def _lateral[U2 >: U, R](jt: JoinType)(using reify: Reify[P, R]) = {
    LateralQuery[R, U](
      Subquery.query(this, Some(jt))(unpackable, reify),
      this
    )
  }

  private def joinPair[P2, U2](right: Query[P2, U2], joinType: JoinType) = {
    val (u1, u2) = (unpackable, right.unpackable)
    val unpack = u1.zip(u2).asInstanceOf[Unpackable[(P, P2), (U, U2)]]
    JoinQuery(unpack)(this, right, joinType)
  }

  private def joinOne(on: P => Node, joinType: JoinType): Query[P, U] = {
    val upu = unpackable.asInstanceOf[Unpackable[P, U]]
    val join = Join(reified, reified, on(upu.value), joinType)
    Query[P, U](
      Unpackable(upu.mapOp(_ => join), upu.unpack)
    )
  }
}

class LateralQuery[+P, +U](subQ: Query[P, U], parentQ: Query[?, ?])
  extends QueryWrap[P, U](subQ.unpackable, Nil, Nil) { self =>

  infix def on[C <: Column[?]: Queryable](f: P => C): Query[P, U] = {
    new QueryWrap[P, U](
      unpackable,
      cond,
      modifiers ::: List(
        Lateral(RefId(parentQ), f(unpackable.value))
      )
    )
  }
} 

class JoinQuery[+P, P2, +U, U2]
  (override val unpackable: Unpackable[? <: (P, P2), ? <: (U, U2)])
  (left: Query[P, U], right: Query[P2, U2], joinType: JoinType)
  extends QueryWrap[(P, P2), (U, U2)](unpackable, Nil, Nil) {

  /**
   * join tables by column criteria
   */
  infix def on[C <: Column[?]: Queryable, R](f: (P, P2) => C)(using Reify[(P, P2), R]) =
    apply[R](
      f(left.unpackable.value, right.unpackable.value)
    )

  /**
   * join tables by foreign key
   */
  infix def on[PP >: P <: Table[?], R](f: P2 => ForeignKeyQuery[PP, U2])(using Reify[(P, P2), R]) =
    apply[R](
      f(right.unpackable.value)
    )

  private def apply[R](on: Node)(using reify: Reify[(P, P2), R]): Query[R, (U, U2)] = {
    Join.query[P, P2, U, U2, R](
      unpackable, reify, Join(
      left.reified, right.reified, on, joinType
    )
    )
  }
}

class QueryWrap[+P, +U](
  val unpackable: Unpackable[? <: P, ? <: U],
  val cond: List[Column[?]],
  val modifiers: List[QueryModifier]
  ) extends Query[P, U] {

  lazy val reified = unpackable.reifiedNode
  lazy val linearizer = unpackable.linearizer
}

object Query extends QueryWrap[Unit, Unit](Unpackable((), Unpack.unpackPrimitive[Unit]), Nil, Nil) {

  def apply[P, U](value: P)(using unpack: Unpack[P, U]): Query[P, U] = wrapper(Unpackable(value, unpack))

  def apply[P, U](unpackable: Unpackable[? <: P, ? <: U]): Query[P, U] = wrapper(unpackable)

  private def wrapper[P, U](unpack: Unpackable[P, U]) =
    new QueryWrap[P, U](unpack, Nil, Nil)
}

