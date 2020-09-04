package slaq.ql

import slaq.util.{Node, ValueLinearizer}
import slaq.ql.{JoinType => jt}

sealed abstract class Query[+P, +U] extends Node {
  override def toString = "Query"

  def reified: Node
  def linearizer: ValueLinearizer[_ <: U]
  def unpackable: Unpackable[_ <: P, _ <: U]
  def cond: List[Column[_]]
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

  def map[P2, U2](f: P => P2)(implicit unpack: Unpack[P2, U2]): Query[P2, U2] = flatMap {
    p => Query(f(p))(unpack)
  }

  def filter[T](f: P => T)(implicit qc: Queryable[T]): Query[P, U] =
    new QueryWrap[P, U](
      unpackable, qc(f(unpackable.value), cond), modifiers
    )
  def withFilter[T](f: P => T)(implicit qc: Queryable[T]): Query[P, U] = filter(f)(qc)

  def groupBy(by: Column[_]*) = new QueryWrap[P, U](
    unpackable, cond, modifiers ::: by.map(Grouping).toList
  )

  def orderBy(by: Ordering*) = new QueryWrap[P, U](
    unpackable, cond, modifiers ::: by.toList
  )

  def having[T <: Column[_]](f: P => T)(implicit qc: Queryable[T]): Query[P, U] = {
    val c = f(unpackable.value)
    val conditions = qc(c, cond).map { x =>
      if (x == c) HavingColumn(c)(c.typeMapper) else x
    }
    new QueryWrap[P, U](unpackable, conditions, modifiers)
  }

  def subquery[U2 >: U, R](implicit reify: Reify[P, R]) = {
    Subquery.query(this)(unpackable, reify)
  }

  def union[P2 >: P, U2 >: U, R](right: Query[P2, U2])(implicit reify: Reify[P2, R]): Query[R, U] = union(right, false)

  def unionAll[P2 >: P, U2 >: U, R](right: Query[P2, U2])(implicit reify: Reify[P2, R]): Query[R, U] = union(right, true)

  private def union[P2 >: P, U2 >: U, R](right: Query[P2, U2], all: Boolean)(implicit reify: Reify[P, R]) =
    Subquery.union(this, right, all)(unpackable, reify)

  def take(num: Int): Query[P, U] = take(ConstColumn(num))
  def drop(num: Int): Query[P, U] = drop(ConstColumn(num))
  def take(node: Column[Int]): Query[P, U] = takeDrop(node, isTake = true)
  def drop(node: Column[Int]): Query[P, U] = takeDrop(node, isTake = false)

  private def takeDrop(node: Column[Int], isTake: Boolean) = {
    val fn = if (isTake) TakeDrop.take(_, _) else TakeDrop.drop(_, _)
    val (mod, others) = fn(modifiers, node)
    new QueryWrap[P, U](unpackable, cond, mod :: others)
  }

  def exists =
    StdFunction[Boolean]("exists", map(_ => ConstColumn(1)))

  def asColumn(implicit ev: P <:< Column[_]): P =
    ev(unpackable.value).mapOp(_ => this).asInstanceOf[P]

  def join[P2, U2](right: Query[P2, U2]) = joinPair(right, jt.Inner)
  def leftJoin[P2, U2](right: Query[P2, U2]) = joinPair(right, jt.Left)
  def rightJoin[P2, U2](right: Query[P2, U2]) = joinPair(right, jt.Right)
  def outerJoin[P2, U2](right: Query[P2, U2]) = joinPair(right, jt.Outer)

  def join[C <: Column[_]: Queryable](on: P => C) = joinOne(on, jt.Inner)
  def leftJoin[C <: Column[_]: Queryable](on: P => C) = joinOne(on, jt.Left)
  def rightJoin[C <: Column[_]: Queryable](on: P => C) = joinOne(on, jt.Right)
  def outerJoin[C <: Column[_]: Queryable](on: P => C) = joinOne(on, jt.Outer)

  private def joinPair[P2, U2](right: Query[P2, U2], joinType: JoinType) = {
    val (u1, u2) = (unpackable, right.unpackable)
    val unpack = u1.zip(u2).asInstanceOf[Unpackable[(P, P2), (U, U2)]]
    new JoinQuery(unpack)(this, right, joinType)
  }

  private def joinOne(on: P => Node, joinType: JoinType): Query[P, U] = {
    val upu = unpackable.asInstanceOf[Unpackable[P, U]]
    val join = Join(reified, reified, on(upu.value), joinType)
    Query[P, U](
      Unpackable(upu.mapOp(_ => join), upu.unpack)
    )
  }
}

class JoinQuery[+P, P2, +U, U2](override val unpackable: Unpackable[_ <: (P, P2), _ <: (U, U2)])(left: Query[P, U], right: Query[P2, U2], joinType: JoinType)
  extends QueryWrap[(P, P2), (U, U2)](unpackable, Nil, Nil) {

  /**
   * join tables by column criteria
   */
  def on[C <: Column[_]: Queryable, R](f: (P, P2) => C)(implicit reify: Reify[(P, P2), R]) =
    apply[R](
      f(left.unpackable.value, right.unpackable.value)
    )

  /**
   * join tables by foreign key
   */
  def on[PP >: P <: Table[_], R](f: P2 => ForeignKeyQuery[PP, U2])(implicit reify: Reify[(P, P2), R]) =
    apply[R](
      f(right.unpackable.value)
    )

  private def apply[R](on: Node)(implicit reify: Reify[(P, P2), R]): Query[R, (U, U2)] = {
    Join.query[P, P2, U, U2, R](
      unpackable, reify, Join(
      left.reified, right.reified, on, joinType
    )
    )
  }
}

class QueryWrap[+P, +U](
  val unpackable: Unpackable[_ <: P, _ <: U],
  val cond: List[Column[_]],
  val modifiers: List[QueryModifier]
) extends Query[P, U] {

  lazy val reified = unpackable.reifiedNode
  lazy val linearizer = unpackable.linearizer
}

object Query extends QueryWrap[Unit, Unit](Unpackable((), Unpack.unpackPrimitive[Unit]), Nil, Nil) {

  def apply[P, U](value: P)(implicit unpack: Unpack[P, U]): Query[P, U] = wrapper(Unpackable(value, unpack))

  def apply[P, U](unpackable: Unpackable[_ <: P, _ <: U]): Query[P, U] = wrapper(unpackable)

  private def wrapper[P, U](unpack: Unpackable[P, U]) =
    new QueryWrap[P, U](unpack, Nil, Nil)
}

