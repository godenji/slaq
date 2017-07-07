package org.scalaquery.ql

import org.scalaquery.util.{Node, BinaryNode}
import org.scalaquery.Fail

object Case {

  class WhenNode(val left: Node, val right: Node) extends BinaryNode

  def If[C <: Column[_]: Queryable](cond: C) = new UntypedWhen(Node(cond))

  class UntypedWhen(cond: Node) {
    def Then[B: BaseTypeMapper](res: Column[B]) =
      new TypedCase[B, B](
        new WhenNode(cond, Node(res)) :: Nil
      )
    def Then[B](res: Column[Option[B]]) = res.typeMapper match {
      case tmt: OptionTypeMapper[_] =>
        new TypedCase[B, Option[B]](
          new WhenNode(cond, Node(res)) :: Nil
        )(tmt.base, tmt)
      case x =>
        Fail(s"""No "Then" match in case-when clause for $x""")
    }
  }

  class TypedCase[B: TypeMapper, T: TypeMapper](clauses: List[WhenNode])
    extends CaseColumn[Option[B]](clauses, ConstColumn.NULL) {

    def If[C <: Column[_]: Queryable](cond: C) = new TypedWhen[B, T](cond, this)

    def Else(res: Column[T]): Column[T] = new TypedCaseWithElse[T](clauses, Node(res))
  }

  class TypedWhen[B: TypeMapper, T: TypeMapper](cond: Node, parent: TypedCase[B, T]) {

    def Then(res: Column[T]) =
      new TypedCase[B, T](
        new WhenNode(cond, Node(res)) :: parent.clauses
      )
  }

  class TypedCaseWithElse[T: TypeMapper](clauses: List[WhenNode], elseClause: Node)
    extends CaseColumn[T](clauses, elseClause)
}
