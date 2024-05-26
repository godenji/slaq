package slaq.ql

import slaq.util.{Node, BinaryNode}
import slaq.Fail

object Case {
  class WhenNode(val left: Node, val right: Node) extends BinaryNode

  infix def If[C <: Column[?]: Queryable](cond: C) = new UntypedWhen(Node(cond))

  class UntypedWhen(cond: Node) {
    infix def Then[B: BaseTypeMapper](res: Column[B]) =
      new TypedCase[B, B](
        new WhenNode(cond, Node(res)) :: Nil
      )
    infix def Then[B](res: Column[Option[B]]) = res.typeMapper match {
      case tmt: OptionTypeMapper[_] =>
        new TypedCase[B, Option[B]](
          new WhenNode(cond, Node(res)) :: Nil
        )(using tmt.base, tmt)
      case x =>
        Fail(s"""No "Then" match in case-when clause for $x""")
    }
  }

  class TypedCase[B: TypeMapper, T: TypeMapper](clauses: List[WhenNode])
    extends CaseColumn[Option[B]](clauses, ConstColumn.NULL) {

    infix def If[C <: Column[?]: Queryable](cond: C) = new TypedWhen[B, T](cond, this)
    infix def Else(res: Column[T]): Column[T] = new TypedCaseWithElse[T](clauses, Node(res))
  }

  class TypedWhen[B: TypeMapper, T: TypeMapper](cond: Node, parent: TypedCase[B, T]) {

    infix def Then(res: Column[T]) =
      new TypedCase[B, T](
        new WhenNode(cond, Node(res)) :: parent.clauses
      )
  }

  class TypedCaseWithElse[T: TypeMapper](clauses: List[WhenNode], elseClause: Node)
    extends CaseColumn[T](clauses, elseClause)
}
