package slaq.ql

import slaq.util.{Node, UnaryNode, BinaryNode}

trait ColumnOps[B1, P1] {
  protected val leftOperand: Node
  import ColumnOps._
  type OM2[B2, BR, P2, R] = OptionMapper2[B1, B2, BR, P1, P2, R]
  type OM2Bin[BR, P2, R] = OM2[B1, BR, P2, R]
  type OM3[B2, B3, BR, P2, P3, R] = OptionMapper3[B1, B2, B3, BR, P1, P2, P3, R]
  type ToOption = OptionMapper2[B1, B1, B1, Option[B1], Option[B1], Option[B1]]
  type ToSame = OM2Bin[B1, P1, P1]
  type Restr2[B, BR, P2, PR] = OptionMapper2[B, B, BR, P1, P2, PR]
  type Restr1[B, BR, PR] = Restr2[B, BR, P1, PR]
  type BaseTM = BaseTypeMapper[B1]
  type Num = BaseTM with NumericTypeMapper

  def is[P2, R](e: Column[P2])(using om: OM2Bin[Boolean, P2, R]) =
    om(Is(leftOperand, Node(e)))
  def =~[P2, R](e: Column[P2])(using om: OM2Bin[Boolean, P2, R]) =
    om(Is(leftOperand, Node(e)))
  def =![P2, R](e: Column[P2])(using om: OM2Bin[Boolean, P2, R]) =
    om(Not(Is(leftOperand, Node(e))))

  def <[P2, R](e: ColumnBase[P2])(using om: OM2Bin[Boolean, P2, R]) =
    om(Relational("<", leftOperand, Node(e)))
  def <=[P2, R](e: ColumnBase[P2])(using om: OM2Bin[Boolean, P2, R]) =
    om(Relational("<=", leftOperand, Node(e)))
  def >[P2, R](e: ColumnBase[P2])(using om: OM2Bin[Boolean, P2, R]) =
    om(Relational(">", leftOperand, Node(e)))
  def >=[P2, R](e: ColumnBase[P2])(using om: OM2Bin[Boolean, P2, R]) =
    om(Relational(">=", leftOperand, Node(e)))

  def inSet[R](seq: Iterable[B1])(using om: OM2Bin[Boolean, P1, R], tm: BaseTM) =
    om(InSet(leftOperand, seq, tm, false))
  def inSetBind[R](seq: Iterable[B1])(using om: OM2Bin[Boolean, P1, R], tm: BaseTM) =
    om(InSet(leftOperand, seq, tm, true))
  def between[P2, P3, R](start: Column[P2], end: Column[P3])(using om: OM3[B1, B1, Boolean, P2, P3, R]) = om(Between(leftOperand, start, end))
  def ifNull[B2, P2, R](e: Column[P2])(using om: OM2[B2, Boolean, P2, R]): Column[P2] =
    EscFunction[P2]("ifnull", leftOperand, Node(e))(e.typeMapper)

  def +[P2, R](e: ColumnBase[P2])(using om: OM2Bin[B1, P2, R], tm: Num) =
    om(Arith[B1]("+", leftOperand, Node(e)))
  def -[P2, R](e: ColumnBase[P2])(using om: OM2Bin[B1, P2, R], tm: Num) =
    om(Arith[B1]("-", leftOperand, Node(e)))
  def *[P2, R](e: ColumnBase[P2])(using om: OM2Bin[B1, P2, R], tm: Num) =
    om(Arith[B1]("*", leftOperand, Node(e)))
  def /[P2, R](e: ColumnBase[P2])(using om: OM2Bin[B1, P2, R], tm: Num) =
    om(Arith[B1]("/", leftOperand, Node(e)))
  def %[P2, R](e: ColumnBase[P2])(using om: OM2Bin[B1, P2, R], tm: Num) =
    om(EscFunction[B1]("mod", leftOperand, Node(e)))

  def abs(using om: ToSame, tm: Num) =
    om(EscFunction[B1]("abs", leftOperand))
  def ceil(using om: ToSame, tm: Num) =
    om(EscFunction[B1]("ceiling", leftOperand))
  def floor(using om: ToSame, tm: Num) =
    om(EscFunction[B1]("floor", leftOperand))
  def avg(using om: ToOption, tm: Num) =
    om(StdFunction[B1]("avg", leftOperand))
  def min(using om: ToOption, tm: BaseTM) =
    om(StdFunction[B1]("min", leftOperand))
  def max(using om: ToOption, tm: BaseTM) =
    om(StdFunction[B1]("max", leftOperand))
  def sum(using om: ToOption, tm: Num) =
    om(StdFunction[B1]("sum", leftOperand))

  def &[P2, R](b: ColumnBase[P2])(using om: Restr2[Boolean, Boolean, P2, R]) =
    om(And(leftOperand, Node(b)))
  def |[P2, R](b: ColumnBase[P2])(using om: Restr2[Boolean, Boolean, P2, R]) =
    om(Or(leftOperand, Node(b)))
  def unary_![R](using om: Restr1[Boolean, Boolean, R]) =
    om(Not(leftOperand))

  def length[R](using om: Restr1[String, Int, R]) =
    om(EscFunction[Int]("length", leftOperand))
  def like[P2, R](e: Column[P2])(using om: Restr2[String, Boolean, P2, R]) =
    om(Like(leftOperand, Node(e), None))
  def like[P2, R](e: Column[P2], esc: Char)(using om: Restr2[String, Boolean, P2, R]) =
    om(Like(leftOperand, Node(e), Some(esc)))
  def ++[P2, R](e: Column[P2])(using om: Restr2[String, String, P2, R]) =
    om(EscFunction[String]("concat", leftOperand, Node(e)))
  def startsWith[R](s: String)(using om: Restr1[String, Boolean, R]) =
    om(new StartsWith(leftOperand, s))
  def endsWith[R](s: String)(using om: Restr1[String, Boolean, R]) =
    om(new EndsWith(leftOperand, s))
}

object ColumnOps {
  case class In(left: Node, right: Node)
    extends OperatorColumn[Boolean] with SimpleBinaryOperator { val name = "in" }

  case class Relational(name: String, left: Node, right: Node)
    extends OperatorColumn[Boolean] with SimpleBinaryOperator

  case class Arith[T: TypeMapper](name: String, left: Node, right: Node)
    extends OperatorColumn[T] with SimpleBinaryOperator

  case class Is(left: Node, right: Node) extends OperatorColumn[Boolean] with BinaryNode
  case class CountDistinct(child: Node) extends OperatorColumn[Int] with UnaryNode

  case class InSet[T](child: Node, seq: Iterable[T], tm: TypeMapper[T], bind: Boolean)
    extends OperatorColumn[Boolean] with UnaryNode

  case class Between(left: Node, start: Node, end: Node)
    extends OperatorColumn[Boolean] {
    def nodeChildren = left :: start :: end :: Nil
  }

  case class And(left: Node, right: Node)
    extends OperatorColumn[Boolean] with SimpleBinaryOperator {
    val name = "and"
  }
  case class Or(left: Node, right: Node)
    extends OperatorColumn[Boolean] with SimpleBinaryOperator {
    val name = "or"
  }
  case class Not(child: Node) extends OperatorColumn[Boolean] with UnaryNode

  case class Like(left: Node, right: Node, esc: Option[Char])
    extends OperatorColumn[Boolean] with BinaryNode

  class StartsWith(n: Node, s: String)
    extends Like(n, ConstColumn(likeEncode(s) + '%'), Some('^'))

  class EndsWith(n: Node, s: String)
    extends Like(n, ConstColumn(s"%${likeEncode(s)}"), Some('^'))

  def likeEncode(s: String) = {
    val b = new StringBuilder
    for (c <- s) c match {
      case '%' | '_' | '^' => b append '^' append c
      case _               => b append c
    }
    b toString
  }
}
