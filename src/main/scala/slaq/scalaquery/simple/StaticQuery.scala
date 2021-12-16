package slaq.simple

import java.sql.PreparedStatement
import slaq.{StatementInvoker, UnitInvokerMixin}
import slaq.session.{PositionedParameters, PositionedResult}
import scala.annotation.unchecked.uncheckedVariance

/**
 * Invoker for raw SQL queries.
 * The companion object contains utility methods for building static queries.
 */
abstract class StaticQuery[-P, +R](query: String, rconv: GetResult[R], pconv: SetParameter[P])
  extends StatementInvoker[P, R] {
  protected def getStatement = query
  protected def setParam(param: P, st: PreparedStatement) = pconv(param, new PositionedParameters(st))
  protected def extractValue(rs: PositionedResult): R = rconv(rs)

  protected[this] type Self <: StaticQuery[P, R]
  protected[this] def copy(query: String = this.query, pconv: SetParameter[P @uncheckedVariance] = this.pconv): Self

  def +(s: String) = copy(query + s)
  def +?[T](v: T)(using p: SetParameter[T]) = copy(query + '?', new SetParameter[P] {
    def apply(param: P, pp: PositionedParameters): Unit = {
      pconv(param, pp)
      p(v, pp)
    }
  })
}

object StaticQuery {
  def apply[R](using GetResult[R]) = queryNA("")
  def apply[P, R](using SetParameter[P], GetResult[R]) = query[P, R]("")
  def u = updateNA("")
  def u1[P](using SetParameter[P]) = update[P]("")

  def query[P, R](query: String)(using rconv: GetResult[R], pconv: SetParameter[P]) =
    new StaticQuery1[P, R](query, rconv, pconv)

  def queryNA[R](query: String)(using conv: GetResult[R]) =
    new StaticQuery0[R](query, conv, SetParameter.SetUnit)

  def update[P](query: String)(using pconv: SetParameter[P]) =
    new StaticQuery1[P, Int](query, GetResult.GetUpdateValue, pconv)

  def updateNA(query: String) =
    new StaticQuery0[Int](query, GetResult.GetUpdateValue, SetParameter.SetUnit)
}

class StaticQuery0[R](query: String, rconv: GetResult[R], pconv: SetParameter[Unit])
  extends StaticQuery[Unit, R](query, rconv, pconv)
  with UnitInvokerMixin[R] {

  protected[this] type Self = StaticQuery0[R]
  protected[this] def copy(query: String, pconv: SetParameter[Unit]): Self =
    new StaticQuery0(query, rconv, pconv)
}

class StaticQuery1[P1, R](query: String, rconv: GetResult[R], pconv: SetParameter[P1])
  extends StaticQuery[P1, R](query, rconv, pconv) {
  protected[this] type Self = StaticQuery1[P1, R]
  protected[this] def copy(query: String, pconv: SetParameter[P1]): Self =
    new StaticQuery1(query, rconv, pconv)
}
