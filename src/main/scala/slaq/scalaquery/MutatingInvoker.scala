package slaq

import java.sql.PreparedStatement
import slaq.session._
import slaq.ql.Query
import slaq.ql.core.Profile
import slaq.util.{ValueLinearizer, NamingContext}
import slaq.session.{PositionedParameters, PositionedResult}
import scala.annotation.unchecked.{uncheckedVariance => uV}

trait MutatingInvoker[-P, R] extends Invoker[P, R] { self =>
  /**
   * Transform a query's results with an updatable result set.
   */
  def mutate(param: P, f: ResultSetMutator[R] => Unit, end: ResultSetMutator[R] => Unit)(using session: Session): Unit

  /**
   * Transform a query's results with an updatable result set.
   */
  final def mutate(param: P)(f: ResultSetMutator[R] => Unit)(using session: Session): Unit = mutate(param, f, null)

  override def apply(parameter: P): MutatingUnitInvoker[R] =
    new AppliedInvoker[P, R] with MutatingUnitInvoker[R] {
      protected val appliedParameter = parameter
      protected val delegate = self
    }
}

trait MutatingUnitInvoker[R] extends UnitInvoker[R] {
  override protected val delegate: MutatingInvoker[Param, R]

  def mutate(f: ResultSetMutator[R] => Unit, end: ResultSetMutator[R] => Unit)(using session: Session): Unit = delegate.mutate(appliedParameter, f, end)

  final def mutate(f: ResultSetMutator[R] => Unit)(using session: Session): Unit = mutate(f, null)
}

trait MutatingStatementInvoker[-P, R]
  extends StatementInvoker[P, R]
  with MutatingInvoker[P, R] {

  protected def query: Query[_, R]
  protected def profile: Profile
  final private lazy val (built, lin) = profile.buildSelect(
    query, NamingContext()
  )

  inline final protected def getStatement = built.sql
  def selectStatement = getStatement

  final protected def setParam(param: P, st: PreparedStatement): Unit = built.setter(new PositionedParameters(st), param)

  final protected def extractValue(rs: PositionedResult): R = lin.asInstanceOf[ValueLinearizer[R]].getResult(profile, rs)

  final protected def updateRowValues(rs: PositionedResult, value: R @uV): Unit =
    lin.asInstanceOf[ValueLinearizer[R]].updateResult(profile, rs, value)

  protected val concurrency: ResultSetConcurrency = ResultSetConcurrency.Updatable
  protected val cursor: ResultSetType = ResultSetType.Auto
  protected val previousAfterDelete = false

  def mutate(param: P, f: ResultSetMutator[R] => Unit, end: ResultSetMutator[R] => Unit)(using session: Session): Unit = {

    results(param, 0, cursor, concurrency).fold(
      _ => Fail("Cannot transform an update result"),
      pr => try {
        val rs = pr.rs
        var current: R = null.asInstanceOf[R]
        val mu = new ResultSetMutator[R] {
          def row = current
          def row_=(value: R): Unit = {
            pr.restart
            updateRowValues(pr, value)
            rs.updateRow()
          }
          def insert(value: R): Unit = {
            rs.moveToInsertRow()
            pr.restart
            updateRowValues(pr, value)
            rs.insertRow()
            rs.moveToCurrentRow()
          }
          def delete(): Unit = {
            rs.deleteRow()
            if (previousAfterDelete) rs.previous()
          }
        }
        while (pr.nextRow) {
          current = extractValue(pr)
          f(mu)
        }
        if (end ne null) {
          end(new ResultSetMutator[R] {
            def row = Fail("After end of result set")
            def row_=(value: R) = Fail("After end of result set")
            def delete() = Fail("After end of result set")
            def insert(value: R): Unit = {
              rs.moveToInsertRow()
              pr.restart
              updateRowValues(pr, value)
              rs.insertRow()
            }
          })
        }
      }
      finally { pr.close() }
    )
  }
}
