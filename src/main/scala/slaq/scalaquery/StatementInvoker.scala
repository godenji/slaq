package slaq

import java.sql.PreparedStatement
import slaq.session._
import slaq.util.CloseableIterator

import slaq.session.{
  ResultSetConcurrency => Concurrency,
  ResultSetHoldability => Holdability
}
import Concurrency._, ResultSetType._

/**
 * An invoker which executes an SQL statement.
 */
abstract class StatementInvoker[-P, +R] extends Invoker[P, R] { self =>

  protected def getStatement: String

  protected def setParam(param: P, st: PreparedStatement): Unit

  def elementsTo(param: P, maxRows: Int)(using Session): CloseableIterator[R] =
    results(param, maxRows).fold(
      r => new CloseableIterator.Single[R](r.asInstanceOf[R]), identity
    )

  /**
   * Invoke the statement and return the raw results.
   * TODO Support multiple results
   */
  def results(
    param: P, maxRows: Int,
    cursor: ResultSetType = ForwardOnly,
    concurrency: Concurrency = ReadOnly,
    holdability: Holdability = Holdability.Default
  )(using session: Session): Either[Int, PositionedResultIterator[R]] = {
    val currStatement = getStatement
    val statement = (
      if (concurrency.intValue == Updatable.intValue)
        s"$currStatement FOR UPDATE"
      else
        currStatement
    )
    val st = session.prepareStatement(
      statement, cursor, concurrency, holdability
    )
    setParam(param, st)
    var doClose = true
    try {
      st.setMaxRows(maxRows)
      if (st.execute) {
        val r = st.getResultSet
        val pri = new PositionedResultIterator[R](r, maxRows) {
          def closeUnderlying() = {
            st.close()
            r.close()
          }
          def extractValue() = self.extractValue(this)
        }
        doClose = false
        Right(pri)
      }
      else Left(st.getUpdateCount)
    }
    finally if (doClose) st.close()
  }

  protected def extractValue(pr: PositionedResult): R

  /** pretty print generated statement */
  def pretty =
    getStatement.replaceAll("`", "").replaceAll("\"", "").split(",").mkString(", ").
      replaceAll("(FROM|INNER|LEFT|RIGHT|FULL|WHERE|GROUP BY|ORDER BY|LIMIT)", "\n$1")
}
