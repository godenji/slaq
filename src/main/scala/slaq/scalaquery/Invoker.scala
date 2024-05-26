package slaq

import scala.collection.immutable.Map
import scala.collection.Factory
import slaq.session.Session
import slaq.util.CloseableIterator
import slaq.iter._

/**
 * Base trait for all statement invokers, using parameter type P and result type R.
 */
trait Invoker[-P, +R] { self =>

  /**
   * Execute the statement and return a CloseableIterator of the converted results.
   * The iterator must either be fully read or closed explicitly.
   */
  final def elements(param: P)(using Session) = elementsTo(param, 0)

  /**
   * Execute the statement and return a CloseableIterator of the converted results.
   * The iterator must either be fully read or closed explicitly.
   *
   * @param maxRows Maximum number of rows to read from the result (0 for unlimited).
   */
  def elementsTo(param: P, maxRows: Int)(using Session): CloseableIterator[R]

  /**
   * Execute the statement and ignore the results.
   */
  infix final def execute(param: P)(using Session): Unit = elements(param).close()

  /**
   * Execute the statement and return the first row of the result set wrapped in
   * Some, or None if the result set is empty.
   */
  infix final def firstOption(param: P)(using Session): Option[R] = {
    var res: Option[R] = None
    foreach(param, { x => res = Some(x) }, 1)
    res
  }

  /**
   * Execute the statement and return the first row of the result set.
   * If the result set is empty, a NoSuchElementException is thrown.
   */
  infix final def first(param: P)(using Session): R =
    firstOption(param).getOrElse(throw new NoSuchElementException("Invoker.first"))

  /**
   * Execute the statement and return an immutable and fully
   * materialized list of the results.
   */
  infix final def list(param: P)(using Session) = build[List[R]](param)

  final def toMap[T, U](param: P)(using Session, R <:< (T, U)): Map[T, U] =
    inline given Factory[R, Map[T, U]] = summon[Factory[(T, U), Map[T, U]]].asInstanceOf[Factory[R, Map[T, U]]]
    build[Map[T, U]](param)

  /**
   * Execute the statement and return a fully materialized collection of the specified type.
   */
  final def build[T](param: P)(using session: Session, canBuildFrom: Factory[R, T]): T = {
    val b = canBuildFrom.newBuilder
    foreach(param, { x => b += x }, 0)
    b.result()
  }

  /**
   * Return a converter for the specified collection type constructor.
   */
  infix final def to[C[_]] = new To[C]()

  final class To[C[_]] private[Invoker] () {
    /**
     * Execute the statement and return a fully materialized collection.
     */
    def apply[RR >: R]()(using Session, Factory[RR, C[RR]]) =
      build[C[RR]](().asInstanceOf[P])
  }

  /**
   * Execute the statement and call f for each converted row of the result set.
   */
  infix final def foreach(param: P, f: R => Unit)(using Session): Unit = {
    val it = elements(param)
    try { it.foreach(f) } finally { it.close() }
  }

  /**
   * Execute the statement and call f for each converted row of the result set.
   *
   * @param maxRows Maximum number of rows to read from the result (0 for unlimited).
   */
  infix final def foreach(param: P, f: R => Unit, maxRows: Int)(using Session): Unit = {
    val it = elementsTo(param, maxRows)
    try { it.foreach(f) } finally { it.close() }
  }

  /**
   * Execute the statement and left-fold the converted rows of the result set.
   */
  infix final def foldLeft[B](param: P, z: B)(op: (B, R) => B)(using Session): B = {
    var _z = z
    foreach(param, { e => _z = op(_z, e) })
    _z
  }

  /**
   * Execute the statement and feed the converted rows of the result set into an iteratee.
   */
  final def enumerate[B, RR >: R](param: P, iter: IterV[RR, B])(using Session): IterV[RR, B] = {
    var _iter = iter
    val it = elements(param)
    try {
      while (it.hasNext && !_iter.isInstanceOf[Done[?, ?]]) {
        val cont = _iter.asInstanceOf[Cont[RR, B]]
        _iter = cont.k(El(it.next()))
      }
    }
    finally it.close()
    _iter
  }

  /**
   * Apply the parameter for this Invoker, creating a parameterless UnitInvoker.
   */
  def apply(parameter: P): UnitInvoker[R] = new AppliedInvoker[P, R] {
    protected val appliedParameter = parameter
    protected val delegate = self
  }

  /**
   * Create a new Invoker which applies the mapping function f to each row
   * of the result set.
   */
  def mapResult[U](f: (R => U)): Invoker[P, U] = new MappedInvoker(this, f)

  /**
   * If the result type of this Invoker is of the form Option[T], execute the statement
   * and return the first row of the result set, or None if the result set is empty.
   */
  def firstFlatten[B](param: P)(using session: Session, ev: R <:< Option[B]): Option[B] =
    firstOption(param).map(ev.apply).getOrElse(None)
}

/**
 * An invoker for a Unit parameter with additional parameterless methods.
 */
trait UnitInvoker[+R] extends Invoker[Unit, R] {
  protected type Param
  protected val appliedParameter: Param
  protected val delegate: Invoker[Param, R]

  infix final def firstOption(using Session): Option[R] = delegate.firstOption(appliedParameter)
  infix final def first()(using Session): R = delegate.first(appliedParameter)
  infix final def list()(using Session): List[R] = delegate.list(appliedParameter)
  infix final def toMap[T, U](using Session, R <:< (T, U)): Map[T, U] = delegate.toMap(appliedParameter)
  infix final def foreach(f: R => Unit)(using Session): Unit = delegate.foreach(appliedParameter, f)
  infix final def foreach(f: R => Unit, maxRows: Int)(using Session): Unit = delegate.foreach(appliedParameter, f, maxRows)
  infix final def elements()(using Session): CloseableIterator[R] = delegate.elements(appliedParameter)
  infix final def elementsTo(maxRows: Int)(using Session): CloseableIterator[R] = delegate.elementsTo(appliedParameter, maxRows)
  infix final def execute()(using Session): Unit = delegate.execute(appliedParameter)
  infix final def foldLeft[B](z: B)(op: (B, R) => B)(using Session): B = delegate.foldLeft(appliedParameter, z)(op)
  infix final def enumerate[B, RR >: R](iter: IterV[RR, B])(using Session): IterV[RR, B] = delegate.enumerate(appliedParameter, iter)

  def firstFlatten[B](using session: Session, ev: R <:< Option[B]): Option[B] =
    firstOption.map(ev.apply).getOrElse(None) //.asInstanceOf[Option[B]]
  override def mapResult[U](f: (R => U)): UnitInvoker[U] = new MappedInvoker(this, f) with UnitInvokerMixin[U]
}

object UnitInvoker {
  val empty: UnitInvoker[Nothing] = new UnitInvokerMixin[Nothing] {
    def elementsTo(param: Unit, maxRows: Int)(using Session) = CloseableIterator.empty
  }
}

trait UnitInvokerMixin[+R] extends UnitInvoker[R] {
  final protected val appliedParameter = ()
  protected val delegate = this
  protected type Param = Unit
}

/**
 * Base trait for applied invokers
 */
trait AppliedInvoker[P, +R] extends UnitInvoker[R] {
  protected type Param = P
  def elementsTo(param: Unit, maxRows: Int)(using Session): CloseableIterator[R] = delegate.elementsTo(appliedParameter, maxRows)
}

/**
 * An Invoker which applies a mapping function to all results of another Invoker.
 */
class MappedInvoker[-P, U, +R](parent: Invoker[P, U], mapper: (U => R)) extends Invoker[P, R] {
  def elementsTo(param: P, maxRows: Int)(using Session) =
    parent.elementsTo(param, maxRows).map(mapper)
}
