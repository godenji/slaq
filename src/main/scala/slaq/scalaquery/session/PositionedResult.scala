package slaq.session

import java.sql.{ResultSet, Blob, Clob, Date, Time, Timestamp}
import java.io.Closeable
import slaq.simple.GetResult
import slaq.util.CloseableIterator
import collection.Factory

/**
 * A database result positioned at a row and column.
 */
sealed abstract class PositionedResult(val rs: ResultSet)
  extends Closeable { outer =>

  protected var pos = Int.MaxValue
  protected val startPos = 0

  lazy val numColumns = rs.getMetaData().getColumnCount()

  final def currentPos = pos
  final def hasMoreColumns = pos < numColumns

  final def skip = { pos += 1; this }
  final def restart: Unit = { pos = startPos }
  final def rewind: Unit = { pos = Int.MinValue }

  def nextRow = {
    val ret = (pos == Int.MinValue) || rs.next
    pos = startPos
    ret
  }

  private def apply[T](f: Int => T): T = {
    val npos = pos + 1
    val r = f(npos)
    pos = npos
    r
  }

  private def apply[T](f: T => Unit, o: Option[T]): Unit =
    o.map(f).getOrElse(updateNull())

  private def option[T](t: T): Option[T] =
    if (rs wasNull) None else Some(t)

  final def <<[T](using f: GetResult[T]): T = f(this)
  final def <<?[T](using GetResult[Option[T]]): Option[T] =
    if (hasMoreColumns) this.<< else None

  final def nextBigDecimal() = {
    val r = apply(rs.getBigDecimal)
    if (r eq null) null else BigDecimal(r)
  }
  final def nextBlob() = apply(rs.getBlob)
  final def nextBoolean() = apply(rs.getBoolean)
  final def nextByte() = apply(rs.getByte)
  final def nextBytes() = apply(rs.getBytes)
  final def nextClob() = apply(rs.getClob)
  final def nextDate() = apply(rs.getDate)
  final def nextDouble() = apply(rs.getDouble)
  final def nextFloat() = apply(rs.getFloat)
  final def nextInt() = apply(rs.getInt)
  final def nextLong() = apply(rs.getLong)
  final def nextObject() = apply(rs.getObject)
  final def nextShort() = apply(rs.getShort)
  final def nextString() = apply(rs.getString)
  final def nextTime() = apply(rs.getTime)
  final def nextTimestamp() = apply(rs.getTimestamp)

  final def nextBigDecimalOption() = option(nextBigDecimal())
  final def nextBlobOption() = option(nextBlob())
  final def nextBooleanOption() = option(nextBoolean())
  final def nextByteOption() = option(nextByte())
  final def nextBytesOption() = option(nextBytes())
  final def nextClobOption() = option(nextClob())
  final def nextDateOption() = option(nextDate())
  final def nextDoubleOption() = option(nextDouble())
  final def nextFloatOption() = option(nextFloat())
  final def nextIntOption() = option(nextInt())
  final def nextLongOption() = option(nextLong())
  final def nextObjectOption() = option(nextObject())
  final def nextShortOption() = option(nextShort())
  final def nextStringOption() = option(nextString())
  final def nextTimeOption() = option(nextTime())
  final def nextTimestampOption() = option(nextTimestamp())

  final def updateBigDecimal(v: BigDecimal) = apply(
    rs.updateBigDecimal(_, v.bigDecimal)
  )
  final def updateBlob(v: Blob) = apply(rs.updateBlob(_, v))
  final def updateBoolean(v: Boolean) = apply(rs.updateBoolean(_, v))
  final def updateByte(v: Byte) = apply(rs.updateByte(_, v))
  final def updateBytes(v: Array[Byte]) = apply(rs.updateBytes(_, v))
  final def updateClob(v: Clob) = apply(rs.updateClob(_, v))
  final def updateDate(v: Date) = apply(rs.updateDate(_, v))
  final def updateDouble(v: Double) = apply(rs.updateDouble(_, v))
  final def updateFloat(v: Float) = apply(rs.updateFloat(_, v))
  final def updateInt(v: Int) = apply(rs.updateInt(_, v))
  final def updateLong(v: Long) = apply(rs.updateLong(_, v))
  final def updateObject(v: AnyRef) = apply(rs.updateObject(_, v))
  final def updateShort(v: Short) = apply(rs.updateShort(_, v))
  final def updateString(v: String) = apply(rs.updateString(_, v))
  final def updateTime(v: Time) = apply(rs.updateTime(_, v))
  final def updateTimestamp(v: Timestamp) = apply(rs.updateTimestamp(_, v))

  final def updateBigDecimalOption(v: Option[BigDecimal]) = apply(updateBigDecimal, v)
  final def updateBlobOption(v: Option[Blob]) = apply(updateBlob, v)
  final def updateBooleanOption(v: Option[Boolean]) = apply(updateBoolean, v)
  final def updateByteOption(v: Option[Byte]) = apply(updateByte, v)
  final def updateBytesOption(v: Option[Array[Byte]]) = apply(updateBytes, v)
  final def updateClobOption(v: Option[Clob]) = apply(updateClob, v)
  final def updateDateOption(v: Option[Date]) = apply(updateDate, v)
  final def updateDoubleOption(v: Option[Double]) = apply(updateDouble, v)
  final def updateFloatOption(v: Option[Float]) = apply(updateFloat, v)
  final def updateIntOption(v: Option[Int]) = apply(updateInt, v)
  final def updateLongOption(v: Option[Long]) = apply(updateLong, v)
  final def updateObjectOption(v: Option[AnyRef]) = apply(updateObject, v)
  final def updateShortOption(v: Option[Short]) = apply(updateShort, v)
  final def updateStringOption(v: Option[String]) = apply(updateString, v)
  final def updateTimeOption(v: Option[Time]) = apply(updateTime, v)
  final def updateTimestampOption(v: Option[Timestamp]) = apply(updateTimestamp, v)
  final def updateNull() = apply(rs.updateNull(_))

  /**
   * Close the ResultSet and the statement which created it.
   */
  infix def close(): Unit

  final def build[C[_], R](gr: GetResult[R])(using canBuildFrom: Factory[R, C[R]]): C[R] = {
    val b = canBuildFrom.newBuilder
    while (nextRow) b += gr(this)
    b.result()
  }

  final def to[C[_]] = new To[C]()

  final class To[C[_]] private[PositionedResult] () {
    def apply[R](gr: GetResult[R])(using Factory[R, C[R]]) =
      build[C, R](gr)
  }
}

/**
 * A PositionedResult which can be used as a CloseableIterator.
 */
abstract class PositionedResultIterator[+T](_rs: ResultSet, maxRows: Int)
  extends PositionedResult(_rs) with CloseableIterator[T] {

  private var done = false
  private var count = 0
  private var closed = false

  final override def nextRow = {
    if (pos == Int.MinValue) super.nextRow else {
      if (maxRows != 0 && count >= maxRows) false
      else {
        val ret = super.nextRow
        if (ret) count += 1 else done = true
        ret
      }
    }
  }

  final def hasNext = {
    val r = !done && ((pos == startPos) || nextRow)
    if (!r) close()
    r
  }

  final def next() = {
    if (done) noNext
    else {
      if (pos != startPos) nextRow
      if (done) noNext
      else {
        val ret = extractValue()
        pos = Int.MaxValue
        ret
      }
    }
  }

  infix final def close(): Unit = {
    if (!closed) {
      closeUnderlying()
      closed = true
    }
  }

  protected def extractValue(): T
  protected def closeUnderlying(): Unit

  final override def foreach[U](f: T => U): Unit = { while (nextRow) f(extractValue()) }
}
