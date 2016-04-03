package org.scalaquery.session

import java.sql.{ResultSet, Blob, Clob, Date, Time, Timestamp}
import java.io.Closeable
import org.scalaquery.simple.GetResult
import org.scalaquery.util.CloseableIterator
import org.scalaquery.UnitInvokerMixin
import collection.generic.CanBuildFrom

/**
 * A database result positioned at a row and column.
 */
sealed abstract class PositionedResult(val rs: ResultSet) 
	extends Closeable { outer =>
		
  protected[this] var pos = Int.MaxValue
  protected[this] val startPos = 0

  lazy val numColumns = rs.getMetaData().getColumnCount()

  final def currentPos = pos
  final def hasMoreColumns = pos < numColumns

  final def skip = { pos += 1; this }
  final def restart { pos = startPos }
  final def rewind { pos = Int.MinValue }

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
  	o.map(f).getOrElse(updateNull)
  	
  private def option[T](t: T): Option[T] = 
  	if(rs wasNull) None else Some(t)

  final def << [T](implicit f: GetResult[T]): T = f(this)
  final def <<? [T](implicit f: GetResult[Option[T]]): Option[T] = 
  	if(hasMoreColumns) this.<< else None

  final def nextBigDecimal() = { 
  	val r = apply(rs.getBigDecimal _) 
  	if(r eq null) null else BigDecimal(r)
  }
  final def nextBlob()       = apply(rs.getBlob _)
  final def nextBoolean()    = apply(rs.getBoolean _)
  final def nextByte()       = apply(rs.getByte _)
  final def nextBytes()      = apply(rs.getBytes _)
  final def nextClob()       = apply(rs.getClob _)
  final def nextDate()       = apply(rs.getDate _)
  final def nextDouble()     = apply(rs.getDouble _)
  final def nextFloat()      = apply(rs.getFloat _)
  final def nextInt()        = apply(rs.getInt _)
  final def nextLong()       = apply(rs.getLong _)
  final def nextObject()     = apply(rs.getObject _)
  final def nextShort()      = apply(rs.getShort _)
  final def nextString()     = apply(rs.getString _)
  final def nextTime()       = apply(rs.getTime _)
  final def nextTimestamp()  = apply(rs.getTimestamp _)

  final def nextBigDecimalOption() = option(nextBigDecimal)
  final def nextBlobOption()       = option(nextBlob)
  final def nextBooleanOption()    = option(nextBoolean) 
  final def nextByteOption()       = option(nextByte)
  final def nextBytesOption()      = option(nextBytes)
  final def nextClobOption()       = option(nextClob)
  final def nextDateOption()       = option(nextDate)
  final def nextDoubleOption()     = option(nextDouble)
  final def nextFloatOption()      = option(nextFloat)
  final def nextIntOption()        = option(nextInt)
  final def nextLongOption()       = option(nextLong)
  final def nextObjectOption()     = option(nextObject)
  final def nextShortOption()      = option(nextShort)
  final def nextStringOption()     = option(nextString)
  final def nextTimeOption()       = option(nextTime)
  final def nextTimestampOption()  = option(nextTimestamp)

  final def updateBigDecimal(v: BigDecimal) = apply(
  	rs.updateBigDecimal(_, v.bigDecimal)
  )
  final def updateBlob(v: Blob)             = apply(rs.updateBlob(_, v))
  final def updateBoolean(v: Boolean)       = apply(rs.updateBoolean(_, v))
  final def updateByte(v: Byte)             = apply(rs.updateByte(_, v))
  final def updateBytes(v: Array[Byte])     = apply(rs.updateBytes(_, v))
  final def updateClob(v: Clob)             = apply(rs.updateClob(_, v))
  final def updateDate(v: Date)             = apply(rs.updateDate(_, v))
  final def updateDouble(v: Double)         = apply(rs.updateDouble(_, v))
  final def updateFloat(v: Float)           = apply(rs.updateFloat(_, v))
  final def updateInt(v: Int)               = apply(rs.updateInt(_, v))
  final def updateLong(v: Long)             = apply(rs.updateLong(_, v))
  final def updateObject(v: AnyRef)         = apply(rs.updateObject(_, v))
  final def updateShort(v: Short)           = apply(rs.updateShort(_, v))
  final def updateString(v: String)         = apply(rs.updateString(_, v))
  final def updateTime(v: Time)             = apply(rs.updateTime(_, v))
  final def updateTimestamp(v: Timestamp)   = apply(rs.updateTimestamp(_, v))

  final def updateBigDecimalOption(v: Option[BigDecimal]) = apply(updateBigDecimal _, v)
  final def updateBlobOption(v: Option[Blob])             = apply(updateBlob _, v)
  final def updateBooleanOption(v: Option[Boolean])       = apply(updateBoolean _, v)
  final def updateByteOption(v: Option[Byte])             = apply(updateByte _, v)
  final def updateBytesOption(v: Option[Array[Byte]])     = apply(updateBytes _, v)
  final def updateClobOption(v: Option[Clob])             = apply(updateClob _, v)
  final def updateDateOption(v: Option[Date])             = apply(updateDate _, v)
  final def updateDoubleOption(v: Option[Double])         = apply(updateDouble _, v)
  final def updateFloatOption(v: Option[Float])           = apply(updateFloat _, v)
  final def updateIntOption(v: Option[Int])               = apply(updateInt _, v)
  final def updateLongOption(v: Option[Long])             = apply(updateLong _, v)
  final def updateObjectOption(v: Option[AnyRef])         = apply(updateObject _, v)
  final def updateShortOption(v: Option[Short])           = apply(updateShort _, v)
  final def updateStringOption(v: Option[String])         = apply(updateString _, v)
  final def updateTimeOption(v: Option[Time])             = apply(updateTime _, v)
  final def updateTimestampOption(v: Option[Timestamp])   = apply(updateTimestamp _, v)
  final def updateNull() = apply(rs.updateNull(_))

  /**
   * Close the ResultSet and the statement which created it.
   */
  def close(): Unit

  /**
   * Create an embedded PositionedResult which extends from the given dataPos
   * column until the end of this PositionedResult, starts at the current row
   * and ends when the discriminator predicate (which can read columns starting
   * at discriminatorPos) returns false or when this PositionedResult ends.
   */
  def view(discriminatorPos: Int, dataPos: Int, discriminator: (PositionedResult => Boolean)): PositionedResult = new PositionedResult(rs) {
    override protected[this] val startPos = dataPos
    pos = Int.MinValue
    def close() {}
    override def nextRow = {
      def disc = {
        pos = discriminatorPos
        val ret = discriminator(this)
        pos = startPos
        ret
      }
      if(pos == Int.MinValue) disc else {
        val outerRet = outer.nextRow
        val ret = outerRet && disc
        pos = startPos
        if(!ret && outerRet) outer.rewind
        ret
      }
    }
  }

  /**
   * Create an embedded PositionedResult with a single discriminator column
   * followed by the embedded data, starting at the current position. The
   * embedded view lasts while the discriminator stays the same. If the first
   * discriminator value is NULL, the view is empty.
   */
  def view1: PositionedResult = {
    val discPos = pos
    val disc = nextObject
    view(discPos, discPos+1, { r => disc != null && disc == r.nextObject })
  }

  final def build[C[_], R](gr: GetResult[R])(implicit canBuildFrom: CanBuildFrom[Nothing, R, C[R]]): C[R] = {
    val b = canBuildFrom()
    while(nextRow) b += gr(this)
    b.result()
  }

  final def to[C[_]] = new To[C]()

  final class To[C[_]] private[PositionedResult] () {
    def apply[R](gr: GetResult[R])(implicit session: Session, canBuildFrom: CanBuildFrom[Nothing, R, C[R]]) =
      build[C, R](gr)
  }
}

/**
 * A PositionedResult which can be used as a CloseableIterator.
 */
abstract class PositionedResultIterator[+T](_rs: ResultSet, maxRows: Int) extends PositionedResult(_rs) with CloseableIterator[T] {

  private[this] var done = false
  private[this] var count = 0
  private[this] var closed = false

  final override def nextRow = {
    if(pos == Int.MinValue) super.nextRow else {
      if(maxRows != 0 && count >= maxRows) false
      else {
        val ret = super.nextRow
        if(ret) count += 1 else done = true
        ret
      }
    }
  }

  final def hasNext = {
    val r = !done && ((pos == startPos) || nextRow)
    if(!r) close()
    r
  }

  final def next() = {
    if(done) noNext
    else {
      if(pos != startPos) nextRow
      if(done) noNext
      else {
        val ret = extractValue()
        pos = Int.MaxValue
        ret
      }
    }
  }

  final def close() {
    if(!closed) {
      closeUnderlying()
      closed = true
    }
  }

  protected def extractValue(): T
  protected def closeUnderlying(): Unit

  final override def foreach[U](f: T =>  U) { while(nextRow) f(extractValue()) }
}
