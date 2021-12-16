package slaq.session

import java.sql.{PreparedStatement, Date, Time, Timestamp, Types, Blob, Clob}
import slaq.simple.SetParameter

class PositionedParameters(val ps: PreparedStatement) {

  var pos = 0
  private def apply(f: Int => Unit): Unit = {
    val npos = pos + 1
    f(npos)
    pos = npos
  }

  def >>[T](value: T)(using f: SetParameter[T]): Unit = f(value, this)

  def setNull(sqlType: Int) = apply(ps.setNull(_, sqlType))
  def setBlob(value: Blob) = apply(ps.setBlob(_, value))
  def setBoolean(value: Boolean) = apply(ps.setBoolean(_, value))
  def setByte(value: Byte) = apply(ps.setByte(_, value))
  def setBytes(value: Array[Byte]) = apply(ps.setBytes(_, value))
  def setClob(value: Clob) = apply(ps.setClob(_, value))
  def setDate(value: Date) = apply(ps.setDate(_, value))
  def setDouble(value: Double) = apply(ps.setDouble(_, value))
  def setFloat(value: Float) = apply(ps.setFloat(_, value))
  def setInt(value: Int) = apply(ps.setInt(_, value))
  def setLong(value: Long) = apply(ps.setLong(_, value))
  def setShort(value: Short) = apply(ps.setShort(_, value))
  def setString(value: String) = apply(ps.setString(_, value))
  def setTime(value: Time) = apply(ps.setTime(_, value))
  def setTimestamp(value: Timestamp) = apply(ps.setTimestamp(_, value))
  def setBigDecimal(value: BigDecimal) = apply(ps.setBigDecimal(_, value.bigDecimal))
  def setObject(value: AnyRef, sqlType: Int) = apply(ps.setObject(_, value, sqlType))

  def setBlobOption(value: Option[Blob]) =
    value.map(setBlob).getOrElse(setNull(Types.BLOB))

  def setBooleanOption(value: Option[Boolean]) =
    value.map(v => setBoolean(v)).getOrElse(setNull(Types.BOOLEAN))

  def setByteOption(value: Option[Byte]) =
    value.map(setByte).getOrElse(setNull(Types.TINYINT))

  def setBytesOption(value: Option[Array[Byte]]) =
    value.map(setBytes).getOrElse(setNull(Types.BLOB))

  def setClobOption(value: Option[Clob]) =
    value.map(setClob).getOrElse(setNull(Types.CLOB))

  def setDateOption(value: Option[Date]) =
    value.map(setDate).getOrElse(setNull(Types.DATE))

  def setDoubleOption(value: Option[Double]) =
    value.map(setDouble).getOrElse(setNull(Types.DOUBLE))

  def setFloatOption(value: Option[Float]) =
    value.map(setFloat).getOrElse(setNull(Types.FLOAT))

  def setIntOption(value: Option[Int]) =
    value.map(setInt).getOrElse(setNull(Types.INTEGER))

  def setLongOption(value: Option[Long]) =
    value.map(setLong).getOrElse(setNull(Types.INTEGER))

  def setShortOption(value: Option[Short]) =
    value.map(setShort).getOrElse(setNull(Types.SMALLINT))

  def setStringOption(value: Option[String]) =
    value.map(setString).getOrElse(setNull(Types.VARCHAR))

  def setTimeOption(value: Option[Time]) =
    value.map(setTime).getOrElse(setNull(Types.TIME))

  def setTimestampOption(value: Option[Timestamp]) =
    value.map(setTimestamp).getOrElse(setNull(Types.TIMESTAMP))

  def setBigDecimalOption(value: Option[BigDecimal]) =
    value.map(v => setBigDecimal(v.bigDecimal)).
      getOrElse(setNull(Types.DECIMAL))

  def setObjectOption(value: Option[AnyRef], sqlType: Int) =
    value.map(v => setObject(v, sqlType)).getOrElse(setNull(sqlType))
}
