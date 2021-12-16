package slaq.ql

import java.util.UUID
import java.sql.{Blob, Clob, Date, Time, Timestamp}
import slaq.session.{PositionedParameters, PositionedResult}
import slaq.Fail
import slaq.ql.core.Profile

/**
 * A (usually implicit) TypeMapper object represents a Scala type that can be
 * used as a column type in the database. The actual implementation of the
 * type is deferred to a TypeMapperDelegate which can depend on the driver.
 *
 * <p>Custom types with a single implementation can implement both traits in
 * one object:</p>
 * <code><pre>
 * given MyTypeMapper
 *     extends TypeMapper[MyType] with TypeMapperDelegate[MyType] {
 *   def apply(p:Profile) = this
 *   def zero = ...
 *   def sqlType = ...
 *   def setValue(v: Long, p: PositionedParameters) = ...
 *   def setOption(v: Option[Long], p: PositionedParameters) = ...
 *   def nextValue(r: PositionedResult) = ...
 *   def updateValue(v: Long, r: PositionedResult) = ...
 * }
 * </pre></code>
 */
sealed trait TypeMapper[T] extends (Profile => TypeMapperDelegate[T]) { self =>
  def createOptionTypeMapper: OptionTypeMapper[T] =
    new OptionTypeMapper[T](self) {
      def apply(profile: Profile) = self(profile).createOptionTypeMapperDelegate
      def getBaseTypeMapper[U](using Option[U] =:= Option[T]): TypeMapper[U] =
        self.asInstanceOf[TypeMapper[U]]
    }
  def getBaseTypeMapper[U](using Option[U] =:= T): TypeMapper[U]
}

object TypeMapper {

  inline given typeMapper2OptionTypeMapper[T](using t: TypeMapper[T]): OptionTypeMapper[T] =
    t.createOptionTypeMapper

  //import godenji.iso._
  //inline inline given mappableType[T <: MappedTo[_]](using TypeMapper[IdT[T]]): Conversion[Isomorphism[T], BaseTypeMapper[T]] =
  //  (iso: Isomorphism[T]) => MappedTypeMapper.base[T, IdT[T]](iso.map, iso.comap)

  inline given BooleanTypeMapper: BaseTypeMapper[Boolean] with {
    def apply(profile: Profile) = profile.typeMapperDelegates.booleanTypeMapperDelegate
  }

  inline given BlobTypeMapper: BaseTypeMapper[Blob] with {
    def apply(profile: Profile) = profile.typeMapperDelegates.blobTypeMapperDelegate
  }

  inline given ByteTypeMapper: BaseTypeMapper[Byte] with NumericTypeMapper with {
    def apply(profile: Profile) = profile.typeMapperDelegates.byteTypeMapperDelegate
  }

  inline given ByteArrayTypeMapper: BaseTypeMapper[Array[Byte]] with {
    def apply(profile: Profile) = profile.typeMapperDelegates.byteArrayTypeMapperDelegate
  }

  inline given ClobTypeMapper: BaseTypeMapper[Clob] with {
    def apply(profile: Profile) = profile.typeMapperDelegates.clobTypeMapperDelegate
  }

  inline given DateTypeMapper: BaseTypeMapper[Date] with {
    def apply(profile: Profile) = profile.typeMapperDelegates.dateTypeMapperDelegate
  }

  inline given DoubleTypeMapper: BaseTypeMapper[Double] with NumericTypeMapper with {
    def apply(profile: Profile) = profile.typeMapperDelegates.doubleTypeMapperDelegate
  }

  inline given FloatTypeMapper: BaseTypeMapper[Float] with NumericTypeMapper with {
    def apply(profile: Profile) = profile.typeMapperDelegates.floatTypeMapperDelegate
  }

  inline given IntTypeMapper: BaseTypeMapper[Int] with NumericTypeMapper with {
    def apply(profile: Profile) = profile.typeMapperDelegates.intTypeMapperDelegate
  }

  inline given LongTypeMapper: BaseTypeMapper[Long] with NumericTypeMapper with {
    def apply(profile: Profile) = profile.typeMapperDelegates.longTypeMapperDelegate
  }

  inline given ShortTypeMapper: BaseTypeMapper[Short] with {
    def apply(profile: Profile) = profile.typeMapperDelegates.shortTypeMapperDelegate
  }

  inline given StringTypeMapper: BaseTypeMapper[String] with {
    def apply(profile: Profile) = profile.typeMapperDelegates.stringTypeMapperDelegate
  }

  inline given TimeTypeMapper: BaseTypeMapper[Time] with {
    def apply(profile: Profile) = profile.typeMapperDelegates.timeTypeMapperDelegate
  }

  inline given TimestampTypeMapper: BaseTypeMapper[Timestamp] with {
    def apply(profile: Profile) = profile.typeMapperDelegates.timestampTypeMapperDelegate
  }

  inline given UnitTypeMapper: BaseTypeMapper[Unit] with {
    def apply(profile: Profile) = profile.typeMapperDelegates.unitTypeMapperDelegate
  }

  inline given UUIDTypeMapper: BaseTypeMapper[UUID] with {
    def apply(profile: Profile) = profile.typeMapperDelegates.uuidTypeMapperDelegate
  }

  inline given BigDecimalTypeMapper: BaseTypeMapper[BigDecimal] with {
    def apply(profile: Profile) = profile.typeMapperDelegates.bigDecimalTypeMapperDelegate
  }

  object NullTypeMapper extends BaseTypeMapper[Null] {
    def apply(profile: Profile) = profile.typeMapperDelegates.nullTypeMapperDelegate
  }
}

trait BaseTypeMapper[T] extends TypeMapper[T] {
  def getBaseTypeMapper[U](using Option[U] =:= T) =
    Fail("A BaseTypeMapper should not have an Option type")
}

abstract class OptionTypeMapper[T](val base: TypeMapper[T]) extends TypeMapper[Option[T]]

trait NumericTypeMapper

trait TypeMapperDelegate[T] { self =>
  /**
   * A zero value for the type. This is used as a default instead of NULL when
   * used as a non-nullable column.
   */
  def zero: T
  /**
   * The constant from java.sql.Types that is used for setting parameters of
   * the type to NULL.
   */
  def sqlType: Int
  /**
   * The default name for the SQL type that is used for column declarations.
   */
  def sqlTypeName: String = TypeMapperDelegate.typeNames.getOrElse(sqlType, Fail(
    s"No SQL type name found in java.sql.Types for code $sqlType"
  ))
  /**
   * Set a parameter of the type.
   */
  def setValue(v: T, p: PositionedParameters): Unit
  /**
   * Set an Option parameter of the type.
   */
  def setOption(v: Option[T], p: PositionedParameters): Unit
  /**
   * Get a result column of the type.
   */
  def nextValue(r: PositionedResult): T
  /**
   * Update a column of the type in a mutable result set.
   */
  def updateValue(v: T, r: PositionedResult): Unit
  def nextValueOrElse(d: => T, r: PositionedResult) = {
    val v = nextValue(r); if (r.rs wasNull) d else v
  }
  def nextOption(r: PositionedResult): Option[T] = {
    val v = nextValue(r); if (r.rs wasNull) None else Some(v)
  }
  def updateOption(v: Option[T], r: PositionedResult): Unit = v match {
    case Some(s) => updateValue(s, r)
    case None    => r.updateNull()
  }
  def value2SQLLiteral(value: T): String = value.toString
  def nullable = false

  def createOptionTypeMapperDelegate: TypeMapperDelegate[Option[T]] =
    new TypeMapperDelegate[Option[T]] {
      def zero = None
      def sqlType = self.sqlType
      override def sqlTypeName = self.sqlTypeName
      def setValue(v: Option[T], p: PositionedParameters) = self.setOption(v, p)
      def setOption(v: Option[Option[T]], p: PositionedParameters) =
        self.setOption(v.getOrElse(None), p)
      def nextValue(r: PositionedResult) = self.nextOption(r)
      def updateValue(v: Option[T], r: PositionedResult) = self.updateOption(v, r)
      override def value2SQLLiteral(value: Option[T]): String =
        value.map(self.value2SQLLiteral).getOrElse("null")
      override def nullable = true
    }
}

object TypeMapperDelegate {
  private[slaq] lazy val typeNames = Map() ++
    (for (f <- classOf[java.sql.Types].getFields)
      yield f.get(null).asInstanceOf[Int] -> f.getName)
}

abstract class MappedTypeMapper[T, U](using tm: TypeMapper[U])
  extends TypeMapper[T] { self =>

  def map(t: T): U
  def comap(u: U): T

  def sqlType: Option[Int] = None
  def sqlTypeName: Option[String] = None
  def value2SQLLiteral(value: T): Option[String] = None
  def nullable: Option[Boolean] = None

  def apply(profile: Profile): TypeMapperDelegate[T] = new TypeMapperDelegate[T] {
    val tmd = tm(profile)
    def zero = comap(tmd.zero)
    def sqlType = self.sqlType.getOrElse(tmd.sqlType)
    override def sqlTypeName = self.sqlTypeName.getOrElse(tmd.sqlTypeName)
    def setValue(v: T, p: PositionedParameters) = tmd.setValue(map(v), p)
    def setOption(v: Option[T], p: PositionedParameters) = tmd.setOption(v.map(map _), p)
    def nextValue(r: PositionedResult) = comap(tmd.nextValue(r))
    def updateValue(v: T, r: PositionedResult) = tmd.updateValue(map(v), r)
    override def value2SQLLiteral(value: T) =
      self.value2SQLLiteral(value).getOrElse(tmd.value2SQLLiteral(map(value)))
    override def nullable = self.nullable.getOrElse(tmd.nullable)
  }
}

object MappedTypeMapper {
  inline def base[T, U](to: T => U, from: U => T)(using TypeMapper[U]): BaseTypeMapper[T] =
    new MappedTypeMapper[T, U] with BaseTypeMapper[T] {
      def map(t: T) = to(t)
      def comap(u: U) = from(u)
    }
}
