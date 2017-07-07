package org.scalaquery.ql.core

class ColumnOptions {
  val NotNull = ColumnOption.NotNull
  val Nullable = ColumnOption.Nullable
  val PrimaryKey = ColumnOption.PrimaryKey
  val AutoInc = ColumnOption.AutoInc
  def Default[T](defaultValue: T) = ColumnOption.Default[T](defaultValue)
  def DBType(dbType: String) = ColumnOption.DBType(dbType)
}
object ColumnOptions extends ColumnOptions

abstract class ColumnOption[+T, -P]
object ColumnOption {
  case object NotNull extends ColumnOption[Nothing, Profile]
  case object Nullable extends ColumnOption[Nothing, Profile]
  case object PrimaryKey extends ColumnOption[Nothing, Profile]
  case object AutoInc extends ColumnOption[Nothing, Profile]
  case class Default[T](val defaultValue: T)
    extends ColumnOption[T, Profile]

  case class DBType(val dbType: String)
    extends ColumnOption[Nothing, Profile]
}
