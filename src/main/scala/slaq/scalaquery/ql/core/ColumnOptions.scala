package slaq.ql.core

class ColumnOptions {
  val NotNull = ColumnOption.NotNull
  val Nullable = ColumnOption.Nullable
  val PrimaryKey = ColumnOption.PrimaryKey
  val AutoInc = ColumnOption.AutoInc
  def Default[T](defaultValue: T) = ColumnOption.Default[T](defaultValue)
  def DBType(dbType: String) = ColumnOption.DBType(dbType)
}
object ColumnOptions extends ColumnOptions

enum ColumnOption[+T, -P]:
  case NotNull extends ColumnOption[Nothing, Profile]
  case Nullable extends ColumnOption[Nothing, Profile]
  case PrimaryKey extends ColumnOption[Nothing, Profile]
  case AutoInc extends ColumnOption[Nothing, Profile]
  case Default[T](val defaultValue: T) extends ColumnOption[T, Profile]
  case DBType(val dbType: String) extends ColumnOption[Nothing, Profile]
