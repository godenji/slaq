package org.scalaquery.ql.core

import org.scalaquery.ql.{ColumnOption=> _ColumnOption}

class ColumnOptions {
  val NotNull 		= ColumnOption.NotNull
  val Nullable 		= ColumnOption.Nullable
  val PrimaryKey 	= ColumnOption.PrimaryKey
  val AutoInc 		= ColumnOption.AutoInc
  def Default[T](defaultValue: T) = ColumnOption.Default[T](defaultValue)
  def DBType(dbType: String) = ColumnOption.DBType(dbType)
}
object ColumnOptions extends ColumnOptions

object ColumnOption {
  case object NotNull 		extends _ColumnOption[Nothing, Profile]
  case object Nullable 		extends _ColumnOption[Nothing, Profile]
  case object PrimaryKey 	extends _ColumnOption[Nothing, Profile]
  case object AutoInc 		extends _ColumnOption[Nothing, Profile]
  case class Default[T](val defaultValue: T) 
  	extends _ColumnOption[T, Profile]
  
  case class DBType(val dbType: String) 
  	extends _ColumnOption[Nothing, Profile]
}
