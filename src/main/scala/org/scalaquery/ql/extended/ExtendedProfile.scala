package org.scalaquery.ql.extended

import org.scalaquery.ql._
import org.scalaquery.ql.basic._
import org.scalaquery.util.NullaryNode

trait ExtendedProfile extends BasicProfile {
  type ImplicitT <: ExtendedImplicitConversions[_ <: ExtendedProfile]
}

abstract class AbstractExtendedTable[T](_schemaName: Option[String], _tableName: String) 
	extends AbstractBasicTable[T](_schemaName, _tableName) {
	
  type ProfileType <: ExtendedProfile
  override val O: ExtendedColumnOptions = ExtendedColumnOptions
}

abstract class ExtendedTable[T](_schemaName: Option[String], _tableName: String) 
	extends AbstractExtendedTable[T](_schemaName, _tableName) {
	
  def this(_tableName: String) = this(None, _tableName)
  type ProfileType = ExtendedProfile
}

/*
 * additional implicit functionality for capable databases
 */
trait ExtendedImplicitConversions[DriverType <: ExtendedProfile] 
	extends BasicImplicitConversions[DriverType] {
  
  @inline implicit final def extendedQueryToDeleteInvoker[T](q: Query[ExtendedTable[T], T]): BasicDeleteInvoker[T] = 
  	new BasicDeleteInvoker(q, scalaQueryDriver)
}

object ExtendedQueryOps {}

/*
 * DDL handling
 */
class ExtendedColumnOptions extends BasicColumnOptions with ExtendedColumn {
  val AutoInc = ExtendedColumnOption.AutoInc
}
object ExtendedColumnOptions extends ExtendedColumnOptions

object ExtendedColumnOption {
	case object AutoInc extends ColumnOption[Nothing, ExtendedProfile]
}

trait ExtendedColumn {	
	protected var autoIncrement = false
	protected def handleColumnOption(o: ColumnOption[_,_]): Unit = o match {
		case ExtendedColumnOption.AutoInc => autoIncrement = true
		case _ =>
	}
}
