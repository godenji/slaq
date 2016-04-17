package org.scalaquery.ql.core

import org.scalaquery.ql._
import org.scalaquery.util.Node

trait ImplicitConversions[DriverType <: Profile] {
  implicit val driverType: DriverType
  
  @inline implicit final 
  	def table2Query[T <: Table[_], U](t: T): Query[T, U] =
  		Query[T, U](t.mapOp(Table.Alias))(Unpack.unpackTable)

  @inline implicit final 
  	def column2ColumnOps[B1 : BaseTypeMapper](c: Column[B1]): 
  		ColumnOps[B1,B1] = new ColumnOps[B1, B1] {
	    	protected[this] val leftOperand = Node(c)
	   	}

  @inline implicit final 
  	def optionColumn2ColumnOps[B1](c: Column[Option[B1]]): 
  		ColumnOps[B1, Option[B1]] = new ColumnOps[B1, Option[B1]] {
	    	protected[this] val leftOperand = Node(c)
	    }

  @inline implicit final def column2OptionColumn[T : BaseTypeMapper]
  	(c: Column[T]): Column[Option[T]] = c.?

  @inline implicit final def value2ConstColumn[T : TypeMapper]
  	(v: T): ConstColumn[T] = new ConstColumn[T](v)

  @inline implicit final
  	def column2Ordering(c: Column[_]): Ordering = Ordering.Asc(Node(c))

  @inline implicit final
  	def query2QueryInvoker[T, U](q: Query[T, U]): QueryInvoker[T, U] = 
  		new QueryInvoker(q, driverType)
  
  @inline implicit final 
  	def query2DeleteInvoker[T](q: Query[Table[T], T]): 
  		DeleteInvoker[T] = new DeleteInvoker(q, driverType)
  
  @inline implicit final def productQuery2UpdateInvoker[T]
  	(q: Query[ColumnBase[T], T]): 
  		UpdateInvoker[T] = new UpdateInvoker(q, driverType)
  
  @inline implicit final def namedColumnQuery2UpdateInvoker[T]
  	(q: Query[_ <: NamedColumn[T], T]): UpdateInvoker[T] = 
  		new UpdateInvoker(q, driverType)
  
  @inline implicit final def columnBase2InsertInvoker[T]
  	(c: ColumnBase[T]): InsertInvoker[ColumnBase[T], T] = 
  		new InsertInvoker(c.toUnpackable, driverType)
  
  @inline implicit final def unpackable2InsertInvoker[T, U]
  	(u: Unpackable[T, U]): InsertInvoker[T, U] = 
  		new InsertInvoker(u, driverType)

  implicit final class NodeLike2Unpackable[T <: ColumnBase[_]](t: T){
  	@inline def toUnpackable[U](implicit unpack: Unpack[T, U]):
  		Unpackable[T, U] = new Unpackable[T, U](t, unpack)
  }
}
