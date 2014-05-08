package org.scalaquery.ql.basic

import org.scalaquery.ql._
import org.scalaquery.util.Node

trait BasicImplicitConversions[DriverType <: BasicProfile] {
  implicit val scalaQueryDriver: DriverType

  @inline implicit def baseColumnToColumnOps[B1 : BaseTypeMapper](c: Column[B1]): ColumnOps[B1, B1] = c match {
    case o: ColumnOps[_,_] => o.asInstanceOf[ColumnOps[B1, B1]]
    case _ => new ColumnOps[B1, B1] { protected[this] val leftOperand = Node(c) }
  }

  @inline implicit def optionColumnToColumnOps[B1](c: Column[Option[B1]]): ColumnOps[B1, Option[B1]] = c match {
    case o: ColumnOps[_,_] => o.asInstanceOf[ColumnOps[B1, Option[B1]]]
    case _ => new ColumnOps[B1, Option[B1]] { protected[this] val leftOperand = Node(c) }
  }

  @inline implicit def columnToOptionColumn[T : BaseTypeMapper](c: Column[T]): Column[Option[T]] = c.?

  @inline implicit def valueToConstColumn[T : TypeMapper](v: T) = new ConstColumn[T](v)

  @inline implicit def tableToQuery[T <: TableBase[_], U](t: T) = 
  	Query[T, Nothing](t.mapOp(n => new AbstractTable.Alias(Node(n))))(Unpack.unpackTableBase)

  @inline implicit def columnToOrdering(c: Column[_]): Ordering = Ordering.Asc(Node(c))

  @inline implicit def queryToQueryInvoker[T, U](q: Query[T, U]): BasicQueryInvoker[T, U] = 
  	new BasicQueryInvoker(q, scalaQueryDriver)
  
  @inline implicit def queryToDeleteInvoker[T](q: Query[BasicTable[T], T]): BasicDeleteInvoker[T] = 
  	new BasicDeleteInvoker(q, scalaQueryDriver)
  
  @inline implicit def productQueryToUpdateInvoker[T](q: Query[ColumnBase[T], T]): BasicUpdateInvoker[T] = 
  	new BasicUpdateInvoker(q, scalaQueryDriver)
  
  @inline implicit def namedColumnQueryToUpdateInvoker[T](q: Query[_ <: NamedColumn[T], T]): BasicUpdateInvoker[T] = 
  	new BasicUpdateInvoker(q, scalaQueryDriver)
  
  @inline implicit def columnBaseToInsertInvoker[T](c: ColumnBase[T]) = 
  	new BasicInsertInvoker(c.toUnpackable, scalaQueryDriver)
  
  @inline implicit def unpackableToInsertInvoker[T, U](u: Unpackable[T, U]) = 
  	new BasicInsertInvoker(u, scalaQueryDriver)

  @inline implicit class NodeLike2Unpackable[T <: ColumnBase[_]](t: T){
  	def toUnpackable[U](implicit unpack: Unpack[T, U]) = new Unpackable[T, U](t, unpack)
  }
}
