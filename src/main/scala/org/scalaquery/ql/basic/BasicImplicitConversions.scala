package org.scalaquery.ql.basic

import org.scalaquery.ql._
import org.scalaquery.util.Node

trait BasicImplicitConversions[DriverType <: BasicProfile] {
  implicit val scalaQueryDriver: DriverType

  @inline implicit final def baseColumnToColumnOps[B1 : BaseTypeMapper](c: Column[B1]): ColumnOps[B1, B1] = c match {
    case o: ColumnOps[_,_] => o.asInstanceOf[ColumnOps[B1, B1]]
    case _ => new ColumnOps[B1, B1] { protected[this] val leftOperand = Node(c) }
  }

  @inline implicit final def optionColumnToColumnOps[B1](c: Column[Option[B1]]): ColumnOps[B1, Option[B1]] = c match {
    case o: ColumnOps[_,_] => o.asInstanceOf[ColumnOps[B1, Option[B1]]]
    case _ => new ColumnOps[B1, Option[B1]] { protected[this] val leftOperand = Node(c) }
  }

  @inline implicit final def columnToOptionColumn[T : BaseTypeMapper](c: Column[T]): Column[Option[T]] = c.?

  @inline implicit final def valueToConstColumn[T : TypeMapper](v: T) = new ConstColumn[T](v)

  @inline implicit final def tableToQuery[T <: TableBase[_], U](t: T) = 
  	Query[T, Nothing](t.mapOp(n => new AbstractTable.Alias(Node(n))))(Unpack.unpackTableBase)

  @inline implicit final def columnToOrdering(c: Column[_]): Ordering = Ordering.Asc(Node(c))

  @inline implicit final def queryToQueryInvoker[T, U](q: Query[T, U]): BasicQueryInvoker[T, U] = 
  	new BasicQueryInvoker(q, scalaQueryDriver)
  
  @inline implicit final def queryToDeleteInvoker[T](q: Query[BasicTable[T], T]): BasicDeleteInvoker[T] = 
  	new BasicDeleteInvoker(q, scalaQueryDriver)
  
  @inline implicit final def productQueryToUpdateInvoker[T](q: Query[ColumnBase[T], T]): BasicUpdateInvoker[T] = 
  	new BasicUpdateInvoker(q, scalaQueryDriver)
  
  @inline implicit final def namedColumnQueryToUpdateInvoker[T](q: Query[_ <: NamedColumn[T], T]): BasicUpdateInvoker[T] = 
  	new BasicUpdateInvoker(q, scalaQueryDriver)
  
  @inline implicit final def columnBaseToInsertInvoker[T](c: ColumnBase[T]) = 
  	new BasicInsertInvoker(c.toUnpackable, scalaQueryDriver)
  
  @inline implicit final def unpackableToInsertInvoker[T, U](u: Unpackable[T, U]) = 
  	new BasicInsertInvoker(u, scalaQueryDriver)

  implicit final class NodeLike2Unpackable[T <: ColumnBase[_]](t: T){
  	@inline def toUnpackable[U](implicit unpack: Unpack[T, U]) = new Unpackable[T, U](t, unpack)
  }
}
