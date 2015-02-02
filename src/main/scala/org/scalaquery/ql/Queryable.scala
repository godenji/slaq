package org.scalaquery.ql

/**
 * a type class that restricts Query monad filter operations
 * to Columnar values; i.e. query condition must be between Column[_]
 */
trait Queryable[-T] {
  def apply(value: T, l: List[Column[_]]): List[Column[_]]
}
object Queryable {
  implicit object BooleanColumnQueryable 
  	extends Queryable[Column[Boolean]] {
    @inline def apply(
    	value: Column[Boolean], l: List[Column[_]]
    ): List[Column[_]] = value :: l
  }
  implicit object BooleanOptionColumnQueryable 
  	extends Queryable[Column[Option[Boolean]]] {
    @inline def apply(
    	value: Column[Option[Boolean]], l: List[Column[_]]
    ): List[Column[_]] = value :: l
  }
  implicit object BooleanQueryable 
  	extends Queryable[Boolean] {
    @inline def apply(value: Boolean, l: List[Column[_]]): List[Column[_]] =
      if(value) l 
      else new ConstColumn(false)(TypeMapper.BooleanTypeMapper) :: Nil
  }
}