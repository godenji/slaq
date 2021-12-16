package slaq.ql

/**
 * a type class that restricts Query monad filter operations
 */
trait Queryable[-T] {
  def apply(value: T, l: List[Column[_]]): List[Column[_]]
}
object Queryable {
  inline given BooleanColumnQueryable: Queryable[Column[Boolean]] with {
    inline def apply(
      value: Column[Boolean], l: List[Column[_]]
    ): List[Column[_]] = value :: l
  }
  inline given BooleanOptionColumnQueryable: Queryable[Column[Option[Boolean]]] with {
    inline def apply(
      value: Column[Option[Boolean]], l: List[Column[_]]
    ): List[Column[_]] = value :: l
  }
  inline given BooleanQueryable: Queryable[Boolean] with {
    inline def apply(value: Boolean, l: List[Column[_]]): List[Column[_]] =
      if (value) l
      else new ConstColumn(false)(TypeMapper.BooleanTypeMapper) :: Nil
  }
}
