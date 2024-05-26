package slaq.ql.core

import slaq.ql._
import slaq.util.Node

trait ImplicitConversions[DriverType <: Profile] {
  given driverType: DriverType

  given table2Query[T <: Table[?], U]: Conversion[T, Query[T, U]] with
    inline def apply(x: T) =
      Query.apply[T, U](x.mapOp(Table.Alias.apply))(using Unpack.unpackTable[T, U])

  given column2ColumnOps[B1: BaseTypeMapper]: Conversion[Column[B1], ColumnOps[B1, B1]] =
    (c: Column[B1]) => new ColumnOps[B1, B1] {
      protected val leftOperand = Node(c)
    }

  given optionColumn2ColumnOps[B1]: Conversion[Column[Option[B1]], ColumnOps[B1, Option[B1]]] =
    (c: Column[Option[B1]]) => new ColumnOps[B1, Option[B1]] {
      protected val leftOperand = Node(c)
    }

  given column2OptionColumn[T: BaseTypeMapper]: Conversion[Column[T], Column[Option[T]]] with
    inline def apply(c: Column[T]) = c.?

  given value2ConstColumn[T: TypeMapper]: Conversion[T, ConstColumn[T]] with
    inline def apply(v: T) = new ConstColumn[T](v)

  given column2Ordering: Conversion[Column[?], Ordering] with
    inline def apply(c: Column[?]) = Ordering.Asc(Node(c))

  given query2QueryInvoker[T, U]: Conversion[Query[T, U], QueryInvoker[T, U]] with
    inline def apply(q: Query[T, U]) = new QueryInvoker(q, driverType)

  given query2DeleteInvoker[T]: Conversion[Query[Table[T], T], DeleteInvoker[T]] with
    inline def apply(q: Query[Table[T], T]) = new DeleteInvoker(q, driverType)

  // inline given namedColumnQuery2UpdateInvoker[T]: Conversion[Query[_ <: NamedColumn[T], T], UpdateInvoker[T]] =
  //   (q: Query[_ <: NamedColumn[T], T]) => new UpdateInvoker(q, driverType)

  given productQuery2UpdateInvoker[T]: Conversion[Query[ColumnBase[T], T], UpdateInvoker[T]] with
    inline def apply(q: Query[ColumnBase[T], T]) = new UpdateInvoker(q, driverType)

  given columnBase2InsertInvoker[T]: Conversion[ColumnBase[T], InsertInvoker[ColumnBase[T], T]] with
    inline def apply(c: ColumnBase[T]) = new InsertInvoker(c.toUnpackable, driverType)

  given unpackable2InsertInvoker[T, U]: Conversion[Unpackable[T, U], InsertInvoker[T, U]] with
    inline def apply(u: Unpackable[T, U]) = new InsertInvoker(u, driverType)

  extension [T <: ColumnBase[?]](t: T) {
    def toUnpackable[U](using unpack: Unpack[T, U]): Unpackable[T, U] = new Unpackable[T, U](t, unpack)
  }

  extension [T <: Table[?]](t: T) {
    def createFinderBy[P]
      (f: T => NamedColumn[P])
      (using profile: Profile, tm: TypeMapper[P]): QueryTemplate[P, t.TableType] =

      Params[P](using tm).flatMap[t.TableType] { p =>
        t.filter(t => ColumnOps.Is(f(t), p))
      }
  }

}
