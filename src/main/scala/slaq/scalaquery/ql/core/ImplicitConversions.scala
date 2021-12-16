package slaq.ql.core

import slaq.ql._
import slaq.util.Node

trait ImplicitConversions[DriverType <: Profile] {
  given driverType: DriverType

  inline given table2Query[T <: Table[_], U]: Conversion[T, Query[T, U]] =
    (x: T) =>
      Query.apply[T, U](x.mapOp(Table.Alias.apply))(using Unpack.unpackTable[T, U])

  inline given column2ColumnOps[B1: BaseTypeMapper]: Conversion[Column[B1], ColumnOps[B1, B1]] =
    (c: Column[B1]) => new ColumnOps[B1, B1] {
      protected[this] val leftOperand = Node(c)
    }

  inline given optionColumn2ColumnOps[B1]: Conversion[Column[Option[B1]], ColumnOps[B1, Option[B1]]] =
    (c: Column[Option[B1]]) => new ColumnOps[B1, Option[B1]] {
    protected[this] val leftOperand = Node(c)
  }

  inline given column2OptionColumn[T: BaseTypeMapper]: Conversion[Column[T], Column[Option[T]]] =
    (c: Column[T]) => c.?

  inline given value2ConstColumn[T: TypeMapper]: Conversion[T, ConstColumn[T]] =
    (v: T) => new ConstColumn[T](v)

  inline given column2Ordering: Conversion[Column[_], Ordering] =
    (c: Column[_]) => Ordering.Asc(Node(c))

  inline given query2QueryInvoker[T, U]: Conversion[Query[T, U], QueryInvoker[T, U]] =
    (q: Query[T, U]) => new QueryInvoker(q, driverType)

  inline given query2DeleteInvoker[T]: Conversion[Query[Table[T], T], DeleteInvoker[T]] =
    (q: Query[Table[T], T]) => new DeleteInvoker(q, driverType)

  // inline given namedColumnQuery2UpdateInvoker[T]: Conversion[Query[_ <: NamedColumn[T], T], UpdateInvoker[T]] =
  //   (q: Query[_ <: NamedColumn[T], T]) => new UpdateInvoker(q, driverType)

  inline given productQuery2UpdateInvoker[T]: Conversion[Query[ColumnBase[T], T], UpdateInvoker[T]] =
    (q: Query[ColumnBase[T], T]) => new UpdateInvoker(q, driverType)

  inline given columnBase2InsertInvoker[T]: Conversion[ColumnBase[T], InsertInvoker[ColumnBase[T], T]] =
    (c: ColumnBase[T]) => new InsertInvoker(c.toUnpackable, driverType)

  inline given unpackable2InsertInvoker[T, U]: Conversion[Unpackable[T, U], InsertInvoker[T, U]] =
    (u: Unpackable[T, U]) => new InsertInvoker(u, driverType)

  extension [T <: ColumnBase[_]](t: T) {
    def toUnpackable[U](using unpack: Unpack[T, U]): Unpackable[T, U] = new Unpackable[T, U](t, unpack)
  }

  extension [T <: Table[_]](t: T) {
    def createFinderBy[P]
      (f: T => NamedColumn[P])
      (using profile: Profile, tm: TypeMapper[P]): QueryTemplate[P, t.TableType] =

      Params[P](tm).flatMap[t.TableType] { p =>
        t.filter(t => ColumnOps.Is(f(t), p))
      }
  }

}
