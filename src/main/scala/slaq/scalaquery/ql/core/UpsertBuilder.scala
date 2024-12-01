package slaq.ql.core

import slaq.Fail
import slaq.ql._
import slaq.util._

/** default upsert builder (standard merge by default, overridden on per driver basis) */
class UpsertBuilder(override val column: Any, override val profile: Profile)
  extends InsertBuilder(column, profile) {

  import profile.sqlUtils._

  override def buildInsert: String = {
    val (t, cols, vals) = buildParts
    val colList = cols.toString.split(",").map(_.trim)

    val start = s"MERGE INTO ${quote(t.tableName)} t USING ("
    val select = s"${colList.map(c => "? AS $c").mkString(",")}"
    val pkeys =
      t.primaryKeys.flatMap(_.columns).collect {
        case n: NamedColumn[_] => n.name
      }
    val where = pkeys.map(quote).map(c => s"t.$c=o.$c").mkString(" AND ")
    val updateCols = s"WHEN MATCHED THEN UPDATE SET ${colList.map(c => s"t.$c=o.$c").mkString(",")}"
    val insertCols = colList.mkString(",")
    val insertVals = colList.map(c => s"o.$c").mkString(",")
    val end = s") o ON ($where) $updateCols WHEN NOT MATCHED THEN INSERT ($insertCols) VALUES ($insertVals)"
    s"$start $select $end"
  }
}
