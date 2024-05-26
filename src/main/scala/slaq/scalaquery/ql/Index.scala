package slaq.ql

import slaq.util.Node
import scala.collection.immutable.IndexedSeq

/**
 * An index (or foreign key constraint with an implicit index).
 */
class Index(
  val name: String,
  val table: Table[?],
  val on: IndexedSeq[Node],
  val unique: Boolean
)
