package slaq.ql

import slaq.util.Node

/**
 * An index (or foreign key constraint with an implicit index).
 */
class Index(
  val name: String,
  val table: Table[_],
  val on: IndexedSeq[Node],
  val unique: Boolean
)
