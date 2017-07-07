package org.scalaquery.ql

import org.scalaquery.util.{Node, BinaryNode}

case class Join(
  left: Node,
  right: Node,
  on: Node, joinType: JoinType
) extends BinaryNode {

  /**
   * extracts target table alias or table from join
   */
  final def extractNode(tableName: String, forTableAlias: Boolean): Node =
    (left, right) match {
      case (la @ Table.Alias(l: Table[_]), ra @ Table.Alias(r: Table[_])) =>
        if (forTableAlias) (
          if (l.tableName == tableName) la else ra
        )
        // else clone target table with Join instance as node delegate
        else if (l.tableName == tableName) l.mapOp(_ => this)
        else r.mapOp(_ => this)
      case (l, _) => l // shouldn't get here (i.e. left/right Node is always a table)
    }
}

object Join {
  def query[P, P2, U, U2, R](
    unpackable: Unpackable[_ <: (P, P2), _ <: (U, U2)],
    reify: Reify[(P, P2), R], join: Join
  ) = {

    def f[PP](unpackable: Unpackable[PP, _ <: (U, U2)]) =
      Unpackable(
        unpackable.mapOp(_ => join), unpackable.unpack
      )
    val r: Unpackable[R, _ <: (U, U2)] = unpackable.reifiedUnpackable(reify)
    Query[R, (U, U2)](f(r))
  }
}