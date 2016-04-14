package org.scalaquery.ql

import org.scalaquery.util.{Node, BinaryNode}

case class Join(
	left: Node, 
	right: Node, 
	on: Node, joinType: JoinType) extends BinaryNode
	
object Join {
	def query[P, P2, U, U2, R]( 
		unpackable: Unpackable[_ <: (P, P2), _ <: (U, U2)], 
		reify: Reify[(P, P2), R], join: Join) = {
		
		def f[PP](unpackable: Unpackable[PP, _ <: (U, U2)]) =
			Unpackable(
				unpackable.mapOp(_ => join), unpackable.unpack
			)
		val r: Unpackable[R, _ <: (U, U2)] = unpackable.reifiedUnpackable(reify)
		Query[R, (U, U2)]( f(r) )
	}
}