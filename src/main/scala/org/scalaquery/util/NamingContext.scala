package org.scalaquery.util

import scala.collection.mutable.HashMap
import org.scalaquery.ql.Table.Name

trait NamingContext { self =>
  def aliasOrFresh(n: Node): Name
  def aliasFor(n: Node): String = aliasOrFresh(n).alias

  def overrideName(node: Node, freshName: String): NamingContext =
    new NamingContext {
      def aliasOrFresh(n: Node) = {
        if (n eq node) Name(freshName, isFresh = false)
        else self.aliasOrFresh(n)
      }
    }
}

object NamingContext {
  def apply() = new NamingContext {
    private val names = new HashMap[RefId[Node], String]
    private var nextId = 1

    def aliasOrFresh(n: Node): Name = {
      names.get(RefId(n)) match {
        case Some(alias) => Name(alias, isFresh = false)
        case None =>
          val alias = s"t$nextId"
          nextId += 1
          //println(s"NamingContext() >> no ref for $n creating $alias")
          names.put(RefId(n), alias)
          Name(alias, isFresh = true)
      }
    }
  }
}
