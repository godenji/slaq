package org.scalaquery.ql.core

class Driver extends Profile {self=>

  type ImplicitT = ImplicitConversions[Driver]
  type TypeMapperDelegatesT = TypeMapperDelegates

  val Implicit = new ImplicitConversions[Driver] {
    implicit val scalaQueryDriver = self
  }
  
  val typeMapperDelegates = new TypeMapperDelegates {}
}
object Driver extends Driver
