package slaq.ql.driver

import slaq.ql.core._

/**
 * generic driver (benchmarking and statement verification)
 */
sealed class Driver extends Profile { self =>

  type ImplicitT = ImplicitConversions[Driver]
  type TypeMapperDelegatesT = TypeMapperDelegates

  val Implicit = new ImplicitConversions[Driver] {
    implicit val driverType = self
  }

  val typeMapperDelegates = new TypeMapperDelegates {}
}
object Driver extends Driver
