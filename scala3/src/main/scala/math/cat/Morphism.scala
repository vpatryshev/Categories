package math.cat

/**
 * Absolutely generic morphism class.
 */
trait Morphism[Domain, Codomain]:
  val d0: Domain
  val d1: Codomain
