package math.cat

/**
 * Absolutely generic morphism class.
 * @author vpatryshev
 */
trait Morphism[Domain, Codomain] {
  val d0: Domain
  val d1: Codomain
}
