package math.cat

import math.sets.{FactorSet, Functions, Sets}
import math.sets.Sets._

/**
 * The idea was to create something that would cover unresolved issues with type assignment
 * while building bigger categorical structures. I'll probably throw it out eventually.
 *
 * @param tag name of the morphism
 * @param d0 domain
 * @param d1 codomain
 * @param f the function that implements the morphism
 */
case class SetFunction(
    override val tag: String,
    override val d0: Untyped,
    override val d1: Untyped,
    f: Any => Any)
        extends SetMorphism[Any, Any](tag, d0, d1, f) { self =>

  /**
    * Composes with another morphism, optionally
    *
    * @param g next morphism: Y -> Z
    * @return their composition g o f: X -> Z
    */
  def compose(g: SetFunction): Option[SetFunction] = {
    if (d1 equals g.d0) {
      Some(new SetFunction(g.tag + " o " + tag, d0, g.d1, (x: Any) => g(this(x))))
    }
    else None
  }

  /**
   * Composes this morphism with the next one.
   *
   * @param g second morphism: Y -> Z
   * @return their composition g o f: X -> Z
   */
  def andThen(g: SetFunction): SetFunction = {
    compose(g) getOrElse(
      throw new IllegalArgumentException(s"Composition not defined for $self and $g")
    )
  }

  /**
   * Composes a morphism with this morphism.
   * TODO: probably get rid of it
   * @param g first morphism: T -> X
   * @return their composition f o g: T -> Y
   */
  def before(g: SetFunction): SetFunction = g andThen this
  
  override def equals(o: Any): Boolean = o match {
    case other: SetFunction =>
      d0 == other.d0 && d1 == other.d1 &&
      d0.forall(x => f(x) == other.f(x))
    case _ => false
  }
  
  override lazy val hashCode: Int =
    d1.hashCode + 2 * d0.map(x => (x, f(x))).hashCode
}

/**
 * Set morphism for typeless sets.
 *
 * All source code is stored at <a href="https://github.com/vpatryshev/Categories">https://github.com/vpatryshev/Categories</a>
 *
 */
object SetFunction {

  /**
   * Factory method. Builds constant morphism from one set to another (constant function).
   *
   * @param d0 domain
   * @param d1 codomain
   * @param value the value in <code>codom</code> that the morphism returns
   * @return constant morphism
   */
  def constant(d0: Untyped, d1: Untyped, value: Any): SetFunction =
    apply(value.toString, d0, d1, Functions.constant(value))

  /**
   * Factory method. Builds an inclusion monomorphism that injects one set to another.
   *
   * @param subset domain of the inclusion
   * @param containerSet codomain of the inclusion
   * @return inclusion monomorphism
   */
  def inclusion(subset: Untyped, containerSet: Untyped): SetFunction = {
    require(subset.subsetOf(containerSet), "It is not an inclusion if it is not a subset.")
    apply("incl", subset, containerSet, Functions.inclusion)
}
  /**
   * Factory method. Builds an inclusion monomorphism that injects one set to another.
   * Subset is defined by a predicate.
   *
   * @param containerSet the set
   * @param predicate defines the condition for elements to be included in the subset
   * @return inclusion monomorphism
   */
  def inclusion(containerSet: Untyped, predicate: Any => Boolean): SetFunction =
    inclusion(containerSet filter predicate, containerSet)

  /**
   * Factory method. Builds identity morphism for a set.
   *
   * @param domain the set
   * @return identity morphism on the given set
   */
  def id(domain: Untyped): SetFunction = new SetFunction("id", domain, domain, x => x)

  /**
   * Factory method. Builds a factorset epimorphism that projects a set to its factorset,
   * given a set and binary relation. Factoring is done on the relation's transitive closure.
   *
   * @param factorset the main set
   * @return factorset epimorphism
   */
  def forFactorset(factorset: FactorSet[Any]): SetFunction =
    SetFunction("Factorset",
      factorset.domain,
      factorset.content.map(identity), 
      factorset.asFunction)

    /**
     * Builds a set of all morphisms from one set to another.
     *
     * @param x exponent (the set from which all morphisms are)
     * @param y base (the set to which all morphisms are)
     * @return y < sup > x, represented as a set of all morphisms.
     */
  def exponent(x: Untyped, y: Untyped): Set[SetFunction] =
    Sets.exponent(x, y) map { apply("exponent", x, y, _) }
  }
