package math.cat

import math.cat.Sets._

/**
 * This opportunistic class does not seem to be used anywhere anymore.
 * The idea was to create something that would cover unresolved issues with type assignment
 * while building bigger categorical structures. I'll probably throw it out eventually.
 *
 * @param tag name of the morphism
 * @param d0 domain
 * @param d1 codomain
 * @param f the function that implements the morphism
 */
class TypelessSetMorphism(
    override val tag: String,
    override val d0: Set[Any],
    override val d1: Set[Any],
    f: (Any => Any))
        extends SetMorphism[Any, Any](tag, d0, d1, f) {

  /**
   * Composes this morphism with the next one.
   *
   * @param g second morphism: Y -> Z
   * @return their composition g o f: X -> Z
   */
  def then(g: TypelessSetMorphism): TypelessSetMorphism = {
    require(d1.equals(g.d0), "Composition not defined")
    new TypelessSetMorphism(tag + " o " + g.tag, d0, g.d1, (x: Any) => g(this(x)))
  }

  /**
   * Composes a morphism with this morphism.
   * TODO: probably get rid of it
   * @param g first morphism: T -> X
   * @return their composition f o g: T -> Y
   */
  def before(g: TypelessSetMorphism): TypelessSetMorphism = g then this
}

/**
 * Set morphism for typeless sets.
 *
 * @author Vlad Patryshev
 * All source code is stored at <a href="https://github.com/vpatryshev/Categories">https://github.com/vpatryshev/Categories</a>
 *
 */
object TypelessSetMorphism {

  /**
   * Constructor
   * @param tag morphism name
   * @param d0 morphism domain (a set)
   * @param d1 morphism codomain (a set)
   */
  def apply(tag: String, d0: Set[Any], d1: Set[Any], f: Any => Any) =
    new TypelessSetMorphism(tag, d0, d1, f)

  // this function is kind of stupid; it extends both domain and codomain, so it's supposed to crash on bad arguments
  // never mind, we may kick it all out eventually
  private def extend[X0, X1 >: X0, Y0, Y1 >: Y0](f: X0 => Y0): (X1 => Y1) = { case (x0: X0) => f(x0) }

  /**
    * Casts a set to a set of values of a superclass.
    * Not good for mutable.
    * TODO: DON'T
    */
  private def upcast[X, Y >: X](s: Set[X]): Set[Y] = s.asInstanceOf[Set[Y]]

  def apply[X, Y] (morphism: SetMorphism[X, Y]): TypelessSetMorphism = {
    apply(morphism.tag, upcast(morphism.d0), upcast(morphism.d1), extend(morphism))
  }

  /**
   * Factory method. Builds constant morphism from one set to another (constant function).
   *
   * @param d0 domain
   * @param d1 codomain
   * @param value the value in <code>codom</code> that the morphism returns
   * @return constant morphism
   */
  def constant(d0: Set[Any], d1: Set[Any], value: Any): TypelessSetMorphism =
    apply(value.toString, d0, d1, Functions.constant(value))

  /**
   * Factory method. Builds an inclusion monomorphism that injects one set to another.
   *
   * @param subset the subset
   * @param set the set
   * @return inclusion monomorphism
   */
  def inclusion(subset: Set[Any], set: Set[Any]): TypelessSetMorphism = {
    require(subset.subsetOf(set), "It is not an inclusion if it is not a subset.")
    apply("incl", subset, set, Functions.inclusion)
}
  /**
   * Factory method. Builds an inclusion monomorphism that injects one set to another.
   * Subset is defined by a predicate.
   *
   * @param set the set
   * @param predicate defines the condition for elements to be included in the subset
   * @return inclusion monomorphism
   */
  def inclusion(set: Set[Any], predicate: Any => Boolean): TypelessSetMorphism =
    inclusion(set filter predicate, set)

  /**
   * Factory method. Builds unit morphism for a set (identity function).
   *
   * @param s the set
   * @return identity morphism on the given set
   */
  def unit(s: Set[Any]): TypelessSetMorphism = new TypelessSetMorphism("1", s, s, x => x)

  /**
   * Composes two morphisms.
   *
   * @param f first morphism: X -> Y
   * @param g second morphism: Y -> Z
   * @return their composition g o f: X -> Z
   */
  def compose(f: TypelessSetMorphism, g: TypelessSetMorphism): TypelessSetMorphism = f then g

  /**
   * Factory method. Builds a factorset epimorphism that projects a set to its factorset,
   * given a set and binary relationship. Factoring is done on the relationship's transitive closure.
   *
   * @param factorset the main set
   * @return factorset epimorphism
   */
  def forFactorset(factorset: FactorSet[Any]): TypelessSetMorphism =
    TypelessSetMorphism("Factorset",
      factorset.domain,
      factorset.factorset.asInstanceOf[Set[Any]], 
      factorset.asFunction)

    /**
     * Builds a set of all morphisms from one set to another.
     *
     * @param x exponent (the set from which all morphisms are)
     * @param y base (the set to which all morphisms are)
     * @return y < sup > x, represented as a set of all morphisms.
     */
  def exponent(x: Set[Any], y: Set[Any]): Set[TypelessSetMorphism] =
    Sets.exponent(x, y) map { apply("exponent", x, y, _) }
  }
