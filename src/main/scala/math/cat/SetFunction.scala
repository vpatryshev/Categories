package math.cat

import math.cat.Sets._

/**
 * The idea was to create something that would cover unresolved issues with type assignment
 * while building bigger categorical structures. I'll probably throw it out eventually.
 *
 * @param tag name of the morphism
 * @param d0 domain
 * @param d1 codomain
 * @param f the function that implements the morphism
 */
class SetFunction(
    override val tag: String,
    override val d0: Set[Any],
    override val d1: Set[Any],
    val f: Any => Any)
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
 * @author Vlad Patryshev
 * All source code is stored at <a href="https://github.com/vpatryshev/Categories">https://github.com/vpatryshev/Categories</a>
 *
 */
object SetFunction {

  /**
   * Constructor
   * @param tag morphism name
   * @param d0 morphism domain (a set)
   * @param d1 morphism codomain (a set)
   */
  def apply(tag: String, d0: Set[Any], d1: Set[Any], f: Any => Any) =
    new SetFunction(tag, d0, d1, f)

  /**
   * Factory method. Builds constant morphism from one set to another (constant function).
   *
   * @param d0 domain
   * @param d1 codomain
   * @param value the value in <code>codom</code> that the morphism returns
   * @return constant morphism
   */
  def constant(d0: Set[Any], d1: Set[Any], value: Any): SetFunction =
    apply(value.toString, d0, d1, Functions.constant(value))

  /**
   * Factory method. Builds an inclusion monomorphism that injects one set to another.
   *
   * @param subset the subset
   * @param set the set
   * @return inclusion monomorphism
   */
  def inclusion(subset: Set[Any], set: Set[Any]): SetFunction = {
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
  def inclusion(set: Set[Any], predicate: Any => Boolean): SetFunction =
    inclusion(set filter predicate, set)

  /**
   * Factory method. Builds unit morphism for a set (identity function).
   *
   * @param s the set
   * @return identity morphism on the given set
   */
  def unit(s: Set[Any]): SetFunction = new SetFunction("1", s, s, x => x)

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
      factorset.factorset.asInstanceOf[Set[Any]], 
      factorset.asFunction)

    /**
     * Builds a set of all morphisms from one set to another.
     *
     * @param x exponent (the set from which all morphisms are)
     * @param y base (the set to which all morphisms are)
     * @return y < sup > x, represented as a set of all morphisms.
     */
  def exponent(x: Set[Any], y: Set[Any]): Set[SetFunction] =
    Sets.exponent(x, y) map { apply("exponent", x, y, _) }
  }
