package math.cat

import scala.collection.Set
import Sets._
import Functions.{Id, Injection, extend}

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
 * All source code is stored on <a href="http://code.google.com/p/categories/">http://code.google.com/p/categories/</a>
 *
 */
object TypelessSetMorphism {

  /**
   * Constructor
   * @param tag morphism name
   * @param domain morphism domain (a set)
   * @param codomain morphism codomain (a set)
   */
  def apply(tag: String, d0: Set[Any], d1: Set[Any], f: Any => Any) =
    new TypelessSetMorphism(tag, d0, d1, f)

  def apply[X, Y] (morphism: SetMorphism[X, Y]): TypelessSetMorphism =
      apply(morphism.tag, upcast(morphism.d0), upcast(morphism.d1), extend(morphism))

  /**
   * Factory method. Builds constant morphism from one set to another (constant function).
   *
   * @param dom domain
   * @param codom codomain
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
  def exponent(x: Set[Any], y: Set[Any]): Set[cat.TypelessSetMorphism] =
    Sets.exponent(x, y) map { apply("exponent", x, y, _) }
  }
