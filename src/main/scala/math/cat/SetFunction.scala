package math.cat

import math.sets.Sets._
import math.sets.{Functions, Sets}
import scalakittens.Result
import Result._

/**
  * The idea was to create something that would cover unresolved issues with type assignment
  * while building bigger categorical structures. I'll probably throw it out eventually.
  *
  * @param tag     name of the morphism
  * @param d0      domain
  * @param d1      codomain
  * @param mapping the function that implements the morphism
  */
case class SetFunction private[cat](
  override val tag: String,
  override val d0: set,
  override val d1: set,
  mapping: Any ⇒ Any)
  extends SetMorphism[Any, Any](tag, d0, d1, mapping) {
  self ⇒

  override lazy val hashCode: Int =
    d1.hashCode + 2 * d0.map(x ⇒ (x, mapping(x))).hashCode

  /**
    * Composes this morphism with the next one.
    *
    * @param g second morphism: Y → Z
    * @return their composition g ∘ f: X → Z
    */
  def andThen(g: SetFunction): SetFunction = {
    compose(g) getOrElse (
      throw new IllegalArgumentException(s"Composition not defined for $self and $g")
      )
  }

  private def tagOfComposition(tag1: String, tag2: String): String = {
    def maybeParens(tag: String) = if (tag contains "∘") s"($tag)" else tag
    maybeParens(tag1) + " ∘ " + maybeParens(tag2)
  }
  
  
  /**
    * Composes with another morphism, optionally
    *
    * @param g next morphism: Y → Z
    * @return their composition g ∘ f: X → Z
    */
  def compose(g: SetFunction): Option[SetFunction] = {
    if (d1 equals g.d0) {
      val newTag = tagOfComposition(g.tag, tag)
      val transform = (x: Any) ⇒ {
        val y = self(x)
        g(y)
      }
      Some(new SetFunction(newTag, d0, g.d1, transform))
    }
    else None
  }

  /**
    * Restricts this function to a new domain
    * @param newDomain new domain
    * @return new function
    */
  override def restrictTo(newDomain: set): Result[SetFunction] = 
    restrictTo(newDomain, d1)

  /**
    * Restricts this function to a new domain and new codomain
    * @param newDomain new domain
    * @param newCodomain new codomain
    * @return new function
    */
  def restrictTo(newDomain: set, newCodomain: set): Result[SetFunction] = {
    val domOk = OKif(newDomain subsetOf d0)
    val codomOk = OKif(newCodomain subsetOf d1)
    domOk andAlso codomOk returning new SetFunction(tag, newDomain, newCodomain, function)
  }
}

/**
  * Set morphism for typeless sets.
  */
object SetFunction {

  def build(d0: set, d1: set, function: Any ⇒ Any): Result[SetFunction] =
    build("", d0, d1, function)

  def build(name: String, d0: set, d1: set, function: Any ⇒ Any): Result[SetFunction] =
    SetMorphism.check[Any, Any, SetFunction](new SetFunction(name, d0, d1, function))
  /**
    * Factory method. Builds constant morphism from one set to another (constant function).
    *
    * @param d0    domain
    * @param d1    codomain
    * @param value the value in <code>codom</code> that the morphism returns
    * @return constant morphism
    */
  def constant(d0: set, d1: set, value: Any): SetFunction =
    apply(value.toString, d0, d1, Functions.constant(value))

  /**
    * Factory method. Builds an inclusion monomorphism that injects one set to another.
    *
    * @param subset       domain of the inclusion
    * @param containerSet codomain of the inclusion
    * @return inclusion monomorphism
    */
  def inclusion(subset: set, containerSet: set): Result[SetFunction] = {
    OKif(subset subsetOf containerSet,
      "It is not an inclusion if it is not a subset.") returning
      apply("incl", subset, containerSet, Functions.inclusion)
  }

  /**
    * Factory method. Builds an inclusion monomorphism that injects one set to another.
    * Subset is defined by a predicate.
    *
    * @param containerSet the set
    * @param predicate    defines the condition for elements to be included in the subset
    * @return inclusion monomorphism
    */
  def filterByPredicate(containerSet: set)(predicate: Any ⇒ Boolean): SetFunction =
    apply("filter", containerSet filter predicate, containerSet, Functions.inclusion)

  /**
    * Factory method. Builds identity morphism for a set.
    *
    * @param domain the set
    * @return identity morphism on the given set
    */
  def id(domain: set): SetFunction = new SetFunction("id", domain, domain, x ⇒ x)

  /**
    * Factory method. Builds a factorset epimorphism that projects a set to its factorset,
    * given a set and binary relation. Factoring is done on the relation's transitive closure.
    *
    * @param theFactorset the main set
    * @return factorset epimorphism
    */
  def forFactorset(theFactorset: factorset): SetFunction =
    SetFunction("Factorset",
      theFactorset.domain,
      theFactorset.content.untyped,
      theFactorset.asFunction)

  /**
    * Builds a set of all morphisms from one set to another.
    *
    * @param x exponent (the set from which all morphisms are)
    * @param y base (the set to which all morphisms are)
    * @return y < sup > x, represented as a set of all morphisms.
    */
  def exponent(x: set, y: set): Set[SetFunction] =
    Sets.exponent(x, y) map { apply("exponent", x, y, _) }
}
