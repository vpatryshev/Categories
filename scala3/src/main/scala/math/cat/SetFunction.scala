package math.cat

import math.Base
import math.Base._
import math.sets.Sets._
import math.sets.{Functions, Sets}
import scalakittens.Result
import scalakittens.Result._
import scala.language.implicitConversions

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
  mapping: Any => Any)
  extends SetMorphism[Any, Any](tag, d0, d1, mapping):
  self =>
  
  private def tagOfComposition(tag1: String, tag2: String): String =
    Base.concat(tag1, "∘", tag2)
  
  /**
    * Composes with another morphism, optionally
    *
    * @param g next morphism: Y -> Z
    * @return their composition g ∘ f: X -> Z
    */
  infix def andThen(g: SetFunction): Option[SetFunction] =
    if d1 == g.d0 then
      val transform = (x: Any) => g(self(x))
      Some(new SetFunction(tagOfComposition(g.tag, tag), d0, g.d1, transform))
    else None

  /**
    * Restricts this function to a new domain
    * @param newDomain new domain
    * @return new function
    */
  override infix def restrictTo(newDomain: set): Result[SetFunction] = 
    restrictTo(newDomain, d1)

  /**
    * Restricts this function to a new domain and new codomain
    * @param newDomain new domain
    * @param newCodomain new codomain
    * @return new function
    */
  infix def restrictTo(newDomain: set, newCodomain: set): Result[SetFunction] =
    val domOk = OKif(newDomain subsetOf d0, "Bad domain for restriction")
    val codomOk = OKif(newCodomain subsetOf d1, "Bad codomain for restriction")
    val success: Outcome = domOk andAlso codomOk
    success returning new SetFunction(tag, newDomain, newCodomain, function)

  override lazy val hashCode: Int =
    d1.hashCode + 2 * d0.map(x => (x, mapping(x))).hashCode

/**
  * Set morphism for typeless sets.
  */
object SetFunction:

  def build(name: String, d0: set, d1: set, function: Any => Any): Result[SetFunction] =
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
    apply(value.toString, d0, d1, Function.const(value))

  /**
    * Factory method. Builds an inclusion monomorphism that injects one set to another.
    *
    * @param subset       domain of the inclusion
    * @param containerSet codomain of the inclusion
    * @return inclusion monomorphism
    */
  def inclusion(subset: set, containerSet: set): Result[SetFunction] =
    OKif(subset subsetOf containerSet,
      "It is not an inclusion if it is not a subset.") returning
      apply("⊂", subset, containerSet, Functions.inclusion)

  /**
    * Factory method. Builds an inclusion monomorphism that injects one set to another.
    * Subset is defined by a predicate.
    *
    * @param containerSet the set
    * @param predicate    defines the condition for elements to be included in the subset
    * @return inclusion monomorphism
    */
  def filterByPredicate(containerSet: set)(predicate: Any => Boolean): SetFunction =
    apply("⊂", containerSet filter predicate, containerSet, Functions.inclusion)

  /**
    * Factory method. Builds identity morphism for a set.
    *
    * @param domain the set
    * @return identity morphism on the given set
    */
  def id(domain: set): SetFunction = new SetFunction("id", domain, domain, x => x)

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
    Sets.exponent(x, y).map { apply("exponent", x, y, _) }

  def fun(from: set, to: set)(name: String, mapping: String => Any) =
    SetFunction.build(name, from, to, x => mapping(x.toString)).iHope

  def asFunction(a: /*almost*/ Any): SetFunction = a.asInstanceOf[SetFunction]
