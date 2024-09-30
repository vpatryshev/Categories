package math.cat

import math.Base._
import math.cat.SetMorphism._
import math.sets.{FactorSet, Sets}
import math.sets.Sets._
import scalakittens.Result
import scalakittens.Result._

import scala.language.{implicitConversions, postfixOps}

/**
 * Morphism class for sets, and the accompanying object.
 */
class SetMorphism[X, Y] (
    val tag: String,
    val d0: Set[X],
    val d1: Set[Y],
    val function: X => Y)
  extends Morphism[Set[X], Set[Y]] with Map[X, Y]:
  
  /**
   * Two set morphisms are equal if they have equal d0s and d1s and map d0 elements to the same values.
   * Note that there's no negation in this calculation; there is a deep reason for it, to be disclosed much, much later.
   *
   * @param other set morphism to compare
   * @return true iff they are equal
   */
  def equals(other: SetMorphism[X, Y]): Boolean =
    this.eq(other) ||
    isFinite(d0) && isFinite(other.d0) &&
    d0 == other.d0 && d1 == other.d1 &&
      d0.forall(x => this(x) == other(x))

  /**
    * Composes with another morphism, optionally
    *
    * @param g next morphism: Y -> Z
    * @return their composition g ∘ f: X -> Z
    */
  infix def andThen[Z](g: SetMorphism[Y, Z]): Option[SetMorphism[X, Z]] =
    OKif (d1 == g.d0) andThen build[X, Z](d0, g.d1, (x: X) => g(this(x))) asOption

  /**
    * Reverts a morphism: given X->Y, produce Y->Set[X]
    * @return a reverse morphism
    */
  def revert: SetMorphism[Y, Set[X]] =
    val d1AsSet: Set[Y] = d1
    val d0AsSet: Set[X] = d0
    val d0ForRevert: Set[Set[X]] = Sets.pow(d0AsSet)
    build[Y, Set[X]](
      d1AsSet,
      d0ForRevert,
      Sets.groupedBy(d0AsSet, this)) iHope

  /**
    * Getter: given a value, apply this morphism.
    * Have to implement it, because we extend `Map`.
    * @param x
    * @return
    */
  def get(x: X): Option[Y] =
    Result.forValue(x).filter(d0 contains).flatMap(x => Result.forValue(function(x))).asOption

  /**
    * Domain size of this morphism.
    * Have to implement it, because we extend `Map`.
    * @return domain size.
    */
  override def size: Int = d0.size

  /**
    * Pretends to be updating this morphism because it's a map.
    * We actually don't.
    * @param x argument
    * @param y value
    * @tparam Y1 value type
    * @return an exception is thrown
    */
  override def updated[Y1 >: Y](x: X, y: Y1): Map[X, Y1] = itsImmutable

  /**
    * Pretends to be removing a value from this morphism because it's a map.
    * We actually don't.
    * @param x argument
    * @param y value
    * @tparam Y1 value type
    * @return an exception is thrown
    */
  override def removed(key: X): scala.collection.immutable.Map[X,Y] = itsImmutable

  /**
    * Iterates over argument/value pairs.
    * @return the iterator
    */
  def iterator: Iterator[(X, Y)] = d0.iterator map (x => (x, function(x)))

  /**
    * Produces a product of this morphism, that is, a set of possible mappings
    * (It's functor Π)
    * @return
    */
  def product: Set[Map[Y, X]] = exponent(d1, d0) filter(m => d1 forall {y => function(m(y)) == y})

  /**
    * Restricts a morphism to a subdomain
    * @param newDomain new domain
    * @return a morphism restricted to `newDomain`
    */
  def restrictTo(newDomain: Set[X]): Result[SetMorphism[X, Y]] =
    OKif(newDomain subsetOf d0) returning new SetMorphism[X, Y](tag, newDomain, d1, function)

  override def toString: String = tag match
    case "" => d0 map (x => s"$x -> ${this(x)}") mkString ("{", ", ", "}")
    case _  => s"$tag: ${plural(d0.size, "element")} -> ${plural(d1.size, "element")}"

  override def hashCode: Int = d0.hashCode * 4/*random number*/ + d1.hashCode

object SetMorphism:
  /**
    * Validates set morphism.
    * All we need is that each d0 element is mapped to a d1 element.
    *
    * @param f morphism to validate
    * @tparam X argument type
    * @tparam Y value type
    * @tparam T type of morphism
    * @return result of validation
    */
  def check[X, Y, T <: SetMorphism[X, Y]](f: T): Result[T] =
    Result.traverse {
      for
        x <- f.d0
      yield
        val y = Result.forValue(f.function(x))
        val yInD1 = y filter (f.d1 contains)
        yInD1 orCommentTheError s"${f.tag}: Value $y for $x should be in d1 ${f.d1}"
    } returning f

  /**
    * Builds a set morphism out of two sets and a function
    * @param d0 domain set
    * @param d1 codomain set
    * @param function how values are mapped from domain and codomain
    * @tparam X domain type
    * @tparam Y codomain type
    * @return result of build
    */
  def build[X, Y](d0: Set[X], d1: Set[Y], function: X => Y): Result[SetMorphism[X, Y]] =
    build[X, Y]("", d0, d1, function)

  /**
    * Builds a set morphism out of two sets and a function
    * 
    * @param name name of this new function
    * @param d0 domain set
    * @param d1 codomain set
    * @param function how values are mapped from domain and codomain
    * @tparam X domain type
    * @tparam Y codomain type
    * @return result of build
    */
  def build[X, Y](name: String, d0: Set[X], d1: Set[Y], function: X => Y): Result[SetMorphism[X, Y]] =
    check[X, Y, SetMorphism[X, Y]](new SetMorphism[X, Y](name, d0, d1, function))

  /**
    * Builds an inclusion of one set into another
    * 
    * @param d0 subset
    * @param d1 set
    * @tparam X element type
    * @return a morphism from `d0` to `d1`
    */
  def inclusion[X](d0: Set[X], d1: Set[X]): Result[SetMorphism[X, X]] =
    OKif(d0 subsetOf d1) returning new SetMorphism("⊂", d0, d1, identity)

  /**
    * Builds an identity morphism on a set
    * @param domain the set
    * @tparam X element type
    * @return an identity morphism on the set
    */
  def id[X](domain: Set[X]) = new SetMorphism[X, X]("id", domain, domain, x => x)

  /**
    * Builds a constant morphism between sets
    * @param d0 domain set
    * @param d1 codomain set
    * @param value value of the constant morphism
    * @tparam X domain element type
    * @tparam Y codomain element type
    * @return the constant morphism
    */
  def const[X, Y](d0: Set[X], d1: Set[Y], value: Y) =
    new SetMorphism[X, Y]("`" + value + "`", d0, d1, _ => value)

  /**
    * Set of morphisms from one set to another
    * @param xs domain set
    * @param ys codomain set
    * @tparam X domain set element type
    * @tparam Y codomain set element type
    * @return set of all morphisms from `xs` to `ys`
    */
  def hom[X, Y](xs: Set[X], ys: Set[Y]): Set[SetMorphism[X, Y]] =
      val maps = Sets.exponent(xs, ys)
      val morphisms = maps flatMap (build(xs, ys, _).asOption)
      def predicate(m: SetMorphism[X, Y]) = m.d0 == xs && m.d1 == ys && maps.contains(m)
      setOf(morphisms, maps.size, predicate)

  /**
    * Builds a pullback of two set morphisms
    * 
    * @param f first morphism
    * @param g second morphism
    * @tparam X domain element type for `f`
    * @tparam Y domain element type for `g`
    * @tparam Z codomain element type for `f` and `g`
    * @return a pullback, a pair of morphisms from `(X,Y)` to `X` and `Y`
    */
  def pullback[X, Y, Z](f: SetMorphism[X, Z], g: SetMorphism[Y, Z]): (SetMorphism[(X, Y), X], SetMorphism[(X, Y), Y]) =
    val pullbackSet = Sets.pullback(f.d0, g.d0, f, g)
    val p0 = (p: (X, Y)) => p._1
    val p1 = (p: (X, Y)) => p._2
    (build(pullbackSet, f.d0, p0) andAlso build(pullbackSet, g.d0, p1)) iHope

  /**
    * Cartesian product of two set morphisms (that is, functions)
    */
  def product2[X1, X2, Y1, Y2](f: SetMorphism[X1, Y1], g: SetMorphism[X2, Y2]):
  SetMorphism[(X1, X2), (Y1, Y2)] =
    new SetMorphism[(X1, X2), (Y1, Y2)](
      concat(f.tag, "×", g.tag),
      Sets.product2(f.d0, g.d0),
      Sets.product2(f.d1, g.d1),
      p => (f(p._1), g(p._2)))

  /**
    * Given a factorset, build a set morphims from its base set
    * @param factorSet a factorset
    * @tparam X base set element type
    * @return a set morphism from base set to the factorset
    */
  def fromFactorSet[X](factorSet: math.sets.FactorSet[X]): SetMorphism[X, Set[X]] =
    SetMorphism.build(factorSet.base, factorSet.content, factorSet.asFunction) iHope
