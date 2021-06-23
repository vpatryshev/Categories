package math.cat

import scala.language.implicitConversions
import scala.language.postfixOps
import math.Base._
import math.cat.SetMorphism._
import math.sets.Sets
import math.sets.Sets._
import scalakittens.Result
import scalakittens.Result._
import math.sets.FactorSet

/**
 * Morphism class for sets, and the accompanying object.
 */
class SetMorphism[X, Y] (
    val tag: String,
    val d0: Set[X],
    val d1: Set[Y],
    val function: X => Y)
  extends Morphism[Set[X], Set[Y]] with Map[X, Y]:
  
  override def toString: String = tag match
    case "" => d0 map (x => s"$x -> ${this(x)}") mkString ("{", ", ", "}")
    case _  => s"$tag: ${plural(d0.size, "element")} -> ${plural(d1.size, "element")}"
  
  override def hashCode: Int = d0.hashCode * 4/*random number*/ + d1.hashCode
  
  /**
   * Two set morphisms are equal if they have equal d0s and d1s and map d0 elements to the same values.
   * Note that there's no negation in this calculation; there is a deep reason for it, to be disclosed much, much later.
   *
   * @param other set morphism to compare
   * @return true iff they are equal
   */
  def equals(other: SetMorphism[X, Y]): Boolean = {
    isFinite(d0) && isFinite(other.d0) &&
    d0.foldLeft(d0 == other.d0 && d1 == other.d1) ((eq, x) => eq & this(x) == other(x))
  }

  /**
    * Composes with another morphism, optionally
    *
    * @param g next morphism: Y -> Z
    * @return their composition g ∘ f: X -> Z
    */
  def andThen[Z](g: SetMorphism[Y, Z]): Option[SetMorphism[X, Z]] =
    OKif (d1 == g.d0) andThen build[X, Z](d0, g.d1, (x: X) => g(this(x))) asOption
  
  def revert: SetMorphism[Y, Set[X]] =
    val d1AsSet: Set[Y] = d1
    val d0AsSet: Set[X] = d0
    val d0ForRevert: Set[Set[X]] = Sets.pow(d0AsSet)
    build[Y, Set[X]](
      d1AsSet,
      d0ForRevert,
      Sets.groupedBy(d0AsSet, this)) iHope

  def get(x: X): Option[Y] = Option(x) filter (d0 contains) map function

  override def size: Int = d0.size

  override def updated[Y1 >: Y](x: X, y: Y1): Map[X, Y1] = itsImmutable

  override def removed(key: X): scala.collection.immutable.Map[X,Y] = itsImmutable

  def iterator: Iterator[(X, Y)] = d0.iterator map (x => (x, function(x)))
  
  def product: Set[Map[Y, X]] = exponent(d1, d0) filter(m => d1 forall {y => function(m(y)) == y})
  
  def restrictTo(newDomain: Set[X]): Result[SetMorphism[X, Y]] =
    OKif(newDomain subsetOf d0) returning new SetMorphism[X, Y](tag, newDomain, d1, function)

object SetMorphism:
  // Validates set morphism.
  // All we need is that each d0 element is mapped to a d1 element.
  def check[X, Y, T <: SetMorphism[X, Y]](f: T): Result[T] =
    Result.traverse {
      for
        x <- f.d0
      yield
        val y = Result.forValue(f.function(x))
        val yInD1 = y filter (f.d1 contains)
        if (yInD1.isBad) {
          println(yInD1)
        }
        yInD1 orCommentTheError s"${f.tag}: Value $y for $x should be in d1 ${f.d1}"
    } returning f
  
  def build[X, Y](d0: Set[X], d1: Set[Y], function: X => Y): Result[SetMorphism[X, Y]] =
    build[X, Y]("", d0, d1, function)

  def build[X, Y](name: String, d0: Set[X], d1: Set[Y], function: X => Y): Result[SetMorphism[X, Y]] =
    check[X, Y, SetMorphism[X, Y]](new SetMorphism[X, Y](name, d0, d1, function))

  def inclusion[X](d0: Set[X], d1: Set[X]): Result[SetMorphism[X, X]] =
    OKif(d0 subsetOf d1) returning new SetMorphism("⊂", d0, d1, identity)
  
  def id[X](domain: Set[X]) = new SetMorphism[X, X]("id", domain, domain, x => x)

  def const[X, Y](d0: Set[X], d1: Set[Y], value: Y) =
    new SetMorphism[X, Y]("`" + value + "`", d0, d1, _ => value)

  def hom[X, Y](xs: Set[X], ys: Set[Y]): Set[SetMorphism[X, Y]] =
      val maps = Sets.exponent(xs, ys)
      val morphisms = maps flatMap (build(xs, ys, _).asOption)
      def predicate(m: SetMorphism[X, Y]) = m.d0 == xs && m.d1 == ys && maps.contains(m)
      setOf(morphisms, maps.size, predicate)

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

  def fromFactorSet[X](factorSet: math.sets.FactorSet[X]): SetMorphism[X, Set[X]] =
    SetMorphism.build(factorSet.base, factorSet.content, factorSet.asFunction) iHope
