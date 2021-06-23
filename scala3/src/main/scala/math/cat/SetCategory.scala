package math.cat

import scala.language.implicitConversions
import scala.language.postfixOps
import math.cat.SetCategory._
import math.cat.SetFunction._
import math.sets.Sets._
import math.sets.{BigSet, BinaryRelation, FactorSet, Sets}
import scalakittens.Result._
import scalakittens.{Good, Result}

/**
  * Category where objects are sets
  */
class SetCategory(objects: BigSet[set]) extends Category:

  /**
    * Inner graph of this category of sets
    */
  override val graph = graphOfSets(objects)
  type Node = set
  type Arrow = SetFunction

  /**
    * Domain of an arrow
    * @param f the arrow
    * @return the domain (which is a set)
    */
  override def d0(f: SetFunction): set = f.d0

  /**
    * Codomain of an arrow
    * @param f the arrow
    * @return the codomain (which is a set)
    */
  override def d1(f: SetFunction): set = f.d1

  /**
    * Composition of arrows
    * @param f first arrow
    * @param g second arrow
    * @return their composition
    */
  override def m(f: Arrow, g: Arrow): Option[Arrow] = f andThen g

  /**
    * Identity arrow on an object
    * @param s an object of this category (a set)
    * @return the identity
    */
  override def id(s: set): SetFunction = SetFunction.id(s)

  override def toString: String = "Category of all Scala Sets"
  
  /**
    * Set `x` to the power of set `y`.
    * This makes our category Cartesian-closed
    * @param x a set
    * @param y a set
    * @return x^y
    */
  def exponent(x: set, y: set): set = Sets.exponent(x, y).untyped

  /**
    * set of arrows from `x` to `y`
    * @param x domain object
    * @param y codomain object
    *  @return the set of all arrows from x to y
    */
  override def arrowsBetween(x: set, y: set): Set[SetFunction] =
    SetFunction.exponent(x, y)

  /**
    * Checks whether an arrow is a mono
    * @param f an arrow to check
    *  @return true iff f is a monomorphism
    */
  override def isMonomorphism(f: SetFunction): Boolean =
    f.d0.forall(x => f.d0.forall(y => !(f(x) == f(y)) || x == y))

  /**
    * Checks whether an arrow is an epi
    * @param f an arrow to check
    *  @return true iff f is an epimorphism
    */
  override def isEpimorphism(arrow: SetFunction): Boolean =
    arrow.d1 forall {y => arrow.d0 exists {y == arrow(_)}}

  /**
    * Equalizer of two arrows (does not have to exist)
    * @param f first arrow
    * @param g second arrow
    *  @return an equalizer arrow, wrapped in Result
    */
  override def equalizer(f: SetFunction, g: SetFunction): Result[SetFunction] =
    OKif(areParallel(f, g), s"Arrows $f and $g must be parallel") andThen
      val filtrator: (Any => Boolean) => SetFunction = SetFunction.filterByPredicate(f.d0)
      val inclusion = filtrator(x => f(x) == g(x))
      Good(inclusion) filter { i => objects.contains(i.d0) }
    

  override def coequalizer(f: SetFunction, g: SetFunction): Result[SetFunction] = 
  OKif(areParallel(f, g), s"Arrows $f and $g must be parallel") andThen {
    val theFactorset: factorset = new FactorSet[Any](f.d1)

    OKif(contains(theFactorset untyped)) returning {
      f.d0.foreach { x => theFactorset.merge(f(x), g(x)) }
      SetFunction.forFactorset(theFactorset)
    }
  }

  override def coequalizer(arrowsToEqualize: Iterable[SetFunction]): Result[SetFunction] =
    OKif(arrowsToEqualize.iterator.hasNext, "Need at least one arrow for coequalizer") andThen
    OKif (arrowsToEqualize.nonEmpty, "Can't equalize an empty collection of arrows") andThen { // need these curlies, or a test fails
      val f = arrowsToEqualize.head
      val domain = f.d0
      val codomain = f.d1
      val dataOk = Result.traverse(for (f <- arrowsToEqualize) yield
        OKif(f.d0 == domain, s"Domain should be $domain") andAlso
          OKif(f.d1 == codomain, s"Codomain should be $codomain")
      )

      dataOk andThen {
        val theFactorset: factorset = new FactorSet(codomain)

        for {
          g <- arrowsToEqualize
          x <- g.d0
        } {theFactorset.merge(f(x), g(x)) }

        Result.forValue(SetFunction.forFactorset(theFactorset))
      }
    }

  override def degree(x: set, n: Int): Result[(set, List[SetFunction])] =
    Result.OKif(n >= 0, s"No negative degree exists yet, for n=$n") andThen {

      val actualDomain: Set[List[Any]] =
        Sets.exponent(Sets.numbers(n), x.untyped) map {
          _.toList.sortBy(_._1).map(_._2)
        }

      val domain: set = actualDomain untyped

      def takeElementAt(i: Int)(obj: Any): Any = obj.asInstanceOf[List[Any]](i)
      
      val projections = (0 until n) map {
        i => SetFunction.build(s"set^$n", domain, x, takeElementAt(i))
      }

      Result.traverse(projections) map { ps => (domain, ps.toList) }
    }

  // need to filter, to eliminate the value that does not belong to a subcategory
  // that may inherit this value
  override lazy val initial: Result[set] = Good(Sets.Empty) filter contains

  override lazy val terminal: Result[set] = {
    val option1: Result[set] = initial map (setOf.elements(_))
    // need to filter, to eliminate the value that does not belong
    option1 filter contains
  }

  override def product(x: set, y: set): Result[(SetFunction, SetFunction)] = {

    val productSet: set = Sets.product2(x, y) untyped
    val p1 = SetFunction.build("p1", productSet, x, { case (a, b) => a })
    val p2 = SetFunction.build("p2", productSet, y, { case (a, b) => b })
    p1 andAlso p2
  }

  override def pullback(f: SetFunction, g: SetFunction):
  Result[(SetFunction, SetFunction)] =
    for {
      prod <- product(f.d0, g.d0)
      productSet = prod._1.d0
      pullbackInProduct =
        filterByPredicate(productSet)(predicate = { case (a, b) => f(a) == g(b) })
      left <- pullbackInProduct andThen prod._1
      right <- pullbackInProduct andThen prod._2
    } yield (left, right)

  override def union(x: set, y: set): Result[(SetFunction, SetFunction)] = {
    def tagX(x: Any) = ("x", x)
    def tagY(y: Any) = ("y", y)
    val taggedX: set = x map tagX
    val taggedY: set = y map tagY
    val unionSet: set = Sets.union(taggedX, taggedY)
    val ix0 = SetFunction.build("ix", x, taggedX, tagX)
    val ix1 = SetFunction.inclusion(taggedX, unionSet)
    val ix =ix0 andAlso ix1 flatMap { case (f, g) => Result(f andThen g) }
    val iy0 = SetFunction.build("iy", y, taggedY, tagY)
    val iy1 = SetFunction.inclusion(taggedY, unionSet)
    val iy = iy0 andAlso iy1 flatMap { case (f, g) => Result(f andThen g) }
    val union = ix andAlso iy
    union
  }

  override def pushout(f: SetFunction, g: SetFunction): Result[(SetFunction, SetFunction)] =
    for {
      (left, right) <- union(f.d1, g.d1)
      leftSide <- Result(f andThen left)
      rightSide <- Result(g andThen right)
      coeq <- coequalizer(leftSide, rightSide)
      leftCorner <- Result(left andThen coeq)
      rightCorner <- Result(right andThen coeq)
    } yield (leftCorner, rightCorner)

  override def hashCode: Int = getClass.hashCode * 7 + objects.hashCode

  override def equals(x: Any): Boolean = x match {
    case sc: SetCategory => objects == sc.objects
    case other => false
  }

object SetCategory {

  private[cat] def graphOfSets(nodes0: BigSet[set]): Graph = {
    Graph.build[set, SetFunction](
      "Sets",
      nodes0,
      BigSet.of[SetFunction]("set of set functions"),
      (f: SetFunction) => f.d0,
      (f: SetFunction) => f.d1)
  }.getOrElse(throw new InstantiationException("This graph should exist"))

  object Setf extends SetCategory(FiniteSets)


  def asMorphism[X](factorSet: FactorSet[X]): SetMorphism[X, Set[X]] = {
    SetMorphism.build(factorSet.base, factorSet.content, factorSet.asFunction) iHope
  }


  /**
    * Builds a factorset epimorphism that projects a set to its factorset,
    * given a set and binary relation.
    * Factoring is done on the relation's transitive closure.
    *
    * @tparam T set element type
    * @param sourceSet the main set
    * @param r         binary relation (not necessarily equivalence relation) that determines factoring
    * @return factorset epimorphism
    */
  def factorset[T](sourceSet: Set[T], r: BinaryRelation[T, T]): SetMorphism[T, Set[T]] = {
    SetCategory.asMorphism(new FactorSet[T](sourceSet, r))
  }

}

/*
простой пример топоса, где не всякая монада является апликативным функтором. Это Set^{ℤ_2}
 */
