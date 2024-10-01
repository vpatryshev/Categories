package math.cat

import math.cat.SetCategory.*
import math.cat.SetFunction.*
import math.sets.Sets.*
import math.sets.{BigSet, BinaryRelation, FactorSet, Sets}
import scalakittens.Result.*
import scalakittens.{Good, Result}

import scala.language.{implicitConversions, postfixOps}
import SetFunction.fun
import math.sets.ZFC.SetZ
import scalakittens.Containers.*
import Setf.*

/**
  * Category where objects are sets
  */
class SetCategory(objects: Set[set]) extends Category("Sets"):
  thisCategory =>
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
      Good(inclusion) filter { _.d0 ∈ objects }


  /**
    * Coequalizer of two set functions
    * @param f first arrow
    * @param g second arrow
    *  @return a coequalizer arrow, if one exists, None othrewise
    */
  override def coequalizer(f: SetFunction, g: SetFunction): Result[SetFunction] = 
  OKif(areParallel(f, g), s"Arrows $f and $g must be parallel") andThen {
    val theFactorset: factorset = new FactorSet[Any](f.d1)
    OKif((theFactorset untyped) ∈ thisCategory.objects) returning {
      f.d0.foreach { x => theFactorset.merge(f(x), g(x)) }
      SetFunction.forFactorset(theFactorset)
    }
  }

  /**
    * Coequalizer of a collection of set functions
    * @param arrowsToEqualize the functions to equalize
    *  @return a coequalizer arrow, if one exists, None othrewise
    */
  override def coequalizer(arrowsToEqualize: Iterable[SetFunction]): Result[SetFunction] =
    OKif(arrowsToEqualize.iterator.hasNext, "Need at least one arrow for coequalizer") andThen {
      val f0 = arrowsToEqualize.head
      val otherArrows = arrowsToEqualize.tail
      val dataOk = Result.traverse(
        for (f <- otherArrows) yield OKif(areParallel(f0, f), s"$f0 and $f must be parallel")
      ) 
      dataOk andThen equalizeFunctions(f0, otherArrows)
    }

  private def equalizeFunctions(
    f: SetFunction,
    arrowsToEqualize: Iterable[SetFunction]): Result[SetFunction] =
      val theFactorset: factorset = new FactorSet(f.d1)

      for
        g <- arrowsToEqualize
        x <- g.d0
      do theFactorset.merge(f(x), g(x))

      Result.forValue(SetFunction.forFactorset(theFactorset))

  /**
    * Degree of a set.
    * @param x the set
    * @param n degree to which to raise object x
    *  @return x^n^ and its projections to x
    */
  override def degree(x: set, n: Int): Result[(set, List[SetFunction])] =
    Result.OKif(n >= 0, s"No negative degree exists yet, for n=$n") andThen {

      val actualDomain: Set[List[Any]] =
        Sets.exponent(Sets.numbers(n), x.untyped).map {
          _.toList.sortBy(_._1).map(_._2)
        }

      val domain: set = actualDomain untyped

      def takeElementAt(i: Int)(obj: Any): Any = obj.asInstanceOf[List[Any]](i)
      
      val projections = (0 until n).map {
        i => SetFunction.build(s"set^$n", domain, x, takeElementAt(i))
      }

      Result.traverse(projections).map { ps => (domain, ps.toList) }
    }

  /**
    * Initial object of this category. Does not have to exist.
    *
    * Need to filter, so that if an empty set does not belong to a subcategory, `initial` is empty
    */
  override lazy val initial: Result[set] = Good(Sets.Empty) filter {
    emptySet => {
      thisCategory.contains(emptySet)
//      emptySet.∈(thisCategory)
    }
  }

  /**
    * Terminal object of this category. Does not have to exist.
    * Need to filter, so that if an empty set does not belong to a subcategory, `terminial` is empty.
    */
  override lazy val terminal: Result[set] =
    initial map (setOf.elements(_)) filter (_ ∈ thisCategory.objects)

  /**
    * Cartesian product of two sets.
    * Does not have to exist in a subcategory.
    * 
    * @param x first set
    * @param y second set
    * @return Good pair of arrows from product object to x and y, or None.
    */
  override def product(x: set, y: set): Result[(SetFunction, SetFunction)] =
    Good(product2(x, y).untyped).filter(contains).flatMap{
      ps =>
        val p1 = SetFunction.build("p1", ps, x, { case (a, b) => a })
        val p2 = SetFunction.build("p2", ps, y, { case (a, b) => b })
        (p1 andAlso p2)
    }

  /**
    * Pullback of two arrows.
    * @param f first arrows
    * @param g second arrow
    * @return Good pair of arrows from pullback object to d0(f) and d0(g), or None.
    */
  override def pullback(f: SetFunction, g: SetFunction):
    Result[(SetFunction, SetFunction)] =
    for
      prod <- product(f.d0, g.d0)
      productSet = prod._1.d0
      pullbackInProduct: SetFunction =
        filterByPredicate(productSet) { case (a, b) => f(a) == g(b) }
      left <- pullbackInProduct andThen prod._1
      right <- pullbackInProduct andThen prod._2
    yield (left, right)

  /**
    * Disjoint (tagged) union of two sets
    * @param x first set
    * @param y second set
    *  @return a good pair of arrows from a and b to their union, or None if none exists.
    */
  override def union(x: set, y: set): Result[(SetFunction, SetFunction)] =
    def tagX(x: Any) = ("x", x)
    def tagY(y: Any) = ("y", y)
    val taggedX: set = x map tagX
    val taggedY: set = y map tagY
    val unionSet: set = Sets.union(taggedX, taggedY)
    val ix0 = SetFunction.build("ix", x, taggedX, tagX)
    val ix1 = SetFunction.inclusion(taggedX, unionSet)
    val ix = (ix0 andAlso ix1).flatMap{ case (f, g) => Result(f andThen g) }
    val iy0 = SetFunction.build("iy", y, taggedY, tagY)
    val iy1 = SetFunction.inclusion(taggedY, unionSet)
    val iy = (iy0 andAlso iy1).flatMap{ case (f, g) => Result(f andThen g) }
    ix andAlso iy

  /**
    * Pushout of two functions
    * @param f first function
    * @param g second function
    *  @return Good pair of function from d1(f) and d1(g) to the pushout set, or None.
    */
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

object SetCategory:

  private[cat] def graphOfSets(nodes0: Set[set]): Graph =
    Graph.build[set, SetFunction](
      "Sets",
      nodes0,
      BigSet.of[SetFunction]("set of set functions"),
      (f: SetFunction) => f.d0,
      (f: SetFunction) => f.d1) orCommentTheError "This graph should exist" iHope

  /**
    * Category of finite sets
    */
  object Setf extends SetCategory(FiniteSets)

  private def asMorphism[X](factorSet: FactorSet[X]): SetMorphism[X, Set[X]] =
    SetMorphism.build(factorSet.base, factorSet.content, factorSet.asFunction) iHope

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
  def factorset[T](sourceSet: Set[T], r: BinaryRelation[T, T]): SetMorphism[T, Set[T]] =
    SetCategory.asMorphism(new FactorSet[T](sourceSet, r))

/*
Simple example of a topos (not well-pointed) where not every monad is applicative: Set^{ℤ_2}
 */
