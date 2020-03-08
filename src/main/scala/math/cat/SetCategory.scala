package math.cat

import scala.language.implicitConversions
import scala.language.postfixOps

import math.cat.SetCategory._
import math.cat.SetFunction._
import math.sets.Sets._
import math.sets.{BigSet, FactorSet, Sets}
import scalakittens.Result._
import scalakittens.{Good, Result}

/**
  * Category where objects are sets
  */

class SetCategory(objects: BigSet[set])
  extends Category {
  val graph = graphOfSets(objects)
  type Node = set
  type Arrow = SetFunction

  override def d0(f: SetFunction): set = f.d0

  override def d1(f: SetFunction): set = f.d1

  override def m(f: Arrow, g: Arrow): Option[Arrow] = f compose g

  override def id(s: set): SetFunction = SetFunction.id(s)

  override def toString: String = "Category of all Scala Sets"
  
  // this makes our category Cartesian-closed
  def exponent(x: set, y: set): set = Sets.exponent(x, y)

  override def arrowsBetween(x: set, y: set): Set[SetFunction] =
    SetFunction.exponent(x, y)

  override def isMonomorphism(f: SetFunction): Boolean =
    f.d0.forall(x ⇒ f.d0.forall(y ⇒ !(f(x) == f(y)) || x == y))

  override def isEpimorphism(arrow: SetFunction): Boolean =
    arrow.d1 forall {y ⇒ arrow.d0 exists {y == arrow(_)}}

  override def equalizer(f: SetFunction, g: SetFunction): Result[SetFunction] = {
    require((f.d0 eq g.d0) && (f.d1 eq g.d1))
    val filtrator: (Any ⇒ Boolean) ⇒ SetFunction = SetFunction.filterByPredicate(f.d0)
    val inclusion = filtrator(x ⇒ f(x) == g(x))
    Good(inclusion) filter { i ⇒ objects.contains(i.d0) }
  }

  override def coequalizer(f: SetFunction, g: SetFunction): Result[SetFunction] = 
  OKif(areParallel(f, g), s"Arrows $f and $g must be parallel") andThen {
    val theFactorset: factorset = new FactorSet[Any](f.d1)
    OKif(contains(theFactorset untyped)) returning {
      for (x ← f.d0) {
        theFactorset.merge(f(x), g(x))
      }
      SetFunction.forFactorset(theFactorset)
    }
  }

  override def coequalizer(arrowsToEqualize: Iterable[SetFunction]): Result[SetFunction] = {
    OKif(arrowsToEqualize.iterator.hasNext, "Need at least one arrow for coequalizer") andThen {
      val f = arrowsToEqualize.head
      val domain = f.d0
      val codomain = f.d1
      val dataOk = Result.traverse(for (f ← arrowsToEqualize) yield {
        OKif(f.d0 == domain, s"Domain should be $domain") andAlso
          OKif(f.d1 == codomain, s"Codomain should be $codomain")
      })
      
      dataOk andThen {
        val theFactorset: factorset = new FactorSet(codomain)

        for (g ← arrowsToEqualize) {
          for (x ← g.d0) theFactorset.merge(f(x), g(x))
        }
        Option(SetFunction.forFactorset(theFactorset))
      }
    }
  }

  override def degree(x: set, n: Int): Result[(set, List[SetFunction])] = {
    Result.OKif(n >= 0, s"No negative degree exists yet, for n=$n") andThen {
      val actualDomain: Set[List[Any]] =
        Sets.exponent(Sets.numbers(n), x.asInstanceOf[set]) map {
          m ⇒ m.toList.sortBy(_._1).map(_._2)
        }

      val domain: set = actualDomain untyped

      def takeElementAt(i: Int)(obj: Any) = obj match {
        case m: List[Any] ⇒ m(i)
        case other ⇒ throw new IllegalArgumentException(s"expected a map, got $other")
      }

      val projections = for {
        i ← 0 until n
      } yield {
        val function = takeElementAt(i)(_)
        SetFunction.build(s"set^$n", domain, x, function)
      }
      
      Result.traverse(projections) map {ps ⇒ (domain, ps.toList)}
    }
  }

  // need to filter, to eliminate the value that does not belong
  override lazy val initial: Result[set] = Good(Sets.Empty) filter contains

  override lazy val terminal: Result[set] = {
    val option1: Result[set] = initial map (setOf(_))
    // need to filter, to eliminate the value that does not belong
    option1 filter contains
  }

  override def product(x: set, y: set): Result[(SetFunction, SetFunction)] = {

    val productSet: set = Sets.product2(x, y) untyped
    val p1 = SetFunction.build("p1", productSet, x, { case (a, b) ⇒ a })
    val p2 = SetFunction.build("p2", productSet, y, { case (a, b) ⇒ b })
    p1 andAlso p2
  }

  override def pullback(f: SetFunction, g: SetFunction):
  Result[(SetFunction, SetFunction)] =
    for {
      prod ← product(f.d0, g.d0)
    } yield {
      val productSet = prod._1.d0
      val pullbackInProduct =
        filterByPredicate(productSet)(predicate = { case (a, b) ⇒ f(a) == g(b) })
      
      (pullbackInProduct andThen prod._1,
       pullbackInProduct andThen prod._2)
    }

  override def union(x: set, y: set): Result[(SetFunction, SetFunction)] = {
    def tagX(x: Any) = ("x", x)
    def tagY(y: Any) = ("y", y)
    val taggedX: set = x map tagX
    val taggedY: set = y map tagY
    val unionSet: set = Sets.union(taggedX, taggedY)
    val ix0 = SetFunction.build("ix", x, taggedX, tagX)
    val ix1 = SetFunction.inclusion(taggedX, unionSet)
    val ix =ix0 andAlso ix1 map { case (f, g) ⇒ f andThen g }
    val iy0 = SetFunction.build("iy", y, taggedY, tagY)
    val iy1 = SetFunction.inclusion(taggedY, unionSet)
    val iy = iy0 andAlso iy1 map { case (f, g) ⇒ f andThen g }
    val union = ix andAlso iy
    union
  }

  override def pushout(f: SetFunction, g: SetFunction): Result[(SetFunction, SetFunction)] =
    for {
      (left, right) ← union(f.d1, g.d1)
      coeq ← coequalizer(f andThen left, g andThen right)
    } yield (left andThen coeq, right andThen coeq)

  override def hashCode: Int = getClass.hashCode * 7 + objects.hashCode

  override def equals(x: Any): Boolean = x match {
    case sc: SetCategory ⇒ objects == sc.objects
    case other ⇒ false
  }

}

object SetCategory {

  private[cat] def graphOfSets(nodes0: BigSet[set]): Graph = {
    Graph.build[set, SetFunction](
      "Sets",
      nodes0,
      BigSet.of[SetFunction],
      (f: SetFunction) ⇒ f.d0,
      (f: SetFunction) ⇒ f.d1)
  }.getOrElse(throw new InstantiationException("This graph should exist"))

  object Setf extends SetCategory(FiniteSets)
}

/*
простой пример топоса, где не всякая монада является апликативным функтором. Это Set^{ℤ_2}
 */
