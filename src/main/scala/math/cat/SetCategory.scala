package math.cat

import math.Base.IntMap
import math.cat.SetFunction._
import math.cat.SetCategory._
import math.sets.{BigSet, FactorSet, Sets}
import math.sets.Sets._

/**
  * Category where objects are sets
  */

class SetCategory(objects: BigSet[Set[Any]]) extends
  Category[set, SetFunction](graphOfSets(objects)) {

  override def d0(f: SetFunction): set = f.d0

  override def d1(f: SetFunction): set = f.d1

  override val m: (SetFunction, SetFunction) => Option[SetFunction] =
    (f, g) => f compose g
  override val id: set => SetFunction = SetFunction.id

  override protected def validate(): Unit = {} // it IS a category

  override def toString: String = "Category of all Scala Sets"

  override def arrowsBetween(x: set, y: set): Set[SetFunction] =
    SetFunction.exponent(x, y)

  override def isMonomorphism(f: SetFunction): Boolean =
    f.d0.forall(x => f.d0.forall(y => !(f(x) == f(y)) || x == y))

  override def isEpimorphism(arrow: SetFunction): Boolean =
    arrow.d1 forall {y => arrow.d0 exists {y == arrow(_)}}

  override def equalizer(f: SetFunction, g: SetFunction): Option[SetFunction] = {
    require((f.d0 eq g.d0) && (f.d1 eq g.d1))
    val inclusion = SetFunction.inclusion(f.d0, x => f(x) == g(x))
    Option(inclusion) filter { i => objects.contains(i.d0) }
  }

  override def coequalizer(f: SetFunction, g: SetFunction): Option[SetFunction] = {
    require(areParallel(f, g), s"Arrows $f and $g must be parallel")
    val theFactorset: factorset = new FactorSet[Any](f.d1)
    
    if (contains(theFactorset untyped)) {
      for (x <- f.d0) {
        theFactorset.merge(f(x), g(x))
      }
      Option(SetFunction.forFactorset(theFactorset))
    } else None
  }

  override def coequalizer(arrowsToEqualize: Iterable[SetFunction]): Option[SetFunction] = {
    require(arrowsToEqualize.iterator.hasNext, "Need at least one arrow for coequalizer")
    val f = arrowsToEqualize.head
    val domain = f.d0
    val codomain = f.d1
    for (f <- arrowsToEqualize) {
      require(f.d0 == domain, s"Domain should be $domain")
      require(f.d1 == codomain, s"Codomain should be $codomain")
    }
    val theFactorset: factorset = new FactorSet(codomain)

    for (g <- arrowsToEqualize) {
      for (x <- g.d0) theFactorset.merge(f(x), g(x))
    }
    Option(SetFunction.forFactorset(theFactorset))
  }

  override def degree(x: set, n: Int): Option[(set, List[SetFunction])] = {
    require(n >= 0, s"Degree of $n can't be calculated")
    val domain: set = Sets.exponent(Sets.numbers(n), x) untyped
    
    // TODO: use Shapeless, get rid of warning
    def takeElementAt(i: Int)(obj: Any) = obj match {
      case m: IntMap[_] => m(i)
    }

    val projections = for {
      i <- 0 until n
    } yield new SetFunction(
      tag = s"set^$n",
      d0 = domain,
      d1 = x,
      f = takeElementAt(i)
    )
    
    Option((domain, projections.toList))
  }

  override lazy val initial: Option[set] = Option(Sets.Empty) filter (this contains)

  override lazy val terminal: Option[set] = {
    val option1: Option[set] = Option(setOf(initial))
    option1 filter (this contains)
  }

  override def product(x: set, y: set): Option[(SetFunction, SetFunction)] = {

    val productSet: set = Sets.product2(x, y) untyped
    val p1 = new SetFunction("p1", productSet, x, { case (a, b) => a })
    val p2 = new SetFunction("p2", productSet, y, { case (a, b) => b })
    Option((p1, p2))
  }

  override def pullback(f: SetFunction, g: SetFunction):
  Option[(SetFunction, SetFunction)] =
    for {
      prod <- product(f.d0, g.d0)
    } yield {
      val productSet = prod._1.d0
      val pullbackInProduct =
        inclusion(productSet, predicate = { case (a, b) => f(a) == g(b) })
      
      (pullbackInProduct andThen prod._1,
       pullbackInProduct andThen prod._2)
    }

  override def union(x: set, y: set): Option[(SetFunction, SetFunction)] = {
    def tagX(x: Any) = ("x", x)
    def tagY(y: Any) = ("y", y)
    val taggedX: set = x map tagX
    val taggedY: set = y map tagY
    val unionSet: set = Sets.union(taggedX, taggedY)
    val ix = SetFunction("ix", x, taggedX, tagX).andThen(SetFunction.inclusion(taggedX, unionSet))
    val iy = SetFunction("iy", y, taggedY, tagY).andThen(SetFunction.inclusion(taggedY, unionSet))
    Option((ix, iy))
  }

  override def pushout(f: SetFunction, g: SetFunction): Option[(SetFunction, SetFunction)] =
    for {
      (left, right) <- union(f.d1, g.d1)
      coeq <- coequalizer(f andThen left, g andThen right)
    } yield (left andThen coeq, right andThen coeq)

  override def hashCode: Int = getClass.hashCode * 7 + objects.hashCode

  override def equals(x: Any): Boolean = x match {
    case sc: SetCategory => objects == sc.objects
    case other => false
  }
}

object SetCategory {

  private[cat] def graphOfSets(nodes0: BigSet[set]): Graph[set, SetFunction] = {

    new Graph[set, SetFunction] {
      def nodes: BigSet[set] = nodes0
      def arrows: BigSet[SetFunction] = BigSet[SetFunction]()
      def d0(f: SetFunction): set = f.d0
      def d1(f: SetFunction): set = f.d1
    }
  }

  object Setf extends SetCategory(FiniteSets)
}
