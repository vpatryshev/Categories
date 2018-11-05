package math.cat

import math.cat.SetFunction._
import math.cat.SetCategory._
import math.cat.Sets._

/**
  * Category where objects are sets
  *
  * @author vpatryshev
  */


class SetCategory(objects: BigSet[Set[Any]]) extends
  Category[Set[Any], SetFunction](graphOfSets(objects)) {

  override val m: (SetFunction, SetFunction) => Option[SetFunction] =
    (f, g) => f compose g
  override val unit: Set[Any] => SetFunction = SetFunction.unit

  override def validate(): Unit = {} // it IS a category

  override def toString: String = "Category of all Scala Sets"

  override def hom(x: Set[Any], y: Set[Any]): Set[SetFunction] =
    SetFunction.exponent(x, y)

  override def isMonomorphism(f: SetFunction): Boolean =
    f.d0.forall(x => f.d0.forall(y => !(f(x) == f(y)) || x == y))

  override def isEpimorphism(arrow: SetFunction): Boolean =
    arrow.d1.forall(y => arrow.d0.exists(x => y == arrow(x)))

  //  @Override
  override def equalizer(f: SetFunction, g: SetFunction): Option[SetFunction] = {
    require((f.d0 eq g.d0) && (f.d1 eq g.d1))
    Option(SetFunction.inclusion(f.d0, x => f(x) == g(x)))
  }

  override def coequalizer(f: SetFunction, g: SetFunction): Option[SetFunction] = {
    require(areParallel(f, g))
    val factorset: Sets.FactorSet[Any] = new Sets.FactorSet[Any](f.d1)

    for (x <- f.d0) {
      factorset.merge(f(x), g(x))
    }
    Option(SetFunction.forFactorset(factorset))
  }

  override def coequalizer(arrowsToEqualize: Iterable[SetFunction]): Option[SetFunction] = {
    if (!arrowsToEqualize.iterator.hasNext) {
      terminal map SetFunction.unit
    } else {
      val f = arrowsToEqualize.head
      val domain = f.d0
      val codomain = f.d1
      for (f <- arrowsToEqualize) {
        require(f.d0 == domain, s"Domain should be $domain")
        require(f.d1 == codomain, s"Codomain should be $codomain")
      }
      val factorset: Sets.FactorSet[Any] = new FactorSet(codomain)

      for (g <- arrowsToEqualize) {
        for (x <- g.d0) factorset.merge(f(x), g(x))
      }
      Option(SetFunction.forFactorset(factorset))
    }
  }

  override def degree(x: Set[Any], n: Int): Option[(Set[Any], List[SetFunction])] = {
    val allMaps: Set[Any] = Sets.exponent(Sets.numbers(n), x).map(m => m)
    val projections = for {
      i <- 0 until n
    } yield new SetFunction(
      tag = s"set^$n",
      d0 = allMaps,
      d1 = x,
      f = { case map: Map[Int, _] => map(i) }
    )
    
    Option((allMaps, projections.toList))
  }

  override lazy val initial: Option[Set[Any]] = Option(Set.empty[Any])

  override lazy val terminal: Option[Set[Any]] = Option(Set(initial))

  override def product(x: Set[Any], y: Set[Any]): Option[(SetFunction, SetFunction)] = {

    val productSet: Set[Any] = Sets.product2(x, y).map(p => p)
    val p1 = new SetFunction("p1", productSet, x, { case (a, b) => a })
    val p2 = new SetFunction("p2", productSet, x, { case (a, b) => b })
    Option((p1, p2))
  }

  override def pullback(f: SetFunction, g: SetFunction):
  Option[(SetFunction, SetFunction)] =
    for {
      prod <- product(f.d0, g.d0)
    } yield {
      val pullbackInProduct = inclusion(prod._1.d0, predicate = { case (a, b) => f(a) == g(a) })
      
      (pullbackInProduct andThen prod._1,
       pullbackInProduct andThen prod._2)
    }

  override def union(x: Set[Any], y: Set[Any]): Option[(SetFunction, SetFunction)] = {
    def tagX(x: Any) = ("x", x)
    def tagY(y: Any) = ("y", y)
    val taggedX: Set[Any] = x map tagX
    val taggedY: Set[Any] = y map tagY
    val unionSet: Set[Any] = Sets.union(taggedX, taggedY)
    val ix = SetFunction("ix", x, taggedX, tagX).andThen(SetFunction.inclusion(taggedX, unionSet))
    val iy = SetFunction("iy", y, taggedX, tagX).andThen(SetFunction.inclusion(taggedX, unionSet))
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
  private type Sets = BigSet[Set[Any]]

  private def graphOfSets(nodes: Sets): Graph[Set[Any], SetFunction] = {
    val arrows = BigSet[SetFunction]()

    new Graph[Set[Any], SetFunction](nodes, arrows, _.d0, _.d1)
  }

  val Setf = new SetCategory(FiniteSets)
}
