package math.cat

import SetMorphism._
import Sets._
import Functions._

/**
 * Functor class: functions for categories.
 *
 * @author Vlad Patryshev
 * All source code is stored at <a href="http://code.google.com/p/categories/">http://code.google.com/p/categories/</a>
 * TODO: have a decent factory, don't use the constructor, too many parameters, eh
 * @tparam XObjects type of nodes in the first category
 * @tparam XArrows type of arrows in the first category
 * @tparam YObjects type of nodes in the second category
 * @tparam YArrows type of arrows in the second category
 */
class Functor[XObjects, XArrows, YObjects, YArrows]
             (override val tag: String,
              val domain: Category[XObjects, XArrows],
              val codomain: Category[YObjects, YArrows],
              val objectsMorphism: XObjects => YObjects,
              override val arrowsMorphism: XArrows => YArrows) extends
      GraphMorphism[XObjects, XArrows, Category[XObjects, XArrows], YObjects, YArrows, Category[YObjects, YArrows]](
                    tag, domain, codomain, objectsMorphism, arrowsMorphism)
{
  validate

  /**
   * Validates this functor.
   * A functor is valid if it is valid as a graph function, and besides,
   * it preserves unit and arrows composition.
   * That is, F(unit(x)) == unit(F(x)), and
   * F(g) o F(f) = F(g o f)
   */
  def validate {
    for (x <- domain.objects) {
      val ux: XArrows = domain.unit(x)
      val y: YObjects = nodesMorphism.apply(x)
      val uy: YArrows = codomain.unit(y)
      require(uy == arrowsMorphism(ux), "Functor must preserve units (failed on " +x + ")")
    }

    for (fx <- domain.arrows;
         gx <- domain.arrows;
         if (domain.follows(gx, fx))) {
      val gx_fx: XArrows = domain.m(fx, gx)
      val fy: YArrows = arrowsMorphism(fx)
      val gy: YArrows = arrowsMorphism(gx)
      val gy_fy: YArrows = codomain.m(fy, gy)
      require(gy_fy == arrowsMorphism(gx_fx),
              "Functor must preserve composition (failed on " + fx + ", " + fy + ", " + gx + ", " + gy + ", " + gy_fy + ", " + arrowsMorphism(gx_fx) + ")")
    }
  }

  /**
   * Composes two functors
   *
   * @tparam XObjects nodes type for the category in the chain
   * @tparam XArrows arrows type for the first category in the chain
   * @tparam YObjects nodes type for the second category in the chain
   * @tparam YArrows arrows type for the second category in the chain
   * @param [ZObjects] nodes type for the third category in the chain
   * @param [ZArrows] arrows type for the third category in the chain
   * @param g : Y -> Z - second functor
   * @return g o this : X -> Z - composition of this functor with functor g
   */
  def compose[ZObjects, ZArrows](g: Functor[YObjects, YArrows, ZObjects, ZArrows]) = {
    require(codomain == g.domain, "Composition not defined")
    val nm = g.nodesMorphism compose this.nodesMorphism
    val am = g.arrowsMorphism compose this.arrowsMorphism 
    new Functor[XObjects, XArrows, ZObjects, ZArrows](this.tag +" o " + g.tag, domain, g.codomain, nm, am)
  }

  /**
   * Cone class for this Functor. A cone is an object y (called apex) and a bundle of arrows cx: y -> F(x)
   * for all objects x of domain category, such that F(f) o cx1 = cx2 for f:x1 -> x2.
   * @param apex the cone's apex object in Y
   * @param arrowTo maps each object of x to an arrow from F(x) to the apex.
   */
  case class Cone(val apex: YObjects, val arrowTo: XObjects => YArrows) {
    require(apex != null, "an apex of a cone can't be null")
    require(arrowTo != null, "a map of arrows of a cone can't be null")

    override def toString: String = {
      return "Cone[" + apex + "]"
    }

    /**
     * A cone from y1 to F is factored by this cone (with apex y)
     * if there is an h : y1 -> y such that each f1: y1 -> F(x) is equal to
     * f o h, where f: y -> F(x).
     *
     * @param factored a cone that may be factored
     * @return true if it is so
     */
    def factorsOnRight(factored: Cone): Boolean =
      codomain.hom(factored.apex, apex).exists(
        (h: YArrows) =>
          domain.objects.forall(
            (x: XObjects) => codomain.m(h, arrowTo(x)).equals(factored.arrowTo(x)))
      )

    /**
     * @return true if this actually a well-formed cone.
     */
    def isWellFormed: Boolean = domain.arrows.forall(
      (f: XArrows) => {
          var yToFx0: YArrows = arrowTo(domain.d0(f))
          var yToFx1: YArrows = arrowTo(domain.d1(f))
          var F_f: YArrows = arrowsMorphism.apply(f)
          codomain.m(yToFx0, F_f).equals(yToFx1)
        }
      )

    override def equals(o: Any): Boolean = {
      this == o || (
        o.isInstanceOf[Cone] && {
          val other: Cone = o.asInstanceOf[Cone]
          apex == other.apex &&
          domain.forall { (x: XObjects) => arrowTo(x) == other.arrowTo(x) }
        }
      )
    }

    override def hashCode: Int = (apex.hashCode /: domain)((hash, x) => hash * 13 + arrowTo(x).hashCode)
  }

  /**
   * Lists all possible cones from given object y to this functor.
   * The requirement is that if f1: y -> F(x1) is in the collection of arrows,
   * and there is a g: x1 -> x2, then f2 = F(g) o f1 : y -> F(x2) is also in this collection.
   *
   * @param y an object from which the cone originates.
   * @return a map that maps objects x of domain category to arrows y -> F(x)
   */
  // will need a pullback to build {(x, a) where a: x->f(y)}
//  def conesFrom (y: YObjects): Set[Cone] = {
//    // a set of sets of pairs (x, a: y -> F(x)), indexed by x
//    val homs = domain.objects.map(
//      (x:XObjects) => (x, codomain.hom(y, objectsMorphism(x))))
//    val homsProjection = SetMorphism(homs, domain.objects, (p:(XObjects, Set[YArrows])) => p._1)
//
//    // a set of sequences
//    val productOfHoms = Sets.product(homsProjection)
//    null
////  var setOfSetsOfPairs: Set[Set[Pair[Any, Any]]] = Set (new IterableToSet[Pair[XObjects, YArrows]].map (productOfHoms) )
////  var allMaps: Set[Map[XObjects, YArrows]] = new PairsToMap[XObjects, YArrows].map (setOfSetsOfPairs)
//    for (functions <- allMaps;
//         c = Cone(y, functions);
//         if (c isWellFormed)) yield c
//  }

/**
 * @return all possible cones to this functor.
 *
  def allCones: Set[Cone] = {
    val s: Set[Set[Cone]] = codomain.objects.map((y: YObjects) => conesFrom(y))
    Sets.union(s)
  }  

  **
   * Checks if a given Cone is a limit
   * @param candidate cone to check
   * @return true iff it is a limit
   *
  def isLimit(candidate: Cone) =
    allCones.forall((anyCone: Cone) => candidate.factorsOnRight(anyCone))

  **
   * Builds a predicate that checks whether a given map constitutes a cone from an object to this functor.
   *
  val isaCone = (candidate: Cone) => candidate.isWellFormed
*/
  /**
    * Lists all possible cocones from this functor to a given object y.
    * The requirement is that if f1: F(x1) -> y is in the collection of arrows,
    * and there is a g: x0 -> x1, then f0 = f1 o F(g) F(x0) -> y is also in this collection.
    *
    * @param y an object at which the cone terminates.
    * @return a map that maps objects x of domain category to arrows F(x) -> y
    *
   private[cat] def coconesTo(y: YObjects): Set[Cocone] = {
 var homs: Set[Set[Pair[Any, Any]]] = new Injection[XObjects, Set[Pair[Any, Any]]] {
   def apply(x: XObjects): Set[Pair[XObjects, YArrows]] = {
     return BasePair.withLeft[XObjects, YArrows](x).map(codomain.arrows(nodesMorphism.apply(x), y))
   }
 }.map(domain.objects)
 var productOfHoms: Set[_ <: Iterable[Any]] = Cartesian.product(homs)
 var setOfSetsOfPairs: Set[Set[Pair[Any, Any]]] = Set(new IterableToSet[Pair[XObjects, YArrows]].map(productOfHoms))
 var allMaps: Set[Map[XObjects, YArrows]] = new PairsToMap[XObjects, YArrows].map(setOfSetsOfPairs)
 var makeCocone: Injection[Map[XObjects, YArrows], Cocone] = new Injection[Map[XObjects, YArrows], Cocone] {
   def apply(map: Map[XObjects, YArrows]): Cocone = {
     return new Cocone(y, map)
   }
 }
 return makeCocone.map(allMaps) filter (_.isWellFormed)
 }
  */
}

object Functor {
  def apply[XObjects, XArrows, YObjects, YArrows] (
            tag: String,
            domain: Category[XObjects, XArrows],
            codomain: Category[YObjects, YArrows],
            objectsMorphism: XObjects => YObjects,
            arrowsMorphism: XArrows => YArrows) =
    new Functor[XObjects, XArrows, YObjects, YArrows] (tag, domain, codomain, objectsMorphism, arrowsMorphism)

  def apply[XObjects, XArrows, YObjects, YArrows] (
            domain: Category[XObjects, XArrows],
            codomain: Category[YObjects, YArrows],
            objectsMorphism: XObjects => YObjects,
            arrowsMorphism: XArrows => YArrows) =
    new Functor[XObjects, XArrows, YObjects, YArrows] ("", domain, codomain, objectsMorphism, arrowsMorphism)

  /**
   * Factory method. Builds unit functor for a category (identity functor).
   *
   * @tparam XObjects type of domain objects
   * @tparam XArrows type of domain arrows
   * @param c the category
   * @return identity functor on the given category
   */
  def unit[XObjects, XArrows] (c: Category[XObjects, XArrows]):
      Functor[XObjects, XArrows, XObjects, XArrows] =
    new Functor[XObjects, XArrows, XObjects, XArrows] ("1", c, c, SetMorphism.unit(c.objects), SetMorphism.unit(c.arrows))

  /**
   * Factory method. Builds constant functor from a category to an object in another.
   *
   * @tparam XObjects type of objects in the first category
   * @tparam XArrows type of arrows in the first category
   * @tparam YObjects type of objects in the second category
   * @tparam YArrows type of arrows in the second category
   * @param X the category
   * @param Y another category
   * @param y an object in category Y
   * @return constant functor on X that takes maps all objects to y and all arrows to y's unit.
   */
  def const[XObjects, XArrows, YObjects, YArrows] (X: Category[XObjects, XArrows], Y: Category[YObjects, YArrows], y: YObjects): Functor[XObjects, XArrows, YObjects, YArrows] =
    new Functor[XObjects, XArrows, YObjects, YArrows] (y.toString, X, Y, SetMorphism.const(X.objects, Y.objects, y), SetMorphism.const(X.arrows, Y.arrows, Y.unit (y)))
}