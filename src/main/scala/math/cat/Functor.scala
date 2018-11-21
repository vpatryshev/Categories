package math.cat

import Sets._
import Functions._

/**
 * Functor class: functions for categories.
 *
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
  validate()

  /**
   * Validates this functor.
   * A functor is valid if it is valid as a graph function, and besides,
   * it preserves unit and arrows composition.
   * That is, F(unit(x)) == unit(F(x)), and
   * F(g) o F(f) = F(g o f)
   */
  def validate() {
    for (x <- domain.objects) {
      val ux: XArrows = domain.id(x)
      val y: YObjects = nodesMorphism.apply(x)
      val uy: YArrows = codomain.id(y)
      require(uy == arrowsMorphism(ux), "Functor must preserve units (failed on " +x + ")")
    }

    for {fx <- domain.arrows
         gx <- domain.arrows
         gx_fx <- domain.m(fx, gx)
         fy = arrowsMorphism(fx)
         gy = arrowsMorphism(gx)
         gy_fy <- codomain.m(fy, gy)
    }
      require(gy_fy == arrowsMorphism(gx_fx),
          s"Functor must preserve composition (failed on $fx, $fy, $gx, $gy, $gy_fy, ${arrowsMorphism(gx_fx)})")
  }

  /**
   * Composes two functors
   *
   * @tparam ZObjects nodes type for the third category in the chain
   * @tparam ZArrows arrows type for the third category in the chain
   * @param g : Y -> Z - second functor
   * @return g o this : X -> Z - composition of this functor with functor g
   */
  def compose[ZObjects, ZArrows](
    g: Functor[YObjects, YArrows, ZObjects, ZArrows]):
       Functor[XObjects, XArrows, ZObjects, ZArrows] = {
    require(codomain == g.domain, "Composition not defined")
    val nm = g.nodesMorphism compose this.nodesMorphism
    val am = g.arrowsMorphism compose this.arrowsMorphism 
    new Functor[XObjects, XArrows, ZObjects, ZArrows](
      g.tag +" o " + this.tag, domain, g.codomain, nm, am)
  }

  /**
    * Cone class for this Functor. A cone is an object y (called apex) and a bundle of arrows cx: y -> F(x)
    * for all objects x of domain category, such that F(f) o cx1 = cx2 for f:x1 -> x2.
    * @param apex the cone's apex object in Y
    * @param arrowTo maps each object of x to an arrow from F(x) to the apex.
    */
  case class Cone(apex: YObjects, arrowTo: XObjects => YArrows) {
    require(apex != null, "an apex of a cone can't be null")
    require(arrowTo != null, "a map of arrows of a cone can't be null")

    override def toString: String = "Cone[" + apex + "]"

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
      this == o || (o match {
        case other: Cone =>
          apex == other.apex &&
            domain.forall { x: XObjects => arrowTo(x) == other.arrowTo(x) }
        case somethingElse => false
      })
    }

    override def hashCode: Int = (apex.hashCode /: domain)((hash, x) => hash * 13 + arrowTo(x).hashCode)
  }

  def cone(apex: YObjects)(arrowTo: Iterable[(XObjects, YArrows)]): Option[Cone] = {
    Option(Cone(apex, arrowTo.toMap)) filter (_.isWellFormed)
  }

  /**
    * Lists all possible cones from given object y to this functor.
    * The requirement is that if f1: y -> F(x1) is in the collection of arrows,
    * and there is a g: x1 -> x2, then f2 = F(g) o f1 : y -> F(x2) is also in this collection.
    *
    * @param y an object from which the cone originates.
    * @return a map that maps objects x of domain category to arrows y -> F(x)
    */
  def conesFrom (y: YObjects): Set[Cone] = {
    // this function builds pairs (x, f:y->F(x)) for all f:y->F(x)) for a given x
    val arrowsFromYtoFX = injection (
      (x: XObjects) => codomain.hom(y, nodesMorphism(x)) map { (x, _) }
    )

    // group (x, f: y->F[x]) by x
    val homsGroupedByX: Set[Set[(XObjects, YArrows)]] = domain.objects map arrowsFromYtoFX

    val set: Set[List[(XObjects, YArrows)]] = product(homsGroupedByX)
    val result: Set[Cone] = set flatMap cone(y)
    result
  }

  /**
    * @return all possible cones to this functor.
    */
  def allCones: Set[Cone] = {
    val s: Set[Set[Cone]] = codomain.objects map conesFrom
    Sets.union(s)
  }

  /**
    * Checks if a given Cone is a limit
    * @param candidate cone to check
    * @return true iff it is a limit
    */
  def isLimit(candidate: Cone): Boolean =
    allCones.forall((anyCone: Cone) => candidate.factorsOnRight(anyCone))

  /**
    * @return this functor's limit
    */
  def limit: Option[Cone] = allCones find isLimit

  /**
    * Cocone class for this Functor. A cocone is an object y (called apex) and a bundle of arrows cx: F(x) -> y
    * for all objects x of domain category, such that F(f) o cx1 = cx2 for f:x1 -> x2.
    * @param apex the cone's apex object in Y
    * @param arrowTo maps each object of x to an arrow from F(x) to the apex.
    */
  case class Cocone(apex: YObjects, arrowTo: XObjects => YArrows) {

    override def toString: String = "Cocone[" + apex + "]"

    /**
      * A cone from y1 to F is factored by this cone (with apex y)
      * if there is an h : y1 -> y such that each f1: y1 -> F(x) is equal to
      * f o h, where f: y -> F(x).
      *
      * @param factored a cone that may be factored
      * @return true if it is so
      */
    def factorsOnRight(factored: Cocone): Boolean =
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
        var yToFx0 = arrowTo(domain.d0(f))
        var yToFx1 = arrowTo(domain.d1(f))
        var F_f = arrowsMorphism.apply(f)
        codomain.m(yToFx0, F_f).equals(yToFx1)
      }
    )

    override def equals(o: Any): Boolean = {
      this == o || (o match {
        case other: Cone =>
          apex == other.apex &&
            domain.forall { x: XObjects => arrowTo(x) == other.arrowTo(x) }
        case somethingElse => false
      })
    }

    override def hashCode: Int = (apex.hashCode /: domain)((hash, x) => hash * 13 + arrowTo(x).hashCode)
  }

  def cocone(apex: YObjects)(arrowTo: Iterable[(XObjects, YArrows)]): Option[Cocone] = {
    Option(Cocone(apex, arrowTo.toMap)) filter (_.isWellFormed)
  }

  /**
    * Lists all possible cones from given object y to this functor.
    * The requirement is that if f1: y -> F(x1) is in the collection of arrows,
    * and there is a g: x1 -> x2, then f2 = F(g) o f1 : y -> F(x2) is also in this collection.
    *
    * @param y an object from which the cone originates.
    * @return a map that maps objects x of domain category to arrows y -> F(x)
    */
  def coconesTo (y: YObjects): Set[Cocone] = {
    // this function builds pairs (x, f:y->F(x)) for all f:y->F(x)) for a given x
    val arrowsFromFXtoY = injection (
      (x: XObjects) => codomain.hom(y, nodesMorphism(x)) map { (x, _) }
    )

    // group (x, f: y->F[x]) by x
    val homsGroupedByX: Set[Set[(XObjects, YArrows)]] = domain.objects map arrowsFromFXtoY

    val set: Set[List[(XObjects, YArrows)]] = product(homsGroupedByX)
    val result: Set[Cocone] = set flatMap cocone(y)
    result
  }

  /**
    * @return all possible cones to this functor.
    */
  def allCocones: Set[Cocone] = {
    val s: Set[Set[Cocone]] = codomain.objects map coconesTo
    
    Sets.union(s)
  }

  /**
    * Checks if a given Cone is a limit
    * @param candidate cone to check
    * @return true iff it is a limit
    */
  def isColimit(candidate: Cocone): Boolean =
    allCocones.forall((anyCocone: Cocone) => candidate.factorsOnRight(anyCocone))
  
  def colimit: Option[Cocone] = allCocones find isColimit
  
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
    new Functor[XObjects, XArrows, XObjects, XArrows] ("id", c, c, SetMorphism.unit(c.objects), SetMorphism.unit(c.arrows))

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
    new Functor[XObjects, XArrows, YObjects, YArrows] (y.toString, X, Y, SetMorphism.const(X.objects, Y.objects, y), SetMorphism.const(X.arrows, Y.arrows, Y.id (y)))
}