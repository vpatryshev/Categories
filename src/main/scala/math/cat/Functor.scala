package math.cat

import math.sets.Sets._
import math.sets.Functions._
import math.sets.Sets
import scalakittens.Result

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
   val objectsMapping: XObjects => YObjects,
   val arrowsMappingCandidate: XArrows => YArrows) extends
      GraphMorphism[XObjects, XArrows, YObjects, YArrows](
     tag, domain, codomain, objectsMapping, arrowsMappingCandidate)
{
  override val d0: Category[XObjects, XArrows] = domain
  override val d1: Category[YObjects, YArrows] = codomain
  
  override val arrowsMapping: XArrows => YArrows = (a: XArrows) => {
    val d0 = domain.d0(a)
    try {
      if (domain.id(d0) == a) codomain.id(objectsMapping(d0)) else arrowsMappingCandidate(a)
    } catch {
      case x: Exception =>
        throw new IllegalArgumentException(
          s"Arrow mapping not found for $a: $d0 -> ${domain.d1(a)}")
    }
  }
  
  try { validate() } catch { case x: Exception =>
      throw x
  }
  
  override def toString: String = s"Functor $tag"

  /**
   * Validates this functor.
   * A functor is valid if it is valid as a graph function, and besides,
   * it preserves identities and arrows composition.
   * That is, F(id(x)) == id(F(x)), and
   * F(g) o F(f) = F(g o f)
   */
  def validate() {
    for (x <- domain.objects) {
      try {
        nodesMapping(x)
      } catch {
        case x: Exception => throw new IllegalArgumentException(s"Object mapping not defined for $x")
      }
    }
    
    for (f <- domain.arrows) {
      val ff = try {
        arrowsMapping(f)
      } catch {
        case x: Exception =>
          throw new IllegalArgumentException(s"Arrow mapping not defined for $f")
      }
      val d0Actual = codomain.d0(ff)
      val d1Actual = codomain.d1(ff)
      val d0Expected = nodesMapping(domain.d0(f))
      val d1Expected = nodesMapping(domain.d1(f))
      require(d0Actual == d0Expected, s"Inconsistent mapping for d0($f)")
      require(d1Actual == d1Expected, s"Inconsistent mapping for d1($f)")
    }
    
    for (x <- domain.objects) {
      val xx = nodesMapping(x)
      val id = domain.id(x)
      val mappedId = try {
        arrowsMapping(id)
      } catch {
        case x: Exception => throw new IllegalArgumentException(s"Arrow mapping undefined for id($x)")
      }
      require(mappedId == codomain.id(xx), s"Arrow mapping inconsistent for id($x)")
    }

    for {fx <- domain.arrows
         gx <- domain.arrows
         gx_fx <- domain.m(fx, gx)
         fy = arrowsMapping(fx)
         gy = arrowsMapping(gx)
         expected = arrowsMapping(gx_fx)
         gy_fy <- codomain.m(fy, gy)
    } {
      require(gy_fy == expected,
        s"Functor must preserve composition (failed on $fx, $fy, $gx, $gy, $gy_fy, $expected)")
    }
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
    val nm = g.nodesMapping compose this.nodesMapping
    val am = g.arrowsMapping compose this.arrowsMapping 
    new Functor[XObjects, XArrows, ZObjects, ZArrows](
      g.tag +" o " + this.tag, domain, g.codomain, nm, am)
  }

  /**
    * Cone class for this Functor. A cone is an object y (called vertex) and a bundle of arrows cx: y -> F(x)
    * for all objects x of domain category, such that F(f) o cx1 = cx2 for f:x1 -> x2.
    * @param vertex the cone's vertex object in Y
    * @param arrowTo maps each object of x to an arrow from F(x) to the vertex.
    */
  case class Cone(vertex: YObjects, arrowTo: XObjects => YArrows) {
    require(vertex != null, "an vertex of a cone can't be null")
    require(arrowTo != null, "a map of arrows of a cone can't be null")

    override def toString: String = "Cone[" + vertex + "]"

    /**
      * A cone from y1 to F is factored by this cone (with vertex y)
      * if there is an h : y1 -> y such that each f1: y1 -> F(x) is equal to
      * f o h, where f: y -> F(x).
      *
      * @param factored a cone that may be factored
      * @return true if it is so
      */
    def factorsOnRight(factored: Cone): Boolean =
      codomain.hom(factored.vertex, vertex) exists { h =>
          domain.objects.forall(
            x => codomain.m(h, arrowTo(x)) contains factored.arrowTo(x))
      }

    /**
      * @return true if this actually a well-formed cone.
      */
    def isWellFormed: Boolean = domain.arrows.forall(
      (f: XArrows) => {
        var yToFx0: YArrows = arrowTo(domain.d0(f))
        var yToFx1: YArrows = arrowTo(domain.d1(f))
        var F_f: YArrows = arrowsMapping(f)
        codomain.m(yToFx0, F_f) contains yToFx1
      }
    )

    override def equals(o: Any): Boolean = {
      o match {
        case other: Cone => eq(other) ||
          (vertex == other.vertex &&
            domain.objects.forall { x: XObjects =>
              Result.forValue(arrowTo(x) == other.arrowTo(x)).getOrElse(false) })
        case somethingElse => false
      }
    }

    override def hashCode: Int = (vertex.hashCode /: domain.objects)((hash, x) => hash * 13 + arrowTo(x).hashCode)
  }

  def cone(vertex: YObjects)(arrowTo: Iterable[(XObjects, YArrows)]): Option[Cone] = {
    Option(Cone(vertex, arrowTo.toMap)) filter (_.isWellFormed)
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
      (x: XObjects) => codomain.hom(y, nodesMapping(x)) map { (x, _) }
    )

    val listOfDomainObjects = domain.objects.toList
    // group (x, f: y->F[x]) by x
    val homsGroupedByX: List[Set[(XObjects, YArrows)]] = listOfDomainObjects map arrowsFromYtoFX

    val coneCandidates: Set[List[(XObjects, YArrows)]] = product(homsGroupedByX)
    val result: Set[Cone] = coneCandidates flatMap cone(y)
    result
  }

  /**
    * @return all possible cones to this functor.
    */
  def allCones: Set[Cone] = {
    val conesGroupedByX: Set[Set[Cone]] = codomain.objects map conesFrom
    Sets.union(conesGroupedByX)
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
    * Cocone class for this Functor. A cocone is an object y (called vertex) and a bundle of arrows cx: F(x) -> y
    * for all objects x of domain category, such that F(f) o cx1 = cx2 for f:x1 -> x2.
    * @param vertex the cone's vertex object in Y
    * @param arrowFrom maps each object of x to an arrow from F(x) to the vertex.
    */
  case class Cocone(vertex: YObjects, arrowFrom: XObjects => YArrows) {

    override def toString: String = "Cocone[" + vertex + "]"

    /**
      * A cocone from F to y1 is factored by this cocone (from F to y)
      * if there is an h : y -> y1 such that each f1: F(x) -> y1 is equal to
      * h o f, where f: F(x) -> y.
      *
      * @param factored a cone that may be factored
      * @return true if it is so
      */
    def factorsOnRight(factored: Cocone): Boolean = {
      val hom = codomain.hom(factored.vertex, vertex)
      val answer = hom exists(
        (h: YArrows) => {
          val failsOn = domain.objects.find(
            (x: XObjects) => !(codomain.m(factored.arrowFrom(x), h) contains arrowFrom(x)))
          val itWorks = failsOn.isEmpty
          itWorks
        }
      )
      
      answer
    }

    /**
      * @return true if this actually a well-formed cone.
      */
    def isWellFormed: Boolean = domain.arrows.forall(
      (f: XArrows) => {
        var Fx02y = arrowFrom(domain.d0(f))
        var Fx12y = arrowFrom(domain.d1(f))
        var F_f = arrowsMapping.apply(f)
        val answer = codomain.m(F_f, Fx12y) contains Fx02y
        answer
      }
    )

    override def equals(o: Any): Boolean = o match {
        case other: Cocone =>
          eq(other) || (
          vertex == other.vertex &&
          domain.objects.forall {
            x: XObjects => arrowFrom(x) == other.arrowFrom(x)
          })
        case somethingElse => false
      }

    override def hashCode: Int =
      (vertex.hashCode /: domain.objects)((hash, x) => hash * 13 + arrowFrom(x).hashCode)
  }

  def cocone(vertex: YObjects)(arrowTo: Iterable[(XObjects, YArrows)]): Option[Cocone] = {
    Option(Cocone(vertex, arrowTo.toMap)) filter (_.isWellFormed)
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
    def arrowsFromFXtoY(x: XObjects)=
      codomain.hom(nodesMapping(x), y) map { (x, _) }

    // group (x, f: y->F[x]) by x
    val homsGroupedByX: List[Set[(XObjects, YArrows)]] = domain.objects.toList map arrowsFromFXtoY

    val coconeCandidates: Set[List[(XObjects, YArrows)]] = product(homsGroupedByX)
    val result: Set[Cocone] = coconeCandidates flatMap cocone(y)
    result
  }

  /**
    * @return all possible cones to this functor.
    */
  def allCocones: Set[Cocone] = {
    val coconesGrouped: Set[Set[Cocone]] = codomain.objects map coconesTo
    
    Sets.union(coconesGrouped)
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
   * Factory method. Builds identity functor for a category (identity functor).
   *
   * @tparam XObjects type of domain objects
   * @tparam XArrows type of domain arrows
   * @param c the category
   * @return identity functor on the given category
   */
  def id[XObjects, XArrows](c: Category[XObjects, XArrows]):
      Functor[XObjects, XArrows, XObjects, XArrows] =
    new Functor[XObjects, XArrows, XObjects, XArrows] ("id", c, c, SetMorphism.id(c.objects), SetMorphism.id(c.arrows))

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
   * @return constant functor on X that takes maps all objects to y and all arrows to y's identities.
   */
  def const[XObjects, XArrows, YObjects, YArrows] (
      X: Category[XObjects, XArrows], Y: Category[YObjects, YArrows], y: YObjects):
    Functor[XObjects, XArrows, YObjects, YArrows] =
    new Functor[XObjects, XArrows, YObjects, YArrows](
      y.toString, X, Y,
      SetMorphism.const(X.objects, Y.objects, y),
      SetMorphism.const(X.arrows, Y.arrows, Y.id(y)))
}