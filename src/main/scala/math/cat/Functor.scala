package math.cat

import math.sets.Sets._
import math.sets.Functions._
import math.sets.Sets
import scalakittens.Result

/**
 * Functor class: functions for categories.
 *
 * TODO: have a decent factory, don't use the constructor, too many parameters, eh
 * @tparam X the first category type 
 * @tparam Y the second category type
 */
abstract class Functor[X <: Category[_, _], Y <: Category[_, _]]
  (val tag: String,
   val domain: X,
   val codomain: Y
  ) extends GraphMorphism[X, Y] {

  val d0: X = domain
  val d1: Y = codomain

  def objectsMapping(x: d0.Object): d1.Object
  def arrowsMappingCandidate(a: d0.Arrow): d1.Arrow
  
  override def arrowsMapping(a: d0.Arrow): d1.Arrow = {
    val d0X: d0.Object = d0.d0(a)
    try {
      if (d0.id(d0X) == a) {
        val d0Y: d1.Object = objectsMapping(d0X)
        d1.id(d0Y)
      } else arrowsMappingCandidate(a)
    } catch {
      case x: Exception =>
        throw new IllegalArgumentException(
          s"Arrow mapping not found for $a: $d0X -> ${d0.d1(a)}")
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
    for (x <- d0.objects) {
      try {
        objectsMapping(x)
      } catch {
        case x: Exception => throw new IllegalArgumentException(s"Object mapping not defined for $x")
      }
    }
    
    for (f <- d0.arrows) {
      val ff: d1.Arrow = try {
        arrowsMapping(f)
      } catch {
        case x: Exception =>
          throw new IllegalArgumentException(s"Arrow mapping not defined for $f")
      }
      val d0Actual = d1.d0(ff)
      val d1Actual = d1.d1(ff)
      val d0Expected = objectsMapping(d0.d0(f))
      val d1Expected = objectsMapping(d0.d1(f))
      require(d0Actual == d0Expected, s"Inconsistent mapping for d0($f)")
      require(d1Actual == d1Expected, s"Inconsistent mapping for d1($f)")
    }
    
    for (x <- d0.objects) {
      val xx: d1.Object = objectsMapping(x)
      val id: d0.Arrow = d0.id(x)
      val mappedId = try {
        arrowsMapping(id)
      } catch {
        case x: Exception => throw new IllegalArgumentException(s"Arrow mapping undefined for id($x)")
      }
      require(mappedId == d1.id(xx), s"Arrow mapping inconsistent for id($x)")
    }

    for {
         fx: d0.Arrow <- d0.arrows
         gx: d0.Arrow <- d0.arrows
         gx_fx: d0.Arrow <- d0.m(fx, gx)
         fy: d1.Arrow = arrowsMapping(fx)
         gy: d1.Arrow = arrowsMapping(gx)
         expected = arrowsMapping(gx_fx)
         gy_fy <- d1.m(fy, gy)
    } {
      require(gy_fy == expected,
        s"Functor must preserve composition (failed on $fx, $fy, $gx, $gy, $gy_fy; $expected)")
    }
  }

  /**
   * Composes two functors
   *
   * @tparam Z the third category in the chain
   * @param g : Y -> Z - second functor
   * @return g o this : X -> Z - composition of this functor with functor g
   */
  def compose[Z <: Category[_, _]](
    g: Functor[Y, Z]):
       Functor[X, Z] = {
    require(d1 == g.d0, "Composition not defined")
    val f = this
    new Functor[X, Z](
      g.tag +" o " + this.tag, domain, g.codomain) { comp: Functor[X, Z] =>
      override def objectsMapping(x: d0.Object): comp.d1.Object = {
        val y: g.d0.Object = objectsMapping(x).asInstanceOf[g.d0.Object] // somehow Scala does not deduce it
        g.objectsMapping(y).asInstanceOf[comp.d1.Object]
      }

      override def arrowsMappingCandidate(a: d0.Arrow): comp.d1.Arrow = {
        g.arrowsMapping(f.arrowsMapping(a.asInstanceOf[f.d0.Arrow]).asInstanceOf[g.d0.Arrow]).asInstanceOf[comp.d1.Arrow] // we either need dependent types, or...
      }

      // TODO(vlad): get rid of this naming problem
      // the following override is not required, because it's just another name for object mapping
      override def nodesMapping(x: d0.Node): comp.d1.Node = {
        val y: g.d0.Node = nodesMapping(x).asInstanceOf[g.d0.Node] // somehow Scala does not deduce it
        g.nodesMapping(y).asInstanceOf[comp.d1.Node]
      }
    }
  }

  /**
    * Cone class for this Functor. A cone is an object y (called vertex) and a bundle of arrows cx: y -> F(x)
    * for all objects x of domain category, such that F(f) o cx1 = cx2 for f:x1 -> x2.
    * @param vertex the cone's vertex object in Y
    * @param arrowTo maps each object of x to an arrow from F(x) to the vertex.
    */
  case class Cone(vertex: d1.Object, arrowTo: d0.Object => d1.Arrow) {
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
      d1.hom(factored.vertex, vertex) exists { h =>
          d0.objects.forall(
            x => d1.m(h, arrowTo(x)) contains factored.arrowTo(x))
      }

    /**
      * @return true if this actually a well-formed cone.
      */
    def isWellFormed: Boolean = d0.arrows.forall(
      (f: d0.Arrow) => {
        var yToFx0: d1.Arrow = arrowTo(d0.d0(f))
        var yToFx1: d1.Arrow = arrowTo(d0.d1(f))
        var F_f: d1.Arrow = arrowsMapping(f)
        d1.m(yToFx0, F_f) contains yToFx1
      }
    )

    override def equals(o: Any): Boolean = {
      o match {
        case other: Cone => eq(other) ||
          (vertex == other.vertex &&
            d0.objects.forall { x: d0.Object =>
              Result.forValue(arrowTo(x) == other.arrowTo(x)).getOrElse(false) })
        case somethingElse => false
      }
    }

    override def hashCode: Int = (vertex.hashCode /: d0.objects)((hash, x) => hash * 13 + arrowTo(x).hashCode)
  }

  def cone(vertex: d1.Object)(arrowTo: Iterable[(d0.Object, d1.Arrow)]): Option[Cone] = {
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
  def conesFrom (y: d1.Object): Set[Cone] = {
    // this function builds pairs (x, f:y->F(x)) for all f:y->F(x)) for a given x
    val arrowsFromYtoFX: Injection[d0.Object, Set[(d0.Object, d1.Arrow)]] = injection (
      (x: d0.Object) => d1.arrowsBetween(y, objectsMapping(x)) map { (x, _) }
    )

    val listOfDomainObjects: List[d0.Object] = d0.objects.toList
    // group (x, f: y->F[x]) by x
    val homsGroupedByX: List[Set[(d0.Object, d1.Arrow)]] = listOfDomainObjects map arrowsFromYtoFX

    val coneCandidates: Set[List[(d0.Object, d1.Arrow)]] = product(homsGroupedByX)
    val result: Set[Cone] = coneCandidates flatMap cone(y)
    result
  }

  /**
    * @return all possible cones to this functor.
    */
  def allCones: Set[Cone] = {
    val conesGroupedByX: Set[Set[Cone]] = d1.objects map conesFrom
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
  case class Cocone(vertex: d1.Object, arrowFrom: d0.Object => d1.Arrow) {

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
      val hom = d1.hom(factored.vertex, vertex)
      val answer = hom exists(
        (h: d1.Arrow) => {
          val failsOn = d0.objects.find(
            (x: d0.Object) => !(d1.m(factored.arrowFrom(x), h) contains arrowFrom(x)))
          val itWorks = failsOn.isEmpty
          itWorks
        }
      )
      
      answer
    }

    /**
      * @return true if this actually a well-formed cone.
      */
    def isWellFormed: Boolean = d0.arrows.forall(
      (f: d0.Arrow) => {
        var Fx02y = arrowFrom(d0.d0(f))
        var Fx12y: d1.Arrow = arrowFrom(d0.d1(f))
        var F_f: d1.Arrow = arrowsMapping(f)
        val answer = d1.m(F_f, Fx12y) contains Fx02y
        answer
      }
    )

    override def equals(o: Any): Boolean = o match {
        case other: Cocone =>
          eq(other) || (
          vertex == other.vertex &&
          d0.objects.forall {
            x: d0.Object => arrowFrom(x) == other.arrowFrom(x)
          })
        case somethingElse => false
      }

    override def hashCode: Int =
      (vertex.hashCode /: d0.objects)((hash, x) => hash * 13 + arrowFrom(x).hashCode)
  }

  def cocone(vertex: d1.Object)(arrowTo: Iterable[(d0.Object, d1.Arrow)]): Option[Cocone] = {
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
  def coconesTo (y: d1.Object): Set[Cocone] = {
    // this function builds pairs (x, f:y->F(x)) for all f:y->F(x)) for a given x
    def arrowsFromFXtoY(x: d0.Object): Set[(d0.Object, d1.Arrow)] =
      d1.arrowsBetween(objectsMapping(x), y) map { (x, _) }

    // group (x, f: y->F[x]) by x
    val homsGroupedByX: List[Set[(d0.Object, d1.Arrow)]] = d0.objects.toList map arrowsFromFXtoY

    val coconeCandidates: Set[List[(d0.Object, d1.Arrow)]] = product(homsGroupedByX)
    val result: Set[Cocone] = coconeCandidates flatMap cocone(y)
    result
  }

  /**
    * @return all possible cones to this functor.
    */
  def allCocones: Set[Cocone] = {
    val coconesGrouped: Set[Set[Cocone]] = d1.objects map coconesTo
    
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
  def apply[X <: Category[_, _], Y <: Category[_, _]] (
    tag: String,
    dom: X,
    codom: Y)(
    objectsMorphism: dom.Object => codom.Object,
    arrowsMorphism: dom.Arrow => codom.Arrow): Functor[X, Y] =
    new Functor[X, Y] (tag, dom, codom) {
      override def objectsMapping(x: d0.Object): d1.Object =
        objectsMorphism(x.asInstanceOf[dom.Object]).asInstanceOf[d1.Object]
      override def arrowsMappingCandidate(a: d0.Arrow): d1.Arrow =
        arrowsMorphism(a.asInstanceOf[dom.Arrow]).asInstanceOf[d1.Arrow]
      override def nodesMapping(n: d0.Node): d1.Node =
        objectsMorphism(n.asInstanceOf[dom.Node]).asInstanceOf[d1.Node]
    }
    
  def apply[X <: Category[_,_], Y <: Category[_,_]] (
            dom: X, codom: Y)(
            objectsMorphism: dom.Object => codom.Object,
            arrowsMorphism: dom.Arrow => codom.Arrow): Functor[X, Y] =
    new Functor[X, Y] (
      "", dom, codom) {
      override def objectsMapping(x: d0.Object): d1.Object =
        objectsMorphism(x.asInstanceOf[dom.Object]).asInstanceOf[d1.Object]
      override def arrowsMappingCandidate(a: d0.Arrow): d1.Arrow =
        arrowsMorphism(a.asInstanceOf[dom.Arrow]).asInstanceOf[d1.Arrow]
      override def nodesMapping(n: d0.Node): d1.Node =
        objectsMorphism(n.asInstanceOf[dom.Node]).asInstanceOf[d1.Node]
    }

  /**
   * Factory method. Builds identity functor for a category (identity functor).
   *
   * @tparam X the category type
   * @param c the category
   * @return identity functor on the given category
   */
  def id[X <: Category[_, _]](c: X):
      Functor[X, X] =
    new Functor[X, X] ("id", c, c) {
      override def objectsMapping(x: d0.Object): d1.Object = x.asInstanceOf[d1.Object]

      override def arrowsMappingCandidate(a: d0.Arrow): d1.Arrow = a.asInstanceOf[d1.Arrow]

      override def nodesMapping(n: d0.Node): d1.Node = n.asInstanceOf[d1.Node]
    }

  /**
   * Factory method. Builds constant functor from a category to an object in another.
   *
   * @tparam X first category type
   * @tparam Y second category type
   * @param x the category
   * @param y second category
   * @param y0 an object in category Y
   * @return constant functor on x that takes maps all objects to y0 and all arrows to y0's identities.
   */
  def const[X <: Category[_, _], Y <: Category[_, _]] (x: X, y: Y)(y0: y.Object):
    Functor[X, Y] =
    apply[X, Y](
      y.toString, x, y)(
      SetMorphism.const(x.objects, y.objects, y0),
      SetMorphism.const(x.arrows, y.arrows, y.id(y0)))
}