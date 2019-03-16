package math.cat

import math.cat.Functor.validateFunctor
import math.sets.Functions._
import math.sets.Sets
import math.sets.Sets._
import scalakittens.{Good, Result}
import scalakittens.Result._

/**
  * Functor class: functions for categories.
  *
  * @tparam X the first category type 
  * @tparam Y the second category type
  */
abstract class Functor[X <: Category[_, _], Y <: Category[_, _]](
  val d0: X, val d1: Y
)
  extends GraphMorphism[X, Y] {

  type Domain = X
  type Codomain = Y
  val tag: String
  def domainObjects: d0.Objects = d0.objects

  val objectsMapping: d0.O => d1.O
  val arrowsMappingCandidate: d0.Arrow => d1.Arrow

  override def toString: String = s"Functor $tag"

  override def arrowsMapping(a: d0.Arrow): d1.Arrow = {
    val domainX: d0.O = d0.d0(a)
    try {
      if (d0.id(domainX) == a) {
        val domainY: d1.O = objectsMapping(domainX)
        d1.id(domainY)
      } else arrowsMappingCandidate(a)
    } catch {
      case x: Exception =>
        throw new IllegalArgumentException(
          s"$tag: arrow mapping not found for $a: $domainX -> ${d0.d1(a)}")
    }
  }

  /**
    * Composes two functors
    *
    * @tparam Z the third category in the chain
    * @param g : Y -> Z - second functor
    * @return g o this : X -> Z - composition of this functor with functor g
    */
  def compose[Z <: Category[_, _]](g: Functor[Y, Z]): Option[Functor[X, Z]] = {
    if (d1 != g.d0) None else Some {
      val f = this
      val objectMap = (x: d0.O) => {
        val y: g.d0.O = objectsMapping(x).asInstanceOf[g.d0.O] // somehow 
        // Scala does not deduce it
        val z: g.d1.O = g.objectsMapping(y)
        z
      }
      new Functor[X, Z](f.d0, g.d1) {
        comp: Functor[X, Z] =>

        val tag: String = g.tag + " o " + this.tag

        override val objectsMapping: d0.O => d1.O = objectMap.asInstanceOf[d0.O => d1.O]

        override val arrowsMappingCandidate: d0.Arrow => d1.Arrow = ((a: d0.Arrow) =>
          g.arrowsMapping(f.arrowsMapping(a.asInstanceOf[f.d0.Arrow]).asInstanceOf[g.d0.Arrow]).asInstanceOf[d1.Arrow]).asInstanceOf[d0.Arrow => d1.Arrow]

        // the following override is not required, because it's just another name for object mapping
        override def nodesMapping(n: d0.Node): d1.Node = {
          val y: g.d0.Node = f.nodesMapping(n.asInstanceOf[f.d0.O]).asInstanceOf[g.d0.Node] // somehow Scala 
          // does not deduce it
          g.nodesMapping(y).asInstanceOf[d1.Node]
        }
      }
    }
  }

  override def nodesMapping(n: d0.Node): d1.Node =
    objectsMapping(n.asInstanceOf[d0.O]).asInstanceOf[d1.Node]

  def cone(vertex: d1.O)(arrowTo: Iterable[(d0.O, d1.Arrow)]): Option[Cone] = {
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
  def conesFrom(y: d1.O): Set[Cone] = {
    // this function builds pairs (x, f:y->F(x)) for all f:y->F(x)) for a given x
    val arrowsFromYtoFX: Injection[d0.O, Set[(d0.O, d1.Arrow)]] = injection(
      (x: d0.O) => d1.arrowsBetween(y.asInstanceOf[d1.Node], objectsMapping(x)) map { (x, _) }
    )

    val listOfDomainObjects: List[d0.O] = domainObjects.toList
    // group (x, f: y->F[x]) by x
    val homsGroupedByX: List[Set[(d0.O, d1.Arrow)]] = listOfDomainObjects map arrowsFromYtoFX

    val coneCandidates: Set[List[(d0.O, d1.Arrow)]] = product(homsGroupedByX)
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
    *
    * @param candidate cone to check
    * @return true iff it is a limit
    */
  def isLimit(candidate: Cone): Boolean =
    allCones.forall((anyCone: Cone) => candidate.factorsOnRight(anyCone))

  /**
    * @return this functor's limit
    */
  def limit: Option[Cone] = allCones find isLimit

  def cocone(vertex: d1.O)(arrowTo: Iterable[(d0.O, d1.Arrow)]): Option[Cocone] = {
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
  def coconesTo(y: d1.O): Set[Cocone] = {
    // this function builds pairs (x, f:y->F(x)) for all f:y->F(x)) for a given x
    def arrowsFromFXtoY(x: d0.O): Set[(d0.O, d1.Arrow)] =
      d1.arrowsBetween(objectsMapping(x).asInstanceOf[d1.Node], y) map { (x, _) }

    // group (x, f: y->F[x]) by x
    val homsGroupedByX: List[Set[(d0.O, d1.Arrow)]] = domainObjects.toList map arrowsFromFXtoY

    val coconeCandidates: Set[List[(d0.O, d1.Arrow)]] = product(homsGroupedByX)
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
    *
    * @param candidate cone to check
    * @return true iff it is a limit
    */
  def isColimit(candidate: Cocone): Boolean =
    allCocones.forall((anyCocone: Cocone) => candidate.factorsOnRight(anyCocone))

  def colimit: Option[Cocone] = allCocones find isColimit

  /**
    * Cone class for this Functor. A cone is an object y (called vertex) and a bundle of arrows cx: y -> F(x)
    * for all objects x of domain category, such that F(f) o cx1 = cx2 for f:x1 -> x2.
    *
    * @param vertex  the cone's vertex object in Y
    * @param arrowTo maps each object of x to an arrow from F(x) to the vertex.
    */
  case class Cone(vertex: d1.O, arrowTo: d0.O => d1.Arrow) {
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
        domainObjects.forall(
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
            domainObjects.forall { x: d0.O =>
              Result.forValue(arrowTo(x) == other.arrowTo(x)).getOrElse(false)
            })
        case somethingElse => false
      }
    }

    override def hashCode: Int = (vertex.hashCode /: domainObjects) ((hash, x) => hash * 13 + arrowTo(x).hashCode)
  }

  /**
    * Cocone class for this Functor. A cocone is an object y (called vertex) and a bundle of arrows cx: F(x) -> y
    * for all objects x of domain category, such that F(f) o cx1 = cx2 for f:x1 -> x2.
    *
    * @param vertex    the cone's vertex object in Y
    * @param arrowFrom maps each object of x to an arrow from F(x) to the vertex.
    */
  case class Cocone(vertex: d1.O, arrowFrom: d0.O => d1.Arrow) {

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
      val answer = hom exists (
        (h: d1.Arrow) => {
          val failsOn = domainObjects.find(
            (x: d0.O) => !(d1.m(factored.arrowFrom(x), h) contains arrowFrom(x)))
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
            domainObjects.forall {
              x: d0.O => arrowFrom(x) == other.arrowFrom(x)
            })
      case somethingElse => false
    }

    override def hashCode: Int =
      (vertex.hashCode /: domainObjects) ((hash, x) => hash * 13 + arrowFrom(x).hashCode)
  }

}

object Functor {
  
  def build[X <: Category[_, _], Y <: Category[_, _]](
    dom: X, codom: Y)(
    objectsMorphism: dom.O => codom.O,
    arrowsMorphism: dom.Arrow => codom.Arrow): Result[Functor[X, Y]] =
    validateFunctor(new Functor[X, Y](dom, codom) {
      val tag = ""
      override val objectsMapping: d0.O => d1.O = objectsMorphism.asInstanceOf[d0.O => d1.O]

      override val arrowsMappingCandidate: d0.Arrow => d1.Arrow = ((a: d0.Arrow) =>
        arrowsMorphism(a.asInstanceOf[dom.Arrow]).asInstanceOf[d1.Arrow]).asInstanceOf[d0.Arrow => d1.Arrow]

      override def nodesMapping(n: d0.Node): d1.Node =
        objectsMorphism(n.asInstanceOf[dom.Node]).asInstanceOf[d1.Node]
    })

  /**
    * Factory method. Builds identity functor for a category (identity functor).
    *
    * @tparam X the category type
    * @param c the category
    * @return identity functor on the given category
    */
  def id[X <: Category[_, _]](c: X):
  Functor[X, X] =
    new Functor[X, X](c, c) {
      val tag = "id"

      override val objectsMapping: d0.O => d1.O = ((x: d0.O) => x).asInstanceOf[d0.O => d1.O]

      override val arrowsMappingCandidate: d0.Arrow => d1.Arrow =
        ((a: d0.Arrow) => a).asInstanceOf[d0.Arrow => d1.Arrow]

      override def nodesMapping(n: d0.Node): d1.Node = n.asInstanceOf[d1.Node]
    }

  /**
    * Factory method. Builds constant functor from a category to an object in another.
    *
    * @tparam X first category type
    * @tparam Y second category type
    * @param x  the category
    * @param y  second category
    * @param y0 an object in category Y
    * @return constant functor on x that takes maps all objects to y0 and all arrows to y0's identities.
    */
  def const[X <: Category[_, _], Y <: Category[_, _]](x: X, y: Y)(y0: y.O):
  Functor[X, Y] =
    unsafeBuild[X, Y]( // won't fail? Check y0, at least
      y.toString, x, y)(
      SetMorphism.const(x.objects, y.objects, y0),
      SetMorphism.const(x.arrows, y.arrows, y.id(y0)))

  private def unsafeBuild[X <: Category[_, _], Y <: Category[_, _]](
    atag: String,
    dom: X,
    codom: Y)(
    objectsMorphism: dom.O => codom.O,
    arrowsMorphism: dom.Arrow => codom.Arrow): Functor[X, Y] =
    new Functor[X, Y](dom, codom) {
      val tag: String = atag
      override val objectsMapping: d0.O => d1.O = objectsMorphism.asInstanceOf[d0.O => d1.O]

      override val arrowsMappingCandidate: d0.Arrow => d1.Arrow = arrowsMorphism.asInstanceOf[d0.Arrow => d1.Arrow]
    }

  def build[X <: Category[_, _], Y <: Category[_, _]](
    atag: String,
    dom: X,
    codom: Y)(
    objectsMorphism: dom.O => codom.O,
    arrowsMorphism: dom.Arrow => codom.Arrow): Result[Functor[X, Y]] = {
    import codom._
    validateFunctor[X, Y](unsafeBuild[X, Y](atag, dom, codom)(objectsMorphism, arrowsMorphism))
  }

  /**
    * Validates a functor candidate.
    * A functor is valid if it is valid as a graph function, and besides,
    * it preserves identities and arrows composition.
    * That is, F(id(x)) == id(F(x)), and
    * F(g) o F(f) = F(g o f)
    */
  def validateFunctor[X <: Category[_, _], Y <: Category[_, _]](f: Functor[X, Y]): Result[Functor[X, Y]] = for {
    _ <- checkObjectMapping(f)
    _ <- checkArrowMapping(f)
    _ <- checkCompositionPreservation(f) andAlso checkCompositionPreservation(f)
  } yield f
  
  private def checkIdentityPreservation[Y <: Category[_, _], X <: Category[_, _]](f: Functor[X, Y]): Outcome = Result.traverse {
    for (x <- f.domainObjects) yield {
      val y: f.d1.O = f.objectsMapping(x)
      OKif(f.arrowsMapping(f.d0.id(x)) == f.d1.id(y), s"Identity must be preserved for $x ↦ $y")
    }

  } andThen OK

  private def checkCompositionPreservation[Y <: Category[_, _], X <: Category[_, _]](f: Functor[X, Y]): Outcome = Result.traverse {
    for {
      fx <- f.d0.arrows
      gx <- f.d0.arrows
      gx_fx <- f.d0.m(fx, gx)
      fy = f.arrowsMapping(fx)
      gy = f.arrowsMapping(gx)
      expected = f.arrowsMapping(gx_fx)
      gy_fy <- f.d1.m(fy, gy)
    } yield {
      OKif(gy_fy == expected,
        s"Functor must preserve composition (failed on $fx, $fy, $gx, $gy, $gy_fy; $expected)")
    }
  } andThen OK
  
    private def checkArrowMapping[Y <: Category[_, _], X <: Category[_, _]](f: Functor[X, Y]): Outcome = Result.traverse {
    for (a <- f.d0.arrows) yield {
      Result.forValue(f.arrowsMapping(a)) flatMap {
        aa =>
          val domainActual = f.d1.d0(aa)
          val codomainActual = f.d1.d1(aa)
          val domainExpected = f.objectsMapping(f.d0.d0(a))
          val codomainExpected = f.objectsMapping(f.d0.d1(a))
          OKif(domainActual == domainExpected, 
            s"Inconsistent mapping for d0($a) - $domainActual vs $domainExpected") andAlso
          OKif(codomainActual == codomainExpected, 
            s"Inconsistent mapping for d1($a) - $codomainActual vs $codomainExpected")
      }
    }
    
  } andThen OK
  
  private def checkObjectMapping[Y <: Category[_, _], X <: Category[_, _]](f: Functor[X, Y]): Outcome =
    Result.traverse {
    for (x <- f.domainObjects) yield {
      try {
        val someY: Result[f.d1.O] =
          Result.forValue(f.objectsMapping(x)) orCommentTheError s"Object mapping fails for $x"
        someY.filter(f.d1.objects, s"Object mapping defined incorrectly for $x")
      } catch {
        case ame: AbstractMethodError =>
          ame.printStackTrace()
          println(s"Object mapping crashed on $x")
          throw ame
      }
    }
  } andThen OK
}