package math.cat

import math.sets.Functions._
import math.sets.Sets
import math.sets.Sets._
import scalakittens.{Bad, Result}
import scalakittens.Result._

/**
  * Functor class: functions for categories.
  *
  * @param d0 domain
  * @param d1 codomain
  */
abstract class Functor(
  val tag: Any,
  val d0: Category, val d1: Category
) extends GraphMorphism {

  def domainObjects: d0.Objects = d0.objects

  val objectsMapping: d0.Obj ⇒ d1.Obj
  val arrowsMappingCandidate: d0.Arrow ⇒ d1.Arrow
  
  override def toString: String = s"Functor $tag"

  override def arrowsMapping(a: d0.Arrow): d1.Arrow = {
    val domainX: d0.Obj = d0.d0(a)
    try {
      if (d0.isIdentity(a)) {
        d1.id(objectsMapping(domainX))
      } else arrowsMappingCandidate(a)
    } catch {
      case x: Exception ⇒
        throw new IllegalArgumentException(
          s"$tag: arrow mapping not found for $a: $domainX → ${d0.d1(a)}", x)
    }
  }

  /**
    * Composes two functors
    *
    * @param g : Y → Z - second functor
    * @return g o this : X → Z - composition of this functor with functor g
    */
  def compose(g: Functor): Option[Functor] = {
    if (d1 != g.d0) None else Some {
      val f = this
      val objectMap = (x: d0.Obj) ⇒ {
        val y: g.d0.Obj = g.d0.obj(objectsMapping(x))
        val z: g.d1.Obj = g.objectsMapping(y)
        z
      }
      new Functor(g.tag + " o " + this.tag, f.d0, g.d1) {
        comp: Functor ⇒

        override val objectsMapping: d0.Obj ⇒ d1.Obj =
          (x: d0.Obj) ⇒ d1.obj(objectMap(Functor.this.d0.obj(x)))

        override val arrowsMappingCandidate: d0.Arrow ⇒ d1.Arrow =
          (a: d0.Arrow) ⇒ d1.arrow(g.arrowsMapping(g.d0.arrow(f.arrowsMapping(f.d0.arrow(a)))))

        // the following override is not required, because it's just another name for object mapping
        override def nodesMapping(n: d0.Node): d1.Node = {
          val y: g.d0.Node = g.d0.node(f.nodesMapping(f.d0.obj(n)))
          d1.obj(g.nodesMapping(y))
        }
      }
    }
  }

  override def nodesMapping(n: d0.Node): d1.Node =
    d1.obj(objectsMapping(d0.obj(n)))

  def cone(vertex: d1.Obj)(arrowTo: Iterable[(d0.Obj, d1.Arrow)]): Option[Cone] = {
    Option(Cone(vertex, arrowTo.toMap)) filter (_.isWellFormed)
  }

  /**
    * Lists all possible cones from given object y to this functor.
    * The requirement is that if f1: y → F(x1) is in the collection of arrows,
    * and there is a g: x1 → x2, then f2 = F(g) o f1 : y → F(x2) is also in this collection.
    *
    * @param y an object from which the cone originates.
    * @return a map that maps objects x of domain category to arrows y → F(x)
    */
  def conesFrom(y: d1.Obj): Set[Cone] = {
    // this function builds pairs (x, f:y→F(x)) for all f:y→F(x)) for a given x
    val arrowsFromYtoFX: Injection[d0.Obj, Set[(d0.Obj, d1.Arrow)]] = injection(
      (x: d0.Obj) ⇒ d1.arrowsBetween(d1.obj(y), objectsMapping(x)) map { (x, _) }
    )

    // group (x, f: y→F[x]) by x
    val homsGroupedByX: List[Set[(d0.Obj, d1.Arrow)]] = d0.listOfObjects map arrowsFromYtoFX

    val coneCandidates: Set[List[(d0.Obj, d1.Arrow)]] = product(homsGroupedByX)
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
    allCones.forall((anyCone: Cone) ⇒ candidate.factorsOnRight(anyCone))

  /**
    * @return this functor's limit
    */
  def limit: Result[Cone] = allCones find isLimit

  def cocone(vertex: d1.Obj)(arrowTo: Iterable[(d0.Obj, d1.Arrow)]): Option[Cocone] = {
    Option(Cocone(vertex, arrowTo.toMap)) filter (_.isWellFormed)
  }

  /**
    * Lists all possible cones from given object y to this functor.
    * The requirement is that if f1: y → F(x1) is in the collection of arrows,
    * and there is a g: x1 → x2, then f2 = F(g) o f1 : y → F(x2) is also in this collection.
    *
    * @param y an object from which the cone originates.
    * @return a map that maps objects x of domain category to arrows y → F(x)
    */
  def coconesTo(y: d1.Obj): Set[Cocone] = {
    // this function builds pairs (x, f:y→F(x)) for all f:y→F(x)) for a given x
    def arrowsFromFXtoY(x: d0.Obj): Set[(d0.Obj, d1.Arrow)] =
      d1.arrowsBetween(d1.obj(objectsMapping(x)), y) map { (x, _) }

    // group (x, f: y→F[x]) by x
    val homsGroupedByX: List[Set[(d0.Obj, d1.Arrow)]] = domainObjects.toList map arrowsFromFXtoY

    val coconeCandidates: Set[List[(d0.Obj, d1.Arrow)]] = product(homsGroupedByX)
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
    allCocones.forall((anyCocone: Cocone) ⇒ candidate.factorsOnRight(anyCocone))

  def colimit: Result[Cocone] = allCocones find isColimit

  /**
    * Cone class for this Functor. A cone is an object y (called vertex) and a bundle of arrows cx: y → F(x)
    * for all objects x of domain category, such that F(f) o cx1 = cx2 for f:x1 → x2.
    *
    * @param vertex  the cone's vertex object in Y
    * @param arrowTo maps each object of x to an arrow from F(x) to the vertex.
    */
  case class Cone(vertex: d1.Obj, arrowTo: d0.Obj ⇒ d1.Arrow) {

    override def toString: String = "Cone[" + vertex + "]"

    /**
      * A cone from y1 to F is factored by this cone (with vertex y)
      * if there is an h : y1 → y such that each f1: y1 → F(x) is equal to
      * f o h, where f: y → F(x).
      *
      * @param factored a cone that may be factored
      * @return true if it is so
      */
    def factorsOnRight(factored: Cone): Boolean =
      d1.hom(factored.vertex, vertex) exists { h ⇒
        domainObjects.forall(
          x ⇒ d1.m(h, arrowTo(x)) contains factored.arrowTo(x))
      }

    /**
      * @return true if this actually a well-formed cone.
      */
    def isWellFormed: Boolean = d0.arrows.forall(
      (f: d0.Arrow) ⇒ {
        var yToFx0: d1.Arrow = arrowTo(d0.d0(f))
        var yToFx1: d1.Arrow = arrowTo(d0.d1(f))
        var F_f: d1.Arrow = arrowsMapping(f)
        d1.m(yToFx0, F_f) contains yToFx1
      }
    )

    override def equals(o: Any): Boolean = {
      o match {
        case other: Cone ⇒ eq(other) ||
          (vertex == other.vertex &&
            domainObjects.forall { x: d0.Obj ⇒
              Result.forValue(arrowTo(x) == other.arrowTo(x)).getOrElse(false)
            })
        case somethingElse ⇒ false
      }
    }

    override def hashCode: Int = (vertex.hashCode /: domainObjects) ((hash, x) ⇒ hash * 13 + arrowTo(x).hashCode)
  }

  /**
    * Cocone class for this Functor. A cocone is an object y (called vertex) and a bundle of arrows cx: F(x) → y
    * for all objects x of domain category, such that F(f) o cx1 = cx2 for f:x1 → x2.
    *
    * @param vertex    the cone's vertex object in Y
    * @param arrowFrom maps each object of x to an arrow from F(x) to the vertex.
    */
  case class Cocone(vertex: d1.Obj, arrowFrom: d0.Obj ⇒ d1.Arrow) {

    override def toString: String = "Cocone[" + vertex + "]"

    /**
      * A cocone from F to y1 is factored by this cocone (from F to y)
      * if there is an h : y → y1 such that each f1: F(x) → y1 is equal to
      * h o f, where f: F(x) → y.
      *
      * @param factored a cone that may be factored
      * @return true if it is so
      */
    def factorsOnRight(factored: Cocone): Boolean = {
      val hom = d1.hom(factored.vertex, vertex)
      val answer = hom exists (
        (h: d1.Arrow) ⇒ {
          val failsOn = domainObjects.find(
            (x: d0.Obj) ⇒ !(d1.m(factored.arrowFrom(x), h) contains arrowFrom(x)))
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
      (f: d0.Arrow) ⇒ {
        var Fx02y = arrowFrom(d0.d0(f))
        var Fx12y: d1.Arrow = arrowFrom(d0.d1(f))
        var F_f: d1.Arrow = arrowsMapping(f)
        val answer = d1.m(F_f, Fx12y) contains Fx02y
        answer
      }
    )

    override def equals(o: Any): Boolean = o match {
      case other: Cocone ⇒
        eq(other) || (
          vertex == other.vertex &&
            domainObjects.forall {
              x: d0.Obj ⇒ arrowFrom(x) == other.arrowFrom(x)
            })
      case somethingElse ⇒ false
    }

    override def hashCode: Int =
      (vertex.hashCode /: domainObjects) ((hash, x) ⇒ hash * 13 + arrowFrom(x).hashCode)
  }

}

object Functor {

  /**
    * Factory method. Builds identity functor for a category (identity functor).
    *
    * @param c the category
    * @return identity functor on the given category
    */
  def id(c: Category): Functor = new Functor("id", c, c) {
      override val objectsMapping: d0.Obj ⇒ d1.Obj = (x: d0.Obj) ⇒ d1.obj(x)

      override val arrowsMappingCandidate: d0.Arrow ⇒ d1.Arrow =
        (a: d0.Arrow) ⇒ d1.arrow(a)

      override def nodesMapping(n: d0.Node): d1.Node = d1.node(n)
    }

  /**
    * Builds a constant functor from a category to an object in another.
    *
    * @param x  the category
    * @param y  second category
    * @param y0 an object in category y
    * @return constant functor on x that takes maps all objects to y0 and all arrows to y0's identities.
    */
  def const(x: Category, y: Category)(y0: y.Obj): Functor =
    unsafeBuild( // won't fail? Check y0, at least
      y.toString, x, y)(
      SetMorphism.const(x.objects, y.objects, y0),
      SetMorphism.const(x.arrows, y.arrows, y.id(y0)))

  private def unsafeBuild(
    tag: String,
    dom: Category,
    codom: Category)(
    objectsMorphism: dom.Obj ⇒ codom.Obj,
    arrowsMorphism: dom.Arrow ⇒ codom.Arrow): Functor =
    new Functor(tag, dom, codom) {
      override val objectsMapping: d0.Obj ⇒ d1.Obj = (x: d0.Obj) ⇒ d1.obj(objectsMorphism(dom.obj(x)))

      override val arrowsMappingCandidate: d0.Arrow ⇒ d1.Arrow =
        (a: d0.Arrow) ⇒ d1.arrow(arrowsMorphism(dom.arrow(a)))
    }

  /**
    * Builds a functor, given the data:
    *
    * @param atag           functor tag
    * @param dom            domain category
    * @param codom          codomain category
    * @param objectsMapping objects mapping
    * @param arrowsMapping  arrows mapping
    * @return
    */
  def apply(
    atag: String,
    dom: Category,
    codom: Category)(
    objectsMapping: dom.Obj ⇒ codom.Obj,
    arrowsMapping: dom.Arrow ⇒ codom.Arrow): Result[Functor] = {
    validateFunctor(unsafeBuild(atag, dom, codom)(objectsMapping, arrowsMapping))
  }

  /**
    * Validates a functor candidate.
    * A functor is valid if it is valid as a graph function, and besides,
    * it preserves identities and arrows composition.
    * That is, F(id(x)) == id(F(x)), and
    * F(g) o F(f) = F(g o f)
    */
  private[cat] def validateFunctor(f: Functor): Result[Functor] = for {
    _ ← checkObjectMapping(f)
    _ ← checkArrowMapping(f)
    _ ← checkIdentityPreservation(f) andAlso checkCompositionPreservation(f)
  } yield f

  private def checkIdentityPreservation(f: Functor): Outcome = Result.traverse {
    for (x ← f.domainObjects) yield {
      val y: f.d1.Obj = f.objectsMapping(x)
      OKif(f.arrowsMapping(f.d0.id(x)) == f.d1.id(y), s"Identity must be preserved for $x ↦ $y")
    }

  } andThen OK

  private def checkCompositionPreservation(f: Functor): Outcome = {
    def check(fx: f.d0.Arrow, gx: f.d0.Arrow): Outcome = {
      val fy = f.arrowsMapping(fx)
      val gy = f.arrowsMapping(gx)
      val gy_fy = f.d1.m(fy, gy)
      val gx_fx = f.d0.m(fx, gx)
      val expected = gx_fx map (gf ⇒ f.arrowsMapping(gf))
      OKif(gy_fy == expected,
        s"Functor must preserve composition (failed on $fx, $fy, $gx, $gy, $gy_fy; $expected)")
    }

    val checked = Result.traverse {
      for {
        (fx, gx) ← Category.composablePairs(f.d0)
      } yield {
        val r = check(fx, gx)
//        if (r.isBad) { // TODO: remove this block, it makes no sense
//          println(r.asInstanceOf[Bad[_]].stackTrace)
//          val fy = f.arrowsMapping(fx)
//          val gy = f.arrowsMapping(gx)
//          val gy_fy = f.d1.m(fy, gy)
//          try {
//            val g = gy_fy.get
//            println(g)
//            println(g)
//          } catch {
//            case x: Exception ⇒
//              x.printStackTrace()
//              throw x
//          }
//        }
        r
      }
    }
    
    checked andThen OK
  }

  private def checkArrowMapping(f: Functor): Outcome = Result.traverse {
    for (a ← f.d0.arrows) yield {
      Result.forValue(f.arrowsMapping(a)) flatMap {
        aa ⇒
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

  private def checkObjectMapping(f: Functor): Outcome =
    Result.traverse {
      for (x ← f.domainObjects) yield {
        val someY: Result[f.d1.Obj] =
          Result.forValue(f.objectsMapping(x)) orCommentTheError s"Object mapping fails for $x"
        someY.filter(f.d1.objects, s"Object mapping defined incorrectly for $x")
      }
    } andThen OK
}