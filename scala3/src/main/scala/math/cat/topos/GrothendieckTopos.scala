package math.cat.topos

import math.Base.*
import math.cat.*
import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import math.sets.Sets.*
import math.sets.{FactorSet, Functions, Sets}
import scalakittens.{Cache, Good, Result}
import scalakittens.Containers.*
import math.cat.topos.Format.shortTitle

import scala.collection.mutable
import scala.language.{implicitConversions, postfixOps}
import SetFunction.inclusion
import math.sets.Functions.Injection
import scalakittens.Params.{debug, verbose}

import scala.annotation.targetName

// see also http://www.cs.man.ac.uk/~david/categories/book/book.pdf - ML implementation of topos

trait GrothendieckTopos
  extends Topos with GrothendieckToposLogic:
  topos =>
  val thisTopos: GrothendieckTopos = this

  type Node = Diagram
  override type Obj = Diagram
  override type Arrow = DiagramArrow
  type ObjectMapping = domain.Obj => set
  type ArrowMapping = domain.Arrow => SetFunction
  case class DiagramMapping( // named tuple does not work, as of Scala version 3.6.3
    ofObjects: ObjectMapping,
    ofArrows: ArrowMapping
  )

  val domain: Category
  
  def tag = s"Set^${domain.name}"

  type Mapping = domain.Obj => Any => Any

//  given Conversion[Functor, Diagramme] = _ match
//    case d: Diagram => d.source.asInstanceOf[Diagramme]
//    case d: Diagramme => d
//    case basura =>
//      throw new IllegalArgumentException(s"Not a diagram: $basura")

  def inclusionOf(p: Point): Includer

  private[topos] def subobjectsOfRepresentables: Map[domain.Obj, Set[Diagramme]]

  /**
    * Subobject classifier. Ω is "Option-Z" on your Mac.
    */
  val Ω: Ωlike = new Ωlike

  val Truth: Point = Ω.True
  val Falsehood: Point = Ω.False

  class Ωlike extends Diagramme("Ω"):
//    override val d1: Category = SetCategory.Setf
    // For each object `x` we produce a set of all subobjects of `Representable(x)`.
    // These are values `Ω(x)`. We cache them in the following map `x => Ω(x)` .
    private[topos] val subrepresentablesIndexed = subobjectsOfRepresentables
    def toposName: String = topos.tag

    // this one is consumed by Functor constructor
    def calculateObjectsMapping(x: d0.Obj): d1.Obj = subrepresentablesIndexed(x: domain.Obj)

    // for each arrow `a: x -> y` produce a transition `Ω(x) -> Ω(y)`.
    private def am(a: domain.Arrow): SetFunction =
      val x = domain.d0(a)
      val y = domain.d1(a)
      val d0 = subrepresentablesIndexed(x) // `Ω(x)` = all subobjects of `Representable(x)`
      val d1 = subrepresentablesIndexed(y) // `Ω(y)` = all subobjects of `Representable(y)`

      // How one diagram is transformed via `a`:
      // For each `rx ⊂ Repr(x)` we have to produce a diagram `ry ⊂ Repr(y)`
      def diaMap(rx: Diagramme): Diagramme /*a subrepresentable on `x`*/ =
        // this is how elements of objects projections, that is, subterminals, are transformed by `a`
        def om1(o: domain.Obj): set = transformingOfSubrepresentables(a, rx)(o)

        // this is how, given an arrow `b`, the new diagram gets from one point to another
        def am1(b: domain.Arrow): SetFunction =
          val x1 = om1(domain.d0(b))
          val y1 = om1(domain.d1(b))

          // A function fom x1 to y1 - it does the transition
          new SetFunction("", x1, y1, g => domain.m(g, b).get)

        Diagramme("", om1, am1) // no validation, we know it's ok

      // no validation here, the function is known to be ok
      new SetFunction(s"[$a]", d0.untyped, d1.untyped, x => diaMap(x.asInstanceOf[Diagramme]))

    protected def calculateArrowsMapping(a: d0.Arrow): d1.Arrow = am(a)

    /**
      * Given an arrow `a`,
      * {f ∈ hom(y, x1) | a compose f ∈ r1(x1)}
      *
      * @param a  an arrow
      * @param rx a subrepresentable
      * @param x1 an object in domain (a "state")
      * @return
      */
    private def transformingOfSubrepresentables(a: domain.Arrow, rx: Diagramme)(x1: domain.Obj): set =
      val y = domain.d1(a)
      val rx_at_x1 = rx(x1)
      for
        f <- domain.hom(y, x1)
        candidate <- domain.m(a, f)
        if candidate ∈ rx_at_x1
      yield f

    Functor.validateFunctor(this) iHope

    // TODO: redefine as classifying an empty
    lazy val False: Point = points.head named "⊥"

    // TODO: redefine as classifying an identity
    lazy val True: Point = points.last named "⊤"

    /**
      * Intersection of two subrepresentables on object `x`
      * @param a first subrepresentable
      * @param b second subrepresentable
      * @return their intersection
      */
    private[this] def intersection(a: Diagramme, b: Diagramme): Diagramme =
      val om = (o: domain.Obj) => a.source(o) & b.source(o)

      // this is how, given an arrow `b`, the new diagram gets from one point to another
      def am(f: domain.Arrow): SetFunction =
        val x = om(domain.d0(f))
        val y = om(domain.d1(f))

        a.arrowsMapping(f).restrictTo(x, y).iHope

      Diagramme(concat(a.tag, "∩", b.tag), om, am)

    lazy val conjunction: DiagramArrow =

      def calcConjunctionOfTwoSubreps(pair: Any): Diagramme = pair match
        case (a: Diagramme, b: Diagramme) => intersection(a,b)
        case bs => throw new IllegalArgumentException(s"Expected a pair of diagrams, got $bs")
        
      val conjunctionOfTwoSubreps: Any => Diagramme = Cache[Any, Diagramme](
        s"∧", calcConjunctionOfTwoSubreps, domain.isFinite
      )

      def calculatePerObject(x: ΩxΩ.d0.Obj): SetFunction =
        val dom = ΩxΩ.source(x)
        val codom = Ω.source(x)
        new SetFunction(s"∧[$x]", dom.untyped, codom, conjunctionOfTwoSubreps)

      new DiagramArrow("∧", ΩxΩ, Ω):
        override def calculateMappingAt(x: d0.d0.Obj): d1.d1.Arrow = calculatePerObject(x)


    lazy val disjunction: DiagramArrow =
      /**
       * Union of two subrepresentables on object `x`
       * @param a first subrepresentable
       * @param b second subrepresentable
       * @return their intersection
       */
      def union(a: Diagramme, b: Diagramme): Diagramme =
        val om = (o: domain.Obj) => a.setAt(o) | b.setAt(o)

        // this is how, given an arrow `b`, the new diagram gets from one point to another
        def am(f: domain.Arrow): SetFunction =
          val o = domain.d0(f)
          val ao = a(o)
          val bo = b(o)
          val x = om(o)
          val y = om(domain.d1(f))

          val fa = a.arrowsMapping(f)
          val fb = b.arrowsMapping(f)

          def unionOfMappings(z: Any): Any =
            if ao(z) then fa(z)
            else if bo(z) then fb(z)
            else throw new IllegalArgumentException(s"$z was supposed to be in $ao or in $bo")

          new SetFunction("", x, y, unionOfMappings)

        topos.Diagramme(
          concat(a.tag, "∪", b.tag),
          o => om(o), f => am(f))

      end union

      def calcDisjunctionOfTwoSubreps(pair: Any): Diagramme = pair match
        case (a: Diagramme, b: Diagramme) => union(a, b)
        case other =>
          throw new IllegalArgumentException(s"Expected a pair of diagrams, but encountered ${other.getClass}")

      val disjunctionOfTwoSubreps: Any => Diagramme = Cache[Any, Diagramme](
        "v", calcDisjunctionOfTwoSubreps, domain.isFinite
      )

      def calculatePerObject(x: ΩxΩ.d0.Obj): SetFunction =
        val dom = ΩxΩ.source(x)
        val codom = Ω.source(x)
        new SetFunction(s"v[$x]", dom.untyped, codom, pair => disjunctionOfTwoSubreps(pair))

      new DiagramArrow("v", ΩxΩ, Ω):
        override def calculateMappingAt(x: d0.d0.Obj): d1.d1.Arrow =
          calculatePerObject(x)

    end disjunction

    lazy val implication: DiagramArrow = χ(inclusionOf(Ω1) in ΩxΩ_Diagramme iHope, "⟹")

  end Ωlike


  val ΩxΩ_Diagramme: Diagramme = product2(Ω, Ω)
  val ΩxΩ: Obj = product2(Ω, Ω).asOldDiagram

  private lazy val firstProjectionOf_ΩxΩ =
    buildArrow("π1", ΩxΩ_Diagramme, Ω, firstProjection)

  /**
    * An equalizer of first projection and intersection, actually
    */
  lazy val Ω1: Diagramme = ΩxΩ_Diagramme.filter("<", _ => {
    case (a: Diagram, b: Diagram) => a.source.asInstanceOf[Diagramme] ⊂ b.source.asInstanceOf[Diagramme]
    case (a: Diagram, b: Diagramme) => a.source.asInstanceOf[Diagramme] ⊂ b
    case (a: Diagramme, b: Diagram) => a ⊂ b.source.asInstanceOf[Diagramme]
    case (a: Diagramme, b: Diagramme) => a ⊂ b
    case somethingElse => false
  }).asInstanceOf[Diagramme]

  /**
    * Diagonal for Ω
    */
  lazy val Δ_Ω: DiagramArrow = buildArrow("Δ", Ω, ΩxΩ.source.asInstanceOf[topos.Diagramme],
    _ => (subrep: Any) => (subrep, subrep)
  )

  private[topos] case class χAt(inclusion: Arrow, x: domain.Obj):
    val A = inclusion.d1.asInstanceOf[Diagramme].source
    val B = inclusion.d0.asInstanceOf[Diagramme].source
    val Ωatx = Ω(x)
    // for each element ax of set Ax find all arrows x->y
    // that map ax to an ay that belongs to By
    def myArrows(ax: Any): Set[(Any, set)] =
      def image_via(f: domain.Arrow) = A.functionForArrow(f)(ax)

      def hits(By: set)(f: domain.Arrow) =
        val image = image_via(f)
        By contains image
      //            image_via(f) ∈ By

      domain.objects.map {
        y =>
          val all_arrows_to_y: domain.Arrows = domain.hom(x, y)
          val By = B(y)
          y -> itsaset(all_arrows_to_y filter hits(By))
        }

//    def sameMapping(repr: Diagram, mapping: Map[Any, set]): Boolean =
//      domain.objects.forall(o => mapping(o) == repr.source(o))

    def sameMapping(repr: topos.Diagramme, mapping: Map[Any, set]): Boolean =
      domain.objects.forall(o => mapping(o) == repr(o))

    def myRepresentable(ax: Any): Any =
      val arrowsSet = myArrows(ax)
      val arrows = arrowsSet.toMap
      val choices = Ωatx find {
        _ match
          case d: Diagram =>
            sameMapping(d.source.asInstanceOf[Diagramme], arrows)
          case td: topos.Diagramme =>
            sameMapping(td, arrows)
          case other =>
            false
      }
      Result(choices) orCommentTheError s"No representable found for $ax -> $arrows" iHope

    def asFunction: SetFunction =
      val Ax = A(x)
      new SetFunction(s"[χ($x)]", Ax, Ω(x), ax => myRepresentable(ax))

  end χAt

  /**
    * Builds a map that classifies a subobject
    * B ----> 1
    * v       v
    * |       |
    * v       v
    * A ----> Ω
    *
    * @param inclusion B >--> A - a natural transformation from diagram B to diagram A
    * @return A -> Ω
    */
  def χ(inclusion: Arrow, theTag: String): Predicate =
    def objToFunction(x: domain.Obj): SetFunction = χAt(inclusion: Arrow, x: domain.Obj).asFunction

    inclusion.d1 match {
      case d: Diagramme =>
        new Predicate(theTag, d):
          override def calculateMappingAt(x: d0.d0.Obj): d1.d1.Arrow =
            objToFunction(x)
    }

  def χ(inclusion: Arrow): Predicate =
    χ(inclusion, s"χ(${inclusion.tag})")

  trait Includer:
    val subdiagram: Diagramme

    infix def in(diagram: Diagram): Result[DiagramArrow] =
      in(diagram.source.asInstanceOf[Diagramme])

    infix def in(diagram: Diagramme): Result[DiagramArrow] =
      val results: IterableOnce[Result[(domain.Obj, subdiagram.d1.Arrow)]] =
        for
          x <- domain.objects
          incl: Result[SetFunction] = inclusion(subdiagram(x), diagram(x))
          pair: Result[(domain.Obj, subdiagram.d1.Arrow)] = incl.map {
            x -> _
          }
        yield pair

      val name = concat(subdiagram.tag, "⊂", diagram.tag)
      for
        map <- Result traverse results
        arrow <- NaturalTransformation.build(name, subdiagram, diagram)(map.toMap)
      yield arrow

    end in
  end Includer

  def inclusionOf(diagram: Diagramme): Includer =
    new Includer:
      val subdiagram: Diagramme = diagram

  /**
    * Builds a `DiagramArrow`, given domain, codomain, and a mapping
    * @param tag arrow tag
    * @param from domain
    * @param to codomain
    * @param mapping maps objects to functions
    * @return a natural transformation (crashes if not)
    */
  def buildArrow(tag: String, from: Diagramme, to: Diagramme,
    mapping: Mapping): DiagramArrow =
    NaturalTransformation.build(tag, from, to)(
      (o: from.d0.Obj) => buildOneArrow(tag, from, to, mapping)(o)).iHope

  private val p1: Any => Any =
    case (a, b) => a
    case trash =>
      throw new IllegalArgumentException(s"Expected a pair, got $trash")

  private val p2: Any => Any =
    case (a, b) => b
    case trash =>
      throw new IllegalArgumentException(s"Expected a pair, got $trash")

  // π1
  protected val firstProjection: Mapping = Function.const (p1)

  // π2
  protected val secondProjection: Mapping = Function.const(p2)

  /**
    * Given a `from` and `to` diagrams, build an arrow
    * `from(o)` -> `to(o)`, for each given `o`,
    * using the provided mapping
    *
    * @param tag tag of a natural transformation
    * @param from domain diagram
    * @param to codomain diagram
    * @param mapping given an object `o`, produce a function over this object
    * @param o the object
    * @return an arrow (it's a `SetFunction`, actually)
    */
  protected def buildOneArrow(
    tag: Any,
    from: Diagramme,
    to: Diagramme,
    mapping: Mapping
  )(o: from.d0.Obj): from.d1.Arrow =
    SetFunction.build(s"$tag[$o]", from(o), to(o), mapping(o)).iHope

  /**
   * Given a `from` and `to` diagrams, build an arrow
   * `from(o)` -> `to(o)`, for each given `o`,
   * using the provided mapping
   *
   * @param tag     tag of a natural transformation
   * @param from    domain diagram
   * @param to      codomain diagram
   * @param mapping given an object `o`, produce a function over this object
   * @param o       the object
   * @return an arrow (it's a `SetFunction`, actually)
   */
  protected def buildOneArrowe(
                               tag: Any,
                               from: Diagramme,
                               to: Diagramme,
                               mapping: Mapping
                             )(o: from.d0.Obj): from.d1.Arrow =
    SetFunction.build(s"$tag[$o]", from(o), to(o), mapping(o)).iHope

  /**
    * Given arrows `f` and `g`, builds an arrow (f×g): dom(f)×dom(g) -> codom(f)×codom(g)
    *
    * @param f first component
    * @param g second component
    * @return a product of `f` and `g`
    */
  def productOfArrows(f: DiagramArrow, g: DiagramArrow): DiagramArrow =
    val fd0: Diagramme = f.d0.asInstanceOf[Diagramme]
    val gd0: Diagramme = g.d0.asInstanceOf[Diagramme]
    val fd1: Diagramme = f.d1.asInstanceOf[Diagramme]
    val gd1: Diagramme = g.d1.asInstanceOf[Diagramme]

    val mapping: Mapping = x =>
      val fx = f(x).asInstanceOf[Any => Any]
      val gx = g(x).asInstanceOf[Any => Any]

      { case (a, b) => (fx(a), gx(b)) }

    val productOfDomains = product2(fd0, gd0)
    val productOfCodomains = product2(fd1, gd1)

    buildArrow(
      concat(f.tag, "×", g.tag),
      productOfDomains,
      productOfCodomains,
      mapping)
  end productOfArrows

  private[topos] case class product2builder(x: Diagramme, y: Diagramme):

    private def productAt(o: domain.Obj) = Sets.product2(x(o), y(o))
    private def mappingOfObjects(o: domain.Obj): set = productAt(o).untyped

    def transition(z: Diagramme)(a: domain.Arrow)(pz: Any) =
      z.asFunction(z.arrowsMapping(a))(pz)

    private def mappingOfArrows(a: domain.Arrow): SetFunction =
      val from = productAt(domain.d0(a))
      val to = productAt(domain.d1(a))
      def f(p: Any): Any = p match
        case (px, py) => (transition(x)(a)(px), transition(y)(a)(py))
        case other =>
          throw new IllegalArgumentException(s"Expected a pair of values, got $other")

      new SetFunction("", from.untyped, to.untyped, f)

    val diagram = Diagramme(concat(x.tag, "×", y.tag), mappingOfObjects, a => mappingOfArrows(a))

  end product2builder

  /**
    * Cartesian product of two diagrams
    * TODO: figure out how to ensure the same d0 in both Di
    */
  def product2(x: Diagram, y: Diagram): Diagram = ??? // will have to rename the one below, when ready
  def product2(x: Diagramme, y: Diagramme): Diagramme = product2builder(x, y).diagram

  def standardInclusion(p: Point, d: Diagramme): Result[DiagramArrow] =
    (inclusionOf(p) in d) map {
      q => (q ∘ uniqueFromTerminalTo(p)) named p.tag
    }

  abstract class Diagramme(tag: String)
    extends Functor(tag, thisTopos.domain, SetCategory.Setf):
    diagramme =>
    private type XObject = d0.Obj // topos.domain.Obj ???
    private type XObjects = Set[XObject]
    private type XArrow = d0.Arrow // topos.domain.Arrow ???
    private type XArrows = Set[XArrow]

    def source: Diagramme = this

    def asOldDiagram: Diagram =
      new Diagram(thisTopos)(diagramme.asInstanceOf[thisTopos.Diagramme]):
        override val d0: Category = diagramme.d0
        override val d1: Category = diagramme.d1
        override def calculateObjectsMapping(x: this.d0.Obj): this.d1.Obj = diagramme(x)
        override def calculateArrowsMapping(a: this.d0.Arrow): d1.Arrow =
          diagramme.arrowsMapping(a)

    given Conversion[d1.Obj, set] = x => x.asInstanceOf[set]

    private[topos] def setAt(x: Any): set = itsaset(calculateObjectsMapping(x))

    @targetName("isSubdiagramOf")
    infix inline def ⊂(other: Diagramme): Boolean =
      d0.objects.forall { o => this (o) subsetOf other(o) }

    @targetName("in")
    infix inline def ∈(other: Diagramme): Boolean =
      d0.objects.forall { o => other(o)(this (o)) }

    def asFunction(a: d1.Arrow): SetFunction = a match
      case sf: SetFunction => sf
      case trash =>
        throw new IllegalArgumentException(s"Expected a set function, got $trash")

    given Conversion[d1.Arrow, SetFunction] = asFunction

    private[topos] def isCompatible(om: Point) = d0.arrows.forall:
      a =>
        val d00 = om(d0.d0(a))
        val d01 = om(d0.d1(a))
        val f = arrowsMapping(a)
        f(d00) == d01

    private lazy val listOfComponents: List[set] =
      val objs = listOfObjects map calculateObjectsMapping
      objs map itsaset

    def point(mapping: XObject => Any, id: String = ""): Point =
      new Point(id, topos, (x: Any) => mapping(x))

    lazy val objMappings: List[Point] =
      (for
        valuesPerObject <- Sets.product(listOfComponents)
        tuples = listOfObjects zip valuesPerObject
        mapping = tuples toMap;
        om: Point = point(mapping) if isCompatible(om)
      yield om).toList

    lazy val points: List[Point] =

      // The following foolish hack does this:
      // any value in curlies goes after a shorter value in curlies,
      // because closing curly is replaced with '!' which goes
      // before all alphanumerics. Cheap trick, but works.
      // Any better suggestions?
      // DO NOT: sort differently without runing TopologyTest
      val sorted = objMappings.sortBy(_.toString.replace("}", "!")) zipWithIndex

      sorted map { p => p._1 named ("p" + p._2) }

    def functionForArrow(a: Any): SetFunction = arrowsMapping(a)

    infix def apply(x: Any): set = itsaset(calculateObjectsMapping(x))

    /**
     * Calculates this diagram's limit
     *
     * @return this functor's limit
     */
    override def limit: Result[Cone] =
      val bundleObjects: XObjects = LimitBuilder.bundles.keySet

      def arrowsFromBundles(obj: XObject): XArrows = LimitBuilder.bundles.get(obj).toSet.flatten

      // For each object of domain we have an arrow from one of the objects used in building the product
      val arrowsInvolved: XArrows =
        bundleObjects flatMap arrowsFromBundles filterNot d0.isIdentity

      val grouped: Map[XObject, XArrows] = arrowsInvolved.groupBy(arrow => d0.d1(arrow))

      val fromRootObjects: Map[XObject, XArrow] =
        grouped.map {
          case (k, v) => k -> v.head
        } toMap // does not matter which one, in this case

      def arrowFromRootObject(x: XObject) =
        if LimitBuilder.rootObjects(x) then d0.id(x) else fromRootObjects(x)

      val vertex = LimitBuilder.vertex

      def coneMap(x: XObject): d1.Arrow =
        val arrowToX: XArrow = arrowFromRootObject(x)
        val rootObject: XObject = d0.d0(arrowToX)
        val f: SetFunction = arrowsMapping(arrowToX)
        val projections: List[Any] => Any = LimitBuilder.projectionForObject(rootObject)
        SetFunction.build(s"vertex to ($tag)[$x]", vertex, f.d1,
          { case point: List[Any] => f(projections(point)) }
        ) iHope // what can go wrong?

      //YObjects vertex
      Good(Cone(LimitBuilder.vertex, coneMap))

    private[cat] object LimitBuilder:
      // have to use List so far, no tool to annotate cartesian product components with their appropriate objects
      final private[cat] lazy val listOfObjects: List[XObject] = listSorted(rootObjects)
      // Here we have a non-repeating collection of sets to use for building a limit
      final private[cat] lazy val setsToUse =
        listOfObjects map nodesMapping map (x => itsaset(x))
      // this is the product of these sets; will have to take a subset of this product
      final private[cat] lazy val prod: Set[List[Any]] = product(setsToUse)
      final lazy private val d0op = Categories.op(d0)
      final lazy private[cat] val cobundles: Map[XObject, XArrows] =
        d0op.buildBundles(opo, opa) // TODO: get rid of casting
          .asInstanceOf[Map[XObject, XArrows]] // TODO: get rid of casting
      // this is the limit object
      final private[cat] lazy val vertex: set = prod filter isPoint untyped
      // bundles maps each "initial" object to a set of arrows from it
      final private[cat] lazy val bundles: Map[XObject, XArrows] =
        d0.buildBundles(rootObjects, participantArrows)
      lazy val rootObjects: XObjects = d0.allRootObjects
      private lazy val participantArrows: XArrows = d0.arrowsFromRootObjects
      // for each domain object, a collection of arrows looking outside
      private lazy val opo: d0op.Objects = d0op.objects
      private lazy val opa: d0op.Arrows = participantArrows.asInstanceOf[d0op.Arrows]

      // this function takes an object and returns a projection set function;
      // we have to compose each such projection
      // with the right arrow from root object to the image of our object
      private[cat] def projectionForObject(x: XObject)(xs: List[Any]): Any =
        xs(index(x))

      private def index(x: XObject): Int = listOfObjects.indexOf(x)

      // Have a product set; have to remove all the bad elements from it
      // this predicate leaves only compatible elements of product (which are lists)
      private[cat] def isPoint(candidate: List[Any]): Boolean =
        val p: Point = point(listOfObjects zip candidate toMap)
        val arrowSets = cobundles.values
        val setsToCheck = arrowSets filterNot (_.forall(d0.isIdentity))
        setsToCheck forall allArrowsAreCompatibleOnPoint(p)

    end LimitBuilder

    /**
     * Builds a predicate that checks if a given set of arrows map a given element of Cartesian product to the same value
     *
     * @param point element of Cartesian product
     * @param f     arrow function
     * @param g     another arrow function, which may use a different element of Cartesian product
     * @return true if they are equal
     */
    private[cat] def allArrowsAreCompatibleOnPoint(point: Point): XArrows => Boolean =
      arrows => arrows.forall(f => arrows.forall(g =>
        arrowsAreCompatibleOnPoint(point)(f, g)
      ))

    /**
     * Checks whether the actions of two arrows on a given point produce the same element.
     *
     * @param point a point in the diagram
     * @param f     first arrow
     * @param g     second arrow
     * @return true if they are
     */
    private[cat] def arrowsAreCompatibleOnPoint(point: Point)(f: XArrow, g: XArrow): Boolean =
      val f_x = arrowActionOnPoint(f, point)
      val g_x = arrowActionOnPoint(g, point)
      f_x == g_x

    /**
     * Builds a predicate that checks if a given set of arrows map a given element of Cartesian product to the same value
     *
     * @param point element of Cartesian product
     * @param f     arrow function
     * @param g     another arrow function, which may use a different element of Cartesian product
     * @return true if they are equal
     */
    private[cat] def arrowActionOnPoint(a: XArrow, point: Point): Any =
      arrowsMapping(a)(point(d0.d0(a)))

    override def colimit: Result[Cocone] =
      val op = Categories.op(d0)
      val participantArrows: Set[op.Arrow] = op.arrowsFromRootObjects // filterNot domain.isIdentity
      // for each object, a set of arrows starting at it object
      val bundles: XObject => XArrows =
        d0.buildBundles(d0.objects, participantArrows.asInstanceOf[XArrows])
      val listOfObjects: List[XObject] = op.listOfRootObjects.asInstanceOf[List[XObject]]
      // Here we have a non-repeating collection of sets to use for building a union
      val setsToJoin: List[Set[Any]] = listOfObjects map nodesMapping
      val union: DisjointUnion[Any] = DisjointUnion(setsToJoin)
      val typelessUnion: set = union.unionSet untyped
      val directIndex: IntMap[XObject] = toMap(listOfObjects)
      val reverseIndex: Map[XObject, Int] = inverse(directIndex)

      // for every object it gives the inclusion of this object's image into the union
      val objectToInjection: Map[XObject, Injection[Any, (Int, Any)]] =
        reverseIndex map {
          case (k, v) => k -> union.injection(v)
        } toMap

      // All possible functions in the diagram, bundled with domain objects
      val functionsToUnion: Set[(XObject, SetFunction)] = for
        o <- d0.objects
        a <- bundles(o)
        from: set = nodesMapping(o)
        aAsMorphism: SetFunction = arrowsMapping(a)
        embeddingToUnion <-
          SetFunction.build("in", aAsMorphism.d1, typelessUnion, objectToInjection(d0.d1(a))).asOption
        g: SetFunction <- aAsMorphism andThen embeddingToUnion
      yield (o, g)

      // Accounts for all canonical functions
      val canonicalFunctionPerObject: Map[XObject, SetFunction] =
        functionsToUnion.toMap

      val theFactorset: factorset = new FactorSet(typelessUnion)

      // have to factor the union by the equivalence relation caused
      // by two morphisms mapping the same element to two possibly different.
      for o <- d0.objects do
        val F_o = nodesMapping(o) // the set to which `o` maps
        val arrowsFrom_o: Seq[XArrow] = bundles(o).toList

        def inclusionToUnion(a: XArrow): Any => Any =
          arrowsMapping(a).mapping andThen objectToInjection(d0.d1(a))

        val inclusions = arrowsFrom_o map inclusionToUnion

        inclusions match
          case f :: tail =>
            for g <- tail
                x <- F_o do
              theFactorset.merge(f(x), g(x))

          case other => // do nothing

      val factorMorphism: SetFunction = SetFunction.forFactorset(theFactorset)

      def coconeMap(x: XObject): d1.Arrow =
        val function = (canonicalFunctionPerObject(x) andThen factorMorphism) iHope

        function //.asInstanceOf[d1.Arrow]

      Good(Cocone(theFactorset.content, coconeMap))

    def toString(contentMapper: XObject => String): String =
      s"Diagram[${d0.name}](${
        listOfObjects map contentMapper filter (_.nonEmpty) mkString ", "
      })".replace("Set()", "{}")

    override def toString: String = toString(x =>
      s"$x ->{${asString(calculateObjectsMapping(x))}}".
        replace(s"Functor ", "").
        replace(s"Diagramme[${d0.name}]", "").
        replace(s"Diagram[${d0.name}]", "")
    )

    def toShortString: String = toString(x => {
      val obRepr = shortTitle(asString(calculateObjectsMapping(x)))
      if obRepr.isEmpty then "" else s"$x->{$obRepr}"
    }.replace(s"Diagramme[${d0.name}]", "")
    )

    def extendToArrows(om: XObject => Sets.set)(a: XArrow): SetFunction =
      val dom: Sets.set = om(d0.d0(a))
      val codom: Sets.set = om(d0.d1(a))
      new SetFunction("", dom, codom, arrowsMapping(a))

    // TODO: write tests !!!!!!!
    def filter[O, A](tag: String, predicate: XObject => Any => Boolean): Diagramme =
      debug(s"$tag: filter $predicate")
      def objectMapping(o: domain.Obj | XObject): Sets.set = // TODO: union is not to be used here
        calculateObjectsMapping(o) filter predicate(o)

      val arrowToFunction = (a: domain.Arrow) => extendToArrows(objectMapping)(a)

      def mappingOfd0Objects(x: Any): set =
        val theSet = objectMapping(d0.obj(x))
        theSet

      Diagramme(tag, mappingOfd0Objects, arrowToFunction)

    def subobjects: Iterable[Diagramme] =
      val allSets: Map[XObject, set] = buildMap(domainObjects, o => itsaset(calculateObjectsMapping(o)))
      val allPowers: Map[XObject, Set[set]] = allSets map {
        case (k, v) => k -> Sets.pow(v)
      } toMap

      val listOfComponents: List[Set[set]] = listOfObjects map allPowers

      def isPresheaf(om: XObject => Sets.set) = d0.arrows.forall:
        a =>
          val d00 = itsaset(om(d0.d0(a)))
          val d01: set = om(d0.d1(a))
          val f = arrowsMapping(a)
          d00 map f subsetOf d01

      val objMappings: Iterable[Map[XObject, Sets.set]] = for
        values <- Sets.product(listOfComponents).view
        om0: Point = point(listOfObjects zip values toMap)
        om: Map[XObject, Sets.set] = buildMap(d0.objects, x => itsaset(om0(x)))
        if isPresheaf(om)
      yield om

      val sorted: Seq[Map[XObject, set]] = listSorted(objMappings)

      sorted.zipWithIndex map :
        case (om, i) =>
          Diagramme(
            i,
            om(_),
            extendToArrows(om))

    end subobjects

  end Diagramme

  object Diagramme:

    private[topos] def apply(
      tag: Any,
      objectsMap: ObjectMapping,
      arrowMap:   ArrowMapping): Diagramme =

      new Diagramme(tag.toString):
        override private[topos] def setAt(x: Any): set = objectsMap(x)
        override def calculateObjectsMapping(o: d0.Obj): d1.Obj = objectsMap(o)
        override def calculateArrowsMapping(a: d0.Arrow): d1.Arrow = arrowMap(a)

    def tryBuild(
      tag: Any,
      objectsMap: ObjectMapping,
      arrowMap:   ArrowMapping): Result[Diagramme] =
      val diagram: Diagramme = apply(tag, objectsMap, arrowMap)

      Functor.validateFunctor(diagram) returning diagram

  def const(tag: String, value: set): Diagram =
    Diagramme(
      tag,
      (x: domain.Obj) => value,
      (a: domain.Arrow) => SetFunction.id(value)).asOldDiagram

  def cleanupString(s: String): String =
    val s1 = s.replaceAll(s"->Diagram\\[[^]]+]", "->")
    s1.replace("Set()", "{}")
