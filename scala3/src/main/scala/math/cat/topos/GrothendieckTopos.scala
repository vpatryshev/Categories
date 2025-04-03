package math.cat.topos

import math.Base.*
import math.cat.*
import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import math.sets.Sets.*
import math.sets.{FactorSet, Functions, Sets}
import scalakittens.{Good, Result}
import scalakittens.Containers.*
import math.cat.topos.Format.shortTitle

import scala.collection.{MapView, View, mutable}
import scala.language.{implicitConversions, postfixOps}
import SetFunction.inclusion
import math.sets.Functions.Injection

import scala.annotation.targetName
import scala.collection.MapView

// see also http://www.cs.man.ac.uk/~david/categories/book/book.pdf - ML implementation of topos

trait GrothendieckTopos
  extends Topos with GrothendieckToposLogic:
  topos =>
  val thisTopos: GrothendieckTopos = this
  type Obj = Diagram
  type Arrow = DiagramArrow

  val domain: Category

  type Mapping = domain.Obj => Any => Any

  given Conversion[Functor, Diagram] = _ match
    case d: Diagram => d
    case d: Diagramme => d.asOldDiagram
    case basura => throw new IllegalArgumentException(s"Not a diagram: $basura")


  def inclusionOf(p: Point): Includer

  private[topos] def subdiagramsOfRepresentables: MapView[domain.Obj, Set[Diagram]]

  private[topos] def subobjectsOfRepresentables: MapView[domain.Obj, Set[Diagramme]]

  /**
    * Subobject classifier. Ω is "Option-Z" on your Mac.
    */
  val Ω: Ωlike = new Ωlike("Ω")

  class Ωlike(name: String) extends Diagramme(name):
    override val d1: Category = SetCategory.Setf
    // For each object `x` we produce a set of all subobjects of `Representable(x)`.
    // These are values `Ω(x)`. We cache them in the following map `x => Ω(x)` .
    private[topos] val subrepresentablesIndexed: MapView[domain.Obj, Set[Diagram]] = subdiagramsOfRepresentables

    // this one is consumed by Functor constructor
    def objectsMapping(x: d0.Obj): d1.Obj = subrepresentablesIndexed(x: domain.Obj)

    // for each arrow `a: x -> y` produce a transition `Ω(x) -> Ω(y)`.
    private def am(a: domain.Arrow): SetFunction =
      val x = domain.d0(a)
      val y = domain.d1(a)
      val d0 = subrepresentablesIndexed(x) // `Ω(x)` = all subobjects of `Representable(x)`
      val d1 = subrepresentablesIndexed(y) // `Ω(y)` = all subobjects of `Representable(y)`

      // How one diagram is transformed via `a`:
      // For each `rx ⊂ Repr(x)` we have to produce a diagram `ry ⊂ Repr(y)`
      def diaMappe(rx: Diagramme): Diagram /*a subrepresentable on `x`*/ =
        // this is how elements of objects projections, that is, subterminals, are transformed by `a`
        def om1(o: domain.Obj): set = transformingOfSubrepresentables(a, rx)(o)

        // this is how, given an arrow `b`, the new diagram gets from one point to another
        def am1(b: domain.Arrow): SetFunction =
          val x1 = om1(domain.d0(b))
          val y1 = om1(domain.d1(b))

          // A function fom x1 to y1 - it does the transition
          new SetFunction("", x1, y1, g => domain.m(g, b).get)

        Diagramme("", om1, am1).asOldDiagram // no validation, we know it's ok

      def diaMap(rx: Diagram): Diagram /*a subrepresentable on `x`*/ =
        // this is how elements of objects projections, that is, subterminals, are transformed by `a`
        def om1(o: domain.Obj): set = transformingOfSubrepresentables(a, rx)(o)

        // this is how, given an arrow `b`, the new diagram gets from one point to another
        def am1(b: domain.Arrow): SetFunction =
          val x1 = om1(domain.d0(b))
          val y1 = om1(domain.d1(b))

          // A function fom x1 to y1 - it does the transition
          new SetFunction("", x1, y1, g => domain.m(g, b).get)

        Diagramme("", om1, am1).asOldDiagram // no validation, we know it's ok

      val tmpTransformer: Any => Any = x => {
        x match
          case xD: Diagramme => diaMappe(xD)
          case xD: Diagram => diaMap(xD)
          case _ =>
            System.err.println("wtf, got $x:${x.getClass")
            throw new IllegalArgumentException(s"Expected a diagramme, got $x of type ${x.getClass}")
      }
      // no validation here, the function is known to be ok
      new SetFunction(s"[$a]", d0.untyped, d1.untyped, tmpTransformer)

    protected def arrowsMappingCandidate(a: d0.Arrow): d1.Arrow = am(a)

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

    private def transformingOfSubrepresentables(a: domain.Arrow, rx: Diagram)(x1: domain.Obj): set =
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
    private[topos] def intersection(a: Diagram, b: Diagram): Diagram =
      val om = (o: domain.Obj) => a(o) & b(o)

      // this is how, given an arrow `b`, the new diagram gets from one point to another
      def am(f: domain.Arrow): SetFunction =
        val x = om(domain.d0(f))
        val y = om(domain.d1(f))

        a.arrowsMapping(f).restrictTo(x, y).iHope

      topos.Diagramme(
        concat(a.tag, "∩", b.tag),
        o => om(o),
        f => am(f)
      ).asOldDiagram

    lazy val conjunction: DiagramArrow =

      def conjunctionOfTwoSubreps(pair: Any): Diagram = pair match
        case (a: Diagram, b: Diagram) =>
          intersection(a,b)
        case bs =>
          throw new IllegalArgumentException(s"Expected a pair of diagrams, got $bs")

      def calculatePerObject(x: ΩxΩ.d0.Obj): SetFunction =
        val dom = ΩxΩ(x)
        val codom = Ω(x)
        new SetFunction(s"∧[$x]", dom.untyped, codom, pair => conjunctionOfTwoSubreps(pair))

      val cache: mutable.Map[ΩxΩ.d0.Obj, SetFunction] =
        mutable.Map[ΩxΩ.d0.Obj, SetFunction]()

      new DiagramArrow("∧"):
        override val d0: Functor = ΩxΩ
        override val d1: Functor = Ω

        def perObject(x: d0.d0.Obj): SetFunction =
          cache.getOrElseUpdate(x, calculatePerObject(x))

        override def mappingAt(x: d0.d0.Obj): d1.d1.Arrow =
          perObject(x)


    lazy val disjunction: DiagramArrow =
      new DiagramArrow("v"):
        override val d0: Functor = ΩxΩ
        override val d1: Functor = Ω

        /**
          * Union of two subrepresentables on object `x`
          * @param a first subrepresentable
          * @param b second subrepresentable
          * @return their intersection
          */
        private def union(a: Diagram, b: Diagram): Diagram =
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

        def disjunctionOfTwoSubreps(pair: Any): Diagram = pair match
          case (a: Diagram, b: Diagram) => union(a,b)

        def perObject(x: d0.d0.Obj): SetFunction =
          val dom = ΩxΩ(x)
          val codom = Ω(x)
          new SetFunction(s"v[$x]", dom.untyped, codom, pair => disjunctionOfTwoSubreps(pair))

        override def mappingAt(x: d0.d0.Obj): d1.d1.Arrow =
          perObject(x)

    end disjunction

    lazy val implication: DiagramArrow = χ(inclusionOf(Ω1) in ΩxΩ iHope, "⟹")

  end Ωlike


  val ΩxΩ: Obj = product2(Ω, Ω)

  private lazy val firstProjectionOf_ΩxΩ =
    buildArrow("π1", ΩxΩ, Ω, firstProjection)

  /**
    * An equalizer of first projection and intersection
    */
  lazy val Ω1: Diagram = ΩxΩ.source.filter("<", _ => {
    case (a: Diagram, b: Diagram) => a ⊂ b
    case (a: Diagramme, b: Diagramme) => a ⊂ b
  }) asOldDiagram

  /**
    * Diagonal for Ω
    */
  lazy val Δ_Ω: DiagramArrow = buildArrow("Δ", Ω, ΩxΩ,
    _ => (subrep: Any) => (subrep, subrep)
  )

  /**
    * Given an inclusion (a natural transformation from a diagram A to a diagram B), and an object x in domain
    * produce a function that maps elements of A(x) to elements of Ω(x)
    * @param inclusion the inclusion
    * @param x an object of domain
    * @return a function A(x) -> Ω(x)
    */
  private[topos] def χAt(inclusion: Arrow)(x: domain.Obj): SetFunction =
    val A: Diagram = inclusion.d1
    val B: Diagram = inclusion.d0

    val Ax = A(x)
    val Bx = B(x) // Bx is a subset of Ax

    // for each element ax of set Ax find all arrows x->y 
    // that map ax to an ay that belongs to By 
    def myArrows(ax: Any): Set[(Any, set)] =
      domain.objects.map {
        y =>
          val all_arrows_to_y: domain.Arrows = domain.hom(x, y)
          def image_via(f: domain.Arrow) = A.functionForArrow(f)(ax)
          val By = B(y)
          def hits_By(f: domain.Arrow) = image_via(f) ∈ By
          y -> itsaset(all_arrows_to_y filter hits_By)
        }

    def sameMapping(repr: Diagram, mapping: Map[Any, set]): Boolean =
      domain.objects.forall(o => mapping(o) == repr(o))

    def sameMappinge(repr: topos.Diagramme, mapping: Map[Any, set]): Boolean =
      domain.objects.forall(o => mapping(o) == repr(o))

    def myRepresentable(ax: Any): Any =
      val arrows = myArrows(ax).toMap
      val choices = Ω(x) find {
        _ match
          case d: Diagram => sameMapping(d, arrows)
          case td: topos.Diagramme => sameMappinge(td, arrows)
          case other => false
      }
      Result(choices) orCommentTheError s"No representable found for $ax -> $arrows" iHope

    new SetFunction(s"[χ($x)]", Ax, Ω(x), ax => myRepresentable(ax))

  end χAt

  /**
    * Builds a map that classifies a subobject
    * B ----> 1
    * v      v
    * |      |
    * v      v
    * A ----> Ω
    *
    * @param inclusion B >--> A - a natural transformation from diagram B to diagram A
    * @return A -> Ω
    */
  def χ(inclusion: Arrow, theTag: String): Predicate =
    val objToFunction: domain.Obj => SetFunction = χAt(inclusion)

    new Predicate(theTag):
      val d0: Diagram = inclusion.d1

      override def mappingAt(x: d0.d0.Obj): d1.d1.Arrow =
        objToFunction(x)

  def χ(inclusion: Arrow): Predicate =
    χ(inclusion, s"χ(${inclusion.tag})")

  trait Includer:
    val subdiagram: Diagram

    infix def in(diagram: Diagram): Result[DiagramArrow] =
      val results: IterableOnce[Result[(domain.Obj, subdiagram.d1.Arrow)]] =
        for
          x <- domain.objects
          incl: Result[SetFunction] = inclusion(subdiagram(x), diagram(x))
          pair: Result[(domain.Obj, subdiagram.d1.Arrow)] = incl.map { x -> _ }
        yield pair

      val name = concat(subdiagram.tag, "⊂", diagram.tag)
      for
        map <- Result traverse results
        arrow <- NaturalTransformation.build(name, subdiagram, diagram)(map.toMap)
      yield arrow

    end in
  end Includer

  def inclusionOf(diagram: Diagram): Includer =
    new Includer:
      val subdiagram: Diagram = diagram

  /**
    * Builds a `DiagramArrow`, given domain, codomain, and a mapping
    * @param tag arrow tag
    * @param from domain
    * @param to codomain
    * @param mapping maps objects to functions
    * @return a natural transformation (crashes if not)
    */
  def buildArrow(tag: Any, from: Diagram, to: Diagram,
    mapping: Mapping): DiagramArrow =
    NaturalTransformation.build(tag, from, to)(
      (o: from.d0.Obj) => buildOneArrow(tag, from, to, mapping)(o)).iHope

  /**
   * Builds a `DiagrammeArrow`, given domain, codomain, and a mapping
   *
   * @param tag     arrow tag
   * @param from    domain
   * @param to      codomain
   * @param mapping maps objects to functions
   * @return a natural transformation (crashes if not)
   */
  def buildArrowe(tag: Any, from: Diagramme, to: Diagramme,
                 mapping: Mapping): DiagramArrow =
    NaturalTransformation.build(tag, from, to)(
      (o: from.d0.Obj) => buildOneArrowe(tag, from, to, mapping)(o)).iHope

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
    from: Diagram,
    to: Diagram,
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

    val mapping: Mapping = x =>
      val fx = f(x).asInstanceOf[SetMorphism[Any, Any]]
      val gx = g(x).asInstanceOf[SetMorphism[Any, Any]]

      { case (a, b) => (fx(a), gx(b)) }

    val productOfDomains = product2(f.d0, g.d0)
    val productOfCodomains = product2(f.d1, g.d1)

    buildArrow(
      concat(f.tag, "×", g.tag),
      productOfDomains,
      productOfCodomains,
      mapping)
  end productOfArrows

  private[topos] case class product2builder(x: Diagram, y: Diagram):

    private def productAt(o: domain.Obj) = Sets.product2(x(o), y(o))
    private def mappingOfObjects(o: domain.Obj): set = productAt(o).untyped

    def transition(z: Diagram)(a: domain.Arrow)(pz: Any) =
      z.arrowsMapping(a)(pz)

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
  def product2(x: Diagram, y: Diagram): Diagram = product2builder(x, y).diagram
  def product2(x: Diagramme, y: Diagramme): Diagramme = product2builder(x, y).diagram

  def standardInclusion(p: Point, d: Diagram): Result[DiagramArrow] =
    (inclusionOf(p) in d) map {
      q => (q ∘ uniqueFromTerminalTo(p)) named p.tag
    }

  abstract class Diagramme(tag: Any)
    extends Functor(s"Diagramme $tag in $topos"):
    diagramme =>
    private type XObject = d0.Obj // topos.domain.Obj ???
    private type XObjects = Set[XObject]
    private type XArrow = d0.Arrow // topos.domain.Arrow ???
    private type XArrows = Set[XArrow]

    override val d0: Category = thisTopos.domain
    override val d1: Category = SetCategory.Setf



    def asOldDiagram: Diagram =
      new Diagram(s"OldDiagram from $tag in $topos", thisTopos)(diagramme.asInstanceOf[thisTopos.Diagramme]):
        override val d0: Category = diagramme.d0
        override val d1: Category = diagramme.d1
        override def objectsMapping(x: this.d0.Obj): this.d1.Obj = diagramme(x)
        override def arrowsMappingCandidate(a: this.d0.Arrow): d1.Arrow =
          diagramme.arrowsMappingCandidate(a)

    given Conversion[d1.Obj, set] = x => x.asInstanceOf[set]

    private[topos] def setAt(x: Any): set = itsaset(objectsMapping(x))

    @targetName("isSubdiagramOf")
    infix inline def ⊂(other: Diagram): Boolean =
      d0.objects.forall { o => this (o) subsetOf other(o) }

    @targetName("in")
    infix inline def ∈(other: Diagram): Boolean =
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
      val objs = listOfObjects map objectsMapping
      objs map itsaset

    def point(mapping: XObject => Any, id: Any = ""): Point =
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
      val sorted = objMappings.toList.sortBy(_.toString.replace("}", "!")) zipWithIndex

      sorted map { p => p._1 named ("p" + p._2) }

    def functionForArrow(a: Any): SetFunction = arrowsMapping(a)

    infix def apply(x: Any): set = itsaset(objectsMapping(x))

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

      val fromRootObjects: MapView[XObject, XArrow] =
        grouped.view.mapValues(_.head) // does not matter which one, in this case

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
      val objectToInjection: MapView[XObject, Injection[Any, (Int, Any)]] =
        reverseIndex.view mapValues union.injection

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
      s"$x ->{${asString(objectsMapping(x))}}".replace(s"Diagramme[${d0.name}]", "").replace(s"Diagram[${d0.name}]", ""))

    def toShortString: String = toString(x => {
      val obRepr = shortTitle(asString(objectsMapping(x)))
      if obRepr.isEmpty then "" else s"$x->{$obRepr}"
    }.replace(s"Diagramme[${d0.name}]", "")
    )

    def extendToArrows(om: XObject => Sets.set)(a: XArrow): SetFunction =
      val dom: Sets.set = om(d0.d0(a))
      val codom: Sets.set = om(d0.d1(a))
      new SetFunction("", dom, codom, arrowsMapping(a))

    // TODO: write tests
    def filter[O, A](tag: String, predicate: XObject => Any => Boolean): Diagramme =
      def objectMapping(o: domain.Obj | XObject): Sets.set = // TODO: union is not to be used here
        objectsMapping(o) filter predicate(o)

      val arrowToFunction = (a: domain.Arrow) => extendToArrows(objectMapping)(a)
      Diagramme(tag, d0.obj andThen objectMapping, arrowToFunction)

    def subobjects: Iterable[Diagramme] =
      val allSets: Map[XObject, set] = buildMap(domainObjects, o => itsaset(objectsMapping(o)))
      val allPowers: MapView[XObject, Set[set]] = allSets.view mapValues Sets.pow

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

//    private[cat] object limitBuilder:
//      // have to use List so far, no tool to annotate cartesian product components with their appropriate objects
//      final private[cat] lazy val listOfObjects: List[XObject] = listSorted(rootObjects)
//      // Here we have a non-repeating collection of sets to use for building a limit
//      final private[cat] lazy val setsToUse =
//        listOfObjects map nodesMapping map (x => itsaset(x))
//      // this is the product of these sets; will have to take a subset of this product
//      final private[cat] lazy val prod: Set[List[Any]] = product(setsToUse)
//      final lazy private val d0op = Categories.op(d0)
//      final lazy private[cat] val cobundles: Map[XObject, XArrows] =
//        d0op.buildBundles(opo, opa) // TODO: get rid of casting
//          .asInstanceOf[Map[XObject, XArrows]] // TODO: get rid of casting
//      // this is the limit object
//      final private[cat] lazy val vertex: set = prod filter isPoint untyped
//      // bundles maps each "initial" object to a set of arrows from it
//      final private[cat] lazy val bundles: Map[XObject, XArrows] =
//        d0.buildBundles(rootObjects, participantArrows)
//      lazy val rootObjects: XObjects = d0.allRootObjects
//      private lazy val participantArrows: XArrows = d0.arrowsFromRootObjects
//      // for each domain object, a collection of arrows looking outside
//      private lazy val opo: d0op.Objects = d0op.objects
//      private lazy val opa: d0op.Arrows = participantArrows.asInstanceOf[d0op.Arrows]
//
//      // this function takes an object and returns a projection set function;
//      // we have to compose each such projection
//      // with the right arrow from root object to the image of our object
//      //    private[cat] def projectionForObject(x: XObject)(xs: List[Any]): Any =
//      //      xs(index(x))
//
//      private def index(x: XObject): Int = listOfObjects.indexOf(x)
//
//      // Have a product set; have to remove all the bad elements from it
//      // this predicate leaves only compatible elements of product (which are lists)
//      private[cat] def isPoint(candidate: List[Any]): Boolean =
//        val p: Point = point(listOfObjects zip candidate toMap)
//        val arrowSets = cobundles.values
//        val setsToCheck = arrowSets filterNot (_.forall(d0.isIdentity))
//        setsToCheck forall allArrowsAreCompatibleOnPoint(p)
//
//    end limitBuilder


  end Diagramme

  object Diagramme:

    private[topos] def apply(
      tag: Any,
      objectsMap: domain.Obj => set,
      arrowMap:   domain.Arrow => SetFunction): Diagramme =

      new Diagramme(tag.toString):
        override val d0: Category = thisTopos.domain
        override val d1: Category = SetCategory.Setf
        override private[topos] def setAt(x: Any): set = objectsMap(x)
        override def objectsMapping(o: d0.Obj): d1.Obj = objectsMap(o)
        override def arrowsMappingCandidate(a: d0.Arrow): d1.Arrow = arrowMap(a)

    def tryBuild(
      tag: Any,
      objectsMap: domain.Obj => set,
      arrowMap:   domain.Arrow => SetFunction): Result[Diagramme] =
      val diagram: Diagramme = apply(tag, objectsMap, arrowMap)

      Functor.validateFunctor(diagram) returning diagram

  def const(tag: String, value: set): Diagram =
    Diagramme(
      tag,
      (x: domain.Obj) => value,
      (a: domain.Arrow) => SetFunction.id(value))

  def cleanupString(s: String): String =
    val s1 = s.replaceAll(s"->Diagram\\[[^]]+]", "->")
    s1.replace("Set()", "{}")
