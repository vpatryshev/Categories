package math.cat.topos

import scala.language.implicitConversions
import math.Base._
import math.cat._
import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import math.sets.Functions._
import math.sets.Sets.{set, _}
import math.sets.{FactorSet, Sets}
import scalakittens.{Good, Result}

import scala.collection.MapView
import scala.language.postfixOps

/**
  * Diagram from a category to Categories.SETF.
  *
  * The term "point" below means a point in categorical meaning:
  * an arrow from a terminal object into a given object.
  * Here we talk about the category of diagrams, so a point is a
  * singleton diagram. It must have been a mapping from objects of the base category to values
  * that are elements of the sets: given a diagram D, p(x) \in D(x).
  */
abstract class Diagram(
  tag: Any,
  val topos: GrothendieckTopos)
  extends Functor(tag, topos.domain, SetCategory.Setf) { diagram =>
//  val d0: Category = d0
  type XObject = d0.Obj
  type XObjects = Set[XObject]
  type XArrow = d0.Arrow
  type XArrows = Set[XArrow]
  
  given Conversion[d1.Obj, set] = x => x.asInstanceOf[set]

  private[topos] def setAt(x: Any): set = setOf(objectsMapping(d0.asObj(x)))
  
  def ⊂(other: Diagram): Boolean =
    d0.objects.forall { o => this(o) subsetOf other(o) }

  def ∈(other: Diagram): Boolean =
    d0.objects.forall { o => other(o)(this(o)) }

  def point(mapping: XObject => Any, id: Any = ""): Point =
    new Point(id, topos, (x: Any) => mapping(diagram.d0.asObj(x)))

  lazy val points: List[Point] = {
    val objMappings = for {
      values <- Sets.product(listOfComponents).view
      mapping = d0.listOfObjects zip values toMap;
      om: Point = point(mapping)
      if isCompatible(om)
    } yield om

    val sorted = objMappings.toList.sortBy(_.toString.replace("}", "!")).zipWithIndex

    sorted map { p => p._1 named ("p" + p._2) }
  }

  implicit def asFunction(a: d1.Arrow): SetFunction = a match {
    case sf: SetFunction => sf
    case trash =>
      throw new IllegalArgumentException(s"Expected a set function, got $trash")
  }

  def functionForArrow(a: Any): SetFunction = {
    val arrowInSets = arrowsMapping(d0.arrow(a))
    asFunction(arrowInSets)
  }

  def apply(x: Any): set = setOf(objectsMapping(d0.obj(x)))

  /**
    * Calculates this diagram's limit
    *
    * @return this functor's limit
    */
  override def limit: Result[Cone] = {
    val bundleObjects: XObjects = limitBuilder.bundles.keySet

    def arrowsFromBundles(obj: XObject): XArrows = limitBuilder.bundles.get(obj).toSet.flatten

    // For each object of domain we have an arrow from one of the objects used in building the product
    val arrowsInvolved: XArrows =
      bundleObjects flatMap (bo => arrowsFromBundles(bo)) filterNot d0.isIdentity

    val grouped: Map[XObject, XArrows] = arrowsInvolved.groupBy(arrow => d0.d1(arrow))
    val fromRootObjects: MapView[XObject, XArrow] =
      grouped.view.mapValues(_.head) // does not matter which one, in this case

    def arrowFromRootObject(x: XObject) =
      if (limitBuilder.rootObjects(x)) d0.id(x) else fromRootObjects(x)

    val vertex = limitBuilder.vertex

    def coneMap(x: XObject): d1.Arrow = d1.arrow {
      val arrowToX: XArrow = arrowFromRootObject(x)
      val rootObject: XObject = d0.d0(arrowToX)
      val f: SetFunction = arrowsMapping(arrowToX)
      val projections: List[Any] => Any = limitBuilder.projectionForObject(rootObject)
      SetFunction.build(s"vertex to ($tag)[$x]", vertex, f.d1,
        { case point: List[Any] => f(projections(point)) }) iHope // what can go wrong?
    }
    //YObjects vertex
    Good(Cone(d1.obj(limitBuilder.vertex), coneMap))
  }

  override def colimit: Result[Cocone] = {
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

    // for every object it gives the inclusion of the image of this object into the union
    val objectToInjection: MapView[XObject, Injection[Any, (Int, Any)]] =
      reverseIndex.view mapValues union.injection

    // All possible functions in the diagram, bundled with domain objects
    val functionsToUnion: Set[(XObject, SetFunction)] = for {
      o <- d0.objects
      a <- bundles(o)
      from: set = nodesMapping(o)
      aAsMorphism: SetFunction = arrowsMapping(a)
      embeddingToUnion <-
      SetFunction.build("in", aAsMorphism.d1, typelessUnion, objectToInjection(d0.d1(a))).asOption
      g <- aAsMorphism andThen embeddingToUnion // do we need it?
    } yield (o, g)

    // Accounts for all canonical functions
    val canonicalFunctionPerObject: Map[XObject, SetFunction] =
      functionsToUnion.toMap

    val theFactorset: factorset = new FactorSet(typelessUnion)

    // have to factor the union by the equivalence relation caused
    // by two morphisms mapping the same element to two possibly different.
    for (o <- d0.objects) {
      val F_o = nodesMapping(o) // the set to which `o` maps
      val arrowsFrom_o: Seq[XArrow] = bundles(o).toList

      def inclusionToUnion(a: XArrow): Any => Any = {
        arrowsMapping(a).mapping andThen objectToInjection(d0.d1(a))
      }

      arrowsFrom_o match {
        case a0 :: tail =>
          val f = inclusionToUnion(a0)
          for (a <- tail) {
            val g = inclusionToUnion(a)
            for (x <- F_o) {
              theFactorset.merge(f(x), g(x))
            }
          }
        case other => // do nothing
      }
    }
    val factorMorphism: SetFunction = SetFunction.forFactorset(theFactorset)

    def coconeMap(x: XObject): d1.Arrow = d1.arrow {
      canonicalFunctionPerObject(x) andThen factorMorphism iHope
    }

    Good(Cocone(d1.obj(theFactorset.content.untyped), coconeMap))
  }

  private[topos] def isCompatible(om: Point) = d0.arrows.forall {
    a =>
      val d00 = om(d0.d0(a))
      val d01 = om(d0.d1(a))
      val f = arrowsMapping(a)
      val itsok = f(d00) == d01

      itsok
  }

  lazy val listOfComponents: List[set] =
    d0.listOfObjects map objectsMapping map (x => setOf(x))
  
  private def extendToArrows1(om: XObject => Sets.set)(a: XArrow): SetFunction = {
    val dom: Sets.set = om(d0.d0(a))
    val codom: Sets.set = om(d0.d1(a))
    new SetFunction("", dom, codom, arrowsMapping(a))
  }

  private def extendToArrows3[O, A](om: O => Sets.set)(a: A): SetFunction = {
    def same_om(o: XObject): Sets.set = om(o.asInstanceOf[O]) // TODO: get rid of casting
    extendToArrows1(same_om)(d0.arrow(a))
  }

  // TODO: write tests
  def filter[O,A](tag: String, predicate: XObject => Any => Boolean): Diagram = {
    def objectMapping(o: XObject): Sets.set = objectsMapping(o) filter predicate(o)

    val arrowToFunction = (a: topos.domain.Arrow) => extendToArrows1(objectMapping)(a.asInstanceOf[XArrow])
    Diagram(tag, topos)(objectMapping, arrowToFunction)
  }

  def subobjects: Iterable[Diagram] = {
    val allSets: Map[XObject, set] = domainObjects map (o => o -> setOf(objectsMapping(o))) toMap
    val allPowers: MapView[XObject, Set[set]] = allSets.view mapValues Sets.pow

    val listOfObjects: List[XObject] = domainObjects.toList
    val listOfComponents: List[Set[set]] = listOfObjects.map(allPowers)

    def isCompatible(om: XObject => Sets.set) = d0.arrows.forall {
      a =>
        val d00 = setOf(om(d0.d0(a)))
        val d01: set = om(d0.d1(a))
        val f = arrowsMapping(a)
        d00 map f subsetOf d01
    }

    val objMappings: Iterable[Map[XObject, Sets.set]] = for {
      values <- Sets.product(listOfComponents).view
      om0: Point = point(listOfObjects zip values toMap)
      om: Map[XObject, Sets.set] = d0.objects map (x => x -> setOf(om0(x))) toMap;
      if isCompatible(om)
    } yield om
    
    val sorted: Seq[Map[XObject, set]] = objMappings.toList.sortBy(_.toString)

    val allCandidates = sorted.zipWithIndex map {
      case (om, i) =>
        def same_om(o: topos.domain.Obj): Sets.set = om(d0.asObj(o))
        Diagram.build(i, topos)(
          same_om,
          extendToArrows3[topos.domain.Obj, topos.domain.Arrow](same_om _) _)
    }
    
    val goodOnes = allCandidates.collect { case Good(d) => d}
    goodOnes
  }
  
  private def toString(contentMapper: XObject => String): String = {
    s"Diagram[${d0.name}](${
      d0.listOfObjects map contentMapper filter(_.nonEmpty) mkString ", "
    })".replace("Set()", "{}")    
  }

  override def toString: String = toString({ x => 
      s"$x ->{${objectsMapping(x).mkString(",")}}" replace(s"Diagram[${d0.name}]", "") })
  
  def toShortString: String = toString({ x => {
      val obRepr = Diagram.cleanupString(objectsMapping(x).mkString(","))
      if (obRepr.isEmpty) "" else s"$x->{$obRepr}"
    } replace(s"Diagram[${d0.name}]", "")
  })

  /**
    * Builds a predicate that checks if a given set of arrows map a given element of Cartesian product to the same value
    *
    * @param point element of Cartesian product
    * @return the predicate
    */
  private[cat] def allArrowsAreCompatibleOnPoint(point: Point): XArrows => Boolean =
    arrows => arrows.forall(f => arrows.forall(g => {
      arrowsAreCompatibleOnPoint(point)(f, g)
    }))

  /**
    * Checks whether two arrows action on a given point produce the same element. 
    *
    * @param point a point in the diagram
    * @param f     first arrow
    * @param g     second arrow
    * @return true if they are
    */
  private[cat] def arrowsAreCompatibleOnPoint(point: Point)(f: XArrow, g: XArrow): Boolean = {
    val f_x = arrowActionOnPoint(f, point)
    val g_x = arrowActionOnPoint(g, point)
    f_x == g_x
  }

  /**
    * Calculates the action of a given arrow on a point of a diagram.
    *
    * @param a     the arrow
    * @param point a mapping in the sets corresponding to objects from the list above
    * @return the value to which the arrow maps a component of the point
    */
  private def arrowActionOnPoint(a: XArrow, point: Point): Any =
    arrowsMapping(a)(point(d0.d0(a)))

  private[cat] def setOf(x: Any): set = x.asInstanceOf[set]

  private[cat] object limitBuilder {
    // have to use list so far, no tool to annotate cartesian product components with their appropriate objects
    final private[cat] lazy val listOfObjects: List[XObject] = rootObjects.toList.sortBy(_.toString)
    // Here we have a non-repeating collection of sets to use for building a limit
    final private[cat] lazy val setsToUse =
      listOfObjects map nodesMapping map (x => setOf(x))
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
    lazy val rootObjects: XObjects = d0.allRootObjects.asInstanceOf[XObjects] // same thing
    private lazy val participantArrows: XArrows = d0.arrowsFromRootObjects
    // for each domain object, a collection of arrows looking outside
    private lazy val opo: d0op.Objects = d0op.objects
    private lazy val opa: d0op.Arrows = participantArrows.asInstanceOf[d0op.Arrows]

    // this function takes an object and returns a projection set function;
    // we have to compose each such projection
    // with the right arrow from root object to the image of our object
    private[cat] def projectionForObject(x: XObject)(xs: List[Any]): Any = {
      val i = index(x)
      xs(i)
    }

    private def index(x: XObject): Int = listOfObjects.indexOf(x)

    // Have a product set; have to remove all the bad elements from it
    // this predicate leaves only compatible elements of product (which are lists)
    private[cat] def isPoint(candidate: List[Any]): Boolean = {
      val p: Point = point(listOfObjects zip candidate toMap)
      val arrowSets = cobundles.values
      val setsToCheck = arrowSets filterNot (_.forall(d0.isIdentity))
      setsToCheck forall allArrowsAreCompatibleOnPoint(p)
    }
  }
}

object Diagram {

  private[topos] def apply[O, A](tag: Any, t: GrothendieckTopos)(
    objectsMap: O => set,
    arrowMap:   t.domain.Arrow => SetFunction): Diagram = {

    new Diagram(tag.toString, t) {
      
      override private[topos] def setAt(x: Any): set =
        d1.asObj(objectsMap(x.asInstanceOf[O])) // TODO: get rid of Any and casting
      
      override def objectsMapping(o: d0.Obj): d1.Obj =
        val x = o.asInstanceOf[O] // TODO: get rid of casting
        val y = objectsMap(x)
        d1.asObj(y) // TODO: get rid of casting

      override protected def arrowsMappingCandidate(a: d0.Arrow): d1.Arrow =
        d1.arrow(arrowMap(a.asInstanceOf[t.domain.Arrow]))
    }
  }
  
  def build(tag: Any, topos: GrothendieckTopos)(
    objectsMap: topos.domain.Obj => set,
    arrowMap:   topos.domain.Arrow => SetFunction): Result[Diagram] = {
    val diagram: Diagram = apply(tag, topos)(objectsMap, arrowMap)

    Functor.validateFunctor(diagram) returning diagram
  }

  private[topos] def cleanupString(s: String): String = {
    val s1 = s.replaceAll(s"->Diagram\\[[^\\]]+]", "->")
    val s2 = s1.replace("Set()", "{}")
    s2
  }

}
