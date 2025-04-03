package math.cat.topos

import math.Base.*
import math.cat.*
import math.sets.Sets.*
import math.sets.Sets

import scala.annotation.targetName
import scala.language.{implicitConversions, postfixOps}

/**
  * Diagram from a category to Categories.Setf.
  *
  * The term "point" below means a point in categorical meaning:
  * an arrow from a terminal object into a given object.
  * Here we talk about the category of diagrams, so a point is a
  * singleton diagram. It must have been a mapping from objects of the base category to values
  * that are elements of the sets: given a diagram D, p(x) \in D(x).
  */
abstract class Diagram(
  tag: Any,
  val t: GrothendieckTopos)(val source: t.Diagramme)
  extends Functor(tag):
  diagram =>
  override val d1: Category = source.d1
  private type XObject = source.d0.Obj // topos.domain.Obj ???
  private type XObjects = Set[XObject]
  private type XArrow = source.d0.Arrow // topos.domain.Arrow ???
  private type XArrows = Set[XArrow]

  given Conversion[d1.Obj, set] = x => x.asInstanceOf[set]

  def asFunction(a: d1.Arrow): SetFunction = source.asFunction(a)

  given Conversion[d1.Arrow, SetFunction] = asFunction

  private[topos] def setAt(x: Any): set = itsaset(objectsMapping(x))

  @targetName("subdiagramOf")
  infix inline def ⊂(other: Diagram): Boolean = source ⊂ other.source

  def functionForArrow(a: Any): SetFunction = source.functionForArrow(a)

  infix def apply(x: Any): set = source(x)

  def extendToArrows(om: XObject => Sets.set)(a: XArrow): SetFunction =
    source.extendToArrows(om)(a)

  private def toString(contentMapper: XObject => String): String =
    source.toString(contentMapper)

  override def toString: String = source.toString

  /**
    * Builds a predicate that checks if a given set of arrows map a given element of Cartesian product to the same value
    *
    * @param point element of Cartesian product
    * @return the predicate
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
    * Calculates the action of a given arrow on a point of a diagram.
    *
    * @param a     the arrow
    * @param point a mapping in the sets corresponding to objects from the list above
    * @return the value to which the arrow maps a component of the point
    */
  private def arrowActionOnPoint(a: XArrow, point: Point): Any =
    arrowsMapping(a)(point(d0.d0(a)))

  private[cat] object limitBuilder:
    // have to use List so far, no tool to annotate cartesian product components with their appropriate objects
    final private[cat] lazy val listOfObjects: List[XObject] = listSorted(rootObjects)
    // Here we have a non-repeating collection of sets to use for building a limit
    final private[cat] lazy val setsToUse =
      listOfObjects map source.nodesMapping map (x => itsaset(x))
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
      source.d0.buildBundles(rootObjects, participantArrows)
    lazy val rootObjects: XObjects = source.d0.allRootObjects
    private lazy val participantArrows: XArrows = source.d0.arrowsFromRootObjects
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
      val p: Point = source.point(listOfObjects zip candidate toMap)
      val arrowSets = cobundles.values
      val setsToCheck = arrowSets filterNot (_.forall(d0.isIdentity))
      setsToCheck forall allArrowsAreCompatibleOnPoint(p)

  end limitBuilder
