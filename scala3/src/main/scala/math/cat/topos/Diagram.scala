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
  private def arrowActionOnPoint(a: XArrow, point: Point): Any = source.arrowActionOnPoint(a, point)
