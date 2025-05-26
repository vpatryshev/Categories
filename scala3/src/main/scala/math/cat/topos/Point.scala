package math.cat.topos

import scalakittens.{Cache, Params, Result}
import math.Base.*
import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import math.cat.{Category, SetCategory, SetFunction}
import math.cat.topos.Format.shortTitle
import math.sets.Sets

import scala.annotation.targetName
import scala.language.implicitConversions

/**
  * A point of a topos object (of a diagram in a Grothendieck topos)
  * @param tag used to visually identify a point
  * @param topos the topos
  * @param fromObjectToSubset for each domain object, choose something in the topos diagram
  */
class Point(
  val tag: String,
  val topos: GrothendieckTopos,
  val fromObjectToSubset: Any => Any) extends (Any => Any):
  p =>
  val baseTopos: GrothendieckTopos = topos
  private val domainCategory: Category = topos.domain
  lazy val mapping = Cache[Any, Any](tag, fromObjectToSubset, true)
  def apply(x: Any): Any = mapping(x)
  
  infix def named(name: String): Point = new Point(name, topos, fromObjectToSubset)

  // @deprecated("This should be redefined via composition", "03/28/2020")
  def transform(f: DiagramArrow): Point =
    def apply(o: Any) =
      f(o) match
        case sf: SetFunction => sf(p(o))

    new Point(s"${f.tag}(${p.tag})", p.topos, apply)

  lazy val asDiagram: topos.Diagram =
    def arrowToFunction(a: topos.thisTopos.domain.Arrow): Any => Any =
      (z: Any) => fromObjectToSubset(topos.thisTopos.domain.d1(a))

    def objectsMapping(x: topos.thisTopos.domain.Obj): Sets.set = Set(fromObjectToSubset(x))

    topos.Diagram(tag,
      (x: topos.domain.Obj) => Set(fromObjectToSubset(x)),
      (a: topos.domain.Arrow) =>
        SetFunction(s"$tag(.)",
          objectsMapping(topos.domain.d0(a)),
          objectsMapping(topos.domain.d1(a)),
          arrowToFunction(a)))

  @targetName("in")
  infix inline def ∈(container: topos.Diagram): Boolean =
    asDiagram ⊂ container

  lazy val predicate: topos.Predicate = topos   predicateFor this

  // TODO: fix this awkward unnecessary casting
  inline infix def asPredicateIn(t: GrothendieckTopos): t.Predicate =
    require(t eq topos) // we don't need full compare, just an identity check
    predicate.asInstanceOf[t.Predicate]

  override def toString: String =
    if tag.toString.nonEmpty then tag.toString else
      val raw = domainCategory.listOfObjects.map(x => s"$x -> ${apply(x)}")
      shortTitle(raw.mkString(s"$tag(", ", ", ")"))

  def toShortString: String =

    val strings: List[String] =
      domainCategory.listOfObjects map {
        x => {
          val obRepr = apply(x) match
//            case d: Diagram =>
//              shortTitle(d.source.toShortString)
            case d: topos.Diagram =>
              shortTitle(d.toShortString)
            case other =>
              other.toString

          s"$x->$obRepr"
        }
      }

    shortTitle(strings.mkString(s"$tag(", ", ", ")"))

  override lazy val hashCode: Int = System.identityHashCode(topos) * 79 + toString.hashCode

  override def equals(obj: Any): Boolean = hashCode == obj.hashCode && (obj match
    case p: Point =>
      p.tag == tag && (p.domainCategory eq domainCategory) &&
        domainCategory.objects.forall(o => p(o) == this(o))
    case other => false
  )
