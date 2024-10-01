package math.cat.topos

import math.Base._
import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import math.cat.{Category, SetFunction}

import scala.annotation.targetName
import scala.language.implicitConversions

/**
  * A point of a topos object (of a diagram in a Grothendieck topos)
  * @param tag used to visually identify a point
  * @param topos the topos
  * @param mapping for each domain object, choose something in the topos diagram
  */
class Point(
  val tag: Any,
  val topos: GrothendieckTopos,
  val mapping: Any => Any) extends (Any => Any):
  p =>

  private val domainCategory: Category = topos.domain

  def apply(x: Any): Any = mapping(x)
  
  infix def named(name: Any): Point = new Point(name, topos, mapping)

  @deprecated("This should be redefined via composition", "03/28/2020")
  def transform(f: DiagramArrow): Point =
    def apply(o: Any) =
      f(o) match
        case sf: SetFunction => sf(p(o))
        case weirdStuff =>
          cannotDo(s"${f(o)} was supposed to be a set function")

    new Point(s"${f.tag}(${p.tag})", p.topos, apply)

  def asDiagram: Diagram =
    new Diagram(tag, topos):
      diagram =>

      override def objectsMapping(x: d0.Obj): d1.Obj = Set(mapping(x))

      private def arrowToFunction(a: d0.Arrow): Any => Any =
        (z: Any) => mapping(d0.d1(a))

      override protected def arrowsMappingCandidate(a: d0.Arrow): d1.Arrow =
        // need a set function from a.d0 to a.d1
          SetFunction(
            s"${diagram.tag}(.)",
            objectsMapping(d0.d0(a)),
            objectsMapping(d0.d1(a)),
            arrowToFunction(a))

  @targetName("in")
  infix inline def ∈(container: Diagram): Boolean = asDiagram ⊂ container

  private lazy val predicate: topos.Predicate = topos predicateFor this

  // TODO: fix this awkward unnecessary casting
  def asPredicateIn(t: GrothendieckTopos): t.Predicate =
    require(t eq topos)
    predicate.asInstanceOf[t.Predicate]

  override def toString: String =
    if tag.toString.nonEmpty then tag.toString else
      val raw = domainCategory.listOfObjects.map(x => s"$x -> ${apply(x)}")
      Diagram.cleanupString(raw.mkString(s"$tag(", ", ", ")"))

  def toShortString: String =
    val raw = domainCategory.objects.map(x => s"$x->${apply(x)}").mkString(s"$tag(", ", ", ")")
    val short = Diagram.cleanupString(raw)

    val strings: List[String] =
      domainCategory.listOfObjects.map { x =>
      val obRepr = apply(x) match
        case d: Diagram => Diagram.cleanupString(d.toShortString)
        case other => other.toString

      s"$x->$obRepr"
    }

    Diagram.cleanupString(strings.mkString(s"$tag(", ", ", ")"))

  override lazy val hashCode: Int = System.identityHashCode(topos) * 79 + toString.hashCode

  override def equals(obj: Any): Boolean = hashCode == obj.hashCode && (obj match
    case p: Point =>
      p.tag == tag && (p.domainCategory eq domainCategory) &&
        domainCategory.objects.forall(o => p(o) == this(o))
    case other => false
  )


