package math.cat.topos

import math.cat.{Category, SetFunction}
import math.cat.topos.CategoryOfDiagrams.DiagramArrow

/**
  * A point of a topos object (of a diagram in a Grothendieck topos)
  * @param tag used to visually identify a point
  * @param topos the topos
  * @param mapping for each domain object, choose something in the topos diagram
  */
class Point(
  val tag: Any,
  val topos: GrothendieckTopos,
  val mapping: Any ⇒ Any) extends (Any ⇒ Any) { p ⇒

  private val domainCategory: Category = topos.domain

  def apply(x: Any): Any = mapping(x)
  
  def named(name: Any): Point = new Point(name, topos, mapping)

  @deprecated("This should be redefined via composition", "03/28/2020")
  def transform(f: DiagramArrow): Point = {
    def apply(o: Any) = {
      f(o) match {
        case sf: SetFunction ⇒ sf(p(o))
        case weirdStuff ⇒
          throw new IllegalArgumentException(s"${f(o)} was supposed to be a set function")
      }
    }

    new Point(s"${f.tag}(${p.tag})", p.topos, apply)
  }

  def asDiagram: Diagram = {
    new Diagram(tag, topos, domainCategory) {

      override val objectsMapping: d0.Obj ⇒ d1.Obj =
        (x: d0.Obj) ⇒ d1.obj(Set(mapping(domainCategory.obj(x))))

      private def arrowToFunction(a: d0.Arrow): Any ⇒ Any =
        (z: Any) ⇒ {
          val y = domainCategory.obj(d0.d1(a))
          val v = mapping(y)
          v
        }

      override protected val arrowsMappingCandidate: d0.Arrow ⇒ d1.Arrow = (a: d0.Arrow) ⇒ {
        d1.arrow( // need a set function from a.d0 to a.d1
          SetFunction(s"$tag(.)", objectsMapping(d0.d0(a)), objectsMapping(d0.d1(a)), arrowToFunction(a))
        )
      }
    }
  }


  def ∈(container: Diagram): Boolean = asDiagram ⊂ container

  private lazy val predicate: topos.Predicate = topos predicateFor this

  def asPredicate[T <: GrothendieckTopos]: T#Predicate = predicate.asInstanceOf[T#Predicate]

  override def toString: String = {
    if (tag.toString.nonEmpty) tag.toString else {
      val raw = domainCategory.listOfObjects.map(x ⇒ s"$x → ${apply(x)}")
      Diagram.cleanupString(raw.mkString(s"$tag(", ", ", ")"))
    }
  }

  def toShortString: String = {
    val raw = domainCategory.objects.map(x ⇒ s"$x→${apply(x)}").mkString(s"$tag(", ", ", ")")
    val short = Diagram.cleanupString(raw)

    val strings: List[String] =
      domainCategory.listOfObjects map { x ⇒ {
      val obRepr = apply(x) match {
        case d: Diagram ⇒
          Diagram.cleanupString(d.toShortString)
        case other ⇒ other.toString
      }
      s"$x→$obRepr"
    }}

    Diagram.cleanupString(strings.mkString(s"$tag(", ", ", ")"))
  }

  override lazy val hashCode: Int = System.identityHashCode(topos) * 79 + toString.hashCode

  override def equals(obj: Any): Boolean = hashCode == obj.hashCode && (obj match {
    case p: Point ⇒
      p.tag == tag && p.domainCategory == domainCategory &&
        domainCategory.objects.forall(o ⇒ p(o) == this(o))
    case other ⇒ false
  })
}


