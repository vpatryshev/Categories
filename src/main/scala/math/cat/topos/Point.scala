package math.cat.topos

import math.cat.{Category, SetFunction}
import math.cat.topos.CategoryOfDiagrams.DiagramArrow

/**
  * A point of an object (of a diagram in a Grothendieck topos)
  * @param tag used to visually identify a point
  * @param domainCategory the domain category of a topos
  * @param mapping for each object of domain category, choose something in the diagram
  */
class Point(
  val tag: Any,
  val domainCategory: Category,
  val mapping: Any => Any) extends (Any => Any) { p =>

  def apply(x: Any): Any = mapping(x)
  
  def named(name: Any): Point = new Point(name, domainCategory, mapping)

  def ∈(diagram: Diagram): Boolean = domainCategory.objects.forall { o ⇒ diagram(o)(this(o)) }

  def transform(f: DiagramArrow): Point = {
    def apply(o: Any) = {
      f(o) match {
        case sf: SetFunction => sf(p(o))
        case weirdStuff =>
          throw new IllegalArgumentException(s"${f(o)} was supposed to be a set function")
      }
    }

    new Point(s"${f.tag}(${p.tag})", p.domainCategory, apply)
  }

  def asDiagram: Diagram = {
    new Diagram(tag, domainCategory) {

      override val objectsMapping: d0.Obj ⇒ d1.Obj =
        (x: d0.Obj) ⇒ d1.obj(Set(mapping(x.asInstanceOf[domainCategory.Obj])))

      private def arrowToFunction(a: d0.Arrow): Any ⇒ Any =
        (z: Any) ⇒ {
          val y = d0.d1(a).asInstanceOf[domainCategory.Obj]
          val v = mapping(y)
          v
        }

      override val arrowsMappingCandidate: d0.Arrow ⇒ d1.Arrow = (a: d0.Arrow) ⇒ {
        d1.arrow( // need a set function from a.d0 to a.d1
          SetFunction(s"$tag(.)", objectsMapping(d0.d0(a)), objectsMapping(d0.d1(a)), arrowToFunction(a))
        )
      }
    }
  }

  override def toString: String = {
    val raw = domainCategory.listOfObjects.map(x => s"$x→${apply(x)}")
    Diagram.cleanupString(raw.mkString(s"$tag(", ", ", ")"))
  }

  def toShortString = {
    val raw = domainCategory.objects.map(x => s"$x→${apply(x)}").mkString(s"$tag(", ", ", ")")
    val short = Diagram.cleanupString(raw)

    val strings: List[String] =
      domainCategory.listOfObjects map { x ⇒ {
      val obRepr = apply(x) match {
        case d: Diagram =>
          Diagram.cleanupString(d.toShortString)
        case other => other.toString
      }
      s"$x→$obRepr"
    }}

    Diagram.cleanupString(strings.mkString(s"$tag(", ", ", ")"))
  }

  override lazy val hashCode: Int = System.identityHashCode(domainCategory) * 79 + toString.hashCode

  override def equals(obj: Any): Boolean = hashCode == obj.hashCode && (obj match {
    case p: Point =>
      p.tag == tag && p.domainCategory == domainCategory &&
        domainCategory.objects.forall(o => p(o) == this(o))
    case other => false
  })
}


