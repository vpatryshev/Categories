package math.cat.topos

import math.Base.*
import math.cat.*
import math.cat.topos
import math.cat.topos.CategoryOfDiagrams.{BaseCategory, *}
import math.sets.*
import math.sets.Sets.*
import scalakittens.Result

import scala.language.{implicitConversions, postfixOps}
import scala.reflect.Selectable.reflectiveSelectable

class CategoryOfDiagrams(val domain: Category)
  extends Category(s"Set^${domain.name}")
  with GrothendieckTopos:
  thisTopos =>

  override type Node = Diagram
  override type Obj = Diagram

  override def nodes: Nodes = BigSet.of[Node](name)

  /**
   * This method is redefined, because there's no set of arrows defined
   * and we also should only accept a DiagramArrow, which type can be checked in runtime
   * @param a
   * @return
   */
  override implicit def asArrow(a: Any): Arrow =
    a match
      case arrow: DiagramArrow => arrow
      case notAnArrow =>
        throw new IllegalArgumentException(s"<<$notAnArrow>> is not a diagram arrow")
  
  // we never scan all arrows in a category of diagrams, so it's not implemented
  override def arrows: Arrows = ???

  override def d0(f: DiagramArrow): Diagram = f.d0.asInstanceOf[Diagram]

  override def d1(f: DiagramArrow): Diagram = f.d1.asInstanceOf[Diagram]

  lazy val subterminals: Set[Diagram] =

    def objectMapping(candidate: Set[domain.Obj]): ObjectMapping =
      (obj: thisTopos.domain.Obj) =>
        if candidate contains obj then _1(obj) else `∅`

    def arrowMapping(candidate: Set[domain.Obj], omc: ObjectMapping): ArrowMapping =
      (a: thisTopos.domain.Arrow) =>
        val d0 = omc(domain.d0(a))
        val d1 = omc(domain.d1(a))
        val arrow = _1.arrowsMapping(a)
        val function = _1.asFunction(arrow)
        function restrictTo(d0, d1) iHope

    def mapping(candidate: Set[domain.Obj]): DiagramMapping =
      val ofObjects = objectMapping(candidate)
      val ofArrows = arrowMapping(candidate, ofObjects)
      val mapping = DiagramMapping(ofObjects, ofArrows)
      mapping

    val allDiagrams: Set[Diagram] =
      for
        (candidate, i) <- Sets.pow(domain.objects).zipWithIndex
        // some mappings are not working for a given candidate
        theMapping: DiagramMapping = mapping(candidate)
        mo = theMapping.ofObjects
        ma = theMapping.ofArrows
      // some of these build attempts will fail, because of compatibility checks
        diagram <- Diagram.tryBuild("__" + i, mo, ma) asOption
      yield diagram

    allDiagrams
  
  end subterminals
  
  val base: Category = BaseCategory

  def objectNamed(name: String): domain.Obj = name

  def pow(d: Diagram): Diagram = ??? // TODO: power object

  override def id(o: Diagram): Arrow =
    def objectMap(x: o.d0.Obj): o.d1.Arrow = o.d1.id(o.calculateObjectsMapping(x))

    new DiagramArrow("Id", o, o):
      thisArrow =>
      override def calculateMappingAt(x: thisArrow.d0.d0.Obj): thisArrow.d1.d1.Arrow =
        objectMap(x)

  override def m(f: Arrow, g: Arrow): Option[Arrow] = if f.d1 == g.d0 then Option(
    new DiagramArrow(concat(g.tag, " ∘ ", f.tag), f.d0, g.d1):
      thisArrow =>

      override def calculateMappingAt(x: thisArrow.d0.d0.Obj): thisArrow.d1.d1.Arrow =
        val f_x = f(x)
        val g_x = g(x)
        m(f_x, g_x)

  ) else None

  private[topos] def subobjectsOfRepresentables: Map[domain.Obj, Set[Diagram]] =
    buildMap[domain.Obj, Set[Diagram]](domain.objects,
      x => Representable(x).subobjects.toSet
    )

  case class Representable(x: domain.Obj) extends thisTopos.Diagram(s"${thisTopos.tag}.hom($x, _)"):
    override def calculateObjectsMapping(y: Representable.this.d0.Obj): Representable.this.d1.Obj =
      Representable.this.d0.hom(x, y)
    
    override protected def calculateArrowsMapping(f: Representable.this.d0.Arrow): Representable.this.d1.Arrow = am(f)
 
    // have to validate right here, because a representable must exist, and all checks should be passing
    private val probablyFunctor: Result[Functor] = Functor.validateFunctor(this)

    /**
      * Maps a domain arrow to set arrow.
      *
      * @param f arrow in domain
      * @return a function from domain.hom(x, f.d0) to domain.hom(x, f.d1)
      */
    private def am(f: domain.Arrow): SetFunction =
      val d0: domain.Arrows = domain.hom(x, domain.d0(f))
      val d1: domain.Arrows = domain.hom(x, domain.d1(f))
      val tuples: Set[(domain.Arrow, domain.Arrow)] = d0.flatMap{ g => domain.m(g, f) map (g -> _) }
      val mapping: Map[domain.Arrow, domain.Arrow] = tuples toMap

      new SetFunction("", itIsaSet(d0), itIsaSet(d1), a => mapping(a))

    probablyFunctor iHope
  
  end Representable
  
  def inclusionOf(p: Point): Includer = inclusionOf(p.asDiagram)

object CategoryOfDiagrams:
  val BaseCategory: Category = SetCategory.Setf
