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
  override val graph: Graph = this
  override def nodes = BigSet.of[Node](name)
  override lazy val toString: String = name
  override type Obj = Diagram
  type Node = Obj
  override type Arrow = DiagramArrow
  
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

  def pow(d: Diagram): Diagram = ??? // power object; tbd

  override def id(o: Obj): Arrow =
    def objectMap(x: o.d0.Obj): o.d1.Arrow = o.d1.id(o.calculateObjectsMapping(x))

    new DiagramArrow("Id", o, o):
      override def calculateMappingAt(x: d0.d0.Obj): d1.d1.Arrow = objectMap(x)

  override def m(f: Arrow, g: Arrow): Option[Arrow] = if f.d1 == g.d0 then Option {
    new DiagramArrow(concat(g.tag, " ∘ ", f.tag), f.d0, g.d1):

      override def calculateMappingAt(x: d0.d0.Obj): d1.d1.Arrow =
        val f_x = f(x)
        val g_x = g(x)
        m(f_x, g_x)

  } else None

  private[topos] def subobjectsOfRepresentables: Map[domain.Obj, Set[Diagram]] =
    buildMap[domain.Obj, Set[Diagram]](domain.objects,
      x => Representable(x).subobjects.toSet
    )

  case class Representable(x: domain.Obj) extends thisTopos.Diagram(s"${thisTopos.tag}.hom($x, _)"):
    override def calculateObjectsMapping(y: d0.Obj): d1.Obj = domain.hom(x, y)
    
    override protected def calculateArrowsMapping(f: d0.Arrow): d1.Arrow = am(f)
 
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

      new SetFunction("", itsaset(d0), itsaset(d1), a => mapping(a))

    probablyFunctor iHope
  
  end Representable
  
  def inclusionOf(p: Point): Includer = inclusionOf(p.asDiagram)

object CategoryOfDiagrams:
  type DiagramArrow = NaturalTransformation
  val BaseCategory: Category = SetCategory.Setf
