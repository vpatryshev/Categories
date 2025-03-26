package math.cat.topos

import math.Base._
import math.cat._
import math.cat.topos
import math.cat.topos.CategoryOfDiagrams.{BaseCategory, _}

import math.sets._
import math.sets.Sets._
import scalakittens.Result

import scala.language.{implicitConversions, postfixOps}
import scala.reflect.Selectable.reflectiveSelectable

class CategoryOfDiagrams(val domain: Category)
  extends Category(s"Set^${domain.name}")
  with GrothendieckTopos:
  thisTopos =>
  override val graph: Graph = graphOfDiagrams(domain.name)
  override def nodes = graph.nodes.asInstanceOf[Nodes] // TODO: remove this cast
  override def toString: String = name
  type Node = Diagram
  override type Obj = Diagram
  override type Arrow = DiagramArrow
  
  lazy val subterminals: Set[Diagram] =
    def objectMapping(candidate: Set[domain.Obj]) =
      (obj: thisTopos.domain.Obj) =>
        if candidate contains obj then _1(obj) else `∅`

    def arrowMapping(candidate: Set[domain.Obj]): domain.Arrow => SetFunction =
      val omc = objectMapping(candidate)
      (a: thisTopos.domain.Arrow) =>
        val d0 = omc(domain.d0(a))
        val d1 = omc(domain.d1(a))
        val function = _1.arrowsMapping(a)
        function restrictTo(d0, d1) iHope

    val all: Set[Diagram] = 
      for
        (candidate, i) <- Sets.pow(domain.objects).zipWithIndex
        // some mappings are not working for a given candidate
        am: Map[domain.Arrow, SetFunction] = domain.arrows map (a => a -> arrowMapping(candidate)(a)) toMap

        om = objectMapping(candidate)
      // some of these build attempts will fail, because of compatibility checks
        diagram: thisTopos.Diagramme <- thisTopos.Diagramme.tryBuild("__" + i, om, am) asOption
      yield diagram.asOldDiagram

    all
  
  end subterminals
  
  val base: Category = BaseCategory

  def objectNamed(name: String): domain.Obj = name

  def pow(d: Diagramme): Diagramme = ??? // power object; tbd

  override def id(o: Obj): Arrow =
    def objectMap(x: o.d0.Obj): o.d1.Arrow = o.d1.id(o.objectsMapping(x))

    new DiagramArrow("Id"):
      override val d0: Functor = o
      override val d1: Functor = o

      override def mappingAt(x: d0.d0.Obj): d1.d1.Arrow =
        objectMap(x)

  override def m(f: Arrow, g: Arrow): Option[Arrow] = if f.d1 == g.d0 then Option {
    new DiagramArrow(concat(g.tag, " ∘ ", f.tag)):
      val d0: Functor = f.d0
      val d1: Functor = g.d1

      override def mappingAt(x: d0.d0.Obj): d1.d1.Arrow =
        val f_x = f.mappingAt(x)
        val g_x = g.mappingAt(x)
        m(f_x, g_x)

  } else None

  private[topos] def subobjectsOfRepresentables: Map[domain.Obj, Set[Diagram]] =
    buildMap[domain.Obj, Set[Diagram]](domain.objects,
      x => {
        val rep: Representable = Representable(x)
        val subobjects: Iterable[Diagram] = rep.subobjects.map(_.toOldDiagram)
        subobjects.toSet
      })

  case class Representable(x: domain.Obj) extends thisTopos.Diagramme(s"hom($x, _)", thisTopos.domain):
    override val d0: Category = thisTopos.domain
    override val d1: Category = SetCategory.Setf
    override def objectsMapping(x: d0.Obj): d1.Obj = om(x)
    override protected def arrowsMappingCandidate(f: d0.Arrow): d1.Arrow = am(f)
 
    // have to validate right here, because a representable must exist, and all checks should be passing
    private val probablyFunctor: Result[Functor] = Functor.validateFunctor(this)

    /**
      * Maps a domain arrow to set arrow.
      *
      * @param f arrow in domain
      * @return a function from domain.hom(x, f.d0) to domain.hom(x, f.d1)
      */
    private def am(f: domain.Arrow): SetFunction =
      val d0: domain.Arrows = om(domain.d0(f))
      val d1: domain.Arrows = om(domain.d1(f))
      val tuples: Set[(domain.Arrow, domain.Arrow)] = d0.flatMap{ g => domain.m(g, f) map (g -> _) }
      val mapping: Map[domain.Arrow, domain.Arrow] = tuples toMap

      new SetFunction("", itsaset(d0), itsaset(d1), a => mapping(a))

    private def om(y: domain.Obj) = domain.hom(x, y)

    probablyFunctor iHope
  
  end Representable

  def inclusionOf(p: Point): Includer = inclusionOf(p.asDiagram)

object CategoryOfDiagrams:
  type DiagramArrow = NaturalTransformation
  val BaseCategory: Category = SetCategory.Setf

  private def nameOfPowerCategory(domainName: String) = s"Sets^$domainName"
  
  private def graphOfDiagrams(domainName: String): Graph =
    new Graph(nameOfPowerCategory(domainName)):
      type Node = Diagram
      type Arrow = DiagramArrow

      override def nodes: Nodes   = BigSet.of[Node](name)
      override def arrows: Arrows = BigSet.of[Arrow](s"Arrows in $name")

      def d0(f: Arrow): Node = f.d0
      def d1(f: Arrow): Node = f.d1
