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
  topos =>
  override val graph = graphOfDiagrams(domain.name)
  override def toString: String = name
  type Node = Diagram
  override type Obj = Diagram
  override type Arrow = DiagramArrow
  
  lazy val subterminals: Set[Diagram] =
    def objectMapping(candidate: Set[domain.Obj]) =
      (obj: domain.Obj) =>
        if candidate contains obj then _1(obj) else Empty

    def arrowMapping(candidate: Set[domain.Obj]): domain.Arrow => SetFunction =
      val omc = objectMapping(candidate)
      (a: domain.Arrow) =>
        val d0 = omc(domain.d0(a))
        val d1 = omc(domain.d1(a))
        val function = _1.asFunction(_1.arrowsMapping(_1.d0.arrow(a)))
        function.restrictTo(d0, d1) iHope

    val all: Set[Diagram] = 
      for
      (candidate, i) <- Sets.pow(domain.objects).zipWithIndex
      // some mappings are not working for a given candidate
      amCandidate: (domain.Arrow => SetFunction) = arrowMapping(candidate)
      am: Set[(domain.Arrow, SetFunction)] = domain.arrows map (a => a -> amCandidate(a))
      om = objectMapping(candidate)
      // some of these build attemps will fail, because of compatibility checks
//        diagram: Diagram <- Result.forValue(Diagram(topos)("__" + i, om, am.toMap)).asOption
      diagram: Diagram <- Diagram.tryBuild(topos)("__" + i, om, am.toMap).asOption
    yield diagram

    all
  
  end subterminals
  
  val base: Category = BaseCategory

  def objectNamed(name: String): domain.Obj = domain.obj(name)

  def pow(d: Diagram): Diagram = ??? // power object; tbd

  override def id(o: Obj): Arrow =
    def objectMap(x: o.d0.Obj): o.d1.Arrow = o.d1.id(o.objectsMapping(x))

    new DiagramArrow("Id"):
      override val d0: Functor = o
      override val d1: Functor = o

      override def transformPerObject(x: d0.d0.Obj): d1.d1.Arrow =
        d1.d1.arrow(objectMap(o.d0.obj(x)))

  override def m(f: Arrow, g: Arrow): Option[Arrow] = if (f.d1 == g.d0) Option {
    new DiagramArrow(concat(g.tag, " ∘ ", f.tag)):
      val d0: Functor = f.d0
      val d1: Functor = g.d1

      override def transformPerObject(x: d0.d0.Obj): d1.d1.Arrow =
        val xObjf = f.domainCategory.obj(x)
        val f_x = f.transformPerObject(f.d0.d0.node(xObjf))
        val xObjg = g.domainCategory.obj(x)
        val g_x = g.transformPerObject(g.d0.d0.node(xObjg))
        val gf_x = m(f_x.asInstanceOf[Arrow], g_x.asInstanceOf[Arrow])
        d1.d1.arrow(gf_x)

  } else None

  private[topos] def subobjectsOfRepresentables: Map[domain.Obj, Set[Diagram]] =
    buildMap[domain.Obj, Set[Diagram]](domain.objects, x => Representable(x).subobjects.toSet)

  case class Representable(x: domain.Obj) extends Diagram(s"hom($x, _)", topos):
    override def objectsMapping(x: d0.Obj): d1.Obj = d1.obj(om(domain.obj(x)))
    override protected def arrowsMappingCandidate(f: d0.Arrow): d1.Arrow =
      am(f.asInstanceOf[domain.Arrow]).asInstanceOf[d1.Arrow]
 
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
      val tuples: Set[(domain.Arrow, domain.Arrow)] = d0 flatMap { g => domain.m(g, f) map (g -> _) }
      val mapping: Map[domain.Arrow, domain.Arrow] = tuples toMap

      new SetFunction("", setOf(d0), setOf(d1), a => mapping(domain.arrow(a)))

    private def om(y: domain.Obj) = domain.hom(domain.obj(x), y)

    probablyFunctor iHope
  
  end Representable

  def inclusionOf(p: Point): Includer = inclusionOf(p.asDiagram)

object CategoryOfDiagrams:
  type DiagramArrow = NaturalTransformation
  val BaseCategory: Category = SetCategory.Setf

  def const[O,A](tag: String, topos: GrothendieckTopos)(value: set): Diagram =
    type O = topos.domain.Obj
    type A = topos.domain.Arrow

    Diagram(topos)(
      tag,
      (x: O) => value,
      (a: A) => SetFunction.id(value))

  def nameOfPowerCategory(domainName: String) = s"Sets^$domainName"
  
  def graphOfDiagrams(domainName: String): Graph =
    new Graph(nameOfPowerCategory(domainName)):
      type Node = Diagram
      type Arrow = DiagramArrow

      override def nodes: Nodes   = BigSet.of[Node](name).asInstanceOf[Nodes]
      override def arrows: Arrows = BigSet.of[Arrow](s"Arrows in $name").asInstanceOf[Arrows]

      def d0(f: Arrow): Node = node(f.d0)
      def d1(f: Arrow): Node = node(f.d1)
