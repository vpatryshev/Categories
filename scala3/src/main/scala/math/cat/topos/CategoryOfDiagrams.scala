package math.cat.topos

import scala.language.implicitConversions
import scala.language.postfixOps
import reflect.Selectable.reflectiveSelectable
import math.Base._
import math.cat.{topos, _}
import math.cat.topos.CategoryOfDiagrams.BaseCategory
import math.sets.Sets._
import math.sets._
import scalakittens.Result
import topos.CategoryOfDiagrams._

class CategoryOfDiagrams(val domain: Category)
  extends Category
  with GrothendieckTopos { topos =>
  override val graph = graphOfDiagrams(domain.name)
  override def toString: String = name
  
  type Node = Diagram
  override type Obj = Diagram
  override type Arrow = DiagramArrow
  
  lazy val subterminals: Set[Diagram] = {
    def objectMapping(candidate: Set[domain.Obj]) =
      (obj: domain.Obj) => if (candidate contains obj) _1(obj) else Set.empty.untyped

    def arrowMapping(candidate: Set[domain.Obj]): domain.Arrow => SetFunction = {
      val omc = objectMapping(candidate)
      (a: domain.Arrow) => {
        // this transformation, domain.arrow, is here due to an intellij bug
        val d0 = omc(domain.d0(domain.arrow(a)))
        val d1 = omc(domain.d1(domain.arrow(a)))
        val mapping = _1.asFunction(_1.arrowsMapping(_1.d0.arrow(a))).mapping

        new SetFunction("", d0, d1, mapping)
      }
    }

    (1 :: Nil).to(LazyList)

    val all: Set[Diagram] = for {
      (candidate, i) <- Sets.powerset(domain.objects).zipWithIndex
      // some mappings are not working for a given candidate
      amCandidate: (domain.Arrow => SetFunction) = arrowMapping(candidate)
      am: Set[(domain.Arrow, SetFunction)] = domain.arrows map (a => a -> amCandidate(a))
      om = objectMapping(candidate)
      // some of these build attemps will fail, because of compatibility checks
      diagram: Diagram <- Diagram.build("__" + i, topos)(om, am.toMap).asOption
    } yield diagram

    all
  }
  val base: Category = BaseCategory

  def asFunction(a: /*almost*/ Any): SetFunction = a.asInstanceOf[SetFunction]

  def objectNamed(name: String): domain.Obj = domain.obj(name)

  def pow(d: Diagram): Diagram = ??? // power object; tbd

  override def id(o: Obj): Arrow = {
    def objectMap(x: o.d0.Obj): o.d1.Arrow = o.d1.id(o.objectsMapping(x))

    new DiagramArrow {
      val tag = "Id"
      override val d0: Functor = o
      override val d1: Functor = o

      override def transformPerObject(x: d0.d0.Obj): d1.d1.Arrow =
        codomainCategory.arrow(objectMap(o.d0.obj(x)))
    }
  }

  override def m(f: Arrow, g: Arrow): Option[Arrow] = if (f.d1 == g.d0) Option {
    new DiagramArrow() {
      val tag = s"${g.tag} ∘ ${f.tag}"
      val d0: Functor = f.d0
      val d1: Functor = g.d1

      override def transformPerObject(x: d0.d0.Obj): d1.d1.Arrow = {
        val xObjf = f.domainCategory.obj(x)
        val f_x = f.transformPerObject(xObjf)
        val xObjg = g.domainCategory.obj(x)
        val g_x = g.transformPerObject(xObjg)
        val gf_x = m(f_x, g_x)
        codomainCategory.arrow(gf_x)
      }
    }
  } else None

  private[topos] def subobjectsOfRepresentables: Map[domain.Obj, Set[Diagram]] =
    domain.objects map (x => x -> Representable(x).subobjects.toSet) toMap

  case class Representable(x: domain.Obj) extends Diagram(s"hom($x, _)", topos) {
    override val objectsMapping: d0.Obj => d1.Obj = x => d1.obj(om(domain.obj(x)))
    override protected val arrowsMappingCandidate: d0.Arrow => d1.Arrow = (f: XArrow) => {
      am(f.asInstanceOf[domain.Arrow]).asInstanceOf[d1.Arrow]
    }
    // have to validate right here, because a representable must exist, and all checks should be passing
    private val probablyFunctor: Result[Functor] = Functor.validateFunctor(this)

    /**
      * Maps a domain arrow to set arrow.
      *
      * @param f arrow in domain
      * @return a function from domain.hom(x, f.d0) to domain.hom(x, f.d1)
      */
    private def am(f: domain.Arrow): SetFunction = {
      val d0: domain.Arrows = om(domain.d0(f))
      val d1: domain.Arrows = om(domain.d1(f))
      val tuples: Set[(domain.Arrow, domain.Arrow)] = d0 flatMap { g => domain.m(g, f) map (g -> _) }
      val mapping: Map[domain.Arrow, domain.Arrow] = tuples toMap

      new SetFunction("", toSet(d0), toSet(d1), a => mapping(domain.arrow(a)))
    }

    private def om(y: domain.Obj) = domain.hom(domain.obj(x), y)

    probablyFunctor.iHope
  }

  def inclusionOf(p: Point): Includer = inclusionOf(p.asDiagram)
}

object CategoryOfDiagrams {
  type DiagramArrow = NaturalTransformation
  val BaseCategory: Category = SetCategory.Setf

  def const[O,A](tag: String, topos: GrothendieckTopos)(value: set): Diagram = {
    type O = topos.domain.Obj
    type A = topos.domain.Arrow

    Diagram[O, A](tag, topos)(
      (x: O) => value,
      (a: A) => SetFunction.id(value))
  }

  def graphOfDiagrams(domainName: String): Graph =
    new Graph {
      override val name = s"Sets^$domainName"
      type Node = Diagram
      type Arrow = DiagramArrow

      override def nodes: Nodes = BigSet.of[Node](name).asInstanceOf[Nodes]

      override def arrows: Arrows = BigSet.of[Arrow]("Collection of arrows in " + name).asInstanceOf[Arrows]

      def d0(f: Arrow): Node = node(f.d0)

      def d1(f: Arrow): Node = node(f.d1)
    }

}
