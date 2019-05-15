package math.cat.topos

import math.cat._
import math.cat.topos.CategoryOfDiagrams._
import math.sets.Sets.{isFinite, _}
import math.sets._
import scalakittens.Result

class CategoryOfDiagrams(val domain: Category)
  extends Category(s"Sets^${domain.name}", graphOfDiagrams)
  with GrothendieckTopos {

  override def toString: String = name
  
  type Node = Diagram
  type Arrow = DiagramArrow

  def domainObjects: domain.Objects = domain.objects
  val listOfDomainObjects = domainObjects.toList
  
  override lazy val initial: Result[Obj] =
    BaseCategory.initial map const("initial", domain)
  lazy val _0: Obj = initial iHope
  override lazy val terminal: Result[Obj] =
    BaseCategory.terminal map const("terminal", domain)
  
  lazy val _1: Obj = terminal iHope
  
  lazy val subterminals: Set[Diagram] = {
    def objectMapping(candidate: Set[domain.Obj]) =
      (obj: domain.Obj) ⇒ if (candidate contains obj) _1(obj) else Set.empty.untyped

    def arrowMappingOpt(candidate: Set[domain.Obj]): domain.Arrow ⇒ Result[SetFunction] = {
      val omc = objectMapping(candidate)
      (a: domain.Arrow) ⇒ {
        // this transformation, domain.arrow, is here due to an intellij bug
        val d0 = omc(domain.d0(domain.arrow(a)))
        val d1 = omc(domain.d1(domain.arrow(a)))
        val mapping = _1.asFunction(_1.arrowsMapping(_1.d0.arrow(a))).mapping

        SetFunction.build("", d0, d1, mapping)
      }
    }

    (1 :: Nil).toStream

    val all: Set[Diagram] = for {
      (candidate, i) <- Sets.powerset(domain.objects).zipWithIndex
      // some mappings are not working for a given candidate
      amCandidate: (domain.Arrow ⇒ Result[SetFunction]) = arrowMappingOpt(candidate)
      arrowsMapOpt: Set[Result[(domain.Arrow, SetFunction)]] = domain.arrows map (a ⇒ amCandidate(a) map (a → _))
      am: Traversable[(domain.Arrow, SetFunction)] <- Result.traverse(arrowsMapOpt).asOption
      om = objectMapping(candidate)
      // some of these build attemps will fail, because of compatibility checks
      diagram: Diagram <- Diagram.build("__" + i, domain)(om, am.toMap).asOption
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

      override def transformPerObject(x: domainCategory.Obj): codomainCategory.Arrow =
        codomainCategory.arrow(objectMap(o.d0.obj(x)))
    }
  }

  override def m(f: Arrow, g: Arrow): Option[Arrow] = if (f.d1 == g.d0) Option {
    new DiagramArrow() {
      val tag = s"${g.tag} ∘ ${f.tag}"
      val d0: Functor = f.d0
      val d1: Functor = g.d1

      override def transformPerObject(x: domainCategory.Obj): codomainCategory.Arrow = {
        val xObjf = f.domainCategory.obj(x)
        val f_x = f.transformPerObject(xObjf)
        val xObjg = g.domainCategory.obj(x)
        val g_x = g.transformPerObject(xObjg)
        val gf_x = m(f_x, g_x)
        codomainCategory.arrow(gf_x)
      }
    }
  } else None

  trait includer {
    val diagram1: Diagram
    
    def in(diagram2: Diagram): Result[DiagramArrow] = {
      val results: TraversableOnce[Result[(domain.Obj, diagram1.d1.Arrow)]] = for {
        x <- domain
        in = SetFunction.inclusion(diagram1(x), diagram2(x))
        pair = in map (x → diagram1.d1.arrow(_))
      } yield pair

      val mapOpt = Result traverse results
      
      val result = for {
        map <- mapOpt
        arrow <- NaturalTransformation.build(s"${diagram1.tag}⊂${diagram2.tag}", diagram1, diagram2)(map.toMap)
      } yield arrow
      
      result
    }
  }
  
  def inclusionOf(diagram: Diagram): includer =
    new includer { val diagram1: Diagram = diagram }

  private[topos] def subobjectsOfRepresentables: Map[domain.Obj, Set[Diagram]] =
    domain.objects map (x ⇒ x → Representable(x).subobjects.toSet) toMap

  case class Representable(x: domain.Obj) extends Diagram(s"hom($x, _)", domain) {
    override val objectsMapping: d0.Obj ⇒ d1.Obj = x ⇒ d1.obj(om(domain.obj(x)))
    override val arrowsMappingCandidate: d0.Arrow ⇒ d1.Arrow = (f: XArrow) ⇒ {
      val arrow = domain.arrow(f)
      val mapping = am(arrow)
      d1.arrow(mapping)
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
      val tuples: Set[(domain.Arrow, domain.Arrow)] = d0 flatMap { g ⇒ domain.m(g, f) map (g → _) }
      val mapping: Map[domain.Arrow, domain.Arrow] = tuples toMap

      SetFunction.build(toSet(d0), toSet(d1), a ⇒ mapping(domain.arrow(a))).iHope
    }

    private def om(y: domain.Obj) = domain.hom(domain.obj(x), y)

    probablyFunctor.iHope
  }

  private[topos] case class product2builder(x: Diagram, y: Diagram) {

    private def productAt(o: domain.Obj) = Sets.product2(x(o), y(o))
    private def om(o: domain.Obj): set = productAt(o).untyped
    
    def transition(z: Diagram)(a: domain.Arrow)(pz: Any) = {
      val sf: SetFunction = z.asFunction(z.arrowsMapping(z.d0.arrow(a)))
      sf(pz)
    }
    
    private def am(a: domain.Arrow): SetFunction = {
      val from = productAt(domain.d0(a))
      val to = productAt(domain.d1(a))
      def f(p: Any): Any = p match {
        case (px, py) ⇒ (transition(x)(a)(px), transition(y)(a)(py))
        case other ⇒
          throw new IllegalArgumentException(s"Expected a pair of values, got $other")
      }
      new SetFunction("", from.untyped, to.untyped, f)
    }
    
    val diagram = Diagram(s"${x.tag}×${y.tag}", domain)(om, a ⇒ am(a))
  }

  /**
    * Cartesian product of two diagrams
    * TODO: figure out how to ensure the same d0 in bothDi
    */
  def product2(x: Diagram, y: Diagram): Diagram = product2builder(x, y).diagram


  def pointsOf(d: Diagram): List[Point] = {
    val objMappings = for {
      values <- Sets.product(d.listOfComponents) //.view
      mapping = listOfDomainObjects zip values toMap;
      om: Point = d.point(o ⇒ mapping(domain.obj(o)))
      if d.isCompatible(om)
    } yield om

    val sorted = objMappings.toList.sortBy(_.toString.replace("}", "!")).zipWithIndex

    sorted map { p ⇒ d.point(p._1, p._2) }
  }

  def inclusionOf(p: Point): includer = {
    val subdiagram = singletonDiagram(p.tag, o ⇒ p(o))
    inclusionOf(subdiagram)
  }

  def singletonDiagram(tag: String, value: domain.Obj ⇒ Any): Diagram = {
    val d = new Diagram(tag, domain) {
      
      override val objectsMapping: d0.Obj ⇒ d1.Obj = (x: d0.Obj) ⇒ d1.obj(Set(value(x.asInstanceOf[domain.Obj])))
      
      private def arrowToFunction(a: d0.Arrow): Any ⇒ Any =
        (z: Any) ⇒ {
          val y = d0.d1(a).asInstanceOf[domain.Obj]
          val v = value(y)
          v
        }
      
      override val arrowsMappingCandidate: d0.Arrow ⇒ d1.Arrow = (a: d0.Arrow) ⇒ {
        d1.arrow( // need a set function from a.d0 to a.d1
          SetFunction(s"$tag(.)", objectsMapping(d0.d0(a)), objectsMapping(d0.d1(a)), arrowToFunction(a))
        )
      }
    }
    val f: Functor = d.asInstanceOf[Functor]
    val dam = f.arrowsMapping(f.d0.arrow("a"))
    val damf = dam.asInstanceOf[SetFunction]
    for {
      ob <- damf.d0
    } {
      val v = damf(ob)
      println(v)
    }
    d
  }
}

object CategoryOfDiagrams {
  type DiagramArrow = NaturalTransformation
  val BaseCategory: Category = SetCategory.Setf

  def const(tag: String, domain: Category)(value: BaseCategory.Obj): Diagram = {
    new Diagram(tag, domain) {
      override val objectsMapping: d0.Obj ⇒ d1.Obj = (x: d0.Obj) ⇒ d1.obj(value)

      override val arrowsMappingCandidate: d0.Arrow ⇒ d1.Arrow =
        (a: XArrow) ⇒ d1.arrow(BaseCategory.id(value))
    }
  }

  def graphOfDiagrams: Graph =
    new Graph {
      type Node = Diagram
      type Arrow = DiagramArrow

      override def nodes: Nodes = BigSet.of[Node].asInstanceOf[Nodes]

      override def arrows: Arrows = BigSet.of[Arrow].asInstanceOf[Arrows]

      def d0(f: Arrow): Node = node(f.d0)

      def d1(f: Arrow): Node = node(f.d1)
    }

}
