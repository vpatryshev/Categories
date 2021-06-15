package math.cat.topos

import scala.language.implicitConversions
import scala.language.postfixOps

import math.Base._
import math.cat._
import math.cat.topos.CategoryOfDiagrams.{BaseCategory, _}
import math.sets.Sets._
import math.sets._
import scalakittens.Result

class CategoryOfDiagrams(val domain: Category)
  extends Category
  with GrothendieckTopos { topos =>
  override val graph = graphOfDiagrams(domain.name)
  override def toString: String = name
  
  type Node = Diagram
  override type Obj = Diagram
  override type Arrow = DiagramArrow
  type Mapping = domain.Obj => Any => Any
  
  private def constSet(name: String)(obj: BaseCategory.Obj): Diagram =
    const(name, topos)(obj.asInstanceOf[set])
  
  override lazy val initial: Result[Obj] = BaseCategory.initial map constSet("initial")
  lazy val _0: Obj = initial iHope
  
  override lazy val terminal: Result[Obj] = BaseCategory.terminal map constSet("terminal")
  lazy val _1: Obj = terminal iHope
  
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

  /**
    * Given a `from` and `to` diagrams, build an arrow
    * `from(o)` -> `to(o)`, for each given `o`,
    * using the provided mapping
    *
    * @param tag tag of a natural transformation
    * @param from domain diagram
    * @param to codomain diagram
    * @param mapping given an object `o`, produce a function over this object
    * @param o the object
    * @return an arrow (it's a `SetFunction`, actually)
    */
  protected def buildOneArrow(
    tag: Any,
    from: Diagram,
    to: Diagram,
    mapping: Mapping
  )(o: from.d0.Obj): from.d1.Arrow = {
    val domO: domain.Obj = domain.obj(o)
    val funOpt = SetFunction.build(s"$tag[$o]", from(o), to(o), mapping(domO))
    from.d1.arrow(funOpt.iHope)
  }

  /**
    * Builds a `DiagramArrow`, given domain, codomain, and a mapping
    * @param tag arrow tag
    * @param from domain
    * @param to codomain
    * @param mapping maps objects to functions
    * @return a natural transformation (crashes if not)
    */
  def buildArrow(tag: Any, from: Diagram, to: Diagram,
    mapping: Mapping): DiagramArrow = {
    NaturalTransformation.build(tag, from, to)(
      (o: from.d0.Obj) => buildOneArrow(tag, from, to, mapping)(o)).iHope
  }

  trait includer {
    val subdiagram: Diagram
    
    def in(diagram: Diagram): Result[DiagramArrow] = {
      val results: IterableOnce[Result[(domain.Obj, subdiagram.d1.Arrow)]] = {
        for {
          x <- domain.objects.iterator
          in = SetFunction.inclusion(subdiagram(x), diagram(x))
          pair = in map (x -> subdiagram.d1.arrow(_))
        } yield pair
      }

      val mapOpt = Result traverse results
      
      val result = for {
        map <- mapOpt
        arrow <- NaturalTransformation.build(s"${subdiagram.tag}⊂${diagram.tag}", subdiagram, diagram)(map.toMap)
      } yield arrow
      
      result
    }
  }
  
  def inclusionOf(diagram: Diagram): includer =
    new includer { val subdiagram: Diagram = diagram }

  private[topos] def subobjectsOfRepresentables: Map[domain.Obj, Set[Diagram]] =
    domain.objects map (x => x -> Representable(x).subobjects.toSet) toMap

  case class Representable(x: domain.Obj) 
    extends Diagram(s"hom($x, _)", topos, topos.domain) {
    
    override val objectsMapping: d0.Obj => d1.Obj = x => d1.obj(om(domain.obj(x)))
    override protected val arrowsMappingCandidate: d0.Arrow => d1.Arrow =
      (f: XArrow) => am(f.asInstanceOf[domain.Arrow]).asInstanceOf[d1.Arrow]

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

  private[topos] case class product2builder(x: Diagram, y: Diagram) {

    private def productAt(o: domain.Obj) = Sets.product2(x(o), y(o))
    private def mappingOfObjects(o: domain.Obj): set = productAt(o).untyped
    
    def transition(z: Diagram)(a: domain.Arrow)(pz: Any) = {
      val sf: SetFunction = z.asFunction(z.arrowsMapping(z.d0.arrow(a)))
      sf(pz)
    }
    
    private def mappingOfArrows(a: domain.Arrow): SetFunction = {
      val from = productAt(domain.d0(a))
      val to = productAt(domain.d1(a))
      def f(p: Any): Any = p match {
        case (px, py) => (transition(x)(a)(px), transition(y)(a)(py))
        case other =>
          throw new IllegalArgumentException(s"Expected a pair of values, got $other")
      }
      new SetFunction("", from.untyped, to.untyped, f)
    }
    
    val diagram = Diagram(s"${x.tag}×${y.tag}", topos)(mappingOfObjects, a => mappingOfArrows(a))
  }

  /**
    * Cartesian product of two diagrams
    * TODO: figure out how to ensure the same d0 in both Di
    */
  def product2(x: Diagram, y: Diagram): Diagram = product2builder(x, y).diagram

  // π1
  protected val firstProjection: Mapping = Functions.constant[domain.Obj, Any => Any] {
    case (a, b) => a
    case trash =>
      throw new IllegalArgumentException(s"Expected a pair, got $trash")
  }

  // π2
  protected val secondProjection: Mapping = Functions.constant[domain.Obj, Any => Any] {
    case (a, b) => b
    case trash =>
      throw new IllegalArgumentException(s"Expected a pair, got $trash")
  }

  /**
    * Given arrows `f` and `g`, builds an arrow (f×g): dom(f)×dom(g) -> codom(f)×codom(g)
    *
    * @param f first component
    * @param g second component
    * @return a product of `f` and `g`
    */
  def productOfArrows(f: DiagramArrow, g: DiagramArrow): DiagramArrow = {

    val mapping: Mapping = x => {
      val fx = f(x).asInstanceOf[SetMorphism[Any, Any]]
      val gx = g(x).asInstanceOf[SetMorphism[Any, Any]]
      
      { case (a, b) => (fx(a), gx(b)) }
    }

    buildArrow(
      concat(f.tag, "×", g.tag),
      product2(f.d0, g.d0),
      product2(f.d1, g.d1),
      mapping)
  }

  def inclusionOf(p: Point): includer = inclusionOf(p.asDiagram)
  
  def standardInclusion(p: Point, d: Diagram): Result[Arrow] = {
    inclusionOf(p) in d map { q => {
      uniqueFromTerminalTo(p) andThen q named p.tag
    } }
  }

  /**
    * An arrow from terminal to the point as a diagram
    * @return
    */
  def uniqueFromTerminalTo(p: Point): Arrow = {
    new DiagramArrow {
      val tag = p.tag

      override val d0: Diagram = _1
      override val d1: Diagram = p.asDiagram

      override def transformPerObject(o: domainCategory.Obj): codomainCategory.Arrow =
      codomainCategory.arrow {
        val value = p(o)
        new SetFunction(s"tag($o)", _1(o), Set(value), _ => value)
      }
    }    
  }
}

object CategoryOfDiagrams {
  type DiagramArrow = NaturalTransformation
  val BaseCategory: Category = SetCategory.Setf

  def const(tag: String, topos: GrothendieckTopos)(value: set): Diagram = {
    Diagram(tag, topos)(
      (x: topos.domain.Obj) => value,
      (a: topos.domain.Arrow) => SetFunction.id(value))
  }

  def graphOfDiagrams(domainName: String): Graph =
    new Graph {
      override val name = s"Sets^$domainName"
      type Node = Diagram
      type Arrow = DiagramArrow

      override def nodes: Nodes = BigSet.of[Node].asInstanceOf[Nodes]

      override def arrows: Arrows = BigSet.of[Arrow].asInstanceOf[Arrows]

      def d0(f: Arrow): Node = node(f.d0)

      def d1(f: Arrow): Node = node(f.d1)
    }

}
