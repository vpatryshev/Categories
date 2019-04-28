package math.cat.topos

import math.Base._
import math.cat._
import math.cat.topos.Diagrams._
import math.sets.Sets._
import math.sets._
import scalakittens.Result

class Diagrams(val domain: Category)
  extends Category(s"Sets^${domain.name}", graphOfDiagrams) {

  type Node = Diagram
  type Arrow = DiagramArrow
  override lazy val initial: Result[Obj] =
    BaseCategory.initial map Diagrams.const("initial", domain)
  lazy val _0: Obj = initial iHope
  override lazy val terminal: Result[Obj] =
    BaseCategory.terminal map Diagrams.const("terminal", domain)
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

    val all = for {
      (candidate, i) <- Sets.powerset(domain.objects).zipWithIndex
      // some mappings are not working for a given candidate
      amCandidate: (domain.Arrow ⇒ Result[SetFunction]) = arrowMappingOpt(candidate)
      arrowsMapOpt: Set[Result[(domain.Arrow, SetFunction)]] = domain.arrows map (a ⇒ amCandidate(a) map (a → _))
      am: Traversable[(domain.Arrow, SetFunction)] <- Result.traverse(arrowsMapOpt).asOption
      om = objectMapping(candidate)
      // some of these build attemps will fail, because of compatibility checks
      diagram <- Diagram.build("__" + i, domain)(om, am.toMap).asOption
    } yield diagram

    all
  }
  val base: Category = BaseCategory

  def objectNamed(name: String): domain.Obj = domain.obj(name)

  def pow(d: Diagram): Diagram = ??? // power object; tbd

  override def id(o: Obj): Arrow = {
    def objectMap(x: o.d0.Obj): o.d1.Arrow = o.d1.id(o.objectsMapping(x))

    new DiagramArrow {
      override val d0: Functor = o
      override val d1: Functor = o

      override def transformPerObject(x: domainCategory.Obj): codomainCategory.Arrow =
        codomainCategory.arrow(objectMap(o.d0.obj(x)))
    }
  }

  override def m(f: Arrow, g: Arrow): Option[Arrow] = if (f.d1 == g.d0) Option {
    new DiagramArrow() {
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

  def inclusionOf(diagram1: Diagram, diagram2: Diagram): Result[DiagramArrow] = {
    val results: TraversableOnce[Result[(domain.Obj, diagram1.d1.Arrow)]] = for {
      x <- domain
      in = SetMorphism.inclusion(diagram1(x), diagram2(x))
      pair = in map (x → diagram1.d1.arrow(_))
    } yield pair

    for {
      map <- Result traverse results
      arrow <- NaturalTransformation.build(diagram1, diagram2)(map.toMap)
    } yield arrow
  }

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

  /**
    * Omega, subobject classifier.
    */
  object Ω extends Diagram("Ω", domain) /* TODO: new Diagram("Ω", domain)*/ {
    // For each object `x` we produce a set of all subobjects of `Representable(x)`.
    // These are values `Ω(x)`. We cache them in the following map map `x ⇒ Ω(x)` .
    val mapOfSubrepresentables: Map[domain.Obj, Set[Diagram]] = subobjectsOfRepresentables

    // this one is consumed by Functor constructor
    val objectsMapping: d0.Obj ⇒ d1.Obj =
      (d: d0.Obj) ⇒ d1.obj(mapOfSubrepresentables(domain.obj(d): domain.Obj))

    // for each arrow `a: x -> y` produce a transition `Ω(x) -> Ω(y)`.
    def am(a: domain.Arrow): SetFunction = {
      val x = domain.d0(a)
      val y = domain.d1(a)
      val d0 = mapOfSubrepresentables(x) // `Ω(x)` = all subobjects of `Representable(x)`
      val d1 = mapOfSubrepresentables(y) // `Ω(y)` = all subobjects of `Representable(y)`

      // How one diagram is transformed via `a`:
      // For each `rx ⊂ Repr(x)` we have to produce a diagram `ry ⊂ Repr(y)`
      def diaMap(rx: Diagram): Diagram /*a subrepresentable on `x`*/ = {
        // this is how elements of objects projections, that is, subterminals, are transformed by `a`
        val om1 = transformingOfSubrepresentables(a, rx) _

        // this is how, given an arrow `b`, the new diagram gets from one point to another
        def am1(b: domain.Arrow): SetFunction = {
          val x1 = om1(domain.d0(b)) //  {f ∈ hom(y, d0(b)) | a o f ∈ r1(d0(b)}
          val y1 = om1(domain.d1(b)) //  {f ∈ hom(y, d1(b)) | a o f ∈ r1(d1(b)}

          // A function fom x1 to y1 - it does the transition
          SetFunction.build("", x1, y1, g ⇒ domain.m(domain.arrow(g), b).get // it must be defined
          ) iHope
        }

        Diagram.build("", domain)(om1, am1) iHope
      }

      SetFunction.build(s"[$a]", d0.untyped, d1.untyped, d ⇒ diaMap(d.asInstanceOf[Diagram])).iHope
    }
    
    val arrowsMappingCandidate: d0.Arrow ⇒ d1.Arrow =
      (a: XArrow) ⇒ d1.arrow(am(domain.arrow(a)))

    /**
      * Given an arrow `a`, 
      * {f ∈ hom(y, x1) | a o f ∈ r1(x1)}
      *
      * @param a  an arrow
      * @param rx a subrepresentable
      * @param x1 an object in domain (a "state")
      * @return
      */
    def transformingOfSubrepresentables(a: domain.Arrow, rx: Diagram)(x1: domain.Obj): set = {
      val y = domain.d1(a)
      val rx_at_x1 = rx(x1)
      for {
        f <- domain.hom(y, x1)
        candidate <- domain.m(a, domain.arrow(f))
        if rx_at_x1 contains candidate
      } yield f
    }

    validate iHope
  }
}

object Diagrams {
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
