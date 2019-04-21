package math.cat.topos

import math.cat._
import math.Base._
import math.sets._
import math.sets.Sets._
import Diagrams._
import scalakittens.Result

class Diagrams(val site: Category)
  extends Category(s"Sets^${site.name}", graphOfDiagrams) {
  
  def objectNamed(name: String): site.Obj = site.obj(name)
  
  case class Representable(x: site.Obj) extends Diagram(s"hom($x, _)", site) {
    private def om(y: site.Obj) = site.hom(site.obj(x), y)

    /**
      * Maps a site arrow to set arrow.
      * 
      * @param f arrow in site
      * @return a function from site.hom(x, f.d0) to site.hom(x, f.d1)
      */
    private def am(f: site.Arrow): SetFunction = {
      val d0: site.Arrows = om(site.d0(f))
      val d1: site.Arrows = om(site.d1(f))
      val tuples: Set[(site.Arrow, site.Arrow)] = d0 flatMap { g ⇒ site.m(g, f) map (g → _) }
      val mapping: Map[site.Arrow, site.Arrow] =tuples toMap

      SetFunction.build(toSet(d0), toSet(d1), a ⇒ mapping(site.arrow(a))).iHope
    }

    override val objectsMapping: d0.Obj ⇒ d1.Obj = x ⇒ d1.obj(om(site.obj(x)))
        
    override val arrowsMappingCandidate: d0.Arrow ⇒ d1.Arrow = (f: XArrow) ⇒ {
      val arrow = site.arrow(f)
      val mapping = am(arrow)
      d1.arrow(mapping)
    }

    // have to validate right here, because a representable must exist, and all checks should be passing
    Functor.validateFunctor(this).iHope
  }

  val base: Category = BaseCategory
  type Node = Diagram
  type Arrow = DiagramArrow
  
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

  override lazy val initial: Result[Obj] =
    BaseCategory.initial map Diagrams.const("initial", site)

  lazy val _0: Obj = initial iHope

  override lazy val terminal: Result[Obj] =
    BaseCategory.terminal map Diagrams.const("terminal", site)
  
  lazy val _1: Obj = terminal iHope

  def inclusionOf(diagram1: Diagram, diagram2: Diagram): Result[DiagramArrow] = {
    val results: TraversableOnce[Result[(site.Obj, diagram1.d1.Arrow)]] = for {
      x <- site
      in = SetMorphism.inclusion(diagram1(x), diagram2(x))
      pair = in map (x → diagram1.d1.arrow(_))
    } yield pair

    for {
      map <- Result traverse results
      arrow <- NaturalTransformation.build(diagram1, diagram2)(map.toMap)
    } yield arrow
  }

  lazy val subterminals: Set[Diagram] = {
    def objectMapping(candidate: Set[site.Obj]) =
      (obj: site.Obj) ⇒ if (candidate contains obj) _1(obj) else Set.empty.untyped
    
    def arrowMappingOpt(candidate: Set[site.Obj]): site.Arrow ⇒ Result[SetFunction] = {
      val omc = objectMapping(candidate)
      (a: site.Arrow) ⇒ {
        // this transformation, site.arrow, is here due to an intellij bug
        val d0 = omc(site.d0(site.arrow(a)))
        val d1 = omc(site.d1(site.arrow(a)))
        val mapping = _1.asFunction(_1.arrowsMapping(_1.d0.arrow(a))).mapping
        
        SetFunction.build("", d0, d1, mapping)
      }
    }
    (1::Nil).toStream
    
    val all = for {
      (candidate, i) <- Sets.powerset(site.objects).zipWithIndex
      // some mappings are not working for a given candidate
      amCandidate: (site.Arrow ⇒ Result[SetFunction]) = arrowMappingOpt(candidate)
      arrowsMapOpt: Set[Result[(site.Arrow, SetFunction)]] = site.arrows map (a ⇒ amCandidate(a) map (a → _))
      am: Traversable[(site.Arrow, SetFunction)] <- Result.traverse(arrowsMapOpt).asOption
      om = objectMapping(candidate)
      // some of these build attemps will fail, because of compatibility checks
      diagram <- Diagram.build("__" + i, site)(om, am.toMap).asOption
    } yield diagram
    
    all
  }
  
  private[topos] def subobjectsOfRepresentables: Map[site.Obj, Set[Diagram]] =
    site.objects map (x ⇒ x → Representable(x).subobjects.toSet) toMap
  
  lazy val Ω: Diagram = {
    // For each object `x` we produce a set of all subobjects of `Representable(x)`.
    // These are values `Ω(x)`
    val om: Map[site.Obj, Set[Diagram]] = subobjectsOfRepresentables // cache the values at objects

    def am(a: site.Arrow): SetFunction = {
      val x = site.d0(a)
      val y = site.d1(a)
      val d0 = om(x) // `Ω(x)` = all subobjects of `Representable(x)`
      val d1 = om(y) // `Ω(y)` = all subobjects of `Representable(y)`

      // How one diagram is transformed via `a`:
      // For each `rx ⊂ Repr(x)` we have to produce a diagram `ry ⊂ Repr(y)`
      // r1 is a subobject of Representable(x)
      def diaMap(rx: Diagram /*a subrepresentable on `x`*/): Diagram /*a subrepresentable on `y`*/ = {
        // this is how elements of objects projections, that is, subterminals, are transformed by `a`
        def om1(x1: site.Obj): set = {
          // {f ∈ hom(y, x1) | a o f ∈ r1(x1)}
          site.hom(y, x1).untyped filter {f ⇒ rx(x1) contains site.m(a, site.arrow(f))}
        }
        // this is how, given an arrow `b`, the new diagram gets from one point to another
        def am1(b: site.Arrow): SetFunction = {
          val x1 = om1(site.d0(b)) //  {f ∈ hom(y, d0(b)) | a o f ∈ r1(d0(b)}
          val y1 = om1(site.d1(b)) //  {f ∈ hom(y, d1(b)) | a o f ∈ r1(d1(b)}
          // here we need a function fom x1 to y1
          val mapping = SetFunction.build("", x1, y1, g ⇒ site.m(b, site.arrow(g))).iHope
          mapping
        } 
        
        Diagram.build("", site)(om1, am1).iHope
      }
      
      SetFunction.build(s"[$a]", d0.untyped, d1.untyped, d ⇒ diaMap(d.asInstanceOf[Diagram])).iHope
    }
    
    Diagram.build("Ω", site)(d ⇒ om(d).untyped, am).iHope
    
  }
}

object Diagrams {
  val BaseCategory: Category = SetCategory.Setf

  def const(tag: String, site: Category)(value: BaseCategory.Obj): Diagram = {
    new Diagram(tag, site) {
      override val objectsMapping: d0.Obj ⇒ d1.Obj = (x: d0.Obj) ⇒ d1.obj(value)

      override val arrowsMappingCandidate: d0.Arrow ⇒ d1.Arrow =
        (a: XArrow) ⇒ d1.arrow(BaseCategory.id(value))
    }
  }

  type DiagramArrow = NaturalTransformation 
  
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
