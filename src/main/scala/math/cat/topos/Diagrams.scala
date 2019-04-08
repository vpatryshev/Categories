package math.cat.topos

import math.cat._
import math.Base._
import math.sets._
import math.sets.Sets._
import Diagrams._
import scalakittens.Result

class Diagrams(val site: Category)
  extends Category(s"Sets^${site.name}", graphOfDiagrams) {
  def representable(obj: site.Obj): Diagram = null

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
    val fArrow = f.asInstanceOf[DiagramArrow]
    val gArrow = g.asInstanceOf[DiagramArrow]
    val composition: DiagramArrow = {
      new DiagramArrow() {
        val d0: Functor = fArrow.d0
        val d1: Functor = gArrow.d1

        override def transformPerObject(x: domainCategory.Obj): codomainCategory.Arrow = {
          val xObjf: fArrow.domainCategory.Obj = fArrow.domainCategory.obj(x)
          val f_x = fArrow.transformPerObject(xObjf)
          val xObjg: gArrow.domainCategory.Obj = gArrow.domainCategory.obj(x)
          val g_x = gArrow.transformPerObject(xObjg)
          val gf_x = f_x.compose(g_x)
          codomainCategory.arrow(gf_x)
        }
      }
    }
    composition.asInstanceOf[Arrow]
  } else None

  override lazy val initial: Option[Obj] =
    BaseCategory.initial map Diagrams.const("initial", site)

  lazy val _0: Obj = initial iHope

  override lazy val terminal: Option[Obj] =
    BaseCategory.terminal map Diagrams.const("terminal", site)
  
  lazy val _1: Obj = terminal iHope

  def inclusionOf(diagram1: Diagram, diagram2: Diagram): Result[DiagramArrow] = {
    val results: TraversableOnce[Result[(site.Obj, diagram1.d1.Arrow)]] = for {
      x <- site
      in = SetMorphism.inclusion(diagram1(x), diagram2(x))
      pair = in map (x -> diagram1.d1.arrow(_))
    } yield pair

    for {
      map <- Result traverse results
      arrow <- NaturalTransformation.build(diagram1, diagram2)(map.toMap)
    } yield arrow
  }

  lazy val subterminals: Set[Diagram] = {
    def objectMapping(candidate: Set[site.Obj]) =
      (obj: site.Obj) => if (candidate contains obj) _1(obj) else Set.empty.untyped
    
    var ii: Int = 42
    
    def arrowMapping(candidate: Set[site.Obj]): site.Arrow => SetFunction = {
      val omc = objectMapping(candidate)
      import _1.d0._
      (a: site.Arrow) => {
        // this transformation, site.arrow, is here due to an intellij bug
        val d0 = omc(site.d0(site.arrow(a)))
        val d1 = omc(site.d1(site.arrow(a)))
        val mapping = _1.asFunction(_1.arrowsMapping(_1.d0.arrow(a))).mapping
        
        SetFunction("", d0, d1, mapping)
      }
    }
    
    val all = for {
      (candidate, i) <- Sets.powerset(site.objects).zipWithIndex
      om = objectMapping(candidate)
      am = arrowMapping(candidate)
      diagram <- SetDiagram.build("__" + i, site)(om, am).toOption
    } yield diagram
    
    all
  } 
}

object Diagrams {
  type Diagram = SetDiagram

  val BaseCategory: Category = SetCategory.Setf

  def const(tag: String, site: Category)(value: BaseCategory.Obj): Diagram = {
    new Diagram(tag, site) {
      override val objectsMapping: d0.Obj => d1.Obj = (x: d0.Obj) => d1.obj(value)

      override val arrowsMappingCandidate: d0.Arrow => d1.Arrow =
        (a: XArrow) => d1.arrow(BaseCategory.id(value))
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
