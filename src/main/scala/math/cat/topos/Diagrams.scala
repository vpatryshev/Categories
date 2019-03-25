package math.cat.topos

import math.cat._
import Diagrams._
import math.sets._

class Diagrams(site: Category)
  extends Category(s"Sets^${site.name}", graphOfDiagrams) {
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
}

object Diagrams {
  type Diagram = SetDiagram
  
  type DiagramArrow = NaturalTransformation
  
  def graphOfDiagrams: Graph =
    new Graph {
      type Node = Diagram
      type Arrow = DiagramArrow

      override def nodes: Nodes = BigSet.of[Node].asInstanceOf[Nodes]

      override def arrows: Arrows = BigSet.of[Arrow].asInstanceOf[Arrows]

      def d0(f: Arrow): Node = f.d0.asInstanceOf[Diagram]

      def d1(f: Arrow): Node = f.d1.asInstanceOf[Diagram]
    }
}
