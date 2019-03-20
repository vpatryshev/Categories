package math.cat.topos

import math.cat._
import Diagrams._
import math.sets._

class Diagrams[C <: Category](site: C)
  extends Category(s"Sets^${site.name}", graphOfDiagrams[C]) {
  type Node = Diagram[C]
  type Arrow = DiagramArrow[C]
  override def id(o: O): Arrow = {
    def objectMap(x: o.d0.O): o.d1.Arrow =
      o.d1.id(o.objectsMapping(x).asInstanceOf[o.d1.O])

    new DiagramArrow[C](o, o) {
      override def transformPerObject(x: from.d0.O): from.d1.Arrow =
        objectMap(x.asInstanceOf[o.d0.O]).asInstanceOf[from.d1.Arrow]
    }
  }

  override def m(f: Arrow, g: Arrow): Option[Arrow] = if (f.d1 == g.d0) Option {
    val fArrow = f.asInstanceOf[DiagramArrow[C]]
    val gArrow = g.asInstanceOf[DiagramArrow[C]]
    val composition: DiagramArrow[C] = {
      new DiagramArrow[C](fArrow.d0.asInstanceOf[Diagram[C]], gArrow.d1.asInstanceOf[Diagram[C]]) {
        override def transformPerObject(x: from.d0.O): from.d1.Arrow = {
          val xObjf: fArrow.from.d0.O = x.asInstanceOf[fArrow.from.d0.O]
          val f_x = fArrow.transformPerObject(xObjf)
          val xObjg: gArrow.from.d0.O = x.asInstanceOf[gArrow.from.d0.O]
          val g_x = gArrow.transformPerObject(xObjg)
          val gf_x = f_x.compose(g_x)
          gf_x.asInstanceOf[from.d1.Arrow]
        }
      }
    }
    composition.asInstanceOf[Arrow]
  } else None
}

object Diagrams {
  type Diagram[C <: Category] = SetDiagram[C]
  
  type DiagramArrow[C <: Category] = NaturalTransformation[C, SetCategory]
  
  def graphOfDiagrams[C <: Category]: Graph =
    new Graph {
      type Node = Diagram[C]
      type Arrow = DiagramArrow[C]

      override def nodes: Nodes = BigSet.of[Node].asInstanceOf[Nodes]

      override def arrows: Arrows = BigSet.of[Arrow].asInstanceOf[Arrows]

      def d0(f: Arrow): Node = f.d0.asInstanceOf[Diagram[C]]

      def d1(f: Arrow): Node = f.d1.asInstanceOf[Diagram[C]]
    }
}
