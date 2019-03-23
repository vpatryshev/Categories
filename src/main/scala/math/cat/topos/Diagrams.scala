package math.cat.topos

import math.cat._
import Diagrams._
import math.sets._

class Diagrams[C <: Category](site: Category)
  extends Category(s"Sets^${site.name}", graphOfDiagrams[Category]) {
  type Node = Diagram
  type Arrow = DiagramArrow[Category]
  override def id(o: Obj): Arrow = {
    def objectMap(x: o.d0.Obj): o.d1.Arrow =
      o.d1.id(o.objectsMapping(x).asInstanceOf[o.d1.Obj])

    new DiagramArrow[Category](o, o) {
      override def transformPerObject(x: from.d0.Obj): from.d1.Arrow =
        objectMap(x.asInstanceOf[o.d0.Obj]).asInstanceOf[from.d1.Arrow]
    }
  }

  override def m(f: Arrow, g: Arrow): Option[Arrow] = if (f.d1 == g.d0) Option {
    val fArrow = f.asInstanceOf[DiagramArrow[C]]
    val gArrow = g.asInstanceOf[DiagramArrow[C]]
    val composition: DiagramArrow[Category] = {
      new DiagramArrow[C](fArrow.d0.asInstanceOf[Diagram], gArrow.d1.asInstanceOf[Diagram]) {
        override def transformPerObject(x: from.d0.Obj): from.d1.Arrow = {
          val xObjf: fArrow.from.d0.Obj = x.asInstanceOf[fArrow.from.d0.Obj]
          val f_x = fArrow.transformPerObject(xObjf)
          val xObjg: gArrow.from.d0.Obj = x.asInstanceOf[gArrow.from.d0.Obj]
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
  type Diagram = SetDiagram[Category]
  
  type DiagramArrow[C <: Category] = NaturalTransformation[Category, SetCategory]
  
  def graphOfDiagrams[C <: Category]: Graph =
    new Graph {
      type Node = Diagram
      type Arrow = DiagramArrow[Category]

      override def nodes: Nodes = BigSet.of[Node].asInstanceOf[Nodes]

      override def arrows: Arrows = BigSet.of[Arrow].asInstanceOf[Arrows]

      def d0(f: Arrow): Node = f.d0.asInstanceOf[Diagram]

      def d1(f: Arrow): Node = f.d1.asInstanceOf[Diagram]
    }
}
