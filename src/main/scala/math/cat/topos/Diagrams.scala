package math.cat.topos

import math.cat._
import Diagrams._
import math.sets._

class Diagrams[C <: Category[_, _]]
  extends Category[Diagram[C], DiagramArrow[C]](graphOfDiagrams[C]) {
  override def id(o: O): Arrow = NaturalTransformation.id(o).asInstanceOf[Arrow]

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

abstract class DiagramArrow[C <: Category[_, _]](override val from: Diagram[C], to: Diagram[C])
  extends NaturalTransformation[C, SetCategory](from, to)

object Diagrams {
  type Diagram[C <: Category[_, _]] = SetDiagram[C]
  def graphOfDiagrams[C <: Category[_, _]]: Graph[Diagram[C], DiagramArrow[C]] =
    new Graph[Diagram[C], DiagramArrow[C]] {
      override def nodes: Nodes = BigSet[Diagram[C]]()

      override def arrows: Arrows = BigSet[DiagramArrow[C]]()

      override def d0(f: Arrow): Node = f.d0.asInstanceOf[Node]

      override def d1(f: Arrow): Node = f.d1.asInstanceOf[Node]
    }
}
