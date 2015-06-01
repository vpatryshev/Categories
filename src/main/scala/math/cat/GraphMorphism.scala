package math.cat




import scala.collection.Set

/**
 * Morphism for graphs.
 */
class GraphMorphism [XNodes, XArrows, GX <: Graph[XNodes, XArrows], YNodes, YArrows, GY <: Graph[YNodes, YArrows]] (
    val tag: String,
    val d0: GX,
    val d1: GY,
    val nodesMorphism: XNodes => YNodes,
    val arrowsMorphism: XArrows => YArrows
) extends Morphism[GX, GY] {

  for (arrowX <- d0.arrows) {
     val xNode0 = d0.d0(arrowX)
     val xNode1 = d0.d1(arrowX)
     val arrowY = arrowsMorphism(arrowX)
  }

  /**
   * Two graph morphisms are equal if they have equal d0s and cod0s and both morphisms for nodes and arrows
   * are equal respectively.
   *
   * @param other morphism to compare
   * @return true iff they are equal
   */
  def equals(other: this.type) =
    d0 == other.d0 &&
    d1 == other.d1 &&
    nodesMorphism == other.nodesMorphism &&
    arrowsMorphism == other.arrowsMorphism

    override def toString: String = "(" + nodesMorphism.toString + "," + arrowsMorphism.toString + ")"

  def compose[ZNodes, ZArrows, GZ <: Graph[ZNodes, ZArrows]]
      (g: GraphMorphism[YNodes, YArrows, GY, ZNodes, ZArrows, GZ]) = {
    require(d1 == g.d0, "Composition not defined")
    val nm = (x: XNodes) => g.nodesMorphism(this.nodesMorphism(x))
    val am = g.arrowsMorphism compose this.arrowsMorphism 
    new GraphMorphism[XNodes, XArrows, GX, ZNodes, ZArrows, GZ](this.tag +" o " + g.tag, d0, g.d1, nm, am)
  }
}

object GraphMorphism {
  def apply[XNodes, XArrows, YNodes, YArrows] (
    d0: Graph[XNodes, XArrows],
    d1: Graph[YNodes, YArrows],
    f0: XNodes => YNodes,
    f1: XArrows => YArrows):
      GraphMorphism[XNodes, XArrows, Graph[XNodes, XArrows], YNodes, YArrows, Graph[YNodes, YArrows]] =
      apply(d0, d1, SetMorphism(d0.nodes, d1.nodes, f0), SetMorphism(d0.arrows, d1.arrows, f1))

  def unit[XNodes, XArrows] = new {
    def apply(d0: Graph[XNodes, XArrows]) =
      new GraphMorphism[XNodes, XArrows, Graph[XNodes, XArrows], XNodes, XArrows, Graph[XNodes, XArrows]]("1", d0, d0, n => n, a => a)
  }
}