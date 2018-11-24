package math.cat


/**
  * Morphism for graphs.
  */
class GraphMorphism[XNodes, XArrows, YNodes, YArrows](
   val tag: String,
   val d0: Graph[XNodes, XArrows],
   val d1: Graph[YNodes, YArrows],
   val nodesMapping: XNodes => YNodes,
   val arrowsMapping: XArrows => YArrows
) extends Morphism[Graph[XNodes, XArrows], Graph[YNodes, YArrows]] {

  /**
    * Two graph morphisms are equal if they have equal d0s and cod0s and both morphisms for nodes and arrows
    * are equal respectively.
    *
    * @param x morphism to compare
    * @return true iff they are equal
    */
  override def equals(x: Any): Boolean = x match {
    case other: GraphMorphism[XNodes, XArrows, YNodes, YArrows] =>
      d0 == other.d0 &&
      d1 == other.d1 &&
      d0.nodes.forall(x => nodesMapping(x) == other.nodesMapping(x)) &&
      d0.arrows.forall(x => arrowsMapping(x) == other.arrowsMapping(x))
    case otherwise => false
  }

  override def hashCode: Int = d0.hashCode | d1.hashCode*2
  
  override def toString: String = s"($nodesMapping, $arrowsMapping)"

  def compose[ZNodes, ZArrows]
  (g: GraphMorphism[YNodes, YArrows, ZNodes, ZArrows]):
      GraphMorphism[XNodes, XArrows, ZNodes, ZArrows] = {
    require(d1 == g.d0, "Composition not defined")
    val nm = (x: XNodes) => g.nodesMapping(this.nodesMapping(x))
    val am = g.arrowsMapping compose this.arrowsMapping
    new GraphMorphism[XNodes, XArrows, ZNodes, ZArrows](this.tag + " o " + g.tag, d0, g.d1, nm, am)
  }
}

object GraphMorphism {
  def apply[XNodes, XArrows, YNodes, YArrows](
      d0: Graph[XNodes, XArrows],
      d1: Graph[YNodes, YArrows],
      f0: XNodes => YNodes,
      f1: XArrows => YArrows):
  GraphMorphism[XNodes, XArrows, YNodes, YArrows] =
    apply(d0, d1, SetMorphism(d0.nodes, d1.nodes, f0), SetMorphism(d0.arrows, d1.arrows, f1))

  def id[XNodes, XArrows](d0: Graph[XNodes, XArrows]) =
    new GraphMorphism[
      XNodes, XArrows,
      XNodes, XArrows](
      "id", d0, d0, identity, identity)
}