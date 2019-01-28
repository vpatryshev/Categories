package math.cat


/**
  * Morphism for graphs.
  */
trait GraphMorphism[X <: Graph[_, _], Y <: Graph[_, _]]
  extends Morphism[X, Y] { m =>
  val tag: String
  val d0: X
  val d1: Y
  type XNode = d0.Node
  type XNodes = d0.Nodes
  type XArrow = d0.Arrow
  type XArrows = d0.Arrows
  type YNode = d1.Node
  type YNodes = d1.Nodes
  type YArrow = d1.Arrow
  type YArrows = d1.Arrows
  
  def nodesMapping(n: XNode): YNode

  def arrowsMapping(a: XArrow): YArrow

  /**
    * Two graph morphisms are equal if they have equal d0s and cod0s and both morphisms for nodes and arrows
    * are equal respectively.
    * 
    * The problem here is that we are encroaching the "material set theory".
    * Normally, equality is not defined for functors, so the same should hold for graph morphisms.
    * But since we are halfway into an internal category theory, that includes an internal graph theory.
    * So let's keep it equational for a while - but with a caveat, the implementation is necessarily dirty.
    *
    * @param gm morphism to compare
    * @return true iff they are equal
    */
  override def equals(gm: Any): Boolean = {

    gm match {
      case other: GraphMorphism[X, Y] =>
        d0 == other.d0 &&
        d1 == other.d1 && {
          def sameNodesMapping(x: XNode): Boolean = {
            nodesMapping(x) == other.nodesMapping(x.asInstanceOf[other.XNode])
          }
          val sameNodes: Boolean = d0.nodes forall sameNodesMapping

          def sameArrowssMapping(a: XArrow): Boolean = {
            arrowsMapping(a) == other.arrowsMapping(a.asInstanceOf[other.XArrow])
          }
          val sameArrows: Boolean = d0.arrows forall sameArrowssMapping
          
          sameNodes && sameArrows
        }
      case otherwise => false
    }
  }

  override def hashCode: Int = d0.hashCode | d1.hashCode * 2

  //  override def toString: String = s"($nodesMapping, $arrowsMapping)"

  def compose[Z <: Graph[_, _]]
  (g: GraphMorphism[Y, Z]):
  GraphMorphism[X, Z] = {
    require(this.d1 == g.d0, "Composition not defined")
    def nm(x: XNode): g.YNode = g.nodesMapping(this.nodesMapping(x).asInstanceOf[g.XNode]) // casting is redundant, intellij says
    def am(a: XArrow): g.YArrow = g.arrowsMapping(this.arrowsMapping(a).asInstanceOf[g.XArrow])
    
    GraphMorphism[X, Z](
      m.tag + " o " + g.tag,
      m.d0, g.d1)(nm, am)
  }
}

object GraphMorphism {
  def apply[X <: Graph[_, _], Y <: Graph[_, _]](
    taggedAs: String,
    domain: X,
    codomain: Y)(
    f0: domain.Node => codomain.Node,
    f1: domain.Arrow => codomain.Arrow):
  GraphMorphism[X, Y] = new GraphMorphism[X, Y] {
    val tag: String = taggedAs
    val d0: X = domain
    val d1: Y = codomain

    def nodesMapping(n: domain.Node): codomain.Node = f0(n)

    def arrowsMapping(a: domain.Arrow): codomain.Arrow = f1(a)
  }

//  def apply[X <: Graph[_, _], Y <: Graph[_, _]]
//    (d0: X, d1: Y)
//    (f0: XNode => YNode,
//     f1: XArrow => YArrow): GraphMorphism[X, Y] =
//    apply("_", d0, d1)(f0, f1)

  def id[G <: Graph[_, _]](graph: G): GraphMorphism[G, G] =
    new GraphMorphism[G, G] {
      val tag = "id"
      val d0: G = graph
      val d1: G = graph

      def nodesMapping(n: XNode): YNode = n.asInstanceOf[YNode] // d1==d0

      def arrowsMapping(a: XArrow): YArrow = a.asInstanceOf[YArrow] // d1==d0
    }
}