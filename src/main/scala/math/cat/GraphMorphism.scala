package math.cat


/**
  * Morphism for graphs.
  */
trait GraphMorphism[X <: Graph, Y <: Graph]
  extends Morphism[X, Y] { m =>
  val tag: String
  val d0: X
  val d1: Y
  
  def nodesMapping(n: d0.Node): d1.Node

  def arrowsMapping(a: d0.Arrow): d1.Arrow

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
          def sameNodesMapping(x: d0.Node): Boolean = {
            nodesMapping(x) == other.nodesMapping(x.asInstanceOf[other.d0.Node])
          }
          val sameNodes: Boolean = d0.nodes forall sameNodesMapping

          def sameArrowssMapping(a: d0.Arrow): Boolean = {
            arrowsMapping(a) == other.arrowsMapping(a.asInstanceOf[other.d0.Arrow])
          }
          val sameArrows: Boolean = d0.arrows forall sameArrowssMapping
          
          sameNodes && sameArrows
        }
      case otherwise => false
    }
  }

  override def hashCode: Int = d0.hashCode | d1.hashCode * 2

  //  override def toString: String = s"($nodesMapping, $arrowsMapping)"

  def compose[Z <: Graph]
  (g: GraphMorphism[Y, Z]):
  GraphMorphism[X, Z] = {
    require(this.d1 == g.d0, "Composition not defined")
    val nm: d0.Node => g.d1.Node = x => g.nodesMapping(nodesMapping(x).asInstanceOf[g.d0.Node]) // casting is redundant, intellij says
    val am: d0.Arrow => g.d1.Arrow = a => g.arrowsMapping(arrowsMapping(a).asInstanceOf[g.d0.Arrow])
    
    GraphMorphism[X, Z](
      m.tag + " o " + g.tag,
      m.d0, g.d1)(
      nm,
      am)
  }
}

object GraphMorphism {
  def apply[X <: Graph, Y <: Graph](
    taggedAs: String,
    domain: X,
    codomain: Y)(
    f0: domain.Node => codomain.Node,
    f1: domain.Arrow => codomain.Arrow):
  GraphMorphism[X, Y] = new GraphMorphism[X, Y] {
    val tag: String = taggedAs
    val d0: X = domain
    val d1: Y = codomain

    override def nodesMapping(n: d0.Node): d1.Node =
      f0(n.asInstanceOf[domain.Node]).asInstanceOf[d1.Node]

    override def arrowsMapping(a: d0.Arrow): d1.Arrow =
      f1(a.asInstanceOf[domain.Arrow]).asInstanceOf[d1.Arrow]
  }

//  def apply[X <: Graph, Y <: Graph]
//    (d0: X, d1: Y)
//    (f0: d0.Node => d1.Node,
//     f1: d0.Arrow => d1.Arrow): GraphMorphism[X, Y] =
//    apply("_", d0, d1)(f0, f1)

  def id[G <: Graph](graph: G): GraphMorphism[G, G] =
    new GraphMorphism[G, G] {
      val tag = "id"
      val d0: G = graph
      val d1: G = graph

      def nodesMapping(n: d0.Node): d1.Node = n.asInstanceOf[d1.Node] // d1==d0

      def arrowsMapping(a: d0.Arrow): d1.Arrow = a.asInstanceOf[d1.Arrow] // d1==d0
    }
}