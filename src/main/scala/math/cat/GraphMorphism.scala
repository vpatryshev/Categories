package math.cat


/**
  * Morphism for graphs.
  */
trait GraphMorphism[X <: Graph[_, _], Y <: Graph[_, _]]
  extends Morphism[X, Y] {
  m =>
  type XNode = X#Node
  type XArrow = X#Arrow
  type YNode = Y#Node
  type YArrow = Y#Arrow
  val tag: String
  val d0: X
  val d1: Y
  val d0Nodes: X#Nodes = d0.nodes: X#Nodes

  def nodesMapping(n: X#Node): Y#Node

  def arrowsMapping(a: X#Arrow): Y#Arrow

  /**
    * Two graph morphisms are equal if they have equal d0s and cod0s and both morphisms for nodes and arrows
    * are equal respectively.
    *
    * @param x morphism to compare
    * @return true iff they are equal
    */
  override def equals(x: Any): Boolean = x match {
    case other: GraphMorphism[X, Y] =>
      d0 == other.d0 &&
        d1 == other.d1 && {
        //        def nm(n: X#Nodes): Y#Nodes = nodesMapping(n)
        def sameNodesMapping(x: XNode): Boolean = nodesMapping(x) == other.nodesMapping(x)
        //        d0.forAllNodes((x: X#Nodes) => sameNodesMapping(x)) &&
        d0.arrows.forall((a: XArrow) => arrowsMapping(a) == other.arrowsMapping(a))
      }
    case otherwise => false
  }

  override def hashCode: Int = d0.hashCode | d1.hashCode * 2

  //  override def toString: String = s"($nodesMapping, $arrowsMapping)"

  def compose[Z <: Graph[_, _]]
  (g: GraphMorphism[Y, Z]):
  GraphMorphism[X, Z] = {
    require(this.d1 == g.d0, "Composition not defined")
    val nm = (x: XNode) => g.nodesMapping(this.nodesMapping(x))
    val gamapping: Y#Arrow => Z#Arrow = g.arrowsMapping
    val am = gamapping.compose[X#Arrow](this.arrowsMapping)
    GraphMorphism[X, Z](
      m.tag + " o " + g.tag,
      m.d0, g.d1,
      nm, am
    )
  }
}

object GraphMorphism {
  def apply[X <: Graph[_, _], Y <: Graph[_, _]](
    taggedAs: String,
    domain: X,
    codomain: Y,
    f0: X#Node => Y#Node,
    f1: X#Arrow => Y#Arrow):
  GraphMorphism[X, Y] = new GraphMorphism[X, Y] {
    val tag: String = taggedAs
    val d0: X = domain
    val d1: Y = codomain

    def nodesMapping(n: X#Node): Y#Node = f0(n)

    def arrowsMapping(a: X#Arrow): Y#Arrow = f1(a)
  }

  def apply[X <: Graph[_, _], Y <: Graph[_, _]](
    d0: X,
    d1: Y,
    f0: X#Node => Y#Node,
    f1: X#Arrow => Y#Arrow):
  GraphMorphism[X, Y] =
    apply("_", d0, d1, f0, f1)

  def id[G <: Graph[_, _]](graph: G) =
    new GraphMorphism[G, G] {
      val tag = "id"
      val d0: G = graph
      val d1: G = graph

      def nodesMapping(n: G#Node): G#Node = n

      def arrowsMapping(a: G#Arrow): G#Arrow = a
    }
}