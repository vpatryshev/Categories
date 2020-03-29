package math.cat


/**
  * Morphism for graphs.
  */
trait GraphMorphism
  extends Morphism[Graph, Graph] { m ⇒
  val tag: Any
  val d0: Graph
  val d1: Graph
  
  def nodesMapping(n: d0.Node): d1.Node

  def arrowsMapping(a: d0.Arrow): d1.Arrow

  /**
    * Good for testing
    * @param other another graph morphism
    * @param x a node
    * @return true or false
    */
  private[cat] def sameNodesMapping(other: GraphMorphism)(x: d0.Node): Boolean = try {
    other.nodesMapping(other.d0.node(x)) == nodesMapping(x)
  } catch {case _: Exception ⇒ false}

  /**
    * Good for testing
    * @param other another graph morphism
    * @return true or false
    */
  private[cat] def sameNodes(other: GraphMorphism): Boolean = {
    d0 == other.d0 && d1 == other.d1 && {
      d0.nodes forall sameNodesMapping(other)
    }
  }

  /**
    * Good for testing
    * @param other another graph
    * @param a arrow to compare
    * @return true or false
    */
  def sameArrowsMapping(other: GraphMorphism)(a: d0.Arrow): Boolean = try {
    other.arrowsMapping(other.d0.arrow(a)) == arrowsMapping(a)
  } catch { case _: Exception ⇒ false }

  /**
    * Good for testing
    * @param other another graph
    * @return true or false
    */
  private[cat] def sameArrows(other: GraphMorphism): Boolean = {
    d1 == other.d1 && {
      d0.arrows forall sameArrowsMapping(other)
    }    
  }
  
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
    this.eq(gm.asInstanceOf[AnyRef]) || (
    gm match {
      case other: GraphMorphism ⇒
        hashCode == other.hashCode && sameNodes(other) && sameArrows(other)
      case otherwise ⇒ false
    })
  }

  override def hashCode: Int = d0.hashCode | d1.hashCode * 2

  //  override def toString: String = s"($nodesMapping, $arrowsMapping)"

  def andThen(g: GraphMorphism): GraphMorphism = {
    require(this.d1 == g.d0, "Composition not defined")
    val nm: d0.Node ⇒ g.d1.Node = x ⇒ g.nodesMapping(g.d0.node(nodesMapping(x)))
    val am: d0.Arrow ⇒ g.d1.Arrow = a ⇒ g.arrowsMapping(g.d0.arrow(arrowsMapping(a)))
    
    GraphMorphism(m.tag + " ∘ " + g.tag, m.d0, g.d1)(nm, am)
  }
}

object GraphMorphism {
  def apply(
    taggedAs: String,
    domain: Graph,
    codomain: Graph)(
    f0: domain.Node ⇒ codomain.Node,
    f1: domain.Arrow ⇒ codomain.Arrow):
  GraphMorphism = new GraphMorphism {
    val tag: String = taggedAs
    val d0: Graph = domain
    val d1: Graph = codomain

    override def nodesMapping(n: d0.Node): d1.Node = d1.node(f0(domain.node(n)))

    override def arrowsMapping(a: d0.Arrow): d1.Arrow = d1.arrow(f1(domain.arrow(a)))
  }

  def id(graph: Graph): GraphMorphism =
    new GraphMorphism {
      val tag = "id"
      val d0: Graph = graph
      val d1: Graph = graph

      def nodesMapping(n: d0.Node): d1.Node = d1.node(n)
      def arrowsMapping(a: d0.Arrow): d1.Arrow = d1.arrow(a)
    }
}