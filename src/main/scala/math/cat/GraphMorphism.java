package math.cat;

import static math.cat.SetMorphism.*;
import java.util.*;

/**
 * GraphMorphism class: morphisms for graphs.
 * All code is <a href="http://myjavatools.com/projects/Category/source.zip">here</a>
 */
public class GraphMorphism<
    XNodes, // type of nodes
    XArrows, // type of arrows
    XType extends Graph<XNodes, XArrows>, // type of domain graph
    YNodes, YArrows,
    YType extends Graph<YNodes, YArrows> // type of codomain graph
    > extends Morphism<XType, YType> {
  SetMorphism<XNodes, Set<XNodes>, YNodes, Set<YNodes>> nodesMorphism;
  SetMorphism<XArrows, Set<XArrows>, YArrows, Set<YArrows>> arrowsMorphism;

  /**
   * Constructor. Builds unnamed graph morphism.
   *
   * @param domain domain graph
   * @param codomain codomain graph
   * @param nodesMorphism maps nodes from the first graph to the nodes of the second graph
   * @param arrowsMorphism maps arrows from the first graph to the arrows of the second graph
   */
  public GraphMorphism(
      XType domain,
      YType codomain,
      SetMorphism<XNodes, Set<XNodes>, YNodes, Set<YNodes>> nodesMorphism,
      SetMorphism<XArrows, Set<XArrows>, YArrows, Set<YArrows>> arrowsMorphism) {
    super(domain, codomain);
    this.nodesMorphism = nodesMorphism;
    this.arrowsMorphism = arrowsMorphism;
    validate();
  }

  /**
   * Constructor. Builds a named graph morphism.
   *
   * @param name name of this morphism
   * @param domain domain graph
   * @param codomain codomain graph
   * @param nodesMorphism maps nodes from the first graph to the nodes of the second graph
   * @param arrowsMorphism maps arrows from the first graph to the arrows of the second graph
   */
  public GraphMorphism(
      String name,
      XType domain,
      YType codomain,
      SetMorphism<XNodes, Set<XNodes>, YNodes, Set<YNodes>> nodesMorphism,
      SetMorphism<XArrows, Set<XArrows>, YArrows, Set<YArrows>> arrowsMorphism) {
    super(name, domain, codomain);
    this.nodesMorphism = nodesMorphism;
    this.arrowsMorphism = arrowsMorphism;
    validate();
  }

  /**
   * Validates this graph morphism.
   * A graph morphism is valid if its nodes and arrows components are valid
   * and arrows are mapped consistently: (f:A->B) -> (F(f):F(A)->F(B)).
   */
  private void validate() {
    for (XArrows arrowX : domain().arrows()) {
      XNodes domX = domain().d0(arrowX);
      XNodes codomX = domain().d1(arrowX);
      YArrows arrowY = arrowsMorphism.apply(arrowX);

      assert nodesMorphism.apply(domX).equals(codomain().d0(arrowY)) : "Graph morphism must preserve domain";
      assert nodesMorphism.apply(codomX).equals(codomain().d1(arrowY)) : "Graph morphism must preserve codomain";
    }
  }

  public int hashCode() {
    return name() != null ? name().hashCode() :
        (nodesMorphism.hashCode() * 4/*random number*/ + arrowsMorphism.hashCode());
  }

  /**
   * Two graph morphisms are equal if their domains and codomains are equal, and their acttions on nodes are the same
   * and their actions on arrows are the same.
   *
   * @param other set morphism to compare
   * @return true if they are equal
   */
  public boolean equals(GraphMorphism<XNodes, XArrows, XType, YNodes, YArrows, YType> other) {
    return domain().equals(other.domain()) &&
           codomain().equals(other.codomain()) &&
           nodesMorphism.equals(other.nodesMorphism) &&
           arrowsMorphism.equals(other.arrowsMorphism);
  }

  public String toString() {
    return name() != null ? name() :
        "(" + nodesMorphism + ", " + arrowsMorphism + ")";
  }

  /**
   * Factory method. Builds unit morphism for a graph (identity function).
   *
   * @param g the graph
   * @return identity morphism on the given graph
   */
  public static <XNodes, XArrows>
    GraphMorphism<XNodes, XArrows, Graph<XNodes, XArrows>, XNodes, XArrows, Graph<XNodes, XArrows>> unit(Graph<XNodes, XArrows> g) {
    return new GraphMorphism<XNodes, XArrows, Graph<XNodes, XArrows>, XNodes, XArrows, Graph<XNodes, XArrows>>
        (g, g, SetMorphism.unit(g.nodes()), SetMorphism.unit(g.arrows()));
  }

  /**
   * Composes two graph morphisms
   * @param f : X -> Y - first morphism
   * @param g : Y -> Z - second morphism
   * @return g o f : X -> Z - their composition
   */
  public static <
      XNodes,  // nodes type for the first graph in the chain
      XArrows, // arrows type for the first graph in the chain
      YNodes,  // nodes type for the second graph in the chain
      YArrows, // arrows type for the second graph in the chain
      ZNodes,  // nodes type for the third graph in the chain
      ZArrows  // arrows type for the third graph in the chain
     >
  GraphMorphism<XNodes, XArrows, Graph<XNodes, XArrows>, ZNodes, ZArrows, Graph<ZNodes, ZArrows>>
  compose(
      final GraphMorphism<XNodes, XArrows, Graph<XNodes, XArrows>, YNodes, YArrows, Graph<YNodes, YArrows>> f,
      final GraphMorphism<YNodes, YArrows, Graph<YNodes, YArrows>, ZNodes, ZArrows, Graph<ZNodes, ZArrows>> g
    ) {
    assert f.codomain().equals(g.domain()): "Composition not defined";
    return new GraphMorphism<XNodes, XArrows, Graph<XNodes, XArrows>, ZNodes, ZArrows, Graph<ZNodes, ZArrows>>(
        f.domain(),
        g.codomain(),
        SetMorphism.compose(f.nodesMorphism, g.nodesMorphism),
        SetMorphism.compose(f.arrowsMorphism, g.arrowsMorphism));
  }

  /**
   * Builds a graph morphism, given two graphs and two maps, one for the set of nodes, the other for the set of arrows.
   *
   * @param domain first graph
   * @param codomain second graph
   * @param nodesMap maps nodes of the first graph to nodes of the second graph
   * @param arrowsMap maps arrows of the first graph to arrows of the second graph
   * @return a graph morphism that encapsluates all this
   */
  public static <
      // generic parameters
      XNodes, XArrows, // first graph
      YNodes, YArrows  // second graph
      >
  GraphMorphism<XNodes, XArrows, Graph<XNodes, XArrows>, YNodes, YArrows, Graph<YNodes, YArrows>> GraphMorphism(
      final Graph<XNodes, XArrows> domain,
      final Graph<YNodes, YArrows> codomain,
      final Map<XNodes, YNodes> nodesMap,
      final Map<XArrows, YArrows> arrowsMap) {

    return new GraphMorphism<XNodes, XArrows, Graph<XNodes, XArrows>, YNodes, YArrows, Graph<YNodes, YArrows>>(
        domain,
        codomain,
        Morphism(domain.nodes(), codomain.nodes(), nodesMap),
        Morphism(domain.arrows(), codomain.arrows(), arrowsMap));
  }
}
