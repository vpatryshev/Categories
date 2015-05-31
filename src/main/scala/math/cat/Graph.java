package math.cat;

import math.cat.Pair;
import static math.cat.Pair.*;
import static math.cat.Base.*;

import java.util.*;

/**
 * Sample Implementation of (oriented multi-) graph.
 * All code is <a href="http://myjavatools.com/projects/Category/source.zip">here</a>
 */
public abstract class Graph<N, A> extends AbstractSet<N> {
  private Set<N> nodes;

  /*
   * Java technicalities: have to override these methods.
   */
  @Override public Iterator<N> iterator() {  return nodes.iterator(); }
  @Override public int size() { return nodes.size(); }
  @Override public int hashCode() { return nodes.hashCode() * 2 + arrows().hashCode(); }
  @Override public boolean equals(Object o) { return o instanceof Graph && equals((Graph)o); }

  /**
   * Lists all nodes of a graph.
   * @return a set of nodes
   */
  public Set<N> nodes() {
    return Collections.unmodifiableSet(nodes); 
  }

  /**
   * Lists all arrows of a graph.
   * @return a set of arrows
   */
  public abstract Set<A> arrows();

  /**
   * Maps an arrow to its domain, aka source.
   * @param arrow an arrow
   * @return the arrow's domain (source)
   */
  public abstract N d0(A arrow);
  /**
   * Maps an arrow to its codomain, aka target.
   * @param arrow an arrow
   * @return the arrow's codomain (target)
   */
  public abstract N d1(A arrow);

  /**
   * An abstract graph constructor. Takes nodes; arrows are defined in subclasses.
   *
   * @param nodes graph nodes.
   */
  protected Graph(Set<N> nodes) {
    this.nodes = nodes;
    validate();
  }

  /**
   * Validates the graph: checks that for each arrow its domain and codomain is in the graph.
   */
  private void validate() {
    for (A arrow : arrows()) {
      assert nodes.contains(d0(arrow)) : "Domain for " + arrow + " not defined";
      assert nodes.contains(d1(arrow)) : "Codomain for " + arrow + " not defined";
    }
  }

  /**
   * Checks equality of this graph to that one.
   * They are equal if they have the same sets of nodes and arrows, and the arrows
   * originate and end at the same nodes.
   *
   * @param that another graph
   * @return true if they are equal.
   */
  private boolean equals(Graph<N, A> that) {
    boolean isEqual =
        this.nodes().equals(that.nodes()) && // same nodes?
        this.arrows().equals(that.arrows()); // same arrows?

    for (A arrow : arrows()) {
      isEqual = isEqual &&
          this.d0(arrow).equals(that.d0(arrow)) && // same d0?
          this.d1(arrow).equals(that.d1(arrow));   // same d1?
    }
    return isEqual;
  }

  public String toString() {
    StringBuffer out = new StringBuffer();

    for (A arrow : arrows()) {
      if (out.length() > 0) out.append(", ");
      out.append(arrow).append(": ").append(d0(arrow)).append("->").append(d1(arrow));
    }
    return "(" + super.toString() + ", {" + out + "})";
  }

  /**
   * Builds a new graph out of this one, with the inverted arrows.
   * @return a new graph with the the same nodes and with arrows pointing in the opposite directions.
   */
  public Graph<N, A> op() {
    final Graph<N, A> source = this;
    return new Graph<N, A>(nodes) {
      public Set<A> arrows() { return source.arrows(); }
      public N d0(A arrow) { return source.d1(arrow); }
      public N d1(A arrow) { return source.d0(arrow); }
    };
  }

  /**
   * Builds a graph from given nodes and arrows.
   *
   * @param nodes graph nodes
   * @param arrows graph arrows, represented here as mapping arrow tags to (domain,codomain) pairs.
   * @return a new graph
   */
  public static <N, A> Graph<N, A> Graph(Set<N> nodes, final Map<A, Pair<N, N>> arrows) {
    return new Graph<N, A>(nodes) {
      public N d0(A arrow) { return arrows.get(arrow).x(); }
      public N d1(A arrow) { return arrows.get(arrow).y(); }
      public Set<A> arrows() { return arrows.keySet(); }
    };
  }

  /**
   * Builds a graph from given nodes and arrows.
   *
   * @param nodes graph nodes
   * @param d0 maps arrows to their domains
   * @param d1 maps arrows to their codomains
   * @return a new graph
   */
  public static <N, A> Graph<N, A> Graph(Set<N> nodes, final Map<A, N> d0, final Map<A, N> d1) {
    assert d0.keySet().equals(d1.keySet());

    return new Graph<N, A>(nodes) {
      public N d0(A arrow) { return d0.get(arrow); }
      public N d1(A arrow) { return d1.get(arrow); }
      public Set<A> arrows() { return d0.keySet(); }
    };
  }

  /**
   * Builds a graph out of a poset. Arrows are pairs (x,y) where x <= y.
   *
   * @param poset original poset
   * @return graph based on he poset
   */
  public static <N> Graph<N, Pair<N, N>> Graph(PoSet<N> poset) {
    final Set<Pair<N, N>> arrows = new HashSet<Pair<N, N>>();
    for (N x : poset) for (N y : poset) if (poset._le_(x, y)) {
      arrows.add(Pair(x, y));
    }
    return new Graph<N, Pair<N, N>>(poset) {
      public N d0(Pair<N, N> arrow) { return arrow.x(); }
      public N d1(Pair<N, N> arrow) { return arrow.y(); }
      public Set<Pair<N, N>> arrows() { return arrows; }
    };
  }

  public static Graph<String, String> Graph(String string) {
    int splitAt = string.indexOf("], {");
    Map<String, Pair<String, String>> arrows = new HashMap<String, Pair<String, String>>();
    for (String arrow : string.substring(splitAt + 4, string.length() - 2).split(",\\s*")) {
      String[] arrowAndNodes = arrow.split(":\\s*");
      if (arrowAndNodes.length == 2) {
        arrows.put(arrowAndNodes[0], Pair(arrowAndNodes[1].split("\\s*->\\s*")));
      }
    }
    return Graph(parseSet(string.substring(1, splitAt + 1)), arrows);
  }

  public static void main(String[] args) {
    Set<Integer> nodes = Set(0, 1, 2);
    Map<String, Pair<Integer, Integer>> arrows =
        Map(array(    "0.id",    "0.1",     "0.2",     "1.id",      "a",       "b",     "2.1",    "2.id",     "2.a",     "2.b",     "2.swap"),
            array(Pair(0,0), Pair(0,1), Pair(0,2), Pair(1,1), Pair(1,2), Pair(1,2), Pair(2,1), Pair(2,2), Pair(2,2), Pair(2,2), Pair(2,2))
        );
    Graph<Integer,String> three = Graph(nodes, arrows);
    System.out.println(three);
  }
}
