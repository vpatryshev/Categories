package java.math.cat;

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
  public Set<N> nodes() { return nodes;
    //Collections.unmodifiableSet(nodes);
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
  public void validate() {
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
}