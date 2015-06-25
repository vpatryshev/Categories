package j.math.cat;

import java.util.*;
import static j.math.cat.Base.*;
import static j.math.cat.Sets.*;
/**
 * Sample Implementation of (oriented multi-) graph.
 * All code is <a href="http://myjavatools.com/projects/Category/source.zip">here</a>
 */
public abstract class Graph<N, A> extends AbstractSet<N> {
  private Set<N> nodes;

  protected interface Quiver<Nq, Aq> {
    public Set<Aq> arrows();
    public Nq d0(Aq arrow);
    public Nq d1(Aq arrow);
  }

  Quiver<N, A> quiver;

  /*
   * Java technicalities: have to override these methods.
   */
  @Override public Iterator<N> iterator() {  return nodes.iterator(); }
  @Override public int size() { return nodes.size(); }
  @Override public int hashCode() { return nodes.hashCode() * 2 + arrows().hashCode(); }

  @SuppressWarnings({"unchecked"})
  @Override public boolean equals(Object o) { return o instanceof Graph && equals((Graph) o); }

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
  public Set<A> arrows() { return quiver.arrows(); }

  /**
   * Maps an arrow to its domain, aka source.
   * @param arrow an arrow
   * @return the arrow's domain (source)
   */
  public N d0(A arrow) { return quiver.d0(arrow); }
  /**
   * Maps an arrow to its codomain, aka target.
   * @param arrow an arrow
   * @return the arrow's codomain (target)
   */
  public N d1(A arrow) { return quiver.d1(arrow); }

  public Set<A> arrows(final N from, final N to) {
    return new Predicate<A>() {

      @Override
      public boolean eval(A a) {
        return d0(a) == from && d1(a) == to;
      }
    }.filter(arrows());

  }

  protected Graph() {
    this.nodes = Collections.emptySet();
    validate();
  }

  /**
   * Either discrete or abstract graph constructor. Takes nodes; arrows are defined in subclasses.
   *
   * @param nodes graph nodes.
   */
  protected Graph(Set<N> nodes) {
    this.nodes = nodes;
    validate();
  }

  /**
   * An abstract graph constructor. Takes nodes; arrows are defined in subclasses.
   *
   * @param nodes graph nodes.
   */
  protected Graph(Set<N> nodes, Quiver<N, A> quiver) {
    this.nodes = nodes;
    this.quiver = quiver;
    validate();
  }

  /**
   * Validates the graph: checks that for each arrow its domain and codomain is in the graph.
   */
  public void validate() {
    if (isEnumerable(arrows())) {
      for (A arrow : arrows()) {
        require(nodes.contains(d0(arrow)), "Domain for " + arrow + " not defined");
        require(nodes.contains(d1(arrow)), "Codomain for " + arrow + " not defined");
      }
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
    if (nodes instanceof BigSet) return "(" + ((BigSet) nodes).whoami() + "...)";

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

  protected static class ArrowMap<N, A> implements Quiver<N, A> {
    private final Map<A, Pair<N, N>> arrows;
    private final Set<N> nodes;

    ArrowMap(Set<N> nodes, Map<A, Pair<N, N>> arrows) {
      this.nodes = nodes;
      this.arrows = arrows;
    }

    private Pair<N,N> d0d1(A arrow) {
      Pair<N,N>fromTo = arrows.get(arrow);
      require(fromTo != null, "Expected in arrows " + arrows.keySet() + ": <<" + arrow + ">>");
      require(nodes.contains(fromTo.x()), "Expected " + fromTo.x() + "=d0(" + arrow + ") to be a member of " + nodes);
      require(nodes.contains(fromTo.y()), "Expected " + fromTo.y() + "=d1(" + arrow + ") to be a member of " + nodes);
      return fromTo;
    }

    public Set<A> arrows() { return arrows.keySet(); }

    public N d0(A arrow)   { return d0d1(arrow).x(); }
    public N d1(A arrow)   { return d0d1(arrow).y(); }
  }

  public static class WithArrows<N, A> extends Graph<N, A> {

    WithArrows(Set<N> nodes, Map<A, Pair<N, N>> arrows) {
      super(nodes, new ArrowMap<N, A>(nodes, arrows));
    }
  }

  /**
   * Builds a graph from given nodes and arrows.
   *
   * @param nodes graph nodes
   * @param arrows graph arrows, represented here as mapping arrow tags to (domain,codomain) pairs.
   * @return a new graph
   */
  public static <N, A> Graph<N, A> Graph(final Set<N> nodes, final Map<A, Pair<N, N>> arrows) {
    return new WithArrows<N, A>(nodes, arrows);
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
    require(d0.keySet().equals(d1.keySet()), "d0 and d1 are defined on different sets of arrows, which is weird");

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
      arrows.add(Pair.of(x, y));
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
    require(string.length() > splitAt + 4, "Bad input string <<" + string + ">>, splitAt = " + splitAt);
    int curlyAt = string.indexOf('}', splitAt + 4);
    for (String arrow : string.substring(splitAt + 4, curlyAt).split(",\\s*")) {
      String[] arrowAndNodes = arrow.split(":\\s*");
      if (arrowAndNodes.length == 2) {
        String arrowText = arrowAndNodes[1];
        String[] fromTo = arrowText.split("\\s*->\\s*");
        require(fromTo.length == 2, "Bad arrow descr: " + arrowText + " - from " + string);
        arrows.put(arrowAndNodes[0], Pair.from(fromTo));
      }
    }
    return Graph(Sets.parseSet(string.substring(1, splitAt + 1)), arrows);
  }


  public static void main(String[] args) {
    Set<Integer> nodes = Sets.Set(0, 1, 2);
    @SuppressWarnings({"rawtypes","unchecked"})
    Map<String, Pair<Integer, Integer>> arrows =
            Base.Map(Base.array("0.id", "0.1", "0.2", "1.id", "a", "b", "2.1", "2.id", "2.a", "2.b", "2.swap"),
                    Base.array(Pair.of(0, 0), Pair.of(0, 1), Pair.of(0, 2), Pair.of(1, 1), Pair.of(1, 2), Pair.of(1, 2), Pair.of(2, 1), Pair.of(2, 2), Pair.of(2, 2), Pair.of(2, 2), Pair.of(2, 2))
            );
    Graph<Integer,String> three = Graph(nodes, arrows);
    System.out.println(three);
  }

}