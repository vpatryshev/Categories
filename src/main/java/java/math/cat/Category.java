package java.math.cat;

import java.util.*;
import static java.math.cat.Base.*;
import static java.math.cat.Sets.*;
import static java.math.cat.Functions.*;

/**
 * Sample Implementation of category.
 *
 * @author Vlad Patryshev
 * All source code is stored on <a href="http://code.google.com/p/categories/">http://code.google.com/p/categories/</a>
 *
 * @param <O> object type
 * @param <A> arrow type
 */
public abstract class Category<O, A> extends Graph<O, A> {

  /**
   * @return the set of objects for this category: same as nodes().
   */
  public Set<O> objects() {
    return nodes();
  }

  /**
   * @param x an object
   * @return unit arrow for object x.
   */
  public abstract A unit(O x);

  /**
   * @param f first arrow
   * @param g second arrow
   * @return composition of arrows f and g (f followed by g)
   */
  public abstract A m(A f, A g);

  /**
   * A function that produces unit arrows for objects
   */
  public final Function<O, A> UNIT = new Function<O, A>() {
    @Override
    public A apply(O x) {
      return unit(x);
    }
  };

  /**
   * A function that produces domains for objects
   */
  public final Function<A, O> D0 = new Function<A, O>() {
    @Override
    public O apply(A f) {
      return d0(f);
    }
  };

  /**
   * A function that produces codomains for objects
   */
  public final Function<A, O> D1 = new Function<A, O>() {
    @Override
    public O apply(A f) {
      return d1(f);
    }
  };

  /*
   * Java technicalities: have to override these methods.
   */
  @SuppressWarnings(value = "unchecked")
  @Override
  public boolean equals(Object o) {
    return o instanceof Category && equals((Category) o);
  }

  /**
   * Interpretation of entities equality in categories. Currently, let them use equals().
   *
   * @param <T> entity type
   * @param t1 first entity
   * @param t2 second entity
   * @return true if they are equal
   */
  public static <T> boolean equal(T t1, T t2) {
    return t1 == t2 || t1 != null && t1.equals(t2);
  }

  /**
   * Builds a category with given set of objects
   * @param objects the objects
   */
  protected Category(Set<O> objects) {
    super(objects);
    validate();
  }

  /**
   * Checks whether two arrows can be composed. Which happens iff the codomain of the first
   * arrow equals the domain of the second one.
   *
   * @param f first arrow
   * @param g second arrow
   * @return true if they can be composed
   */
  public boolean canCompose(A f, A g) {
    return equal(d1(f), d0(g));
  }

  /**
   * Checks if an arrow is an endomorphism.
   *
   * @param f the arrow to check
   * @return true iff f is endomorphism (i.e. d0(f) == d1(f)).
   */
  public boolean isEndomorphism(A f) {
    return equal(d0(f), d1(f));
  }

  /**
   * Checks whether two arrows have the same domain.
   *
   * @param f first arrow
   * @param g second arrow
   * @return true if they have the same domain
   */
  public boolean sameDomain(A f, A g) {
    return equal(d0(f), d0(g));
  }

  /**
   * Checks whether two arrrows have the same codomain.
   *
   * @param f first arrow
   * @param g second arrow
   * @return true if they have the same codomain
   */
  public boolean sameCodomain(A f, A g) {
    return equal(d1(f), d1(g));
  }

  /**
   * A predicate that checks if a pair of arrows is parallel
   */
  public final BinaryRelationship<A, A> isParallelPair = new BinaryRelationship<A, A>() {
    @Override
    public boolean eval(Pair<A, A> p) {
      return sameDomain(p.x(), p.y()) && sameCodomain(p.x(), p.y());
    }
  };

  /**
   * Validates this category, checking all the axioms.
   */
  @Override
  public void validate() {
    super.validate();
    for (O x : objects()) {
      A unit = unit(x);
      assert arrows().contains(unit) : "Unit for " + x + " not defined";
      assert equal(d0(unit), x) : "Domain of unit " + unit + " should be " + x;
      assert equal(d1(unit), x) : "Codomain of unit " + unit + " should be " + x;
    }

    for (A f : arrows()) {
      A f0 = m(unit(d0(f)), f);
      assert equal(f, f0) : "Left unit law broken for " + unit(d0(f)) + " and " + f + ": got " + f0;
      A f1 = m(f, unit(d1(f)));
      assert equal(f, f1) : "Right unit law broken for " + unit(d1(f)) + " and " + f + ": got " + f1;
    }

    for (A f : arrows()) {
      for (A g : arrows()) {
        if (canCompose(f, g)) {
          A gf = m(f, g);
          assert gf != null : "Composition of " + f + " and " + g + " not defined";
          assert sameDomain(gf, f) : "Wrong composition " + gf + " of " + f + " and " + g + ": its d0 is " + d0(gf) + ", must be " + d0(f);
          assert sameCodomain(gf, g) : "Wrong composition " + gf + " of " + f + " and " + g + ": its d1 is " + d1(gf) + ", must be " + d1(g);
        }
      }
    }

    for (A f : arrows()) {
      for (A g : arrows()) {
        if (canCompose(f, g)) {
          A gf = m(f, g);
          for (A h : arrows()) {
            if (canCompose(g, h)) {
              assert equal(m(gf, h), m(f, m(g, h))) : "Associativity broken for " + f + ", " + g + " and " + h;
            }
          }
        }
      }
    }
  }

  private boolean equals(Category<O, A> other) {
    boolean isEqual = // two categories are equal if:
            objects().equals(other.objects()) && // they have the same objects
                    arrows().equals(other.arrows()); // and they have the same arrows

    for (O x : objects()) {
      isEqual = isEqual && equal(unit(x), other.unit(x)); // objects have the same unit arrows
    }

    for (A f : arrows()) {
      isEqual = isEqual &&
              equal(d0(f), other.d0(f)) && // and arrows have the same domains
              equal(d1(f), other.d1(f));   // and the same codomains
      for (A g : arrows()) {
        if (canCompose(f, g)) {
          isEqual = isEqual && equal(m(f, g), other.m(f, g)); // and arrow composition is the same
        }
      }
    }
    return isEqual;
  }

  @Override
  public String toString() {
    StringBuffer out = new StringBuffer();

    for (A f : arrows()) {
      for (A g : arrows()) {
        if (canCompose(f, g)) {
          if (out.length() > 0) {
            out.append(", ");
          }
          out.append(g).append(" o ").append(f).append(" = ").append(m(f, g));
        }
      }
    }
    return "(" + super.toString() + ", {" + out + "})";
  }

  // sets of arrows between two objects cached here; in classics it is called hom
  private final Map<Pair<O, O>, Set<A>> hom = new HashMap<Pair<O, O>, Set<A>>();

  /**
   * Returned a cashed set of arrows from x to y.
   *
   * @param from first object
   * @param to   second object
   * @return the set of all arrows from x to y
   */
  public Set<A> arrows(O from, O to) {
    Pair<O, O> key = Pair.of(from, to);

    if (hom.containsKey(key)) {
      return hom.get(key);
    }

    Set<A> theArrows = new HashSet<A>();
    for (A arrow : arrows()) {
      if (equal(d0(arrow), from) && equal(d1(arrow), to)) {
        theArrows.add(arrow);
      }
    }
    hom.put(key, theArrows);
    return theArrows;
  }

  private Map<A, A> inverse = new HashMap<A, A>();

  /**
   * Returnes an inverse arrow. Returns null if none exists.
   *
   * @param arrow an arrow for which we are looking an inerse
   * @return inverse arrow
   */
  public A inverse(A arrow) {
    if (inverse.containsKey(arrow)) {
      return inverse.get(arrow);
    }

    for (A candidate : arrows(d1(arrow), d0(arrow))) {
      if (equal(m(arrow, candidate), unit(d0(arrow))) &&
              equal(m(candidate, arrow), unit(d1(arrow)))) {
        inverse.put(arrow, candidate);
        return candidate;
      }
    }
    inverse.put(arrow, null);
    return null;
  }

  /**
   * Checks whether an arrow is an isomorphism.
   *
   * @param arrow an arrow to check
   * @return true if arrow is an isomorphism
   */
  public boolean isIsomorphism(A arrow) {
    return inverse(arrow) != null; // there should be a better way to detect existence
  }

  private final Map<A, Boolean> monomorphisms = new HashMap<A, Boolean>();

  /**
   * Checks whether an arrow is an monomorphism.
   *
   * @param arrow an arrow to check
   * @return true if arrow is an monomorphism
   */
  public boolean isMonomorphism(A arrow) {
    if (monomorphisms.containsKey(arrow)) {
      return monomorphisms.get(arrow);
    }

    O x = d0(arrow);
    boolean result = true;
    for (A f1 : arrows()) {
      if (result && equal(d1(f1), x)) {
        for (A f2 : arrows(d0(f1), x)) {
          if (equal(m(f1, arrow), m(f2, arrow))) {
            result = result && equal(f1, f2);
          }
        }
      }
    }
    monomorphisms.put(arrow, result);
    return result;
  }

  private final Map<A, Boolean> epimorphisms = new HashMap<A, Boolean>();

  /**
   * Checks whether an arrow is an epimorphism.
   *
   * @param arrow an arrow to check
   * @return true if arrow is an epimorphism
   */
  public boolean isEpimorphism(A arrow) {
    if (epimorphisms.containsKey(arrow)) return epimorphisms.get(arrow);

    O y = d1(arrow);
    boolean result = true;
    for (A f1 : arrows()) if (result && d0(f1).equals(y)) {
      for (A f2 : arrows(y, d1(f1))) {
        if (m(arrow, f1).equals(m(arrow, f2))) result = result && f1.equals(f2);
      }
    }
    epimorphisms.put(arrow, result);
    return result;
  }

  /**
   * Builds a category given a graph, composition table, and a list of unit arrows.
   *
   * @param graph the graph on which we are to create a category
   * @param units maps objects to unit arrows
   * @param composition defines composition
   * @return a category built based on the data above
   */
  public static <O, A> Category<O, A>
  Category(final Graph<O, A> graph,
           final Map<O, A> units,
           final Map<Pair<A, A>, A> composition) {
    return new Category<O, A>(graph.nodes()) {
      public O d0(A f) { return graph.d0(f); }
      public O d1(A f) { return graph.d1(f); }
      public A unit(O x) { return units.get(x); }
      public A m(A f, A g) { return composition.get(Pair.of(f, g)); }
      public Set<A> arrows() { return graph.arrows(); }
    };
  }

  /**
   * Creates an instance of Category given a graph, when no composition is required
   *
   * @param graph the underlying graph
   * @return new category
   */
  public static <T> Category<T, T> Category(Graph<T, T> graph) {
    return Category(graph, null); // no composition specified
  }

  /**
   * Creates an instance of Category given a graph and arrow composition table
   *
   * @param graph the underlying graph
   * @param composition arrows composition table
   * @return new category
   */
  public static <T> Category<T, T>
  Category(final Graph<T, T> graph,
           final Map<Pair<T, T>, T> composition) {
    final Set<T> allArrows = new HashSet<T>(graph.nodes());
    allArrows.addAll(graph.arrows());
    return new Category<T, T>(graph.nodes()) {
      public T d0(T f) { return objects().contains(f) ? f : graph.d0(f); }
      public T d1(T f) { return objects().contains(f) ? f : graph.d1(f); }
      public T unit(T x) { return x; }
      public T m(T f, T g) {
        return objects().contains(f) ? g : objects().contains(g) ? f : composition.get(Pair.of(f, g));
      }
      public Set<T> arrows() { return Collections.unmodifiableSet(allArrows); }
    };
  }

  /**
   * Creates a new instance of of category, given objects, arrows, units, and composition table.
   *
   * @param objects set of category's objects
   * @param arrows maps arrows to their domain-codomain pairs
   * @param units maps objects to unit arrows
   * @param composition composition table
   * @return a new category
   */
  public static <O, A> Category<O, A> Category(
          final Set<O> objects,
          final Map<A, Pair<O, O>> arrows,
          final Map<O, A> units,
          final Map<Pair<A, A>, A> composition) {
    return Category(Graph(objects, arrows), units, composition);
  }

  /**
   * Creates a new instance of of category, given objects, arrows, units, and composition table.
   *
   * @param objects category's objects
   * @param d0 maps arrows to domains
   * @param d1 maps arrows to codomains
   * @param units maps objects to unit arrows
   * @param composition composition table
   * @return a new category
   */
  public static <O, A> Category<O, A> Category(
          final Set<O> objects,
          final Map<A, O> d0,
          final Map<A, O> d1,
          final Map<O, A> units,
          final Map<Pair<A, A>, A> composition) {
    return Category(Graph(objects, d0, d1), units, composition);
  }
  /**
   * This method helps fill in obvious choices for arrows composition.
   * Case 1. There's an arrow f:a->b, and an arrow g:b->c; and there's just one arrow h:a->c.
   * What would be the composition of f and g? h is the only choice.
   *
   * Case 2. h o (g o f) = k; what is (h o g) o f? It is k. and vice versa.
   *
   * @param d0 maps arrows to domains
   * @param d1 maps arrows to codomains
   * @param m composition table
   */
  private static <O, A> void
  fillCompositionTable(Map<A, O> d0, Map<A, O> d1, Map<Pair<A, A>, A> m) {
    Set<A> arrows = d0.keySet();
    // First, fill in composition table when choice is unique
    for (A f : arrows) for (A g : arrows) {
      if (d1.get(f).equals(d0.get(g))) {
        O d0f = d0.get(f);
        O d1g = d1.get(g);
        A candidate = null;
        boolean unique = true;
        for (A arrow : d0.keySet()) if (d0.get(arrow).equals(d0f) && d1.get(arrow).equals(d1g)) {
          unique = candidate == null;
          candidate = arrow;
        }

        if (unique) {
          m.put(Pair.of(f, g), candidate);
        }
      }
    }

    for (A f : arrows) for (A g : arrows) for (A h : arrows) {
      if (d1.get(f).equals(d0.get(g)) && d1.get(g).equals(d0.get(h))) {
        // Here we have three consecutive arrows;
        // we can compose them as h(gf) or as (hg)f;
        // and in case one of these compositions is not defined,
        // we define it right here, since we have enough information
        A gf = m.get(Pair.of(f, g));
        A hg = m.get(Pair.of(g, h));
        if (gf != null && hg != null) {
          A h_gf = m.get(Pair.of(gf, h));
          A hg_f = m.get(Pair.of(f, hg));
          if (hg_f == null && h_gf != null) m.put(Pair.of(f, hg), h_gf);
          if (h_gf == null && hg_f != null) m.put(Pair.of(gf, h), hg_f);
        }
      }
    }
  }

  /**
   * Builds a category given a limited (but sufficient) amount of data.
   * Objects have the same name as their units.
   *
   * @param units set of units (and objects)
   * @param d0 maps arrows to domains
   * @param d1 maps arrows tocodomain
   * @param mSource source table of arrows composition (may be incomplete)
   * @return a newly-built category
   */
  public static <A> Category<A, A>
  buildCategory(final Set<A> units,
                final Map<A, A> d0,
                final Map<A, A> d1,
                final Map<Pair<A, A>, A> mSource) {
    final Map<A, A> domain = new HashMap<A, A>(d0);
    final Map<A, A> codomain = new HashMap<A, A>(d1);
    final Map<Pair<A, A>, A> m = new HashMap<Pair<A, A>, A>(mSource);
    for(A unit : units) {
      domain.put(unit, unit); // define d0 for unit arrows
      codomain.put(unit, unit); // define d1 for unit arrows
      for (A f : domain.keySet()) { // define composition for unit arrows
        if (domain.get(f).equals(unit)) {
          m.put(Pair.of(unit, f), f);
        }
        if (codomain.get(f).equals(unit)) {
          m.put(Pair.of(f, unit), f);
        }
      }
    }

    fillCompositionTable(domain, codomain, m);

    return Category(Graph(units, domain, codomain), id(units), m);
  }

  /**
   * Builds a category based on a set of units (same as objects),
   * a map that maps pairs of objects to sets of arrows between them,
   * and a composition table.
   *
   * @param units set of units (same as objects)
   * @param arrows a map where keys are pairs of objects (domain and codomain), and
   *               values are sets of arrows between these two objects
   * @param mSource source table of arrows composition (may be incomplete)
   * @return newly-built category
   */
  public static <A> Category<A, A>
  buildCategory(final Set<A> units,
                final Map<Pair<A, A>, Set<A>> arrows,
                final Map<Pair<A, A>, A> mSource) {
    final Map<A, A> domain = new HashMap<A, A>();
    final Map<A, A> codomain = new HashMap<A, A>();

    for (Pair<A, A> x_y : arrows.keySet()) { // list all arrows from x to y
      for (A arrow : arrows.get(x_y)) {
        domain.put(arrow, x_y.x());
        codomain.put(arrow, x_y.y());
      }
    }

    return buildCategory(units, domain, codomain, mSource);
  }

  /**
   * Builds a category out of a poset. Arrows are pairs (x,y) where x <= y.
   *
   * @param poset original poset
   * @return category based on he poset
   */
  public static <O> Category<O, Pair<O, O>> Category(PoSet<O> poset) {
    final Set<Pair<O, O>> arrows = new HashSet<Pair<O, O>>();
    for (O x : poset) for (O y : poset) if (poset._le_(x, y)) {
      arrows.add(Pair.of(x, y));
    }
    return new Category<O, Pair<O, O>>(poset) {
      public O d0(Pair<O, O> arrow) {
        return arrow.x(); }
      public O d1(Pair<O, O> arrow) { return arrow.y();
      }

      public Set<Pair<O, O>> arrows() { return arrows; }
      @SuppressWarnings("unchecked")
      public Pair<O, O> unit(O x) { return Pair.of(x, x); }
      public Pair<O, O> m(Pair<O, O> f, Pair<O, O> g) { return Pair.of(f.x(), g.y()); }
    };
  }

  public static Category<String, String> Category(String string) {
    int splitAt = string.indexOf("}), {");
    assert splitAt > 0 : "Malformed string representation, missing composition table";
    Graph<String, String> graph = Graph(string.substring(1, splitAt + 2));
    Map<Pair<String, String>, String> m = new HashMap<Pair<String, String>, String>();
    for (String rule : string.substring(splitAt + 5, string.length() - 2).split(",\\s*")) {
      String[] pairAndResult = rule.split("\\s+=\\s+");
      if (pairAndResult.length == 2) {
        String[] g_f = pairAndResult[0].split("\\s+o\\s+");
        assert g_f.length == 2;
        m.put(Pair.of(g_f[1], g_f[0]), pairAndResult[1]);
      }
    }
    return Category(graph, id(graph.nodes()), m);
  }

  /**
   * Builds a category out of a segment of integers between 0 and n (not included).
   *
   * @param n number of elements
   * @return a new category
   */
  public static Category<Integer, Pair<Integer, Integer>> segment(int n) {
    return Category(PoSet.range(0, n - 1, 1));
  }

  public static final Category<Integer, Pair<Integer, Integer>> _0_ = segment(0);
  public static final Category<Integer, Pair<Integer, Integer>> _1_ = segment(1);
  public static final Category<Integer, Pair<Integer, Integer>> _2_ = segment(2);
  public static final Category<Integer, Pair<Integer, Integer>> _3_ = segment(3);
  public static final Category<Integer, Pair<Integer, Integer>> _4_ = segment(4);
  public static final Category<String, String> PARALLEL_PAIR =
          Category(Graph(Set("0", "1"), Map(array("a", "b"), array(Pair.of("0","1"), Pair.of("0","1")))), null);
  public static final Category<String, String> Z2 = Category("(([1], {1: 1 -> 1, -1: 1 -> 1}), {1 o 1 = 1, 1 o -1 = -1, -1 o 1 = -1, -1 o -1 = 1})");

  /**
   * Creates an opposite category from this one.
   * That is, all arrows are inverted.
   *
   * @return this<sup>op</sup>
   */
  public Category<O, A> op() {
    final Category<O, A> source = this;
    return new Category<O, A>(objects()) {
      public Set<A> arrows() { return source.arrows(); }
      public A unit(O x) { return source.unit(x); }
      public O d0(A f) { return source.d1(f); }
      public O d1(A f) { return source.d0(f); }
      public A m(A f, A g) { return source.m(g, f); }
    };
  }
  public static void main(String[] args) {
    Category<String,String> three = buildCategory(Set("0", "1", "2"),
            Map(array("0.1", "0.2", "a",  "b", "2.1", "2.a", "2.b",   "2.swap"), // d0
                    array("0", "0", "1", "1", "2",   "2",   "2",   "2")),
            Map(array("0.1", "0.2", "a",  "b", "2.1", "2.a", "2.b",   "2.swap"), // d1
                    array("1",   "2",   "2",  "2", "1", "2",   "2",   "2")),
            Map(array(Pair.of("0.1", "a"), Pair.of("0.1", "b"), Pair.of("2.1", "a"), Pair.of("2.1", "b"), Pair.of("a", "2.swap"), Pair.of("b", "2.swap"), Pair.of("2.swap", "2.swap")), // composition map
                    array(     "0.2",            "0.2",            "2.a",            "2.b",            "b",                 "a",                 "2"))
    );
    System.out.println(three);

    Category<String, String> vital_sol = buildCategory(Set("a", "b"),
            Map(array("green", "yellow", "red", "blue", "big", "medium", "small"),
                    array("a", "a", "a", "a", "b", "b", "b")),
            Map(array("green", "yellow", "red", "blue", "big", "medium", "small"),
                    array("b", "b", "b", "b", "a", "a", "a")),
            Map(array(Pair.of("green", "medium")), array("a")));
    System.out.println(vital_sol);
  }
}