package j.math.cat;

import java.util.*;
import static j.math.cat.Base.*;
import static j.math.cat.Functions.*;

/**
 * Sample Implementation of category.
 *
 *
 * All source code is stored at <a href="https://github.com/vpatryshev/Categories">https://github.com/vpatryshev/Categories</a>
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
  public final Functions.Function<O, A> UNIT = new Functions.Function<O, A>() {
    @Override
    public A apply(O x) {
      return unit(x);
    }
  };

  /**
   * A function that produces domains for objects
   */
  public final Functions.Function<A, O> D0 = new Functions.Function<A, O>() {
    @Override
    public O apply(A f) {
      return d0(f);
    }
  };

  /**
   * A function that produces codomains for objects
   */
  public final Functions.Function<A, O> D1 = new Functions.Function<A, O>() {
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

  static <T> Map<T, Pair<T, T>> buildUnits(Set<T> objects) {
    return new Function<T, Pair<T, T>>() {
      @Override public Pair<T, T> apply(T t) { return Pair.of(t, t); }
    }.toMap(objects);
  }

  /**
   * Builds a category with given set of objects
   * @param objects the objects
   */
  protected Category(Set<O> objects, Quiver<O, A> arrows) {
  super(objects, arrows);
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
  public final BinaryRelation<A, A> isParallelPair = new BinaryRelation<A, A>() {
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
      require(arrows().contains(unit), "Unit for " + x + " not defined");
      require(equal(d0(unit), x), "Domain of unit " + unit + " should be " + x);
      require(equal(d1(unit), x), "Codomain of unit " + unit + " should be " + x);
    }

    for (A f : arrows()) {
      final A u0f = unit(d0(f));
      require(u0f != null, "Missing: unit for " + d0(f) + " which is from " + f);
      final A u1f = unit(d1(f));
      require(u1f != null, "Missing: unit for " + d1(f) + " which is from " + f);
      A f0 = m(u0f, f);
      require(equal(f, f0), "Left unit law broken for " + u0f + " and " + f + ": got " + f0);
      A f1 = m(f, u1f);
      require(equal(f, f1), "Right unit law broken for " + u1f + " and " + f + ": got " + f1);
    }

    for (A f : arrows()) {
      for (A g : arrows()) {
        if (canCompose(f, g)) {
          A gf = m(f, g);
          require(gf != null, "Composition of " + f + " and " + g + " not defined");
          require(sameDomain(gf, f), "Wrong composition " + gf + " of " + f + " and " + g + ": its d0 is " + d0(gf) + ", must be " + d0(f));
          require(sameCodomain(gf, g), "Wrong composition " + gf + " of " + f + " and " + g + ": its d1 is " + d1(gf) + ", must be " + d1(g));
        }
      }
    }

    for (A f : arrows()) {
      for (A g : arrows()) {
        if (canCompose(f, g)) {
          A gf = m(f, g);
          for (A h : arrows()) {
            if (canCompose(g, h)) {
              require(equal(m(gf, h), m(f, m(g, h))), "Associativity broken for " + f + ", " + g + " and " + h);
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
    StringBuilder out = new StringBuilder();

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
  private final Map<Pair<O, O>, Set<A>> hom = new HashMap<>();

  /**
   * Returns a cashed set of arrows from x to y.
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

    Set<A> theArrows = new HashSet<>();
    for (A arrow : arrows()) {
      if (equal(d0(arrow), from) && equal(d1(arrow), to)) {
        theArrows.add(arrow);
      }
    }
    hom.put(key, theArrows);
    return theArrows;
  }

  private Map<A, A> inverse = new HashMap<>();

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

  private final Map<A, Boolean> monomorphisms = new HashMap<>();

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

  private final Map<A, Boolean> epimorphisms = new HashMap<>();

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

    final Map<A,O> unit2object = new HashMap<>();
    for (O o : units.keySet()) unit2object.put(units.get(o), o);

    final Set<A> allArrows = new HashSet<>(unit2object.keySet());
    allArrows.addAll(graph.arrows());

    class AllArrows implements Quiver<O, A> {
      public O d0(A f) { return isUnit(f) ? unit2object.get(f) : graph.d0(f); }
      public O d1(A f) { return isUnit(f) ? unit2object.get(f) : graph.d1(f); }
      public Set<A> arrows() { return allArrows; }
      boolean isUnit(A a) { return unit2object.containsKey(a); }
    }

    final AllArrows myQuiver = new AllArrows();

    return new Category<O, A>(graph.nodes(), myQuiver) {
      public A unit(O x) { return units.get(x); }
      public A m(A f, A g) { return myQuiver.isUnit(f) ? g : myQuiver.isUnit(g) ? f : composition.get(Pair.of(f, g)); }
    };
  }

  /**
   * Creates an instance of Category given a graph, when no composition is required
   *
   * @param graph the underlying graph
   * @return new category
   */
  public static <N, A0, A extends A0/*&N*/> Category<N, A> Category(Graph<N, A> graph) {
    return Category(graph, null); // no composition specified
  }

  /**
   * Creates an instance of Category given a graph and arrow composition table
   *
   * @param graph the underlying graph
   * @param composition arrows composition table
   * @return new category
   */
  public static <N, A0, A extends A0/*&N*/> Category<N, A>
  Category(final Graph<N, A> graph,
           final Map<Pair<A, A>, A> composition) {
    @SuppressWarnings({"rawtypes","unchecked"})
    final Set<A> allArrows = new HashSet(graph.nodes());
    allArrows.addAll(graph.arrows());

    final Set<N> myNodes = graph.nodes();

    class AllArrows implements Quiver<N, A> {
      @SuppressWarnings({"unchecked"})
      public N d0(Object f) { return myNodes.contains(f) ? (N)f : graph.d0((A)f); }
      @SuppressWarnings({"unchecked"})
      public N d1(Object f) { return myNodes.contains(f) ? (N)f : graph.d1((A)f); }
      public Set<A> arrows() { return allArrows; }
      boolean isUnit(Object a) { return myNodes.contains(a); }
    }

    final AllArrows myQuiver = new AllArrows();

    return new Category<N, A>(myNodes, myQuiver) {
      @SuppressWarnings({"unchecked"})
      public A unit(N x) { return (A)x; } // the problem being, Java does not have union types
      public A m(A f, A g) {
        return objects().contains(f) ? g : objects().contains(g) ? f : composition.get(Pair.of(f, g));
      }
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
    final Map<A, A> domain = new HashMap<>(d0);
    final Map<A, A> codomain = new HashMap<>(d1);
    final Map<Pair<A, A>, A> m = new HashMap<>(mSource);
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

    return Category(Graph(units, domain, codomain), Functions.id(units), m);
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
    final Map<A, A> domain = new HashMap<>();
    final Map<A, A> codomain = new HashMap<>();

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
    final Set<Pair<O, O>> arrows = new HashSet<>();
    for (O x : poset) for (O y : poset) if (poset._le_(x, y)) {
      arrows.add(Pair.of(x, y));
    }
    Quiver<O,Pair<O,O>> myQuiver = new Quiver<O, Pair<O, O>>() {
      public Set<Pair<O, O>> arrows() { return arrows; }
      public O d0(Pair<O, O> arrow) { return arrow.x(); }
      public O d1(Pair<O, O> arrow) { return arrow.y(); }
    };

    return new Category<O, Pair<O, O>>(poset.elements, myQuiver) {
      @SuppressWarnings("unchecked")
      public Pair<O, O> unit(O x) { return Pair.of(x, x); }
      public Pair<O, O> m(Pair<O, O> f, Pair<O, O> g) { return Pair.of(f.x(), g.y()); }
    };
  }

  public static Category<String, String> Category(String string) {
    int splitAt = string.indexOf("}), {");
    if (splitAt < 0) splitAt = string.length() - 2;
    String graphText = string.substring(1, splitAt + 2);
    require(graphText.length() > 0, "Missing graph repr in <<" + string + ">>");
    Graph<String, String> graph = Graph(string.substring(1, splitAt + 2));
    Map<Pair<String, String>, String> m = new HashMap<>();

    int multAt = splitAt + 5;
    int curlyAt = string.indexOf('}', multAt);

    if (splitAt < string.length() - 3 && curlyAt > 0) {
      String multMapString = string.substring(multAt, curlyAt);
//require(multMapString.isEmpty(), "Oops!!! " + multMapString);
      for (String rule : multMapString.split(",\\s*")) {
        String[] pairAndResult = rule.split("\\s+=\\s+");
        if (pairAndResult.length == 2) {
          String[] g_f = pairAndResult[0].split("\\s+o\\s+");
          require(g_f.length == 2, "Failed to split " + pairAndResult);
          m.put(Pair.of(g_f[1], g_f[0]), pairAndResult[1]);
        }
      }
    }
    return Category(graph, id(graph.nodes()), m);
  }

  /**
   * Assertion that two arrows are parallel
   * @param f first arrow
   * @param g second arrow
   */
  void assertParallelPair(A f, A g) {
    require(sameDomain(f, g), "" + f + " and " + g + " should have the same domain");
    require(sameCodomain(f, g), "" + f + " and " + g + " should have the same codomain");
  }

  /**
   * Builds a predicate that checks whether an arrow equalizes two other arrows,
   * that is, whether f o h = g o h  for a given arrow h.
   *
   * @param f first arrow
   * @param g second arrow
   * @return a predicate defined on arrows.
   */
  public Predicate<A> equalizes(final A f, final A g) {
    assertParallelPair(f, g);
    return new Predicate<A>() {
      @Override
      public boolean eval(A h) {
        return equalizes(h, f, g);
      }
    };
  }

  /**
   * Builds a set of all arrows that equalize f: A -> B and g: A -> B, that is,
   * such arrows h: X -> A that f o h = g o h.
   *
   * @param f first arrow
   * @param g second arrow
   * @return a set of arrows that equalize f and g
   */
  Set<A> allEqualizingArrows(final A f, final A g) {
    return equalizes(f, g).find(arrows());
  }

  /**
   * Builds a set of arrows coeqalizing two given arrows f,g: A -> B, that is,
   * such arrows h: B -> C that h o f = h o g.
   *
   * @param f first arrow
   * @param g second arrow
   * @return a set of coequalizing arrows
   */
  private Set<A> allCoequalizingArrows(final A f, final A g) {
    return coequalizes(f, g).find(arrows());
  }
  /**
   * Builds a predicate that checks whether an arrow h: A -> B
   * composed with g: B -> C gives f: A -> C.
   *
   * @param g first arrow
   * @param f second arrow
   * @return a predicate that checks the condition.
   */
  public Predicate<A> factorsOnRight(final A g, final A f) {
    return new Predicate<A>() {
      @Override
      public boolean eval(A h) {
        return equal(m(h, g), f);
      }
    };
  }

  /**
   * Builds a predicate that checks whether an arrow h: B -> C
   * composed with g: A -> B gives f: A -> C.
   *
   * @param g first arrow
   * @param f second arrow
   * @return a predicate that checks the condition.
   */
  public Predicate<A> factorsOnLeft(final A g, final A f) {
    return new Predicate<A>() {
      @Override
      public boolean eval(A h) {
        return equal(m(f, h), g);
      }
    };
  }

  /**
   * Builds a predicate that checks if arrow g: y -> z
   * uniquely factors on the left the arrow f: x -> z - that is,
   * there is just one h: x -> y such that f = g o h.
   *
   * @param f arrow being factored
   * @return the specified predicate
   */
  public Predicate<A> factorsUniquelyOnLeft(final A f) {
    final O y = d0(f);
    return new Predicate<A>() {
      @Override
      public boolean eval(final A g) {
        final O x = d0(g);
        return factorsOnRight(f, g).existsUnique(arrows(x, y));
      }
    };
  }
  /**
   * Builds a predicate that checks if arrow g: x -> y
   * uniquely factors on the right the arrow f: z -> z - that is,
   * there is just one h: y -> z such that f = h o g.
   *
   * @param f factored arrow
   * @return the specified predicate
   */
  public Predicate<A> factorsUniquelyOnRight(final A f) {
    final O z = d1(f);
    return new Predicate<A>() {
      @Override
      public boolean eval(final A g) {
        O y = d1(g);
        return factorsOnLeft(g, f).existsUnique(arrows(y, z));
      }
    };
  }

  /**
   * Builds a predicate that checks
   * if an arrow is an equalizer of the other two arrows.
   *
   * @param f first arrow
   * @param g second arrow
   * @return a predicate that checks if an arrow is an equalizer of f and g
   */
  public Predicate<A> isEqualizer(final A f, final A g) {
    assertParallelPair(f, g);
    return new Predicate<A>() {
      @Override
      public boolean eval(final A h) {
        return
                equalizes(h, f, g) &&
                        factorsUniquelyOnLeft(h).forall(allEqualizingArrows(f, g));
      }
    };
  }

  /**
   * Checks if arrow h equalizes arrows f and g (that is, whether f o h == g o h).
   *
   * @param h arrow that may equalize f and g
   * @param f first arrow
   * @param g second arrow
   * @return true iff f o h == g o h
   */
  public boolean equalizes(A h, A f, A g) {
    assertParallelPair(f, g);
    return canCompose(h, f) && equal(m(h, f), m(h, g));
  }

  /**
   * Builds an equalizer arrow for a parallel pair of arrows.
   *
   * @param f first arrow
   * @param g second arrow
   * @return an equalizer arrow, if one exists, null othrewise
   */
  public A equalizer(final A f, final A g) {
    assertParallelPair(f, g);
    return isEqualizer(f, g).findOne(arrows());
  }
  /**
   * Builds a predicate that checks if an arrow is a coequalizer of the other two arrows.
   *
   * @param f first arrow
   * @param g second arrow
   * @return true if it is a coequalizer
   */
  public Predicate<A> isCoequalizer(final A f, final A g) {
    assertParallelPair(f, g);
    return new Predicate<A>() {
      @Override
      public boolean eval(A h) {
        return
                coequalizes(h, f, g) &&
                        factorsUniquelyOnRight(h).forall(allCoequalizingArrows(f, g));
      }
    };
  }

  /**
   * Builds a predicate that checks whether an arrow coequalizes two other arrows,
   * that is, whether h o f = h o g  for a given arrow h.
   *
   * @param f first arrow
   * @param g second arrow
   * @return a predicate defined on arrows.
   */
  public Predicate<A> coequalizes(final A f, final A g) {
    assertParallelPair(f, g);
    return new Predicate<A>() {
      @Override
      public boolean eval(A h) {
        return coequalizes(h, f, g);
      }
    };
  }

  /**
   * Checks if arrow h coequalizes arrows f and g (that is, whether h o f == h o g).
   *
   * @param h arrow that may coequalize f and g
   * @param f first arrow
   * @param g second arrow
   * @return true iff h o f == h o g
   */
  protected boolean coequalizes(A h, A f, A g) {
    assertParallelPair(f, g);
    return canCompose(f, h) && equal(m(f, h), m(g, h));
  }

  /**
   * Builds a coequalizer arrow for a parallel pair of arrows.
   *
   * @param f first arrow
   * @param g second arrow
   * @return a coequalizer arrow, if one exists, null othrewise
   */
  public A coequalizer(final A f, final A g) {
    assertParallelPair(f, g);
    return isCoequalizer(f, g).findOne(arrows());
  }
  /**
   * Calculates a coequalizer of a collection of parallel arrows.
   * Since the collection may be empty, should provide the codomain.
   *
   * @param arrowsToEqualize the arrows, all of which shold be equalized
   * @param codomain         the arrows' common codomain
   * @return a coequalizer arrow
   */
  public A coequalizer(final Iterable<A> arrowsToEqualize, O codomain) {
    throw new UnsupportedOperationException("to be implemented later, maybe");
  }

  /**
   * Builds a predicate that checks whether an arrow h: B -> A is such that
   * px o h = qx and py o h = qy
   * for q = (qx, qy): B -> X x Y and p = (px, py): A -> X x Y.
   *
   * @param q factoring pair of arrows
   * @param p factored pair of arrows
   * @return the specified predicate.
   */
  private Predicate<A> pairFactorsOnLeft(final Pair<A, A> p, final Pair<A, A> q) {
    return new Predicate<A>() {
      @Override
      public boolean eval(A h) {
        return equal(m(p.x(), h), q.x()) && equal(m(p.y(), h), q.y());
      }
    };
  }

  /**
   * Builds a predicate that checks whether an arrow h: A -> B is such that
   * h o px = qx and py o h = qy o h for q = (qx, qy), and p = (px, py)
   * where qx: X -> B, qy: Y -> B, px: X -> A, py: Y -> A.
   *
   * @param q factoring pair of arrows
   * @param p factored pair of arrows
   * @return the predicate described above.
   */
  private Predicate<A> pairFactorsOnRight(final Pair<A, A> p, final Pair<A, A> q) {
    return new Predicate<A>() {
      @Override
      public boolean eval(A h) {
        return equal(m(p.x(), h), q.x()) && equal(m(p.y(), h), q.y());
      }
    };
  }

  /**
   * Builds a predicate that checks if a pair of arrows p = (px, py) : A -> X x Y
   * factors uniquely a pair q = (qx, qy): B -> X x Y on the right,
   * that is, if there exists a unique arrow h: B -> A such that qx = px o h and qy = py o h.
   *
   * @param p pair of arrows
   * @return true if p factors q uniquely on the right
   */
  private Predicate<Pair<A, A>> pairFactorsUniquelyOnRight(final Pair<A, A> p) {
    return new Predicate<Pair<A, A>>() {
      @Override
      public boolean eval(final Pair<A, A> q) {
        return pairFactorsOnLeft(p, q).existsUnique(arrows(d0(q.x()), d0(p.x())));
      }
    };
  }

  /**
   * Builds a predicate that checks if a pair of arrows p = (px, py), where
   * px: X -> A, py: Y -> A, factors uniquely a pair q = (qx, qy)
   * (where qx: X -> B, qy: Y -> B) on the left,
   * that is, if there exists a unique arrow h: A -> B
   * such that qx = h o px and qy = h o py.
   *
   * @param p pair of arrows
   * @return true if q factors p uniquely on the left
   */
  private Predicate<Pair<A, A>> pairFactorsUniquelyOnLeft(final Pair<A, A> p) {
    return new Predicate<Pair<A, A>>() {
      @Override
      public boolean eval(final Pair<A, A> q) {
        return pairFactorsOnRight(p, q).existsUnique(arrows(d1(p.x()), d1(q.x())));
      }
    };
  }

  /**
   * Builds a set of all pairs (px, py) of arrows that start at the same domain and end
   * at d0(f) and d0(g), equalizing them: f o px = g o py, that is, making the square
   * <pre>
   *       py
   *   U -----> Y
   *   |        |
   * px|        | g
   *   |        |
   *   v        v
   *   X -----> Z
   *       f
   * </pre>
   * commutative.
   *
   * @param f first arrow
   * @param g second arrow
   * @return the set of all such pairs of arrows
   */
  private Set<Pair<A, A>> pairsEqualizing(final A f, final A g) {
    return new Predicate<Pair<A, A>>() {
      @Override
      public boolean eval(Pair<A, A> p) {
        return
                sameDomain(p.x(), p.y()) &&
                        canCompose(p.x(), f) &&
                        canCompose(p.y(), g) &&
                        equal(m(p.x(), f), m(p.y(), g));
      }
    }.find(Base.setProduct(arrows(), arrows()));
  }

  /**
   * Builds a set of all pairs (qx, qy) of arrows that end at the same codomain and start
   * at d1(f) and d1(g), coequalizing them: m(f, px = m(py, g), making the square
   * <pre>
   *       g
   *   Z -----> Y
   *   |        |
   *  f|        | qy
   *   |        |
   *   v        v
   *   X -----> U
   *       qx
   * </pre>
   * commutative.
   *
   * @param f first arrow
   * @param g second arrow
   * @return the set of all such pairs of arrows
   */
  private Set<Pair<A, A>> pairsCoequalizing(final A f, final A g) {
    return new Predicate<Pair<A, A>>() {
      @Override
      public boolean eval(Pair<A, A> p) {
        return
                sameCodomain(p.x(), p.y()) &&
                        canCompose(f, p.x()) &&
                        canCompose(g, p.y()) &&
                        equal(m(f, p.x()), m(g, p.y()));
      }
    }.find(Base.setProduct(arrows(), arrows()));
  }

  /**
   * Builds a set of all arrows to x and y (respectively) that start at the same object.
   *
   * @param x first object
   * @param y second object
   * @return a set of pairs of arrows with the same domain, ending at x and y.
   */
  private Set<Pair<A, A>> pairsWithTheSameDomain(final O x, final O y) {
    return new Predicate<Pair<A, A>>() {
      @Override
      public boolean eval(Pair<A, A> p) {
        return
                sameDomain(p.x(), p.y()) &&
                        equal(d1(p.x()), x) &&
                        equal(d1(p.y()), y);
      }
    }.find(Base.setProduct(arrows(), arrows()));
  }


  /**
   * Builds a set of all arrows that start at x and y, respectively, and end at the same object.
   *
   * @param x first object
   * @param y second object
   * @return a set of pairs of arrows with the same codomain, starting at x and y.
   */
  private Set<Pair<A, A>> pairsWithTheSameCodomain(final O x, final O y) {
    return new Predicate<Pair<A, A>>() {
      @Override
      public boolean eval(Pair<A, A> p) {
        return
                sameCodomain(p.x(), p.y()) &&
                        equal(d0(p.x()), x) &&
                        equal(d0(p.y()), y);
      }
    }.find(Base.setProduct(arrows(), arrows()));
  }

  /**
   * Checks if (px, py) is a Cartesian product of objects x and y.
   *
   * @param p pair of projections from product to x and y
   * @param x first object
   * @param y second object
   * @return true if this is a cartesian product
   */
  boolean isProduct(final Pair<A, A> p, final O x, final O y) {
    final O prod = d0(p.x());
    return
            equal(d0(p.y()), prod) &&
                    equal(d1(p.x()), x) &&
                    equal(d1(p.y()), y) &&
                    pairFactorsUniquelyOnRight(p).forall(pairsWithTheSameDomain(x, y));
  }

  /**
   * Builds a Cartesian product of two objects, if it exists. Returns null otherwise.
   * The product is represented as a pair of projections from the product object to the
   * two which are being multiplied.
   *
   * @param x first object
   * @param y second object
   * @return a pair of arrows from product object to x and y, or null if none exists.
   */
  public Pair<A, A> product(final O x, final O y) {
    return new Predicate<Pair<A, A>>() {
      @Override
      public boolean eval(Pair<A, A> p) {
        return isProduct(p, x, y);
      }
    }.findOne(Base.setProduct(arrows(), arrows()));
  }
  /**
   * Checks if (ix, iy) is a union of objects x and y.
   *
   * @param i pair of insertions from x and y to their union
   * @param x first object
   * @param y second object
   * @return true if this is a union
   */
  public boolean isUnion(final Pair<A, A> i, final O x, final O y) {
    final O unionObject = d1(i.x());
    return
            equal(d1(i.y()), unionObject) &&
                    equal(d0(i.x()), x) &&
                    equal(d0(i.y()), y) &&
                    pairFactorsUniquelyOnLeft(i).forall(pairsWithTheSameCodomain(x, y));
  }

  /**
   * Builds a union of two objects, if it exists. Returns null otherwise.
   * The union is represented as a pair of insertions of the two objects into their union
   *
   * @param x first object
   * @param y second object
   * @return a pair of arrows from a and b to their union, or null if none exists.
   */
  public Pair<A, A> union(final O x, final O y) {
    return new Predicate<Pair<A, A>>() {
      @Override
      public boolean eval(Pair<A, A> p) {
        return isUnion(p, x, y);
      }
    }.findOne(Base.setProduct(arrows(), arrows()));
  }

  /**
   * Checks if (pa, pb) is a pullback of arrows f and g.
   *
   * @param p pair of projections from alleged pullback to d0(f) and d0(g)
   * @param f first arrow
   * @param g second arrow
   * @return true if this is a pullback
   */
  public boolean isPullback(final Pair<A, A> p, final A f, final A g) {
    final O pullbackObject = d0(p.x());
    return
            equal(d0(p.y()), pullbackObject) &&
                    canCompose(p.x(), f) &&
                    canCompose(p.y(), g) &&
                    equal(m(p.x(), f), m(p.y(), g)) &&
                    pairFactorsUniquelyOnRight(p).forall(pairsEqualizing(f, g));
  }

  /**
   * Builds a pullback of two arrows, if it exists. Returns null otherwise.
   * The pullback is represented as a pair of projections from the pullback object to the
   * domains of the two arrows.
   *
   * @param f first arrows
   * @param g second arrow
   * @return a pair of arrows from pullback object to d0(f) and d0(g), or null if none exists.
   */
  public Pair<A, A> pullback(final A f, final A g) {
    require(sameCodomain(f, g), "Codomains of " + f + " and " + g + " should be the same");
    return new Predicate<Pair<A, A>>() {
      @Override
      public boolean eval(Pair<A, A> p) {
        boolean result = isPullback(p, f, g);
        trace("isPullback?(", p, ",", f, ",", g, ")->", result);
        return result;
      }
    }
//    .startTracing("pullbackBuilder")
            .findOne(Base.setProduct(arrows(), arrows()));
  }

  /**
   * Checks if (pa, pb) is a pushout of arrows f and g.
   *
   * @param p pair of coprojections from d1(f) and d1(g) to the alleged pushout object
   * @param f first arrow
   * @param g second arrow
   * @return true if this is a pushout
   */
  protected boolean isPushout(Pair<A, A> p, A f, A g) {
    final O pushoutObject = d1(p.x());
    return
            equal(d1(p.y()), pushoutObject) &&
                    canCompose(f, p.x()) &&
                    canCompose(g, p.y()) &&
                    equal(m(f, p.x()), m(g, p.y())) &&
                    pairFactorsUniquelyOnRight(p).forall(pairsCoequalizing(f, g));
  }

  /**
   * Builds a pushout of two arrows, if it exists. Returns null otherwise.
   * The pushout is represented as a pair of coprojections from the codomains of the two arrows
   * to the pushout object.
   *
   * @param f first arrows
   * @param g second arrow
   * @return a pair of arrows from d1(f) and d1(g) to the pushout object, or null if none exists.
   */
  public Pair<A, A> pushout(final A f, final A g) {
    require(sameDomain(f, g), "Domains should be the same");
    return new Predicate<Pair<A, A>>() {
      @Override
      public boolean eval(Pair<A, A> p) {
        boolean result = isPushout(p, f, g);
        trace("isPullback?(", p, ",", f, ",", g, ")->", result);
        return result;
      }
    }.findOne(Base.setProduct(arrows(), arrows()));
  }

  /**
   * Checks if a given object (candidate) is a terminal object (aka unit).
   * Terminal object is the one which has just one arrow from every other object.
   */
  final public Predicate<O> isTerminal = new Predicate<O>() {
    @Override
    public boolean eval(final O candidate) {
      return new Predicate<O>() {
        @Override
        public boolean eval(O x) {
          return arrows(x, candidate).size() == 1;
        }
      }.forall(objects());
    }
  };

  private O terminal = null;
  private boolean terminalFound = false;

  /**
   * @return terminal object for this category (null if none found)
   */
  public O terminal() {
    if (!terminalFound) {
      terminal = isTerminal.findOne(objects());
      terminalFound = true;
    }
    return terminal;
  }

  /**
   * Checks if a given object (candidate) is an initial object (aka zero).
   * Initial object is the one which has just one arrow to every other object.
   */
  final public Predicate<O> isInitial = new Predicate<O>() {
    @Override
    public boolean eval(final O candidate) {
      return new Predicate<O>() {
        @Override
        public boolean eval(O x) {
          return arrows(candidate, x).size() == 1;
        }
      }.forall(objects());
    }
  };

  private O initial = null;
  private boolean initialFound = false;

  /**
   * @return initial object for this category, or null if none found
   */
  public O initial() {
    if (!initialFound) {
      initial = isInitial.findOne(objects());
      initialFound = true;
    }
    return initial;
  }
  /**
   * @return a (hard) set of all objects that do not have any non-endomorphic arrows pointing at them.
   *         Constructively, these are all such objects that if an arrow ends at such an object, it is an endomophism.
   *         Since producing a lazy set is too heavy, I just build it in an old-fashion way.
   */
  public Set<O> allInitialObjects() {
    final Set<O> objects = new HashSet<>(objects());

    // kick out objects that are found on the other end of an arrow
    for (A a : arrows()) {
      O dom = d0(a);
      O codom = d1(a);
      if (arrows(codom, dom).isEmpty()) {
        objects.remove(d1(a));
      }
    }
    return objects;
  }

  /**
   * @return a set of all arrows that originate at initial objects (see allInitialObjects)
   */
  Set<A> arrowsFromInitialObjects() {
    Set<O> initialObjects = allInitialObjects();
    Set<A> arrows = new HashSet<>();
    // include only arrows that originate at component objects
    for (A a : arrows()) {
      if (initialObjects.contains(d0(a))) {
        arrows.add(a);
      }
    }
    return arrows;
  }

  /**
   * Given a set of objects and a set of arrows, build a map that maps each object to
   * a set of arrows starting at it.
   *
   * @param objects objects for which to build the bundles.
   * @param arrows  arrows that participate in the bundles.
   * @return a map.
   */
  Map<O, Set<A>> buildBundles(Set<O> objects, Set<A> arrows) {
    return SetMorphism.Morphism(arrows, objects, D0).revert().asMap();
  }


  /**
   * Builds a degree object (x^n) for a given object.
   *
   * @param x the source object
   * @param n degree to which to raise object x
   * @return x^n and its projections to x
   * @TODO(vpatryshev): write good unitests
   */
  @SuppressWarnings("unchecked")
  public Pair<O, List<A>> degree(O x, int n) {
    require(n >= 0, "Degree should be positive, we have " + n);
    if (n == 0) {
      return Pair.of(terminal(), (List<A>) Collections.EMPTY_LIST);
    }
    if (n == 1) {
      return Pair.of(x, Arrays.asList(this.unit(x)));
    }
    Pair<O, List<A>> previous = degree(x, n - 1);
    Pair<A, A> xn = product(x, previous.x());
    List<A> projections = new ArrayList<>();
    projections.add(xn.x());
    for (A a : previous.y()) {
      projections.add(m(xn.y(), a));
    }
    return Pair.of(d0(xn.x()), projections);
  }

  /**
   * Creates an opposite category from this one.
   * That is, all arrows are inverted.
   *
   * @return this<sup>op</sup>
   */
  @Override
  public Category<O, A> op() {
    final Category<O, A> source = this;
    Quiver<O, A> myArrows = new Quiver<O, A>() {
      @Override
      public Set<A> arrows() { return source.arrows(); }
      @Override public O d0(A f) {
        return source.d1(f);
      }
      @Override public O d1(A f) {
        return source.d0(f);
      }
    };

    return new Category<O, A>(objects(), myArrows) {

      @Override
      public A unit(O x) {
        return source.unit(x);
      }

      @Override
      public A m(A f, A g) {
        return source.m(g, f);
      }
    };
  }

  @SuppressWarnings({"rawtypes","unchecked"})
  public static void main(String[] args) {

    Category<String,String> three = buildCategory(Sets.Set("0", "1", "2"),
            Base.Map(Base.array("0.1", "0.2", "a", "b", "2.1", "2.a", "2.b", "2.swap"), // d0
                    Base.array("0", "0", "1", "1", "2", "2", "2", "2")),
            Base.Map(Base.array("0.1", "0.2", "a", "b", "2.1", "2.a", "2.b", "2.swap"), // d1
                    Base.array("1", "2", "2", "2", "1", "2", "2", "2")),
            Base.Map(Base.array(Pair.of("0.1", "a"), Pair.of("0.1", "b"), Pair.of("2.1", "a"), Pair.of("2.1", "b"), Pair.of("a", "2.swap"), Pair.of("b", "2.swap"), Pair.of("2.swap", "2.swap")), // composition map
                    Base.array("0.2", "0.2", "2.a", "2.b", "b", "a", "2"))
    );

    Category<String, String> vital_sol = buildCategory(Sets.Set("a", "b"),
            Base.Map(Base.array("green", "yellow", "red", "blue", "big", "medium", "small"),
                    Base.array("a", "a", "a", "a", "b", "b", "b")),
            Base.Map(Base.array("green", "yellow", "red", "blue", "big", "medium", "small"),
                    Base.array("b", "b", "b", "b", "a", "a", "a")),
            Base.Map(Base.array(Pair.of("green", "medium")), Base.array("a")));
    System.out.println(vital_sol);
    Category c = Category("([0,1,2], {a: 0 -> 2, b: 1 -> 2})");
    System.out.println(c);
  }
}