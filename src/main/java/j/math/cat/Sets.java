package j.math.cat;

import static java.util.Collections.singleton;
import static j.math.cat.Base.buildProductIterator;
import static j.math.cat.Base.concat;
import static j.math.cat.Base.countEntries;
import static j.math.cat.Base.range;
import static j.math.cat.Base.zip;

import j.math.cat.Functions.Function;
import j.math.cat.Functions.Inclusion;
import j.math.cat.Functions.Injection;
import j.math.cat.Functions.IterableToSet;
import j.math.cat.Functions.PairsToMap;

import java.util.*;
import java.util.Map.Entry;

/**
 * All kinds of additional Set functionality.
 * 
 * @author Vlad Patryshev
 * All source code is stored at <a href="https://github.com/vpatryshev/Categories">https://github.com/vpatryshev/Categories</a>
 * 
 */
public class Sets {

  /**
   * This class serves as a namespace for Cartesian product of iterables.
   * <p/>
   * Given a sequence of sequences, Cartesian.product builds a product
   * of these sequences.
   * <p/>
   * Type parameters:
   * @param <X> the type of elements of each sequence (must be the same)
   * @param <XS> the type of sequences, must extend Set&lt;X>. 
   */
  public static class Cartesian<X, XS extends Set<X>> {
    /*
    (Notes written by _navi_ on how this is implementable in Haskell.

    product :: [[a]] -> [[a]]
    product [] = [[]]
    product (xs:xss) = product' xs (product xss)
      where
        product' xs pxs = concat $ map (\x -> map (x:) pxs) xs

    -- concat [[[4]], [[5]], [[6]]] = [[4], [5], [6]]
    -- product' [4,5,6] [[]] = concat $ map (\x -> [[x]]) [4,5,6] = [[4], [5], [6]]
    -- product [[1,2,3], [4,5,6]] = product' [1,2,3] [[4],[5],[6]] =
    --   concat $ map (\x -> map (x:) [[4],[5],[6]]) [1,2,3] =
    --   concat $ [[[1,4], [1,5], [1,6]], [[2,4],[2,5],[2,6]], [[3,4],[3,5],[3,6]]]
     */

    /**
     * Public method that builds the Cartesian product. The product is lazy.
     * @param <X> element type
     * @param <XS> set type
     *
     * @param xss sequence of sequences that are being multiplied.
     * @return Cartesian product of the sequences.
     */
    public static <X, XS extends Set<X>> Set<? extends Iterable<X>> product(final Iterable<XS> xss) {
      return new Cartesian<X, XS>().calculateProduct(xss);
    }

    /**
     * Public method that builds the Cartesian product. The product is lazy.
     * @param <X> element type
     * @param <XS> set type
     *
     * @param xss list of sets that are being multiplied.
     * @return Cartesian product of the sets.
     */
    public static <X, XS extends Set<X>> Set<? extends List<X>> product(final List<XS> xss) {
      return new Cartesian<X, XS>().calculateProduct(xss);
    }

    /**
     * Builds a function that builds projections from a Cartesian product of a list of sets
     * to its components
     * @param <X> element type
     *
     * @return a function that for each i returns a function just takes an i-th element of a list.
     */
    public static <X> Function<Integer, Function<List<X>, X>> projection() {
      return new Function<Integer, Function<List<X>, X>>() {

        @Override
        public Function<List<X>, X> apply(final Integer i) {
          return new Function<List<X>, X>() {

            @Override
            public X apply(List<X> list) {
              return list.get(i);
            }
          };
        }
      };
    }

    /**
     * Public method that takes a vararg of sequences and builds their Cartesian product.
     * @param <X> element type
     * @param <XS> set type
     *
     * @param xss (a plurality of) sequences that are being multiplied.
     * @return Cartesian product of the sequences.
     */
    @SuppressWarnings({"unchecked"})
    public static <X, XS extends Set<X>> Iterable<? extends List<X>> product(final XS... xss) {
      return product(Arrays.asList(xss));
    }

    /**
     * Actually builds the product of the sequence of sequences.
     *
     * @param xss sequences to multiply.
     * @return their Cartesian product.
     */
    private Set<? extends Iterable<X>> calculateProduct(final Iterable<XS> xss) {
      if (xss.iterator().hasNext()) {
        Pair<XS, Iterable<XS>> pair = Base.split(xss);
        XS head = pair.x();
        Iterable<XS> tail = pair.y();
        return appendComponentToProductOfIterable(head, calculateProduct(tail));
      }
      return singleton(new ArrayList<X>());
    }

    /**
     * Actually builds the product of the list of sets.
     *
     * @param xss list of sets to multiply.
     * @return their Cartesian product.
     */
    private Set<? extends List<X>> calculateProduct(final List<XS> xss) {
      if (xss.size() == 0) {
        return singleton(new ArrayList<X>());
      }
      Pair<XS, List<XS>> pair = Base.split(xss);
      XS head = pair.x();
      List<XS> tail = pair.y();
      return appendComponentToProductOfList(head, calculateProduct(tail));
    }

    /**
     * Given a Cartesian product of zero or more sequences (components), appends
     * yet another component. So that if we have A0 and A1 x A2 x A3,
     * we get A0 x A1 x A2 x A3.
     *
     * @param component      a component to append to the head of Cartesian product.
     * @param partialProduct the product of zero or more components of the same kind.
     * @return the new product.
     */
    private Set<Iterable<X>>
    appendComponentToProductOfIterable(XS component, Set<? extends Iterable<X>> partialProduct) {
      // E.g. [1, 2, 3], [[4, 6], [5, 6]] ->
      // [[[1, 4, 6], [1, 5, 6]], [[2, 4, 6], [2, 5, 6]], [[3, 4, 6], [3, 5, 6]]] ->
      // [[1, 4, 6], [1, 5, 6], [2, 4, 6], [2, 5, 6], [3, 4, 6], [3, 5, 6]]
      Set<Set<Iterable<X>>> concatenatedComponents = consWithIterableProduct(partialProduct).map(component);

      return union(concatenatedComponents);
    }

    /**
     * Given a Cartesian product of zero or more sequences (components), appends
     * yet another component. So that if we have A0 and A1 x A2 x A3,
     * we get A0 x A1 x A2 x A3.
     *
     * @param component      a component to append to the head of Cartesian product.
     * @param partialProduct the product of zero or more components of the same kind.
     * @return the new product.
     */
    private Set<List<X>>
    appendComponentToProductOfList(XS component, Set<? extends List<X>> partialProduct) {
      // E.g. [1, 2, 3], [[4, 6], [5, 6]] ->
      // [[[1, 4, 6], [1, 5, 6]], [[2, 4, 6], [2, 5, 6]], [[3, 4, 6], [3, 5, 6]]] ->
      // [[1, 4, 6], [1, 5, 6], [2, 4, 6], [2, 5, 6], [3, 4, 6], [3, 5, 6]]
      Set<Set<List<X>>> concatenatedComponents = consWithListProduct(partialProduct).map(component);

      return union(concatenatedComponents);
    }
    
    /**
     * Takes [[x11,x12,...],[x21,x22,...]], that is, the product of n-1 components,
     * and produces a function that, given an x, returns a sequence of
     * [[x,x11,x12,...],[x,x21,x22,...]].
     * @param <ElementOfProduct> type of product element
     *
     * @param pxs the product of n-1 iterables (components)
     * @return the product of singleton {x} and the given product
     */
    public <ElementOfProduct extends Iterable<X>> Injection<X, Set<Iterable<X>>>
    consWithIterableProduct(final Set<ElementOfProduct> pxs) {
      return new Injection<X, Set<Iterable<X>>>() {
        // takes an x, returns a function appending x to sequences
        @Override
        public Set<Iterable<X>> apply(final X x) {
          // Takes a sequence [x1, x2, ...], returns [x, x1, x2, ...]
          return new Injection<ElementOfProduct, Iterable<X>>() {
            @SuppressWarnings({ "unchecked"})
            @Override
            public Iterable<X> apply(ElementOfProduct y) {
              List<X> sl = Collections.singletonList(x);
              return concat(sl, y);
            }

          }.map(pxs);
        }
      };
    }

    /**
     * Takes [[x11,x12,...],[x21,x22,...]], that is, the product of n-1 components,
     * and produces a function that, given an x, returns a sequence of
     * [[x,x11,x12,...],[x,x21,x22,...]].
     * @param <ElementOfProduct> the type of product element (probably a list of X)
     *
     * @param pxs the product of n-1 lists (components)
     * @return the product of singleton {x} and the given product
     */
    public <ElementOfProduct extends List<X>> Injection<X, Set<List<X>>>
    consWithListProduct(final Set<ElementOfProduct> pxs) {
      return new Injection<X, Set<List<X>>>() {
        // takes an x, returns a function appending x to sequences
        @Override
        public Set<List<X>> apply(final X x) {
          // Takes a sequence [x1, x2, ...], returns [x, x1, x2, ...]
          return new Injection<ElementOfProduct, List<X>>() {
            @SuppressWarnings("unchecked")
            @Override
            public List<X> apply(ElementOfProduct y) {
              return Base.concat(Arrays.asList(x), y);
            }
          }.map(pxs);
        }
      };
    }
  }

  private Sets() {}

  /**
   * Given an iterable of elements, builds a new (lazy) set with elements from that iterable.
   * The size of the set is unknown; we could calculate it, of course, but we should not.
   * Since its size is unknown, you should not use size() or isEmpty().
   * That's a phisosophical issue. Usually programmers check the size
   * of a set out of fear that they would be caught scanning an empty list. It's okay actually.
   *
   * @param elements an iterable listing the elements of the new set.
   * @return a set that contains all these elements.
   */
  static <X> Set<X> SetOfUnknownSize(final Iterable<X> elements) {
    return new AbstractSet<X>() {

      @Override
      public boolean isEmpty() {
        return !iterator().hasNext();
      }

      @Override
      public Iterator<X> iterator() {
        return elements.iterator();
      }

      @Override
      public boolean equals(Object o) {
        if (o == this)
           return true;

        if (!(o instanceof Set)) return false;

        Set<X> c = (Set<X>) o;
        try {
          return containsAll(c) && c.containsAll(this);
        } catch (ClassCastException unused)   { return false;
        } catch (NullPointerException unused) { return false;
        }
      }

      private int ENOUGH = 42;

      @Override
      public int hashCode() {
        int h = 0; int i = ENOUGH;
        for (X x : this) {
          h += x.hashCode();
          if (0 <-- i) break;
        }
        return h;
      }

      @Override
      public int size() {
        return Integer.MAX_VALUE;
      }
    };
  }

  /**
   * Builds an actual set out of elements provided
   * @param <T> element type
   * @param elements the elements to add
   * @return the set
   */
  @SuppressWarnings({"unchecked"})
  public static <T> Set<T> Set(T... elements) {
    Set<T> source = new HashSet<T>();
    source.addAll(Arrays.asList(elements));
    return Set(source);
  }

  /**
   * Builds a (virtual) set of numbers 0..n-1
   * @param n the number of numbers
   * @return the set
   */
  public static Set<Integer> numbers(int n) {
    return numbers(0, n);
  }

  /**
   * Builds a (virtual) set of numbers a..b-1
   * @param a the first number
   * @param b the last but one number
   * @return the set
   */
  public static Set<Integer> numbers(int a, int b) {
    return numbers(a, b, 1);
  }

  /**
   * Builds a (virtual) set of numbers from a to b-c with step c
   * @param a the first number
   * @param b the last but one number
   * @param c step; can be negative
   * @return the set
   */
  public static Set<Integer> numbers(final int a, final int b, final int c) {
    int delta = b - a;
    final int size = c == 0 || delta == 0 || c * delta < 0 ? 0 :
        Math.max(0, (delta + c + (c < 0 ? 1 : -1)) / c);

    return new AbstractSet<Integer>() {

      @Override
      public Iterator<Integer> iterator() {
        return new Iterator<Integer>() {
          private int i = 0;

          public boolean hasNext() {
            return i < size;
          }

          public Integer next() {
            if (hasNext()) {
              return a + (i++) * c;
            }
            throw new NoSuchElementException();
          }

          public void remove() {
            throw new UnsupportedOperationException();
          }
        };
      }

      @Override
      public int size() {
        return size;
      }
    };
  }

  /**
   * Take an iterable, trust it to be non-repeating,
   * return a virtual set based on the iterable's elements.
   * 
   * @param <T> element type
   * @param iterable source of elements
   * @return a set consisting of the iterable's elements.
   */
  public static <T> Set<T> Set(final Iterable<T> iterable) {
    return new AbstractSet<T>() {

      @Override
      public Iterator<T> iterator() {
        return iterable.iterator();
      }

      @Override
      public boolean isEmpty() {
        return !iterator().hasNext();
      }

      @Override
      public int size() {
        return countEntries(this);
      }
    };
  }

  /**
   * Take an iterable, trust it to be non-repeating,
   * return a virtual set based on the iterable's elements.
   *
   * @param <T> element type
   * @param iterable source of elements
   * @return a set consisting of the iterable's elements.
   */
  public static <T> Set<T> Set(final Iterable<T> iterable, final int size) {
    return new AbstractSet<T>() {

      @Override
      public Iterator<T> iterator() {
        return iterable.iterator();
      }

      @Override
      public int size() {
        return size;
      }
    };
  }

  /**
   * Extracts a set from a string as produced by Set.toString().
   * Entries in the set can not be empty strings and cannot contain commas.
   *
   * @param s the string
   * @return restored set
   */
  public static Set<String> parseSet(String s) {
    String[] content = s.substring(1, s.length() - 1).split(",\\s*");
    Set<String> result = new HashSet<String>();
    for (String entry : content) {
      String trimmed = entry.trim();
      if (!trimmed.isEmpty()) {
        result.add(trimmed);
      }
    }
    return result;
  }

  /**
   * Builds a (virtual) map of unknown size, based on a given iterable of pairs.
   * You cannot know the size of an iterable without scanning it; but still
   * maps based on unlimited collections (iterables) are pretty useful; the
   * only problem is halting problem: if the iterable is infinite, and the key
   * cannot be found, it will run forever looking.
   * 
   * @param <X> key type
   * @param <Y> value type
   * @param <P> the type of key-value pairs
   * @param pairs key-balue pairs
   * @return the map
   */
  public static <X, Y, P extends Pair<X, Y>> Map<X, Y> Map(final Iterable<P> pairs) {
    final Injection<P, Entry<X, Y>> cast = new Inclusion<Entry<X, Y>, P>();

    return new AbstractMap<X, Y>() {
      @Override
      public Set<Entry<X, Y>> entrySet() {
        return cast.map(Set(pairs));
      }
    };

/*
    final Set<Entry<X, Y>> entries = cast.map(SetOfUnknownSize(pairs));
    Map<X,Y> res = new HashMap<X, Y>();

    for (Entry<X, Y> p : entries) {
      res.put(p.getKey(), p.getValue());
    }
    return res;
    */
  }

  /**
   * Creates a (virtual) map based on a given set of key-value pairs
   * @param <X> key type
   * @param <Y> value type
   * @param <P> the type of key-value pairs
   * @param pairs the pairs
   * @return the (virtual) map
   */
  public static <X, Y, P extends Pair<X, Y>> Map<X, Y> Map(final Set<P> pairs) {
    final Injection<P, Entry<X, Y>> cast = new Inclusion<Entry<X, Y>, P>();

    return new AbstractMap<X, Y>() {
      @Override
      public Set<Entry<X, Y>> entrySet() {
        return cast.map(pairs);
      }
    };
  }

  /**
   * Builds a (virtual) set of all maps from one set (<code>setX</code>) to another (<code>setY</code>).
   * 
   * @param <X> the type of elements of the first set (<code>setX</code>)
   * @param <Y> the type of elements of the second set (<code>setY</code>)
   * @param setX the first set (domain)
   * @param setY the second set (codomain)
   * @return the set of all maps
   */
  public static <X, Y> Set<Map<X, Y>> allMaps(final Set<X> setX, final Set<Y> setY) {
    Set<Set<Pair<X, Y>>> sections =
        new Injection<X, Set<Pair<X, Y>>>() {
          // this function builds pairs (x, y) for all y for a given x
          @Override
          public Set<Pair<X, Y>> apply(final X x) {
            return BasePair.<X, Y>withLeft(x).map(setY);
          }
        }.map(setX);

    Set<? extends Iterable<Pair<X, Y>>> product = Cartesian.product(sections);
    Set<Set<Pair<X, Y>>> setOfSetsOfPairs =
        Set(new IterableToSet<Pair<X, Y>>().map(product));
    return new PairsToMap<X, Y>().map(setOfSetsOfPairs);
  }

  /**
   * Factory method. Builds a function that makes one set a factorset of another.
   * 
   * @param <X> the main set element type
   * @param set the set
   * @param factorset the subset
   * @return projection from the set to the factorset.
   */
  public static <X> Function<X, Set<X>> factorset(final Set<X> set, final Set<Set<X>> factorset) {
    assert union(factorset).equals(set) : "factorset's components should contain all elements of main set";
    int size = 0;
    for (Set<X> component : factorset) {
      assert component.size() > 0 : "factorset component should be non-empty";
      size += component.size();
    }
    assert size <= set.size() : "factorset should be disjoint";

    return new Function<X, Set<X>>() {
      @Override
      public Set<X> apply(X x) {
        for (Set<X> component : factorset) {
          if (component.contains(x)) {
            return component;
          }
        }
        throw new IllegalArgumentException("Factorset component not found for " + x);
      }
    };
  }

  /**
   * Implements factorset functionality. A factorset may be built given a BinaryRelationship,
   * or incrementally, when the client provides pairs of equivalent elements.
   * The factorset is not lazy; equivalence classes are stored in a map, and the resulting
   * factorset is returned as a new HashSet.
   * 
   * @param <X> element type
   */
  public static class FactorSet<X> {
    /**
     * The set being factored.
     */
    Set<X> set;
    
    /**
     * Maps elements of the main set to their equivalence classes (they constitute the factorset).
     */
    Map<X, Set<X>> equivalenceClasses;

    /**
     * Builds an initial factorset, given the domain set.
     *
     * @param set domain set.
     */
    @SuppressWarnings("unchecked")
    public FactorSet(Set<X> set) {
      this.set = set;
      equivalenceClasses = new HashMap<X, Set<X>>();
      for (X x : set) {
        equivalenceClasses.put(x, Set(x));
      }
    }

    /**
     * Builds a factorset of a given set, by the transitive closure of a given relationship.
     *
     * @param set base set
     * @param r   binary relationship
     */
    public FactorSet(Set<X> set, BinaryRelationship<X, X> r) {
      this(set);
      factorByRelationship(r);
    }

    /**
     * Given a <code>BinaryRelationship r</code>, merges equivalent classes if they
     * contain elements that are in <code>r</code>.
     *
     * @param r the binary relationship. Does not have to be symmetrical or transitive.
     */
    public void factorByRelationship(BinaryRelationship<X, X> r) {
      for (X x1 : set) {
        for (X x2 : set) {
          if (x1 == x2) {
            continue; // skip same value
          }
          if (equivalenceClasses.get(x1) == equivalenceClasses.get(x2)) {
            continue; // skip same class
          }
          if (r.eval(x1, x2) || r.eval(x2, x1)) {
            merge(x1, x2);
          }
        }
      }
    }

    /**
     * Merges equivalence classes for two elements
     *
     * @param x1 first element
     * @param x2 second element
     */
    public void merge(X x1, X x2) {
      Set<X> class1 = equivalenceClasses.get(x1);
      Set<X> class2 = equivalenceClasses.get(x2);
      Set<X> merged = class1 == null ? new HashSet<X>() : new HashSet<X>(class1);
      if (class2 != null) merged.addAll(class2);

      for (X x3 : merged) {
        equivalenceClasses.put(x3, merged);
      }
    }

    /**
     * @return the latest version of factorset built here.
     */
    public Set<Set<X>> factorset() {
      return new HashSet<Set<X>>(equivalenceClasses.values());
    }

    /**
     * @return the function from the domain set to the factorset.
     */
    public Function<X, Set<X>> asFunction() {
      return Functions.forMap(equivalenceClasses);
    }

    /**
     * @return the domain set.
     */
    public Set<X> domain() {
      return set;
    }
  }

  /**
   * Builds a union of disjoint sets.
   *
   * @param <T> element type
   * @param components sets to be used in building the union.
   * @return a set containing the elements of all the given sets.
   */
  @SuppressWarnings({"unchecked"})
  public static <T> Set<T> union(Set<T>... components) {
    return union(Arrays.asList(components));
  }

  /**
   * Builds a union of an Iterable of disjoint sets.
   *
   * @param <T> element type
   * @param iterableOfSets provides sets to be used in building the union.
   * @return a set containing the elements of all the sets of the Iterable.
   */
  public static <T> Set<T> union(final Iterable<? extends Set<T>> iterableOfSets) {
    return new AbstractSet<T>() {
      @Override
      public Iterator<T> iterator() {
        final Iterator<? extends Iterable<T>> top = iterableOfSets.iterator();
        assert top != null : "Top iterable's iterator() should not be null";
        return new Iterator<T>() {
          Iterator<T> current = Base.emptyIterator();

          public boolean hasNext() {
            while (!current.hasNext() && top.hasNext()) {
              Iterable<T> nextTop = top.next();
              assert nextTop != null : "Top level iterator should not return nulls";
              current = nextTop.iterator();
            }
            return current.hasNext();
          }

          public T next() {
            if (!hasNext()) {
              throw new NoSuchElementException();
            }
            return current.next();
          }

          public void remove() {
            current.remove();
          }
        };
      }

      @Override
      public int size() {
        int n = 0;
        for (Set<T> s : iterableOfSets) {
          n += s.size();
        }
        return n;
      }
    };
  }

  
  /**
   * Encapsulates disjoint union of a list of sets. Disjoint means that even if the 
   * sets contain common elements, the union will make them distinct by tagging all elements.
   * The result consists of pairs, the first being list index, the second an element of a set.
   * E.g. disjointUnion(LIst(singleton("a"), singleton("b")) returns
   * Set(Pair(0, "a"), Pair(1, "b")).
   * 
   * @param <T> the type of elements in the sets being joined. The same for all sets (it's Java...)
   */
  public static class DisjointUnion<T> {
    
    /**
     * The sets being joined. 
     */
    List<Set<T>> sets;
    
    /**
     * @param sets The sets to join.
     */
    public DisjointUnion(List<Set<T>> sets) {
      this.sets = sets;
    }
    
    /**
     * @return the (virtual) set that is the disjoint union of given sets
     */
    public Set<Pair<Integer, T>> unionSet() {
      Iterable<Pair<Integer, Set<T>>> indexed = zip(range(sets.size()), sets);
      Iterable<Set<Pair<Integer, T>>> tojoin = new Function<Pair<Integer, Set<T>>, Set<Pair<Integer, T>>>() {
        @Override
        public Set<Pair<Integer, T>> apply(Pair<Integer, Set<T>> pair) {
          int index = pair.x();
          Set<T> set = pair.y();
          return injection(index).map(set);
        }
      }.map(indexed);
      return union(tojoin);
    }
    
    /**
     * Maps an i-th set into the union
     * @param i index of the set to inject in the list of sets
     * @return the injection (which is an Injection which is a Function))
     */
    public Injection<T, Pair<Integer, T>> injection(int i) {
      return BasePair.<Integer, T>withLeft(i);
    }

    /**
     * @return the list of injections of original sets to their union
     */
    public List<Injection<T, Pair<Integer, T>>> injections() {
      return new Function<Integer, Injection<T, Pair<Integer, T>>>() {

        @Override
        public Injection<T, Pair<Integer, T>> apply(Integer index) {
          return injection(index);
        }
      }.map(range(sets.size()));
    }
  }
  
  /**
   * Builds a Cartesian product of two sets.
   * 
   * @param <X> element type for the first set 
   * @param <Y> element type for the second set
   * @param x first product component.
   * @param y second product component.
   * @return a product set, contains all possible pairs of elements from x and y.
   */
  public static <X, Y> Set<Pair<X, Y>> product(final Set<X> x, final Set<Y> y) {
    return new AbstractSet<Pair<X, Y>>() {

      @Override
      public Iterator<Pair<X, Y>> iterator() {
        return buildProductIterator(x, y);
      }

      @Override
      public int size() {
        return x.size() * y.size();
      }
    };
  }

  /**
   * Given two sets, xs and ys, and a function f: xs->ys,
   * builds a map ys->xss, for each y returning all those x in xs that f(x)=y.
   * 
   * @param <X> domain element type
   * @param <Y> codomain element type
   * @param xs domain
   * @param ys codomain
   * @param f function
   * @return the map
   */
  public static <X, Y> Map<Y, Set<X>> groupBy(Set<X> xs, Set<Y> ys, Function<X, Y> f) {
    Map<Y, Set<X>> result = new HashMap<Y, Set<X>>();
    for (Y y : ys) {
      result.put(y, new HashSet<X>());
    }

    for (X x : xs) {
      result.get(f.apply(x)).add(x);
    }

    return result;
  }

  /**
   * Powerset of a set. Cheap solution with bitmask.
   * 
   * @param <X> set element type
   * @param <XS> powerset element type
   * @param xs the set which powerset we are calculating.
   * @return a powerset, lazy.
   */
  public static <X, XS extends Set<X>> Set<Set<X>> powerset(final XS xs) {
    return new AbstractSet<Set<X>>() {

      @Override
      public Iterator<Set<X>> iterator() {
        return new Iterator<Set<X>>() {
          int i = 0;

          public boolean hasNext() {
            return i < size();
          }

          public Set<X> next() {
            i++;
            return new AbstractSet<X>() {
              int bits = i - 1;

              @Override
              public Iterator<X> iterator() {
                return new Predicate<X>() {
                  int mask = bits * 2; // yeah, I'm lazy this Christmas morning

                  @Override
                  public boolean eval(X x) {
                    return (mask /= 2) % 2 == 1;
                  }
                }.filter(xs.iterator());
              }

              @Override
              public int size() {
                return Integer.bitCount(bits);
              }
            };
          }

          public void remove() {
            throw new UnsupportedOperationException();
          }
        };
      }

      @Override
      public int size() {
        return 1 << xs.size();
      }
    };
  }


  public static <X> boolean isEnumerable(Set<X> set) {
    return set.size() < Integer.MAX_VALUE;
  }

}
