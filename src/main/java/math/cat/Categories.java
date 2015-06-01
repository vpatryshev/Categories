package math.cat;

import static java.util.Collections.singleton;
import static math.cat.Base.Map;
import static math.cat.Base.array;
import static math.cat.BasePair.Pair;
import static math.cat.BasePair.withLeft;
import static math.cat.Category.Category;
import static math.cat.Functions.id;
import static math.cat.Graph.Graph;
import static math.cat.Sets.Set;
import static math.cat.TypelessSetMorphism.compose;
import static math.cat.TypelessSetMorphism.forFunction;
import math.cat.Functions.Injection;
import math.cat.Sets.FactorSet;

import java.util.AbstractList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Some specific categories here.
 * 
 * @author Vlad Patryshev
 * All source code is stored on <a href="http://code.google.com/p/categories/">http://code.google.com/p/categories/</a>
 */
public final class Categories {
  private Categories() {}

  /**
   * Builds a category out of a segment of integers between 0 and n (not included).
   *
   * @param n number of elements
   * @return a new category
   */
  public static Category<Integer, Pair<Integer, Integer>> segment(int n) {
    return Category(PoSet.range(0, n - 1, 1));
  }

  /**
   * Builds a discrete category on a given set of objects. 
   * 
   * @param <T> object type
   * @param objects set of this category's objects
   * @return the category
   */
  @SuppressWarnings("unchecked")
  public static <T> Category<T, T> discreteCategory(Set<T> objects) {
    return Category(
        Graph(objects, id(objects), id(objects)),
        (Map<Pair<T, T>, T>) Collections.EMPTY_MAP);
  }

  /**
   * Empty category
   */
  public static final Category<Integer, Pair<Integer, Integer>> _0_ = segment(0);
  
  /**
   * Singleton category
   */
  public static final Category<Integer, Pair<Integer, Integer>> _1_ = segment(1);
  
  /**
   * Discrete 2-object category
   */
  public static final Category<String, String> _1plus1_ = discreteCategory(Set("a", "b"));
  
  /**
   * Category <b>2</b>: 2 objects linearly ordered
   */
  public static final Category<Integer, Pair<Integer, Integer>> _2_ = segment(2);
  
  /**
   * Category <b>3</b>: 3 objects linearly ordered
   */
  public static final Category<Integer, Pair<Integer, Integer>> _3_ = segment(3);
  
  /**
   * Category <b>4</b>: 4 objects linearly ordered
   */
  public static final Category<Integer, Pair<Integer, Integer>> _4_ = segment(4);
  
  /**
   * Category with 2 objects and 2 parallel arrows from one to another
   */
  @SuppressWarnings("unchecked")
  public static final Category<String, String> PARALLEL_PAIR =
      Category(Graph(Set("0", "1"), Map(array("a", "b"), array(Pair("0", "1"), Pair("0", "1")))), null);
  
  /**
   * Category <b>Z2</2> - a two-element monoid
   */
  public static final Category<String, String> Z2 = Category("(([1], {1: 1 -> 1, a: 1 -> 1}), {1 o 1 = 1, 1 o a = a, a o 1 = a, a o a = 1})");
  
  /**
   * "Split Monomorphism" category (see http://en.wikipedia.org/wiki/Morphism)
   * Two objects, and a split monomorphism from a to b 
   */
  public static final Category<String, String> SPLIT_MONO = 
      Category("(([a,b], {ab: a -> b, ba: b -> a, bb: b -> b}), {ab o ba = bb, ba o ab = a, bb o ab = ab, ba o bb = ba, bb o bb = bb})");
  
  /**
   * Commutative square category
   */
  public static final Category<String, String> SQUARE = Category("(([a,b,c,d], {ab: a -> b, ac: a -> c, bd: b -> d, cd: c -> d, ad: a -> d}), {bd o ab = ad, cd o ac = ad})");
  
  /**
   * Pullback category: a -> c <- b 
   */
  public static final Category<String, String> PULLBACK = Category("(([a,b,c], {ac: a -> c, bc: b -> c}), {})");
  
  /**
   * Pushout category: b <- a -> c 
   */
  public static final Category<String, String> PUSHOUT = Category("(([a,b,c], {ab: a -> b, ac: a -> c}), {})");
    
  /**
   * Sample W-shaped category: a -> b <- c -> d <- e
   */
  public static final Category<String, String> W = Category("(([a,b,c,d,e], {ab: a -> b, cb: c -> b, cd: c -> d, ed: e -> d}), {})");
  
  /**
   * Sample M-shaped category: a <- b -> c <- d -> e
   */
  public static final Category<String, String> M = Category("(([a,b,c,d,e], {ba: b -> a, bc: b -> c, dc: d -> c, de: d -> e}), {})");
  
  /**
   * Category of finite sets.
   */
  @SuppressWarnings("unchecked")
  public static Category<Set, TypelessSetMorphism> SETF =
      new Category<Set, TypelessSetMorphism>(BigSet.FINITE_SETS) {
        private final Set<TypelessSetMorphism> ALL_MORPHISMS =
            new BigSet<TypelessSetMorphism>() {
              public boolean contains(Object f) {
                return f instanceof SetMorphism;
              }
            };

        @Override
        public TypelessSetMorphism unit(Set x) {
          // I don't know wtf prevents dispatching TypelessSetMorphism.unit to TypelessSetMorphism...
          return TypelessSetMorphism.unitMorphism(x);
        }

        @Override
        public TypelessSetMorphism m(TypelessSetMorphism f, TypelessSetMorphism g) {
          assert f.codomain() == g.domain() : "Domain and codomain should match";
          return TypelessSetMorphism.compose(f, g);
        }

        @Override
        public Set<TypelessSetMorphism> arrows() {
          return ALL_MORPHISMS;
        }

        @Override
        protected void validate() { // it IS a category
        }

        @Override
        public String toString() {
          return "Category of all Java Sets";
        }

        @Override
        public Set<TypelessSetMorphism> arrows(Set x, Set y) {
          return TypelessSetMorphism.power(x, y);
        }

        @Override
        public boolean isIsomorphism(TypelessSetMorphism arrow) {
          return super.isIsomorphism(arrow);
        }

        @Override
        public boolean isMonomorphism(final TypelessSetMorphism arrow) {
          return new Predicate<Pair<Object, Object>>() {
            @Override
            public boolean eval(final Pair<Object, Object> p) {
              return equal(arrow.apply(p.x()), arrow.apply(p.y())) ? equal(p.x(), p.y()) : true;
            }
          }.forall(Sets.product(arrow.domain(), arrow.domain()));
        }

        @Override
        public boolean isEpimorphism(final TypelessSetMorphism arrow) {
          return new Predicate<Object>() {
            @Override
            public boolean eval(final Object y) {
              return new Predicate<Object>() {
                @Override
                public boolean eval(Object x) {
                  return y.equals(arrow.apply(x));
                }
              }.exists(arrow.domain());
            }
          }.forall(arrow.codomain());
        }

        @Override
        public Predicate<TypelessSetMorphism> factorsOnRight(TypelessSetMorphism g, TypelessSetMorphism f) {
          return super.factorsOnRight(g, f);
        }

        @Override
        public Predicate<TypelessSetMorphism> factorsOnLeft(TypelessSetMorphism g, TypelessSetMorphism f) {
          return super.factorsOnLeft(g, f);
        }

        @Override
        public Predicate<TypelessSetMorphism> factorsUniquelyOnLeft(TypelessSetMorphism f) {
          return super.factorsUniquelyOnLeft(f);
        }

        @Override
        public Predicate<TypelessSetMorphism> factorsUniquelyOnRight(TypelessSetMorphism f) {
          return super.factorsUniquelyOnRight(f);
        }

        @Override
        public TypelessSetMorphism equalizer(
            final TypelessSetMorphism f, final TypelessSetMorphism g) {
          assert f.domain() == g.domain() && f.codomain() == g.codomain();
          return TypelessSetMorphism.inclusion(f.domain(), new Predicate() {
            @Override
            public boolean eval(Object x) {
              return equal(f.apply(x), g.apply(x));
            }
          });
        }

        @Override
        public TypelessSetMorphism coequalizer(TypelessSetMorphism f, TypelessSetMorphism g) {
          assertParallelPair(f, g);
          FactorSet<Object> factorset = new FactorSet<Object>(f.codomain());
          for (Object x : f.domain()) {
            factorset.merge(f.apply(x), g.apply(x));
          }
          return TypelessSetMorphism.forFactorset(factorset);
        }

        @Override
        public TypelessSetMorphism coequalizer(final Iterable<TypelessSetMorphism> arrowsToEqualize, Set codomain) {
          if (!arrowsToEqualize.iterator().hasNext()) {
            return TypelessSetMorphism.unitMorphism(codomain);
          }
          FactorSet<Object> factorset = new FactorSet<Object>(codomain);
          Set domain = null;
          for (TypelessSetMorphism f : arrowsToEqualize) {
            assert equal(f.codomain(), codomain) : "Codomain should be " + codomain;
            if (domain == null) {
              domain = f.domain();
            } else {
              assert equal(f.domain(), domain) : "Arrows should have the same domain";
            }
          }
          TypelessSetMorphism f = null;
          for (TypelessSetMorphism g : arrowsToEqualize) {
            if (f != null) {
              for (Object x : g.domain()) {
                factorset.merge(f.apply(x), g.apply(x));
              }
            }
            f = g;
          }
          return TypelessSetMorphism.forFactorset(factorset);
        }

        @Override
        public Pair<TypelessSetMorphism, TypelessSetMorphism>
               product(Set x, Set y) {
          Set productSet = Sets.product(x, y);
          TypelessSetMorphism p = new TypelessSetMorphism(productSet, x) {
            @Override
            public Object apply(Object pair) {
              return ((Pair) pair).x();
            }
          };
          TypelessSetMorphism q = new TypelessSetMorphism(productSet, y) {
            @Override
            public Object apply(Object pair) {
              return ((Pair) pair).y();
            }
          };
          return Pair(p, q);
        }

        @Override
        public Pair<TypelessSetMorphism, TypelessSetMorphism> union(Set x, Set y) {
          Injection tagX = withLeft("x");
          Injection tagY = withLeft("y");
          Set taggedX = tagX.map(x);
          Set taggedY = tagY.map(y);
          Set unionSet = Sets.union(taggedX, taggedY);
          TypelessSetMorphism ix =
              compose(forFunction(x, taggedX, tagX), TypelessSetMorphism.inclusion(taggedX, unionSet));
          TypelessSetMorphism iy =
              compose(forFunction(y, taggedY, tagY), TypelessSetMorphism.inclusion(taggedY, unionSet));
          return Pair(ix, iy);
        }

        @Override
        public Pair<TypelessSetMorphism, TypelessSetMorphism> pullback(
            final TypelessSetMorphism f,
            final TypelessSetMorphism g) {
          Pair<TypelessSetMorphism, TypelessSetMorphism> product =
              product(f.domain(), g.domain());

          BinaryRelationship<Object, Object> pairIsGood =
              new BinaryRelationship<Object, Object>() {
                @Override
                public boolean eval(Pair<Object, Object> pair) {
                  return equal(f.apply(pair.x()), g.apply(pair.y()));
                }
              };

          TypelessSetMorphism pullbackToProduct =
              TypelessSetMorphism.inclusion(product.x().domain(), pairIsGood);
          return Pair(compose(pullbackToProduct, product.x()), compose(pullbackToProduct, product.y()));
        }

        @Override
        public Pair<TypelessSetMorphism, TypelessSetMorphism> pushout(
            TypelessSetMorphism f,
            TypelessSetMorphism g) {
          Pair<TypelessSetMorphism, TypelessSetMorphism> union =
              union(f.codomain(), g.codomain());
          TypelessSetMorphism coequalizer =
              coequalizer(compose(f, union.x()), compose(g, union.y()));
          return Pair(compose(union.x(), coequalizer), compose(union.y(), coequalizer));
        }

        @Override
        public Set initial() {
          return Collections.EMPTY_SET;
        }

        private final Set TERMINAL_SET = singleton(initial());

        @Override
        public Set terminal() {
          return TERMINAL_SET;
        }

        @Override
        public Pair<Set, List<TypelessSetMorphism>> degree(final Set x, final int n) {
          final Set<Map> allMaps = Sets.allMaps(Sets.numbers(n), x);
          return Pair(
              (Set) allMaps,
              (List<TypelessSetMorphism>) new AbstractList<TypelessSetMorphism>() {
                @Override
                public TypelessSetMorphism get(final int i) {
                  return new TypelessSetMorphism(allMaps, x) {
                    @Override
                    public Object apply(Object map) {
                      return ((Map) map).get(i);
                    }
                  };
                }

                @Override
                public int size() {
                  return n;
                }
              });
        }

        @Override
        public Category<Set, TypelessSetMorphism> op() {
          throw new UnsupportedOperationException("TODO(vpatryshev): try to implement it.");
        }

        @Override
        public Iterator<Set> iterator() {
          throw new UnsupportedOperationException("Not an enumerable entity.");
        }

        @Override
        public int size() {
          throw new UnsupportedOperationException("This is an infinite category.");
        }

        @Override
        public int hashCode() {
          return 7688721;
        }

        @Override
        public Set<Set> nodes() {
          return super.nodes();
        }

        @Override
        public Set d0(TypelessSetMorphism arrow) {
          return arrow.domain();
        }

        @Override
        public Set d1(TypelessSetMorphism arrow) {
          return arrow.codomain();
        }
      };
}