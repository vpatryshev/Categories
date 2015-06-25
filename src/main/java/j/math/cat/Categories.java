package j.math.cat;

import static java.util.Collections.singleton;
import static j.math.cat.Category.*;

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
 * All source code is stored at <a href="https://github.com/vpatryshev/Categories">https://github.com/vpatryshev/Categories</a>
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

    final PoSet<Integer> range = PoSet.range(0, n - 1, 1);
    return Category(range);
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
        Graph(objects, Functions.id(objects), Functions.id(objects)),
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
  public static final Category<String, String> _1plus1_ = discreteCategory(Sets.Set("a", "b"));
  
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
      Category(Graph(Sets.Set("0", "1"), Base.Map(Base.array("a", "b"), Base.array(BasePair.Pair("0", "1"), BasePair.Pair("0", "1")))), null);

  /**
   * Category <b>Z2</2> - a two-element monoid
   */
  public static final Category<String, String> Z2 = Category("(([1], {1: 1 -> 1, -1: 1 -> 1}), {1 o 1 = 1, 1 o -1 = -1, -1 o 1 = -1, -1 o -1 = 1})");
  
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

    @SuppressWarnings({"rawtypes","unchecked"})
    static class SetCategory extends Category<Set<Object>, TypelessSetMorphism> {

        public SetCategory(BigSet classOfSets) {
            super(classOfSets, new Quiver<Set<Object>, TypelessSetMorphism>() {
                private final Set<TypelessSetMorphism> ALL_MORPHISMS =
                        new BigSet<TypelessSetMorphism>() {
                            @Override public boolean contains(Object f) {
                                return f instanceof SetMorphism;
                            }
                        };

                @Override
                public Set<TypelessSetMorphism> arrows() {
                    return ALL_MORPHISMS;
                }

                @Override
                public Set d0(TypelessSetMorphism arrow) {
                    return arrow.domain();
                }

                @Override
                public Set d1(TypelessSetMorphism arrow) {
                    return arrow.codomain();
                }
            });
        }

        @Override
        @SuppressWarnings({"rawtypes"})
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
        public void validate() { // it IS a category
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
                    return Base.equal(arrow.apply(p.x()), arrow.apply(p.y())) ? Base.equal(p.x(), p.y()) : true;
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

        //  @Override
        public TypelessSetMorphism equalizer(
                final TypelessSetMorphism f, final TypelessSetMorphism g) {
            assert f.domain() == g.domain() && f.codomain() == g.codomain();
            return TypelessSetMorphism.inclusion(f.domain(), new Predicate() {
                @Override
                public boolean eval(Object x) {
                    return Base.equal(f.apply(x), g.apply(x));
                }
            });
        }

        @Override
        public TypelessSetMorphism coequalizer(TypelessSetMorphism f, TypelessSetMorphism g) {
            assertParallelPair(f, g);
            Sets.FactorSet<Object> factorset = new Sets.FactorSet<Object>(f.codomain());
            for (Object x : f.domain()) {
                factorset.merge(f.apply(x), g.apply(x));
            }
            return TypelessSetMorphism.forFactorset(factorset);
        }

        @Override
        public TypelessSetMorphism coequalizer(final Iterable<TypelessSetMorphism> arrowsToEqualize, Set<Object> codomain) {
            if (!arrowsToEqualize.iterator().hasNext()) {
                return TypelessSetMorphism.unitMorphism(codomain);
            }
            Sets.FactorSet<Object> factorset = new Sets.FactorSet<Object>(codomain);
            Set domain = null;
            for (TypelessSetMorphism f : arrowsToEqualize) {
                assert Base.equal(f.codomain(), codomain) : "Codomain should be " + codomain;
                if (domain == null) {
                    domain = f.domain();
                } else {
                    assert Base.equal(f.domain(), domain) : "Arrows should have the same domain";
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
            return BasePair.Pair(p, q);
        }

        @Override
        public Pair<TypelessSetMorphism, TypelessSetMorphism> union(Set x, Set y) {
            Functions.Injection tagX = BasePair.withLeft("x");
            Functions.Injection tagY = BasePair.withLeft("y");
            Set taggedX = tagX.map(x);
            Set taggedY = tagY.map(y);
            Set unionSet = Sets.union(taggedX, taggedY);
            TypelessSetMorphism ix =
                    TypelessSetMorphism.compose(TypelessSetMorphism.forFunction(x, taggedX, tagX), TypelessSetMorphism.inclusion(taggedX, unionSet));
            TypelessSetMorphism iy =
                    TypelessSetMorphism.compose(TypelessSetMorphism.forFunction(y, taggedY, tagY), TypelessSetMorphism.inclusion(taggedY, unionSet));
            return BasePair.Pair(ix, iy);
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
                            return Base.equal(f.apply(pair.x()), g.apply(pair.y()));
                        }
                    };

            TypelessSetMorphism pullbackToProduct =
                    TypelessSetMorphism.inclusion(product.x().domain(), pairIsGood);
            return BasePair.Pair(TypelessSetMorphism.compose(pullbackToProduct, product.x()), TypelessSetMorphism.compose(pullbackToProduct, product.y()));
        }

        @Override
        public Pair<TypelessSetMorphism, TypelessSetMorphism> pushout(
                TypelessSetMorphism f,
                TypelessSetMorphism g) {
            Pair<TypelessSetMorphism, TypelessSetMorphism> union =
                    union(f.codomain(), g.codomain());
            TypelessSetMorphism coequalizer =
                    coequalizer(TypelessSetMorphism.compose(f, union.x()), TypelessSetMorphism.compose(g, union.y()));
            return BasePair.Pair(TypelessSetMorphism.compose(union.x(), coequalizer), TypelessSetMorphism.compose(union.y(), coequalizer));
        }

        @Override
        public Category<Set<Object>, TypelessSetMorphism> op() {
            throw new UnsupportedOperationException("TODO(vpatryshev): try to implement it.");
        }
        @Override
        public Set initial() {
            return Collections.EMPTY_SET;
        }

        private final Set TERMINAL_SET = singleton(initial());

        @Override
        @SuppressWarnings({"rawtypes","unchecked"})
        public Set terminal() {
            return TERMINAL_SET;
        }

        @Override
        @SuppressWarnings({"rawtypes","unchecked"})
        public Pair<Set, List<TypelessSetMorphism>> degree(final Set x, final int n) {
            final Set allMaps = Sets.allMaps(Sets.numbers(n), x);
            return BasePair.Pair(
                    allMaps,
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
        public Iterator<Set<Object>> iterator() {
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
        public Set<Set<Object>> nodes() {
            return super.nodes();
        }

        @Override
        public Set<Object> d0(TypelessSetMorphism arrow) {
            return arrow.domain();
        }

        @Override
        public Set<Object> d1(TypelessSetMorphism arrow) {
            return arrow.codomain();
        }
    }

        /**
     * Category of finite sets.
     */

    @SuppressWarnings("unchecked")
    public static final SetCategory SETF = new SetCategory(BigSet.FINITE_SETS);

    @SuppressWarnings({"rawtypes","unchecked"})
    public static void main(String[] args) {
        for (Category cat : Base.array(_1_, _2_, _3_, _4_, Z2, _1plus1_, M, W, SPLIT_MONO, SQUARE)) {
            cat.validate();
            System.out.println(cat.toString());
        }
    }
}