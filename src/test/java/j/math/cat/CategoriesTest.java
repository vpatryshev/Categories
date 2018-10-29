package j.math.cat;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import static j.math.cat.Categories.*;
import static j.math.cat.Sets.isEnumerable;

import junit.framework.TestCase;

/**
 * Unittest for Categories class
 * 
 * @author Vlad Patryshev
 * All source code is stored at <a href="https://github.com/vpatryshev/Categories">https://github.com/vpatryshev/Categories</a>
 * 
 */
@SuppressWarnings({"rawtypes","unchecked"})
public class CategoriesTest extends TestCase {
  private final Set<String> setofAandB = Sets.Set("a", "b");
  private final Set<Boolean> setofTrueAndFalse = Sets.Set(true, false);
  private final BinaryRelation<Integer, Integer> distanceIs2 =
      new BinaryRelation<Integer, Integer>() {
        @Override
        public boolean eval(Pair<Integer, Integer> p) {
          return p.x() - p.y() == 2;
        }
      };

  public void testValidity() {
    for (Category cat : Base.array(_1_, _2_, _3_, _4_, Z2, _1plus1_, M, W, SPLIT_MONO, SQUARE, PARALLEL_PAIR, PULLBACK, PUSHOUT)) {
      cat.validate();
    }
  }

  public void testZero() {
    assertTrue(Categories._0_.objects().isEmpty());
    assertTrue(Categories._0_.arrows().isEmpty());
  }

  public void testOne() {
    assertEquals(1, Categories._1_.objects().size());
    assertEquals(1, Categories._1_.arrows().size());
  }

  public void testTwo() {
    assertEquals(2, Categories._2_.objects().size());
    assertEquals(3, Categories._2_.arrows().size());
    assertEquals(1, Categories._2_.arrows(0, 1).size());
  }

  public void testThree() {
    assertEquals(3, Categories._3_.objects().size());
    assertEquals(6, Categories._3_.arrows().size());
    assertEquals(1, Categories._3_.arrows(1, 2).size());
  }

  public void testSegment() {
    Category<Integer, Pair<Integer, Integer>> n = Categories.segment(10);
    assertEquals(10, n.objects().size());
    assertEquals(55, n.arrows().size());
  }

  public void testParallelPair() {
    assertEquals(2, Categories.PARALLEL_PAIR.objects().size());
    assertEquals(4, Categories.PARALLEL_PAIR.arrows().size());
    assertEquals(2, Categories.PARALLEL_PAIR.arrows("0", "1").size());
  }

  public void testZ2() {
    int expected = -1;
    String f = "1";
    for (int i = 1; i < 5; i++) {
      f = Categories.Z2.m(f, "-1");
      assertEquals("Error in a^" + i, Integer.toString(expected), f);
      expected = -expected;
    }
  }

  public void testInverse_Z2() {
    assertEquals("1", Categories.Z2.inverse("1"));
    assertEquals("-1", Categories.Z2.inverse("-1"));
  }

  public void testSquare() {
    assertEquals(9, Categories.SQUARE.arrows().size());
    assertEquals(Categories.SQUARE.m("ab", "bd"), Categories.SQUARE.m("ac", "cd"));
  }

  public void testDiscreteCategory() {
    Category<Integer, Integer> d3 = Categories.discreteCategory(Sets.numbers(3));
    assertEquals(3, d3.nodes().size());
    assertEquals(3, d3.arrows().size());
  }

  public void testSetf_validateAsGraph() {
    TypelessSetMorphism arrow = new TypelessSetMorphism(Sets.Set("a", "b", "c"), Sets.Set(1, 2, 3)) {
      @Override
      public Object apply(Object o) {
        return 2;
      }
    };
    assertFalse("Should not be able to enumerate all finite sets", isEnumerable(BigSet.FINITE_SETS));

    final Map<Set, Pair<Set, Set>> bigUnits = Category.buildUnits(BigSet.FINITE_SETS);

    Graph g0 = Graph.Graph(BigSet.FINITE_SETS, bigUnits);

    assertEquals("go.nodes() must be FINITE_SETS: ", BigSet.FINITE_SETS, g0.nodes());
    assertEquals("SETF.nodes() must be FINITE_SETS: ", BigSet.FINITE_SETS, Categories.SETF.nodes());
    assertEquals("SETF.objects() must be FINITE_SETS: ", BigSet.FINITE_SETS, Categories.SETF.objects());
    final Set<Set<Object>> allSets = Categories.SETF.objects();
    assertFalse("Should not be able to enumerate all sets", isEnumerable(allSets));
    assertTrue("Domain for " + arrow + " not defined", allSets.contains(Categories.SETF.d0(arrow)));
    assertTrue("Codomain for " + arrow + " not defined", allSets.contains(Categories.SETF.d1(arrow)));
  }

  @SuppressWarnings("unchecked")
  public void testSetf_validateUnitDomCodom() {
    Set s = Sets.Set("abc", 123);
    TypelessSetMorphism unit = Categories.SETF.unit(s);
    assertTrue("Unit for " + s + " not found", Categories.SETF.arrows().contains(unit));
    assertEquals(s, Categories.SETF.d0(unit));
    assertEquals(s, Categories.SETF.d1(unit));
  }

  public void testSetf_validateUnitsComposition() {
    Set<String> x = Sets.Set("One", "dark", "night");
    Set<Integer> y = Sets.Set(3, 4, 5, 6);
    TypelessSetMorphism x2y = new TypelessSetMorphism(x, y) {
      @Override
      public Object apply(Object o) {
        return o.toString().length();
      }
    };

    TypelessSetMorphism unitX = Categories.SETF.unit(x);
    TypelessSetMorphism unitY = Categories.SETF.unit(y);
    assertEquals(x2y, Categories.SETF.m(unitX, x2y));
    assertEquals(x2y, Categories.SETF.m(x2y, unitY));
  }

  public void testSetf_validateCompositionDomainCodomain() {
    Set<String> x = Sets.Set("One", "dark", "night");
    Set<Integer> y = Sets.Set(3, 4, 5, 6);
    Set<String> z = Sets.Set("1", "2", "3", "4", "5", "6", "7");
    TypelessSetMorphism x2y = new TypelessSetMorphism(x, y) {
      @Override
      public Object apply(Object o) {
        return o.toString().length();
      }
    };

    TypelessSetMorphism y2z = new TypelessSetMorphism(y, z) {
      @Override
      public Object apply(Object o) {
        return o.toString();
      }
    };

    TypelessSetMorphism x2z = Categories.SETF.m(x2y, y2z);
    assertEquals(x, Categories.SETF.d0(x2z));
    assertEquals(z, Categories.SETF.d1(x2z));
  }

  public void testSetf_validateComposition() {
    Set<String> x = Sets.Set("One", "dark", "night");
    Set<Integer> y = Sets.Set(3, 4, 5, 6);
    Set<Integer> z = Sets.Set(1, 2, 3, 4);
    Set<String> t = Sets.Set("0", "1", "2", "3", "4", "5", "6", "7");

    TypelessSetMorphism x2y = new TypelessSetMorphism(x, y) {
      @Override
      public Object apply(Object o) {
        return o.toString().length();
      }
    };

    TypelessSetMorphism y2z = new TypelessSetMorphism(y, z) {
      @Override
      public Object apply(Object o) {
        return ((Integer) o) - 2;
      }
    };

    TypelessSetMorphism z2t = new TypelessSetMorphism(z, t) {
      @Override
      public Object apply(Object o) {
        return o.toString();
      }
    };
    TypelessSetMorphism x2z = Categories.SETF.m(x2y, y2z);
    TypelessSetMorphism y2t = Categories.SETF.m(y2z, z2t);
    TypelessSetMorphism x2t1 = Categories.SETF.m(x2z, z2t);
    TypelessSetMorphism x2t2 = Categories.SETF.m(x2y, y2t);
    assertEquals(x2t1, x2t2);
  }

  public void testSetf_arrows() {
    Set<TypelessSetMorphism> expected = new HashSet<>();
    for (String version : Sets.Set("", "a", "b", "ab")) {
      expected.add(buildAMorphism(version));
    }

    assertEquals(expected, Categories.SETF.arrows(setofAandB, setofTrueAndFalse));
  }

  private TypelessSetMorphism buildAMorphism(final String s) {
    return new TypelessSetMorphism(setofAandB, setofTrueAndFalse) {
      @Override
      public Object apply(Object o) {
        return s.contains(o.toString());
      }
    };
  }

  public void testSetf_inverse() {
    TypelessSetMorphism expected = new TypelessSetMorphism(setofTrueAndFalse, setofAandB) {

      @Override
      public Object apply(Object o) {
        return ((Boolean) o) ? "b" : "a";
      }
    };
    assertEquals(expected, Categories.SETF.inverse(buildAMorphism("b")));
  }

  public void testSetf_isIsomorphism() {
    assertFalse(Categories.SETF.isIsomorphism(buildAMorphism("")));
    assertTrue(Categories.SETF.isIsomorphism(buildAMorphism("a")));
    assertTrue(Categories.SETF.isIsomorphism(buildAMorphism("b")));
    assertFalse(Categories.SETF.isIsomorphism(buildAMorphism("ab")));
  }

  public void testSetf_isMonomorphism() {
    assertFalse(Categories.SETF.isMonomorphism(buildAMorphism("")));
    assertTrue(Categories.SETF.isMonomorphism(buildAMorphism("a")));
    assertTrue(Categories.SETF.isMonomorphism(buildAMorphism("b")));
    assertFalse(Categories.SETF.isMonomorphism(buildAMorphism("ab")));
  }

  public void testSetf_isEpimorphism() {
    assertFalse(Categories.SETF.isEpimorphism(buildAMorphism("")));
    assertTrue(Categories.SETF.isEpimorphism(buildAMorphism("a")));
    assertTrue(Categories.SETF.isEpimorphism(buildAMorphism("b")));
    assertFalse(Categories.SETF.isEpimorphism(buildAMorphism("ab")));
  }

  public void testSetf_equalizer_empty() {
    assertTrue(
        Categories.SETF.equalizer(buildAMorphism("a"), buildAMorphism("b")).domain().isEmpty());
  }

  public void testSetf_equalizer_plain() {
    assertEquals(TypelessSetMorphism.inclusion(Sets.Set("a"), this.setofAandB),
        Categories.SETF.equalizer(buildAMorphism("a"), buildAMorphism("ab")));
  }

  private Set<Integer> numbers(int from, int to) {
    Set<Integer> result = new HashSet<>();
    for (int i = from; i < to; i++) {
      result.add(i);
    }

    return result;
  }

  @SuppressWarnings("unchecked")
  private TypelessSetMorphism shift(Set domain, Set codomain, final int shift) {
    return new TypelessSetMorphism(domain, codomain) {
      @Override
      public Object apply(Object o) {
        return ((Integer) o) + shift;
      }
    };
  }

  @SuppressWarnings("unchecked")
  public void testSetf_coequalizer_plain() {
    Set six = numbers(0, 6);
    Set four = numbers(0, 4);
    TypelessSetMorphism expected =
        TypelessSetMorphism.factorset(six, distanceIs2);
    assertEquals(expected,
        Categories.SETF.coequalizer(shift(four, six, 2), TypelessSetMorphism.inclusion(four, six)));
  }

  @SuppressWarnings("unchecked")
  public void testSetf_coequalizer_multiple() {
    Set six = numbers(0, 6);
    Set four = numbers(0, 4);
    TypelessSetMorphism expected =
        TypelessSetMorphism.factorset(six, distanceIs2);
    assertEquals(expected,
        Categories.SETF.coequalizer(
            Sets.Set(shift(four, six, 2), TypelessSetMorphism.inclusion(four, six)), six));
  }

  @SuppressWarnings("unchecked")
  public void testSetf_product() {
    Set first = Sets.Set("a", "b");
    Set second = numbers(1, 3);
    Pair<TypelessSetMorphism, TypelessSetMorphism> product =
        Categories.SETF.product(first, second);
    assertEquals(Sets.Set(BasePair.Pair("a", 1), BasePair.Pair("a", 2), BasePair.Pair("b", 1), BasePair.Pair("b", 2)),
        product.x().domain());
  }

  @SuppressWarnings("unchecked")
  public void testSetf_union() {
    Set first = Sets.Set("a", "b");
    Set second = Sets.Set("a", "c");
    assertEquals(
        Sets.Set(BasePair.Pair("x", "a"), BasePair.Pair("x", "b"), BasePair.Pair("y", "a"), BasePair.Pair("y", "c")),
        Categories.SETF.union(first, second).x().codomain());
  }

  @SuppressWarnings("unchecked")
  public void testSetf_pullback() {
    TypelessSetMorphism f = shift(numbers(1, 4), numbers(0, 4), 0);
    TypelessSetMorphism g = shift(numbers(0, 2), numbers(0, 4), 2);
    Set expectedSet = Sets.Set(BasePair.Pair(2, 0), BasePair.Pair(3, 1));
    TypelessSetMorphism expectedX = new TypelessSetMorphism(expectedSet, numbers(1, 4)) {
      @Override
      public Object apply(Object p) {
        return ((Pair) p).x();
      }
    };
    TypelessSetMorphism expectedY = new TypelessSetMorphism(expectedSet, numbers(0, 2)) {
      @Override
      public Object apply(Object p) {
        return ((Pair) p).y();
      }
    };
    Pair<TypelessSetMorphism, TypelessSetMorphism> expected =
        BasePair.Pair(expectedX, expectedY);
    assertEquals(expected, Categories.SETF.pullback(f, g));
  }

  @SuppressWarnings("unchecked")
  public void testSetf_pushout() {
    Set three = numbers(0, 3);
    Set oneToThree = numbers(1, 4);
    Set twoToFour = numbers(2, 5);
    final Set<Pair<String, Integer>>[] expectedValues = Base.array(
            Sets.Set(BasePair.Pair("x", 1), BasePair.Pair("y", 2)),
            Sets.Set(BasePair.Pair("x", 2), BasePair.Pair("y", 3)),
            Sets.Set(BasePair.Pair("x", 3), BasePair.Pair("y", 4))
    );

    Set<Set<Pair<String, Integer>>> expectedSet = Sets.Set(Arrays.asList(expectedValues));

    TypelessSetMorphism f = shift(three, oneToThree, 1);
    TypelessSetMorphism g = shift(three, twoToFour, 2);
    TypelessSetMorphism expectedX = new TypelessSetMorphism(oneToThree, expectedSet) {
      @Override
      public Object apply(Object o) {
        return expectedValues[((Integer) o) - 1];
      }
    };
    TypelessSetMorphism expectedY = new TypelessSetMorphism(twoToFour, expectedSet) {
      @Override
      public Object apply(Object o) {
        return expectedValues[((Integer) o) - 2];
      }
    };
    Pair<TypelessSetMorphism, TypelessSetMorphism> expected =
        BasePair.Pair(expectedX, expectedY);
    assertEquals(expected, Categories.SETF.pushout(f, g));
  }

  @SuppressWarnings("unchecked")
  public void testSetf_degree() {
    Set x = Sets.Set("a", "b");
    Pair<Set, List<TypelessSetMorphism>> degree = Categories.SETF.degree(x, 3);
    assertEquals(8, degree.x().size());
    Map elementOfProduct = Base.Map(new Integer[]{0, 1, 2}, new String[]{"b", "b", "a"});
    assertEquals("a", degree.y().get(2).apply(elementOfProduct));
  }
}
