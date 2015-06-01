package math.cat;

import static java.math.cat.Base.Map;
import static java.math.cat.Base.array;
import static java.math.cat.BasePair.Pair;
import static java.math.cat.Categories.PARALLEL_PAIR;
import static java.math.cat.Categories.SETF;
import static java.math.cat.Categories.SQUARE;
import static java.math.cat.Categories.Z2;
import static java.math.cat.Categories._0_;
import static java.math.cat.Categories._1_;
import static java.math.cat.Categories._2_;
import static java.math.cat.Categories._3_;
import static java.math.cat.Categories.discreteCategory;
import static java.math.cat.Categories.segment;
import static java.math.cat.Sets.Set;

import java.math.cat.BinaryRelationship;
import java.math.cat.Pair;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import junit.framework.TestCase;

/**
 * Unittest for Categories class
 * 
 * @author Vlad Patryshev
 * All source code is stored on <a href="http://code.google.com/p/categories/">http://code.google.com/p/categories/</a>
 * 
 */
public class CategoriesTest extends TestCase {
  private final Set<String> setofAandB = Set("a", "b");
  private final Set<Boolean> setofTrueAndFalse = Set(true, false);
  private final java.math.cat.BinaryRelationship<Integer, Integer> distanceIs2 =
      new BinaryRelationship<Integer, Integer>() {
        @Override
        public boolean eval(java.math.cat.Pair<Integer, Integer> p) {
          return p.x() - p.y() == 2;
        }
      };

  public void testZero() {
    assertTrue(_0_.objects().isEmpty());
    assertTrue(_0_.arrows().isEmpty());
  }

  public void testOne() {
    assertEquals(1, _1_.objects().size());
    assertEquals(1, _1_.arrows().size());
  }

  public void testTwo() {
    assertEquals(2, _2_.objects().size());
    assertEquals(3, _2_.arrows().size());
    assertEquals(1, _2_.arrows(0, 1).size());
  }

  public void testThree() {
    assertEquals(3, _3_.objects().size());
    assertEquals(6, _3_.arrows().size());
    assertEquals(1, _3_.arrows(1, 2).size());
  }

  public void testSegment() {
    Category<Integer, java.math.cat.Pair<Integer, Integer>> n = segment(10);
    assertEquals(10, n.objects().size());
    assertEquals(55, n.arrows().size());
  }

  public void testParallelPair() {
    assertEquals(2, PARALLEL_PAIR.objects().size());
    assertEquals(4, PARALLEL_PAIR.arrows().size());
    assertEquals(2, PARALLEL_PAIR.arrows("0", "1").size());
  }

  public void testZ2() {
    int expected = -1;
    String f = "1";
    for (int i = 1; i < 5; i++) {
      f = Z2.m(f, "-1");
      assertEquals("Error in (-1)^" + i, Integer.toString(expected), f);
      expected = -expected;
    }
  }

  public void testInverse_Z2() {
    assertEquals("1", Z2.inverse("1"));
    assertEquals("-1", Z2.inverse("-1"));
  }

  public void testSquare() {
    assertEquals(9, SQUARE.arrows().size());
    assertEquals(SQUARE.m("ab", "bd"), SQUARE.m("ac", "cd"));
  }

  public void testDiscreteCategory() {
    Category<Integer, Integer> d3 = discreteCategory(java.math.cat.Sets.numbers(3));
    assertEquals(3, d3.nodes().size());
    assertEquals(3, d3.arrows().size());
  }

  public void testSetf_validateAsGraph() {
    java.math.cat.TypelessSetMorphism arrow = new java.math.cat.TypelessSetMorphism(Set("a", "b", "c"), Set(1, 2, 3)) {
      @Override
      public Object apply(Object o) {
        return 2;
      }
    };
    assertTrue("Domain for " + arrow + " not defined", SETF.objects().contains(SETF.d0(arrow)));
    assertTrue("Codomain for " + arrow + " not defined", SETF.objects().contains(SETF.d1(arrow)));
  }

  @SuppressWarnings("unchecked")
  public void testSetf_validateUnitDomCodom() {
    Set s = Set("abc", 123);
    java.math.cat.TypelessSetMorphism unit = SETF.unit(s);
    assertTrue("Unit for " + s + " not found", SETF.arrows().contains(unit));
    assertEquals(s, SETF.d0(unit));
    assertEquals(s, SETF.d1(unit));
  }

  public void testSetf_validateUnitsComposition() {
    Set<String> x = Set("One", "dark", "night");
    Set<Integer> y = Set(3, 4, 5, 6);
    java.math.cat.TypelessSetMorphism x2y = new java.math.cat.TypelessSetMorphism(x, y) {
      @Override
      public Object apply(Object o) {
        return o.toString().length();
      }
    };

    java.math.cat.TypelessSetMorphism unitX = SETF.unit(x);
    java.math.cat.TypelessSetMorphism unitY = SETF.unit(y);
    assertEquals(x2y, SETF.m(unitX, x2y));
    assertEquals(x2y, SETF.m(x2y, unitY));
  }

  public void testSetf_validateCompositionDomainCodomain() {
    Set<String> x = Set("One", "dark", "night");
    Set<Integer> y = Set(3, 4, 5, 6);
    Set<String> z = Set("1", "2", "3", "4", "5", "6", "7");
    java.math.cat.TypelessSetMorphism x2y = new java.math.cat.TypelessSetMorphism(x, y) {
      @Override
      public Object apply(Object o) {
        return o.toString().length();
      }
    };

    java.math.cat.TypelessSetMorphism y2z = new java.math.cat.TypelessSetMorphism(y, z) {
      @Override
      public Object apply(Object o) {
        return o.toString();
      }
    };

    java.math.cat.TypelessSetMorphism x2z = SETF.m(x2y, y2z);
    assertEquals(x, SETF.d0(x2z));
    assertEquals(z, SETF.d1(x2z));
  }

  public void testSetf_validateComposition() {
    Set<String> x = Set("One", "dark", "night");
    Set<Integer> y = Set(3, 4, 5, 6);
    Set<Integer> z = Set(1, 2, 3, 4);
    Set<String> t = Set("0", "1", "2", "3", "4", "5", "6", "7");

    java.math.cat.TypelessSetMorphism x2y = new java.math.cat.TypelessSetMorphism(x, y) {
      @Override
      public Object apply(Object o) {
        return o.toString().length();
      }
    };

    java.math.cat.TypelessSetMorphism y2z = new java.math.cat.TypelessSetMorphism(y, z) {
      @Override
      public Object apply(Object o) {
        return ((Integer) o) - 2;
      }
    };

    java.math.cat.TypelessSetMorphism z2t = new java.math.cat.TypelessSetMorphism(z, t) {
      @Override
      public Object apply(Object o) {
        return o.toString();
      }
    };
    java.math.cat.TypelessSetMorphism x2z = SETF.m(x2y, y2z);
    java.math.cat.TypelessSetMorphism y2t = SETF.m(y2z, z2t);
    java.math.cat.TypelessSetMorphism x2t1 = SETF.m(x2z, z2t);
    java.math.cat.TypelessSetMorphism x2t2 = SETF.m(x2y, y2t);
    assertEquals(x2t1, x2t2);
  }

  public void testSetf_arrows() {
    Set<java.math.cat.TypelessSetMorphism> expected = new HashSet<java.math.cat.TypelessSetMorphism>();
    for (String version : Set("", "a", "b", "ab")) {
      expected.add(buildAMorphism(version));
    }

    assertEquals(expected, SETF.arrows(setofAandB, setofTrueAndFalse));
  }

  private java.math.cat.TypelessSetMorphism buildAMorphism(final String s) {
    return new java.math.cat.TypelessSetMorphism(setofAandB, setofTrueAndFalse) {
      @Override
      public Object apply(Object o) {
        return s.indexOf(o.toString()) >= 0;
      }
    };
  }

  public void testSetf_inverse() {
    java.math.cat.TypelessSetMorphism expected = new java.math.cat.TypelessSetMorphism(setofTrueAndFalse, setofAandB) {

      @Override
      public Object apply(Object o) {
        return ((Boolean) o) ? "b" : "a";
      }
    };
    assertEquals(expected, SETF.inverse(buildAMorphism("b")));
  }

  public void testSetf_isIsomorphism() {
    assertFalse(SETF.isIsomorphism(buildAMorphism("")));
    assertTrue(SETF.isIsomorphism(buildAMorphism("a")));
    assertTrue(SETF.isIsomorphism(buildAMorphism("b")));
    assertFalse(SETF.isIsomorphism(buildAMorphism("ab")));
  }

  public void testSetf_isMonomorphism() {
    assertFalse(SETF.isMonomorphism(buildAMorphism("")));
    assertTrue(SETF.isMonomorphism(buildAMorphism("a")));
    assertTrue(SETF.isMonomorphism(buildAMorphism("b")));
    assertFalse(SETF.isMonomorphism(buildAMorphism("ab")));
  }

  public void testSetf_isEpimorphism() {
    assertFalse(SETF.isEpimorphism(buildAMorphism("")));
    assertTrue(SETF.isEpimorphism(buildAMorphism("a")));
    assertTrue(SETF.isEpimorphism(buildAMorphism("b")));
    assertFalse(SETF.isEpimorphism(buildAMorphism("ab")));
  }

  public void testSetf_equalizer_empty() {
    SETF.equalizer(buildAMorphism("a"), buildAMorphism("b")).domain().isEmpty();
  }

  public void testSetf_equalizer_plain() {
    assertEquals(java.math.cat.TypelessSetMorphism.inclusion(Set("a"), this.setofAandB),
        SETF.equalizer(buildAMorphism("a"), buildAMorphism("ab")));
  }

  Set<Integer> numbers(int from, int to) {
    Set<Integer> result = new HashSet<Integer>();
    for (int i = from; i < to; i++) {
      result.add(i);
    }

    return result;
  }

  @SuppressWarnings("unchecked")
  java.math.cat.TypelessSetMorphism shift(Set domain, Set codomain, final int shift) {
    return new java.math.cat.TypelessSetMorphism(domain, codomain) {
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
    java.math.cat.TypelessSetMorphism expected =
        java.math.cat.TypelessSetMorphism.factorset(six, distanceIs2);
    assertEquals(expected,
        SETF.coequalizer(shift(four, six, 2), java.math.cat.TypelessSetMorphism.inclusion(four, six)));
  }

  @SuppressWarnings("unchecked")
  public void testSetf_coequalizer_multiple() {
    Set six = numbers(0, 6);
    Set four = numbers(0, 4);
    java.math.cat.TypelessSetMorphism expected =
        java.math.cat.TypelessSetMorphism.factorset(six, distanceIs2);
    assertEquals(expected,
        SETF.coequalizer(
            Set(shift(four, six, 2), java.math.cat.TypelessSetMorphism.inclusion(four, six)), six));
  }

  @SuppressWarnings("unchecked")
  public void testSetf_product() {
    Set first = Set("a", "b");
    Set second = numbers(1, 3);
    java.math.cat.Pair<java.math.cat.TypelessSetMorphism, TypelessSetMorphism> product =
        SETF.product(first, second);
    assertEquals(Set(Pair("a", 1), Pair("a", 2), Pair("b", 1), Pair("b", 2)),
        product.x().domain());
  }

  @SuppressWarnings("unchecked")
  public void testSetf_union() {
    Set first = Set("a", "b");
    Set second = Set("a", "c");
    assertEquals(
        Set(Pair("x", "a"), Pair("x", "b"), Pair("y", "a"), Pair("y", "c")),
        SETF.union(first, second).x().codomain());
  }

  @SuppressWarnings("unchecked")
  public void testSetf_pullback() {
    java.math.cat.TypelessSetMorphism f = shift(numbers(1, 4), numbers(0, 4), 0);
    java.math.cat.TypelessSetMorphism g = shift(numbers(0, 2), numbers(0, 4), 2);
    Set expectedSet = Set(Pair(2, 0), Pair(3, 1));
    java.math.cat.TypelessSetMorphism expectedX = new java.math.cat.TypelessSetMorphism(expectedSet, numbers(1, 4)) {
      @Override
      public Object apply(Object p) {
        return ((java.math.cat.Pair) p).x();
      }
    };
    java.math.cat.TypelessSetMorphism expectedY = new java.math.cat.TypelessSetMorphism(expectedSet, numbers(0, 2)) {
      @Override
      public Object apply(Object p) {
        return ((java.math.cat.Pair) p).y();
      }
    };
    java.math.cat.Pair<java.math.cat.TypelessSetMorphism, java.math.cat.TypelessSetMorphism> expected =
        Pair(expectedX, expectedY);
    assertEquals(expected, SETF.pullback(f, g));
  }

  @SuppressWarnings("unchecked")
  public void testSetf_pushout() {
    Set three = numbers(0, 3);
    Set oneToThree = numbers(1, 4);
    Set twoToFour = numbers(2, 5);
    final Set<java.math.cat.Pair<String, Integer>>[] expectedValues = array(
        Set(Pair("x", 1), Pair("y", 2)),
        Set(Pair("x", 2), Pair("y", 3)),
        Set(Pair("x", 3), Pair("y", 4))
    );

    Set<Set<java.math.cat.Pair<String, Integer>>> expectedSet = Set(Arrays.asList(expectedValues));

    java.math.cat.TypelessSetMorphism f = shift(three, oneToThree, 1);
    java.math.cat.TypelessSetMorphism g = shift(three, twoToFour, 2);
    java.math.cat.TypelessSetMorphism expectedX = new java.math.cat.TypelessSetMorphism(oneToThree, expectedSet) {
      @Override
      public Object apply(Object o) {
        return expectedValues[((Integer) o) - 1];
      }
    };
    java.math.cat.TypelessSetMorphism expectedY = new java.math.cat.TypelessSetMorphism(twoToFour, expectedSet) {
      @Override
      public Object apply(Object o) {
        return expectedValues[((Integer) o) - 2];
      }
    };
    Pair<java.math.cat.TypelessSetMorphism, java.math.cat.TypelessSetMorphism> expected =
        Pair(expectedX, expectedY);
    assertEquals(expected, SETF.pushout(f, g));
  }

  @SuppressWarnings("unchecked")
  public void testSetf_degree() {
    Set x = Set("a", "b");
    java.math.cat.Pair<Set, List<java.math.cat.TypelessSetMorphism>> degree = SETF.degree(x, 3);
    assertEquals(8, degree.x().size());
    Map elementOfProduct = Map(new Integer[]{0, 1, 2}, new String[]{"b", "b", "a"});
    assertEquals("a", degree.y().get(2).apply(elementOfProduct));
  }
}
