package math.cat;

import static math.cat.Base.Map;
import static math.cat.Base.array;
import static math.cat.BasePair.Pair;
import static math.cat.Categories.PARALLEL_PAIR;
import static math.cat.Categories.SETF;
import static math.cat.Categories.SQUARE;
import static math.cat.Categories.Z2;
import static math.cat.Categories._0_;
import static math.cat.Categories._1_;
import static math.cat.Categories._2_;
import static math.cat.Categories._3_;
import static math.cat.Categories.discreteCategory;
import static math.cat.Categories.segment;
import static math.cat.Sets.Set;

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
  private final BinaryRelationship<Integer, Integer> distanceIs2 =
      new BinaryRelationship<Integer, Integer>() {
        @Override
        public boolean eval(Pair<Integer, Integer> p) {
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
    Category<Integer, Pair<Integer, Integer>> n = segment(10);
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
    Category<Integer, Integer> d3 = discreteCategory(Sets.numbers(3));
    assertEquals(3, d3.nodes().size());
    assertEquals(3, d3.arrows().size());
  }

  public void testSetf_validateAsGraph() {
    TypelessSetMorphism arrow = new TypelessSetMorphism(Set("a", "b", "c"), Set(1, 2, 3)) {
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
    TypelessSetMorphism unit = SETF.unit(s);
    assertTrue("Unit for " + s + " not found", SETF.arrows().contains(unit));
    assertEquals(s, SETF.d0(unit));
    assertEquals(s, SETF.d1(unit));
  }

  public void testSetf_validateUnitsComposition() {
    Set<String> x = Set("One", "dark", "night");
    Set<Integer> y = Set(3, 4, 5, 6);
    TypelessSetMorphism x2y = new TypelessSetMorphism(x, y) {
      @Override
      public Object apply(Object o) {
        return o.toString().length();
      }
    };

    TypelessSetMorphism unitX = SETF.unit(x);
    TypelessSetMorphism unitY = SETF.unit(y);
    assertEquals(x2y, SETF.m(unitX, x2y));
    assertEquals(x2y, SETF.m(x2y, unitY));
  }

  public void testSetf_validateCompositionDomainCodomain() {
    Set<String> x = Set("One", "dark", "night");
    Set<Integer> y = Set(3, 4, 5, 6);
    Set<String> z = Set("1", "2", "3", "4", "5", "6", "7");
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

    TypelessSetMorphism x2z = SETF.m(x2y, y2z);
    assertEquals(x, SETF.d0(x2z));
    assertEquals(z, SETF.d1(x2z));
  }

  public void testSetf_validateComposition() {
    Set<String> x = Set("One", "dark", "night");
    Set<Integer> y = Set(3, 4, 5, 6);
    Set<Integer> z = Set(1, 2, 3, 4);
    Set<String> t = Set("0", "1", "2", "3", "4", "5", "6", "7");

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
    TypelessSetMorphism x2z = SETF.m(x2y, y2z);
    TypelessSetMorphism y2t = SETF.m(y2z, z2t);
    TypelessSetMorphism x2t1 = SETF.m(x2z, z2t);
    TypelessSetMorphism x2t2 = SETF.m(x2y, y2t);
    assertEquals(x2t1, x2t2);
  }

  public void testSetf_arrows() {
    Set<TypelessSetMorphism> expected = new HashSet<TypelessSetMorphism>();
    for (String version : Set("", "a", "b", "ab")) {
      expected.add(buildAMorphism(version));
    }

    assertEquals(expected, SETF.arrows(setofAandB, setofTrueAndFalse));
  }

  private TypelessSetMorphism buildAMorphism(final String s) {
    return new TypelessSetMorphism(setofAandB, setofTrueAndFalse) {
      @Override
      public Object apply(Object o) {
        return s.indexOf(o.toString()) >= 0;
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
    assertEquals(TypelessSetMorphism.inclusion(Set("a"), this.setofAandB),
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
  TypelessSetMorphism shift(Set domain, Set codomain, final int shift) {
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
        SETF.coequalizer(shift(four, six, 2), TypelessSetMorphism.inclusion(four, six)));
  }

  @SuppressWarnings("unchecked")
  public void testSetf_coequalizer_multiple() {
    Set six = numbers(0, 6);
    Set four = numbers(0, 4);
    TypelessSetMorphism expected =
        TypelessSetMorphism.factorset(six, distanceIs2);
    assertEquals(expected,
        SETF.coequalizer(
            Set(shift(four, six, 2), TypelessSetMorphism.inclusion(four, six)), six));
  }

  @SuppressWarnings("unchecked")
  public void testSetf_product() {
    Set first = Set("a", "b");
    Set second = numbers(1, 3);
    Pair<TypelessSetMorphism, TypelessSetMorphism> product =
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
    TypelessSetMorphism f = shift(numbers(1, 4), numbers(0, 4), 0);
    TypelessSetMorphism g = shift(numbers(0, 2), numbers(0, 4), 2);
    Set expectedSet = Set(Pair(2, 0), Pair(3, 1));
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
        Pair(expectedX, expectedY);
    assertEquals(expected, SETF.pullback(f, g));
  }

  @SuppressWarnings("unchecked")
  public void testSetf_pushout() {
    Set three = numbers(0, 3);
    Set oneToThree = numbers(1, 4);
    Set twoToFour = numbers(2, 5);
    final Set<Pair<String, Integer>>[] expectedValues = array(
        Set(Pair("x", 1), Pair("y", 2)),
        Set(Pair("x", 2), Pair("y", 3)),
        Set(Pair("x", 3), Pair("y", 4))
    );

    Set<Set<Pair<String, Integer>>> expectedSet = Set(Arrays.asList(expectedValues));

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
        Pair(expectedX, expectedY);
    assertEquals(expected, SETF.pushout(f, g));
  }

  @SuppressWarnings("unchecked")
  public void testSetf_degree() {
    Set x = Set("a", "b");
    Pair<Set, List<TypelessSetMorphism>> degree = SETF.degree(x, 3);
    assertEquals(8, degree.x().size());
    Map elementOfProduct = Map(new Integer[]{0, 1, 2}, new String[]{"b", "b", "a"});
    assertEquals("a", degree.y().get(2).apply(elementOfProduct));
  }
}
