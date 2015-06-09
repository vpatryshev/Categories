package j.math.cat;

import static j.math.cat.Base.Map;
import static j.math.cat.Base.array;
import static j.math.cat.BasePair.Pair;
import static j.math.cat.Category.*;
import static j.math.cat.Categories.*;
import static j.math.cat.Graph.Graph;
import static j.math.cat.Sets.Set;
import junit.framework.TestCase;

/**
 * Unittest for Category class
 * 
 * @author Vlad Patryshev
 * All source code is stored on <a href="http://code.google.com/p/categories/">http://code.google.com/p/categories/</a>
 * 
 */
public class CategoryTest extends TestCase {
  
  @SuppressWarnings("unchecked")
  public void testParse() {
    Category<String, String> testCategory = buildCategory(Sets.Set("0", "1", "2"),
        Map(array("0.1", "0.2", "a", "b", "2.1", "2.a", "2.b", "2.swap"), // d0
            array("0", "0", "1", "1", "2", "2", "2", "2")),
        Map(array("0.1", "0.2", "a", "b", "2.1", "2.a", "2.b", "2.swap"), // d1
            array("1", "2", "2", "2", "1", "2", "2", "2")),
        Map(array(Pair("0.1", "a"), Pair("0.1", "b"), Pair("2.1", "a"), Pair("2.1", "b"), Pair("a", "2.swap"), Pair("b", "2.swap"), Pair("2.swap", "2.swap")), // composition map
            array("0.2", "0.2", "2.a", "2.b", "b", "a", "2"))
    );
    assertEquals(testCategory, Category(testCategory.toString()));
  }

  public void testFunctorRegression() {
    Category c = Category("(([0,1,2], {a: 0 -> 2, b: 1 -> 2}))");
    assertTrue("0 should be there", c.objects().contains("0"));
  }

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
    Category<Integer, Pair<Integer, Integer>> n = segment(100);
    assertEquals(100, n.objects().size());
    assertEquals(5050, n.arrows().size());
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

  private Integer Int(int n) { return Integer.valueOf(n); }

  public void test_4_() {
    for (int i = 0; i < 4; i++ ) {
      assertEquals("unit for " + i + " missing?", Pair(Int(i), Int(i)), _4_.unit(Int(i)));
      for (int j = 0; j < 4; j++ ) {
        int expected = i <= j ? 1 : 0;
        assertEquals("Expected " + expected + "arrows from " + i + " to " + j,
                expected, _4_.arrows(Int(i), Int(j)).size());
      }
    }

  }

  public void testInverse_Z2() {
    assertEquals("1", Z2.inverse("1"));
    assertEquals("-1", Z2.inverse("-1"));
  }

  public void testInverse_noneActually() {
    assertEquals(Pair(1, 1), _3_.inverse(Pair(1, 1)));
  }

  public void testEqualizingArrows_pairNotParallel() {
    try {
      _3_.allEqualizingArrows(Pair(0, 1), Pair(1, 2));
      fail("Should have thrown an exception, pair is not parallel");
    } catch (Throwable e) {
      // as expected
    }
  }

  public void testEqualizingArrows_none() {
    Category<String, String> c = PARALLEL_PAIR;
    assertTrue(c.allEqualizingArrows("a", "b").isEmpty());
  }

  @SuppressWarnings("unchecked")
  public void testEqualizingArrows_plain() {
    Category<String, String> c =
        Category(
            Graph(Sets.Set("0", "1", "2"), Map(array("eq", "a", "b", "0.2"), array(Pair("0", "1"), Pair("1", "2"), Pair("1", "2"), Pair("0", "2")))),
            Map(array(Pair("eq", "a"), Pair("eq", "b")), array("0.2", "0.2"))
        );
    assertEquals(Sets.Set("eq"), c.allEqualizingArrows("a", "b"));
  }

  public void testEqualizer_pairNotParallel() {
    try {
      _3_.equalizer(Pair(0, 1), Pair(1, 2));
      fail("Should have thrown an exception, pair is not parallel");
    } catch (Throwable e) {
      // as expected
    }
  }

  public void testEqualizer_none() {
    Category<String, String> c = PARALLEL_PAIR;
    String eq = c.equalizer("a", "b");
    assertNull(eq);
  }

  @SuppressWarnings("unchecked")
  public void testEqualizer_plain() {
    Category<String, String> c =
        Category(
            Graph(Sets.Set("0", "1", "2"), Map(array("eq", "a", "b", "0.2"), array(Pair("0", "1"), Pair("1", "2"), Pair("1", "2"), Pair("0", "2")))),
            Map(array(Pair("eq", "a"), Pair("eq", "b")), array("0.2", "0.2"))
        );
    assertEquals("eq", c.equalizer("a", "b"));
  }

  public void testCoEqualizer_none() {
    Category<String, String> c = PARALLEL_PAIR;
    String coeq = c.coequalizer("a", "b");
    assertNull(coeq);
  }

  @SuppressWarnings("unchecked")
  public void testCoEqualizer_plain() {
    Category<String, String> c =
        Category(
            Graph(Sets.Set("0", "1", "2"), Map(array("coeq", "a", "b", "0.2"), array(Pair("1", "2"), Pair("0", "1"), Pair("0", "1"), Pair("0", "2")))),
            Map(array(Pair("a", "coeq"), Pair("b", "coeq")), array("0.2", "0.2"))
        );
    assertEquals("coeq", c.coequalizer("a", "b"));
  }

  public void testProduct_none() {
    Category<String, String> c = PARALLEL_PAIR;
    Pair<String, String> p = c.product("0", "1");
    assertNull(p);
  }

  public void testProduct_plain() {
    Category<String, String> c = SQUARE;

    final Pair<String, String> expected = Pair("ab", "ac");
    String obj = c.d0(expected.getKey());
    assertEquals("a", obj);
    assertTrue("ab and ac is a product of b, c", c.isProduct(expected, "b", "c"));
    assertEquals(expected, c.product("b", "c"));
  }

  public void testUnion_none() {
    Category<String, String> c = PARALLEL_PAIR;
    Pair<String, String> p = c.union("0", "1");
    assertNull(p);
  }

  public void testUnion_plain() {
    Category<String, String> c = SQUARE;
    assertEquals(Pair("bd", "cd"), c.union("b", "c"));
  }

  public void testPullback_none() {
    Category<String, String> c = PARALLEL_PAIR;
    Pair<String, String> p = c.pullback("a", "b");
    assertNull(p);
  }

  public void testPullback_same() {
    Category<String, String> c = PARALLEL_PAIR;
    Pair<String, String> p = c.pullback("a", "a");
    assertNotNull(p);
    assertEquals(p.x(), "0");
    assertEquals(p.y(), "0");
  }

  public void testPullback_plain() {
    Category<String, String> c = SQUARE;
    assertEquals(Pair("ab", "ac"), c.pullback("bd", "cd"));
  }

  public void testPushout_none() {
    Category<String, String> c = PARALLEL_PAIR;
    Pair<String, String> p = c.pushout("a", "b");
    assertNull(p);
  }

  public void testPushout_same() {
    Category<String, String> c = PARALLEL_PAIR;
    Pair<String, String> p = c.pushout("a", "a");
    assertNotNull(p);
    assertEquals(p.x(), "1");
    assertEquals(p.y(), "1");
  }

  public void testPushout_plain() {
    Category<String, String> c = SQUARE;
    assertEquals(Pair("bd", "cd"), c.pushout("ab", "ac"));
  }

  public void testTerminal_none() {
    assertNull(Z2.terminal());
    assertNull(PARALLEL_PAIR.terminal());
  }

  public void testIsTerminal_positive() {
    /*
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

     */

    assertTrue("3 is terminal in _4_", _4_.isTerminal.eval(3));
    assertEquals("deconstructed: d is terminal in square, a",
            1, SQUARE.arrows("a", "d").size());
    assertEquals("deconstructed: d is terminal in square, b",
            1, SQUARE.arrows("b", "d").size());
    assertEquals("deconstructed: d is terminal in square, c",
            1, SQUARE.arrows("c", "d").size());
    String _1d = SQUARE.unit("d");
    assertEquals("Where's d.unit?", "d", SQUARE.unit("d"));
    assertEquals("unit not a unit at d0?", "d", SQUARE.d0(_1d));
    assertEquals("unit not a unit at d1?", "d", SQUARE.d1(_1d));
    assertEquals("deconstructed: d is terminal in square, d",
            1, SQUARE.arrows("d", "d").size());
    assertTrue("d is terminal in square", SQUARE.isTerminal.eval("d"));
  }

  public void testIsTerminal_negative() {
    assertFalse(SQUARE.isTerminal.eval("a"));
    assertFalse(SQUARE.isTerminal.eval("b"));
    assertFalse(_4_.isTerminal.eval(0));
    assertFalse(_4_.isTerminal.eval(1));
    assertFalse(PARALLEL_PAIR.isTerminal.eval("0"));
    assertFalse(PARALLEL_PAIR.isTerminal.eval("1"));
  }

  public void testTerminal_misc() {
    assertEquals("d", SQUARE.terminal());
    assertEquals(Int(3), _4_.terminal());
  }

  public void testInitial_none() {
    assertNull(Z2.initial());
    assertNull(PARALLEL_PAIR.initial());
  }

  public void testInitial_misc() {
    assertEquals("a", SQUARE.initial());
    assertTrue("0 must be initial in _4_", _4_.isInitial.eval(Int(0)));
    assertEquals(Int(0), _4_.initial());
  }

  public void testAllInitialObjects() {
    assertEquals(Sets.Set("0"), PARALLEL_PAIR.allInitialObjects());
    assertEquals(Sets.Set("a"), SQUARE.allInitialObjects());
    assertEquals(Sets.Set("a", "b"), PULLBACK.allInitialObjects());
    assertEquals(Sets.Set("b", "d"), M.allInitialObjects());
    assertEquals(Sets.Set("a", "c", "e"), W.allInitialObjects());
  }
}