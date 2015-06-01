package math.cat;

import static math.cat.Base.Map;
import static math.cat.Base.array;
import static math.cat.BasePair.Pair;
import static math.cat.Categories.M;
import static math.cat.Categories.PARALLEL_PAIR;
import static math.cat.Categories.PULLBACK;
import static math.cat.Categories.SQUARE;
import static math.cat.Categories.W;
import static math.cat.Categories.Z2;
import static math.cat.Categories._3_;
import static math.cat.Categories._4_;
import static math.cat.Category.Category;
import static math.cat.Category.buildCategory;
import static math.cat.Graph.Graph;
import static math.cat.Sets.Set;
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
    Category<String, String> testCategory = buildCategory(Set("0", "1", "2"),
        Map(array("0.1", "0.2", "a", "b", "2.1", "2.a", "2.b", "2.swap"), // d0
            array("0", "0", "1", "1", "2", "2", "2", "2")),
        Map(array("0.1", "0.2", "a", "b", "2.1", "2.a", "2.b", "2.swap"), // d1
            array("1", "2", "2", "2", "1", "2", "2", "2")),
        Map(array(Pair("0.1", "a"), Pair("0.1", "b"), Pair("2.1", "a"), Pair("2.1", "b"), Pair("a", "2.swap"), Pair("b", "2.swap"), Pair("2.swap", "2.swap")), // composition map
            array("0.2", "0.2", "2.a", "2.b", "b", "a", "2"))
    );
    assertEquals(testCategory, Category(testCategory.toString()));
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
            Graph(Set("0", "1", "2"), Map(array("eq", "a", "b", "0.2"), array(Pair("0", "1"), Pair("1", "2"), Pair("1", "2"), Pair("0", "2")))),
            Map(array(Pair("eq", "a"), Pair("eq", "b")), array("0.2", "0.2"))
        );
    assertEquals(Set("eq"), c.allEqualizingArrows("a", "b"));
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
            Graph(Set("0", "1", "2"), Map(array("eq", "a", "b", "0.2"), array(Pair("0", "1"), Pair("1", "2"), Pair("1", "2"), Pair("0", "2")))),
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
            Graph(Set("0", "1", "2"), Map(array("coeq", "a", "b", "0.2"), array(Pair("1", "2"), Pair("0", "1"), Pair("0", "1"), Pair("0", "2")))),
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
    assertEquals(Pair("ab", "ac"), c.product("b", "c"));
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
    assertTrue(SQUARE.isTerminal.eval("d"));
    assertTrue(_4_.isTerminal.eval(3));
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
    assertEquals(Integer.valueOf(3), _4_.terminal());
  }

  public void testInitial_none() {
    assertNull(Z2.initial());
    assertNull(PARALLEL_PAIR.initial());
  }

  public void testInitial_misc() {
    assertEquals("a", SQUARE.initial());
    assertEquals(Integer.valueOf(0), _4_.initial());
  }

  public void testAllInitialObjects() {
    assertEquals(Set("0"), PARALLEL_PAIR.allInitialObjects());
    assertEquals(Set("a"), SQUARE.allInitialObjects());
    assertEquals(Set("a", "b"), PULLBACK.allInitialObjects());
    assertEquals(Set("b", "d"), M.allInitialObjects());
    assertEquals(Set("a", "c", "e"), W.allInitialObjects());
  }
}