package math.cat;

import junit.framework.TestCase;
import static math.cat.Base.*;
import static math.cat.Category.*;
import static math.cat.Pair.Pair;

public class CategoryTest extends TestCase {
  public void testParse() {
    Category<String,String> testCategory = buildCategory(Set("0", "1", "2"),
        Map(array("0.1", "0.2", "a",  "b", "2.1", "2.a", "2.b",   "2.swap"), // d0
            array("0",   "0",   "1",  "1", "2",   "2",   "2",   "2")),
        Map(array("0.1", "0.2", "a",  "b", "2.1", "2.a", "2.b",   "2.swap"), // d1
            array("1",   "2",   "2",  "2", "1", "2",   "2",   "2")),
        Map(array(Pair("0.1", "a"), Pair("0.1", "b"), Pair("2.1", "a"), Pair("2.1", "b"), Pair("a", "2.swap"), Pair("b", "2.swap"), Pair("2.swap", "2.swap")), // composition map
            array(     "0.2",            "0.2",            "2.a",            "2.b",            "b",                 "a",                 "2"))
        );
    assertEquals(testCategory, Category(testCategory.toString()));
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

  public void testInverse_Z2() {
    assertEquals("1", Z2.inverse("1"));
    assertEquals("-1", Z2.inverse("-1"));
  }

  public void testInverse_noneActually() {
    assertEquals(Pair(1, 1), _3_.inverse(Pair(1, 1)));
    assertEquals(null, Z2.inverse("-1"));
  }
}