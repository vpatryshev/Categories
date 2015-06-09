package j.math.cat;

import static j.math.cat.BasePair.Pair;
import static j.math.cat.Category.Category;
import static j.math.cat.Sets.Set;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import junit.framework.TestCase;

/**
 * Unittest for Functor class.
 * 
 * @author Vlad Patryshev
 * All source code is stored on <a href="http://code.google.com/p/categories/">http://code.google.com/p/categories/</a>
 *
 */
public class FunctorTest extends TestCase {
  private final Category<String, String> categoryTwoToOne =
      Category("(([0,1,2], {a: 0 -> 2, b: 1 -> 2}))");

  private final Category<String, String> categorySquareWithTwoTopLeftCorners =
      Category("(([a0,a1,b,c,d], {a0a1: a0 -> a1, a0b: a0 -> b, a0c: a0 -> c, a1b: a1 -> b, a1c: a1 -> c, bd: b -> d, cd: c -> d, a0d: a0 -> d, a1d: a1 -> d}), {bd o a0b = a0d, cd o a0c = a0d, bd o a1b = a1d, cd o a1c = a1d, a1b o a0a1 = a0b, a1c o a0a1 = a0c, a1d o a0a1 = a0d})");

  Functor<String, String, String, String> functorFrom2to1toDoubleSquare =
      Functor.Functor(categoryTwoToOne,
              categorySquareWithTwoTopLeftCorners,
              Base.Map(new String[]{"0", "1", "2"},
                      new String[]{"b", "c", "d"}),
              Base.Map(new String[]{"0", "1", "2", "a", "b"},
                      new String[]{"b", "c", "d", "bd", "cd"}));

  Category<String, String> category1to2 =
      Category("(([0,1,2], {a: 0 -> 1, b: 0 -> 2}))");

  Category<String, String> categorySquareWithTwoRightCorners =
      Category(
          "(([a,b,c,d0,d1], " +
              "{ab: a -> b, ac: a -> c, ad0: a -> d0, bd0: b -> d0, cd0: c -> d0, ad1: a -> d1, bd1: b -> d1, cd1: c -> d1, d0d1: d0 -> d1}), " +
              "{bd0 o ab = ad0, cd0 o ac = ad0, bd1 o ab = ad1, cd1 o ac = ad1, d0d1 o ad0 = ad1, , d0d1 o bd0 = bd1, d0d1 o cd0 = cd1})");

  Functor<String, String, String, String> functorFrom1to2toDoubleSquare =
      Functor.Functor(category1to2, categorySquareWithTwoRightCorners,
              Base.Map(new String[]{"0", "1", "2"},
                      new String[]{"a", "b", "c"}),
              Base.Map(new String[]{"0", "1", "2", "a", "b"},
                      new String[]{"a", "b", "c", "ab", "ac"}));

  public void testFunctor_missingObjectMapppings() {
    try {
      Functor.Functor(Categories._4_, Categories._4_, Base.Map(new Integer[]{0}, new Integer[]{1}),
              new HashMap<Pair<Integer, Integer>, Pair<Integer, Integer>>()
      );
      fail("This constructor should have failed");
    } catch (Throwable e) {
      // as expected
    }
  }

  public void testFunctor_missingArrowMapppings() {
    try {
      Functor.Functor(Categories._4_, Categories._4_,
              Base.Map(
                      new Integer[]{0, 1, 2, 3},
                      new Integer[]{1, 2, 3, 3}),
              new HashMap<Pair<Integer, Integer>, Pair<Integer, Integer>>()
      );
      fail("This constructor should have failed");
    } catch (Throwable e) {
      // as expected
    }
  }

  @SuppressWarnings("serial")
  public void testFunctor_unitBroken() {
    try {
      Functor.Functor(Categories._4_, Categories._4_,
              new HashMap<Integer, Integer>() {
                  {
                      put(0, 1);
                      put(1, 2);
                      put(2, 3);
                      put(3, 3);
                  }
              },
              new HashMap<Pair<Integer, Integer>, Pair<Integer, Integer>>() {
                  {
                      put(BasePair.Pair(0, 0), BasePair.Pair(1, 1));
                      put(BasePair.Pair(0, 1), BasePair.Pair(1, 2));
                      put(BasePair.Pair(0, 2), BasePair.Pair(1, 3));
                      put(BasePair.Pair(0, 3), BasePair.Pair(1, 3));
                      put(BasePair.Pair(1, 1), BasePair.Pair(2, 3));
                      put(BasePair.Pair(1, 2), BasePair.Pair(2, 3));
                      put(BasePair.Pair(1, 3), BasePair.Pair(2, 3));
                      put(BasePair.Pair(2, 2), BasePair.Pair(3, 3));
                      put(BasePair.Pair(2, 3), BasePair.Pair(3, 3));
                      put(BasePair.Pair(3, 3), BasePair.Pair(3, 3));
                  }
              }
      );
      fail("This constructor should have failed");
    } catch (Throwable e) {
      // as expected
    }
  }

  @SuppressWarnings("serial")
  public void testFunctor_plain() {
    try {
      Functor.Functor(Categories._4_, Categories._4_,
              new HashMap<Integer, Integer>() {
                  {
                      put(0, 1);
                      put(1, 2);
                      put(2, 3);
                      put(3, 3);
                  }
              },
              new HashMap<Pair<Integer, Integer>, Pair<Integer, Integer>>() {
                  {
                      put(BasePair.Pair(0, 0), BasePair.Pair(1, 1));
                      put(BasePair.Pair(0, 1), BasePair.Pair(1, 2));
                      put(BasePair.Pair(0, 2), BasePair.Pair(1, 3));
                      put(BasePair.Pair(0, 3), BasePair.Pair(1, 3));
                      put(BasePair.Pair(1, 1), BasePair.Pair(2, 2));
                      put(BasePair.Pair(1, 2), BasePair.Pair(2, 3));
                      put(BasePair.Pair(1, 3), BasePair.Pair(2, 3));
                      put(BasePair.Pair(2, 2), BasePair.Pair(3, 3));
                      put(BasePair.Pair(2, 3), BasePair.Pair(3, 3));
                      put(BasePair.Pair(3, 3), BasePair.Pair(3, 3));
                  }
              }
      );
      fail("This constructor should have failed");
    } catch (Throwable e) {
      // as expected
    }
  }

  @SuppressWarnings("serial")
  public void testLimit_cartesianProductActually() {
    Category<Integer, Integer> from = Categories.discreteCategory(Sets.numbers(2));
    Category<String, String> to = Categories.SQUARE;
    Map<Integer, String> map =
        new HashMap<Integer, String>() {
          {
            put(0, "b");
            put(1, "c");
          }
        };

    Functor<Integer, Integer, String, String> f = Functor.Functor(from, to, map, map);
    Functor.Cone limit = f.limit();
    assertNotNull(limit);
    TestCase.assertEquals("ab", limit.arrowTo(0));
    TestCase.assertEquals("ac", limit.arrowTo(1));
  }

  @SuppressWarnings({ "serial" })
  public void testAllCones() {
    Set<Functor<String,String,String,String>.Cone> allCones =
        functorFrom2to1toDoubleSquare.allCones();
    Functor.Cone c1 =
        functorFrom2to1toDoubleSquare.new Cone("a0",
            new HashMap<String, String>() {
              {
                put("0", "a0b");
                put("1", "a0c");
                put("2", "a0d");
              }
            });
    Functor.Cone c2 =
        functorFrom2to1toDoubleSquare.new Cone("a1",
            new HashMap<String, String>() {
              {
                put("0", "a1b");
                put("1", "a1c");
                put("2", "a1d");
              }
            }
        );
    Set<Functor.Cone> expectedCones = Sets.Set(c1, c2);
    assertEquals(expectedCones.size(), allCones.size());
    assertTrue("c1 should belong", allCones.contains(c1));
    assertTrue("c2 should belong", allCones.contains(c1));
    for (Functor.Cone c: allCones) {
      assertTrue("this cone: " + c + " is unexpected", expectedCones.contains(c));
    }
    assertEquals(expectedCones, allCones);
  }

  public void testLimit_twoCandidates() {
    Functor.Cone limit = functorFrom2to1toDoubleSquare.limit();
    assertNotNull(limit);
    TestCase.assertEquals("a1", limit.apex());
    TestCase.assertEquals("a1b", limit.arrowTo("0"));
    TestCase.assertEquals("a1c", limit.arrowTo("1"));
  }

  @SuppressWarnings({ "serial"})
  public void testCoconesTo() {
    Set<Functor<String,String,String,String>.Cocone> actual =
        functorFrom1to2toDoubleSquare.coconesTo("d0");
    Functor.Cocone expected =
        functorFrom1to2toDoubleSquare.new Cocone("d0",
            new HashMap<String, String>() {
              {
                put("0", "ad0");
                put("1", "bd0");
                put("2", "cd0");
              }
            });
    TestCase.assertEquals(Sets.Set(expected), actual);
  }

  @SuppressWarnings({ "serial" })
  public void testAllCocones() {
    Set<Functor<String,String,String,String>.Cocone> allCocones =
        functorFrom1to2toDoubleSquare.allCocones();
    Functor.Cocone c1 =
        functorFrom1to2toDoubleSquare.new Cocone("d0",
            new HashMap<String, String>() {
              {
                put("0", "ad0");
                put("1", "bd0");
                put("2", "cd0");
              }
            });
    Functor.Cocone c2 =
        functorFrom1to2toDoubleSquare.new Cocone("d1",
            new HashMap<String, String>() {
              {
                put("0", "ad1");
                put("1", "bd1");
                put("2", "cd1");
              }
            }
        );
    Set<Functor.Cocone> expectedCocones = Sets.Set(c1, c2);
    for (Functor.Cocone cocone : allCocones) {
      assertTrue(cocone.equals(c1) || cocone.equals(c2));
    }
    assertEquals(expectedCocones, allCocones);
  }

  public void testColimit_twoCandidates() {
    Pair<String, Map<String, String>> colimit = functorFrom1to2toDoubleSquare.colimit();
    assertNotNull(colimit);
    assertEquals("d0", colimit.x());
    assertEquals("ad0", colimit.y().get("0"));
    assertEquals("bd0", colimit.y().get("1"));
    assertEquals("cd0", colimit.y().get("2"));
  }
}
