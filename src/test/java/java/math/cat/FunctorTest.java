package java.math.cat;

import static java.math.cat.Base.Map;
import static java.math.cat.BasePair.Pair;
import static java.math.cat.Categories.SQUARE;
import static java.math.cat.Categories._4_;
import static java.math.cat.Categories.discreteCategory;
import static java.math.cat.Category.Category;
import static java.math.cat.Functor.Functor;
import static java.math.cat.Sets.Set;

import java.math.cat.Functor;
import java.math.cat.Pair;
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

  java.math.cat.Functor<String, String, String, String> functorFrom2to1toDoubleSquare =
      Functor(categoryTwoToOne,
          categorySquareWithTwoTopLeftCorners,
          Map(new String[]{"0", "1", "2"},
              new String[]{"b", "c", "d"}),
          Map(new String[]{"0", "1", "2", "a", "b"},
              new String[]{"b", "c", "d", "bd", "cd"}));

  Category<String, String> category1to2 =
      Category("(([0,1,2], {a: 0 -> 1, b: 0 -> 2}))");

  Category<String, String> categorySquareWithTwoRightCorners =
      Category(
          "(([a,b,c,d0,d1], " +
              "{ab: a -> b, ac: a -> c, ad0: a -> d0, bd0: b -> d0, cd0: c -> d0, ad1: a -> d1, bd1: b -> d1, cd1: c -> d1, d0d1: d0 -> d1}), " +
              "{bd0 o ab = ad0, cd0 o ac = ad0, bd1 o ab = ad1, cd1 o ac = ad1, d0d1 o ad0 = ad1, , d0d1 o bd0 = bd1, d0d1 o cd0 = cd1})");

  Functor<String, String, String, String> functorFrom1to2toDoubleSquare =
      Functor(category1to2, categorySquareWithTwoRightCorners, 
              Map(new String[]{"0", "1", "2"}, 
                  new String[]{"a", "b", "c"}), 
              Map(new String[]{"0", "1", "2", "a", "b"}, 
                  new String[]{"a", "b", "c", "ab", "ac"}));

  public void testFunctor_missingObjectMapppings() {
    try {
      Functor(_4_, _4_, Map(new Integer[] {0}, new Integer[] {1}),
          new HashMap<java.math.cat.Pair<Integer, Integer>, java.math.cat.Pair<Integer, Integer>>()
      );
      fail("This constructor should have failed");
    } catch (Throwable e) {
      // as expected
    }
  }

  public void testFunctor_missingArrowMapppings() {
    try {
      Functor(_4_, _4_,
          Map(
              new Integer[] {0, 1, 2, 3},
              new Integer[] {1, 2, 3, 3}),
          new HashMap<java.math.cat.Pair<Integer, Integer>, java.math.cat.Pair<Integer, Integer>>()
      );
      fail("This constructor should have failed");
    } catch (Throwable e) {
      // as expected
    }
  }

  @SuppressWarnings("serial")
  public void testFunctor_unitBroken() {
    try {
      Functor(_4_, _4_,
          new HashMap<Integer, Integer>() {
            {
              put(0, 1);
              put(1, 2);
              put(2, 3);
              put(3, 3);
            }
          },
          new HashMap<java.math.cat.Pair<Integer, Integer>, java.math.cat.Pair<Integer, Integer>>() {
            {
              put(Pair(0, 0), Pair(1, 1));
              put(Pair(0, 1), Pair(1, 2));
              put(Pair(0, 2), Pair(1, 3));
              put(Pair(0, 3), Pair(1, 3));
              put(Pair(1, 1), Pair(2, 3));
              put(Pair(1, 2), Pair(2, 3));
              put(Pair(1, 3), Pair(2, 3));
              put(Pair(2, 2), Pair(3, 3));
              put(Pair(2, 3), Pair(3, 3));
              put(Pair(3, 3), Pair(3, 3));
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
      Functor(_4_, _4_,
          new HashMap<Integer, Integer>() {
            {
              put(0, 1);
              put(1, 2);
              put(2, 3);
              put(3, 3);
            }
          },
          new HashMap<java.math.cat.Pair<Integer, Integer>, java.math.cat.Pair<Integer, Integer>>() {
            {
              put(Pair(0, 0), Pair(1, 1));
              put(Pair(0, 1), Pair(1, 2));
              put(Pair(0, 2), Pair(1, 3));
              put(Pair(0, 3), Pair(1, 3));
              put(Pair(1, 1), Pair(2, 2));
              put(Pair(1, 2), Pair(2, 3));
              put(Pair(1, 3), Pair(2, 3));
              put(Pair(2, 2), Pair(3, 3));
              put(Pair(2, 3), Pair(3, 3));
              put(Pair(3, 3), Pair(3, 3));
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
    Category<Integer, Integer> from = discreteCategory(java.math.cat.Sets.numbers(2));
    Category<String, String> to = SQUARE;
    Map<Integer, String> map =
        new HashMap<Integer, String>() {
          {
            put(0, "b");
            put(1, "c");
          }
        };

    java.math.cat.Functor<Integer, Integer, String, String> f = Functor(from, to, map, map);
    java.math.cat.Functor<Integer, Integer, String, String>.Cone limit = f.limit();
    assertNotNull(limit);
    assertEquals("ab", limit.arrowTo(0));
    assertEquals("ac", limit.arrowTo(1));
  }

  @SuppressWarnings({ "serial", "unchecked" })
  public void testAllCones() {
    Set<java.math.cat.Functor<String, String, String, String>.Cone> allCones =
        functorFrom2to1toDoubleSquare.allCones();
    java.math.cat.Functor<String, String, String, String>.Cone c1 =
        functorFrom2to1toDoubleSquare.new Cone("a0",
            new HashMap<String, String>() {
              {
                put("0", "a0b");
                put("1", "a0c");
                put("2", "a0d");
              }
            });
    java.math.cat.Functor<String, String, String, String>.Cone c2 =
        functorFrom2to1toDoubleSquare.new Cone("a1",
            new HashMap<String, String>() {
              {
                put("0", "a1b");
                put("1", "a1c");
                put("2", "a1d");
              }
            }
        );
    Set<java.math.cat.Functor<String, String, String, String>.Cone> expectedCones = Set(c1, c2);
    assertEquals(expectedCones.size(), allCones.size());
    assertEquals(expectedCones, allCones);
  }

  public void testLimit_twoCandidates() {
    java.math.cat.Functor<String, String, String, String>.Cone limit = functorFrom2to1toDoubleSquare.limit();
    assertNotNull(limit);
    assertEquals("a1", limit.apex());
    assertEquals("a1b", limit.arrowTo("0"));
    assertEquals("a1c", limit.arrowTo("1"));
  }

  @SuppressWarnings({ "serial", "unchecked" })
  public void testCoconesTo() {
    Set<java.math.cat.Functor<String, String, String, String>.Cocone> actual =
        functorFrom1to2toDoubleSquare.coconesTo("d0");
    java.math.cat.Functor<String, String, String, String>.Cocone expected =
        functorFrom1to2toDoubleSquare.new Cocone("d0",
            new HashMap<String, String>() {
              {
                put("0", "ad0");
                put("1", "bd0");
                put("2", "cd0");
              }
            });
    assertEquals(Set(expected), actual);
  }

  @SuppressWarnings({ "serial", "unchecked" })
  public void testAllCocones() {
    Set<java.math.cat.Functor<String, String, String, String>.Cocone> allCocones =
        functorFrom1to2toDoubleSquare.allCocones();
    java.math.cat.Functor<String, String, String, String>.Cocone c1 =
        functorFrom1to2toDoubleSquare.new Cocone("d0",
            new HashMap<String, String>() {
              {
                put("0", "ad0");
                put("1", "bd0");
                put("2", "cd0");
              }
            });
    java.math.cat.Functor<String, String, String, String>.Cocone c2 =
        functorFrom1to2toDoubleSquare.new Cocone("d1",
            new HashMap<String, String>() {
              {
                put("0", "ad1");
                put("1", "bd1");
                put("2", "cd1");
              }
            }
        );
    Set<java.math.cat.Functor<String, String, String, String>.Cocone> expectedCocones = Set(c1, c2);
    for (java.math.cat.Functor<String, String, String, String>.Cocone cocone : allCocones) {
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
