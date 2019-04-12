package j.math.cat;

import static j.math.cat.BasePair.Pair;
import static j.math.cat.Sets.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import junit.framework.TestCase;

/**
 * Tests for Sets class
 */
@SuppressWarnings({"rawtypes","unchecked"})
public class SetsTest extends TestCase {

  @SuppressWarnings("serial")
  public void testSet_plain() {
    mustBeEqual("hashset vs our set",
            new HashSet<String>() {
              {
                add("a");
                add("bc");
                add("def");
              }
            },
            Sets.Set("bc", "a", "def"));
  }

  public void testSet_none() {
    mustBeEqual("Junst an empty set", Collections.EMPTY_SET, Sets.Set());
  }

  @SuppressWarnings("serial")
  public void testSet_duplicates() {
    assertEquals(Set(new HashSet<String>() {
              {
                add("a");
                add("bc");
                add("def");
              }
            }),
            Sets.Set("bc", "a", "def", "bc", "a", "a"));
  }

  public void testParse() {
    assertEquals(Sets.Set("abc", "def"), Sets.parseSet("[abc, def]"));
  }

  public void testParse_empty() {
    assertEquals(Collections.EMPTY_SET, Sets.parseSet("[]"));
  }

  public void testParse_toStringAndBack() {
    Set<String> testSet = Sets.Set("abc", "def");
    assertEquals(testSet, Sets.parseSet(testSet.toString()));
  }

  public void testParse_evilWithSpaces() {
    Set<String> testSet = Sets.Set("a b c", "d e f");
    assertEquals(testSet, Sets.parseSet(testSet.toString()));
  }

  @SuppressWarnings("unchecked")
  public void testDisjointUnion_plain() {
    Set<String> actual = Base.disjointUnion(Sets.Set("ab", "ac", "ad", "bd", "cd"), Sets.Set("a", "b", "c", "d"));
    Set<String> expected = Sets.Set("a", "b", "c", "d", "ab", "ac", "ad", "bd", "cd");
    int count = 0;
    for (@SuppressWarnings("unused") String s : actual) {
      count++;
    }
    assertEquals(expected.size(), count);
    assertEquals(expected, actual);
  }

  @SuppressWarnings("unchecked")
  public void testDisjointUnion_withEmpty() {
    Set<String> actual = Base.disjointUnion(new HashSet<>(), Sets.Set("ab", "ac", "ad", "bd", "cd"), new HashSet(), Sets.Set("a", "b", "c", "d"), new HashSet());
    Set<String> expected = Sets.Set("a", "b", "c", "d", "ab", "ac", "ad", "bd", "cd");
    assertEquals(expected, actual);
  }

  @SuppressWarnings("unchecked")
  public void testDisjointUnion_empty() {
    assertEquals(Collections.EMPTY_SET, Base.disjointUnion());
  }

  @SuppressWarnings("unchecked")
  public void testSetProduct_plain() {
    assertEquals(
            Sets.Set(BasePair.Pair("a", "0"), BasePair.Pair("a", "1"), BasePair.Pair("a", "2"), BasePair.Pair("b", "0"), BasePair.Pair("b", "1"), BasePair.Pair("b", "2")),
            Base.setProduct(Sets.Set("a", "b"), Sets.Set("0", "1", "2")));
  }

  public void testSetProduct_emptyFirst() {
    assertTrue(Base.setProduct(new HashSet<String>(), Sets.Set("0", "1", "2")).isEmpty());
  }

  public void testSetProduct_emptySecond() {
    assertTrue(Base.setProduct(Sets.Set("0", "1", "2"), new HashSet<String>()).isEmpty());
  }

  public void testRange_oneParam() {
    assertTrue(Sets.numbers(-1).isEmpty());
    assertTrue(Sets.numbers(0).isEmpty());
    assertEquals(Sets.Set(0), Sets.numbers(1));
    assertEquals(Sets.Set(0, 1, 2), Sets.numbers(3));
  }

  public void testRange_twoParam() {
    assertTrue(Sets.numbers(-1, -3).isEmpty());
    assertTrue(Sets.numbers(0, 0).isEmpty());
    assertEquals(Sets.Set(3), Sets.numbers(3, 4));
    assertEquals(Sets.Set(4, 5, 6), Sets.numbers(4, 7));
  }

  public void testRange_threeParamPositiveStep() {
    assertTrue(Sets.numbers(-1, -3, 3).isEmpty());
    assertTrue(Sets.numbers(0, 0, 1).isEmpty());
    assertEquals(Sets.Set(3), Sets.numbers(3, 4, 6));
    assertEquals(Sets.Set(4, 7, 10), Sets.numbers(4, 11, 3));
  }

  public void testRange_threeParamNonPositiveStep() {
    assertTrue(Sets.numbers(-1, -3, 0).isEmpty());
    assertTrue(Sets.numbers(0, 0, 0).isEmpty());
    assertTrue(Sets.numbers(0, 1, 0).isEmpty());
    assertTrue(Sets.numbers(0, 1, -1).isEmpty());
    assertEquals(Sets.Set(3), Sets.numbers(3, 2, -1));
    assertEquals(Sets.Set(4, 7, 10), Sets.numbers(10, 2, -3));
  }

  @SuppressWarnings("unchecked")
  public void testCartesianProduct_plain() {
    Iterable<? extends Iterable<Integer>> actual = Sets.Cartesian.product(Arrays.asList(Sets.Set(1, 2, 3), Sets.Set(4, 5, 6)));
    assertEquals(9, Sets.Set(actual).size());
  }

  @SuppressWarnings("unchecked")
  public void testProduct_oneSet() {
    Iterable<? extends Iterable<Integer>> actual = Sets.Cartesian.product(Arrays.asList(Sets.Set(1, 2, 3)));
    assertEquals(3, Sets.Set(actual).size());
  }

  @SuppressWarnings("unchecked")
  public void testProduct_none() {
    Iterable<? extends List<Integer>> actual = Sets.Cartesian.product(new ArrayList<Set>());
    assertEquals(1, Sets.Set(actual).size());
  }

  @SuppressWarnings("unchecked")
  public void testProduct_singleEmpty() {
    List<Set<Long>> oneEmptyComponent = new ArrayList<>();
    oneEmptyComponent.add(Collections.EMPTY_SET);
    Iterable<? extends Iterable<Long>> actual = Sets.Cartesian.product(oneEmptyComponent);
    assertTrue(Sets.Set(actual).isEmpty());
  }

  @SuppressWarnings("unchecked")
  public void testProduct_emptyAndNot() {
    Set<Long> longButEmpty = Collections.EMPTY_SET;
    Iterable<? extends Iterable<Long>> actual =
        Sets.Cartesian.product(longButEmpty, Sets.Set(42L));
    Set<? extends Iterable<Long>> set = Sets.Set(actual);
    assertEquals(0, set.size());
  }

  @SuppressWarnings({ "serial", "unchecked" })
  public void testMap_forPairs() {
    Map<String, Integer> expected = new HashMap<String, Integer>() {
      {
        put("morning", 4);
        put("day", 2);
        put("evening", 3);
      }
    };

    Map<String, Integer> actual = Sets.Map(Sets.Set(
            BasePair.Pair("morning", 4),
            BasePair.Pair("day", 2),
            BasePair.Pair("evening", 3)));

    for (String k : expected.keySet()) {
      assertEquals(expected.get(k), actual.get(k));
    }
    for (String k : actual.keySet()) {
      assertEquals(expected.get(k), actual.get(k));
    }
  }

  public void testAllMaps_emptyBase() {
    assertTrue(Sets.allMaps(Sets.Set("a", "b", "c"), Sets.Set()).isEmpty());
  }

  public void testAllMaps_emptyExponent() {
    final Set<Map<Object, String>> actual = Sets.allMaps(Sets.Set(), Sets.Set("a", "b", "c"));
    Map<Object, String> emptyMap = Collections.emptyMap();
    assertTrue(actual.iterator().hasNext());
    Map<Object, String> content = actual.iterator().next();
    assertEquals(emptyMap, content);
    mustBeEqual("just a set of empty map", Set(emptyMap), actual);
  }

  @SuppressWarnings({ "serial", "unchecked" })
  public void testAllMaps_plain() {
    Map<String, Integer> a1b1c1 = new HashMap<>();
    a1b1c1.put("a", 1);
    a1b1c1.put("b", 1);
    a1b1c1.put("c", 1);

    Set<? extends Map<String, Integer>> expected = Sets.Set(
            a1b1c1,
            new HashMap<String, Integer>() {
              {
                put("a", 1);
                put("b", 1);
                put("c", 2);
              }
            },
            new HashMap<String, Integer>() {
              {
                put("a", 1);
                put("b", 2);
                put("c", 1);
              }
            },
            new HashMap<String, Integer>() {
              {
                put("a", 1);
                put("b", 2);
                put("c", 2);
              }
            },
            new HashMap<String, Integer>() {
              {
                put("a", 2);
                put("b", 1);
                put("c", 1);
              }
            },
            new HashMap<String, Integer>() {
              {
                put("a", 2);
                put("b", 1);
                put("c", 2);
              }
            },
            new HashMap<String, Integer>() {
              {
                put("a", 2);
                put("b", 2);
                put("c", 1);
              }
            },
            new HashMap<String, Integer>() {
              {
                put("a", 2);
                put("b", 2);
                put("c", 2);
              }
            }
    );


    final Set<Map<String, Integer>> actual = Sets.allMaps(Sets.Set("a", "b", "c"), Sets.Set(1, 2));

    Map<String, Integer> act1 = actual.iterator().next();
    final Set<Map.Entry<String, Integer>> entriesActual = act1.entrySet();
    final Set<Map.Entry<String, Integer>> entriesExpected = a1b1c1.entrySet();

    mustBeEqual("just entries", entriesExpected, entriesActual);

    for (Map<String, Integer> m: expected) {
      assertTrue("missing " + m, actual.contains(m));
    }

    for (Map<String, Integer> m: actual) {
      assertTrue("wrong " + m, expected.contains(m));
    }
    assertTrue("left", expected.containsAll(actual));
    assertTrue("right", actual.containsAll(expected));
  }

  @SuppressWarnings("unchecked")
  public void testFactorset1() {
    Set<Integer> set = Sets.Set(1, 2, 3, 4, 5);
    Set<Set<Integer>> expected = Sets.Set(Sets.Set(1, 3, 5), Sets.Set(2, 4));
    final BinaryRelation<Integer, Integer> relationship =
            new BinaryRelation<Integer, Integer>() {
      @Override
      public boolean eval(Pair<Integer, Integer> p) {
        return p.x() % 2 == p.y() % 2;
      }
    };
    assertTrue(relationship.eval(Pair(7, 17)));
    assertTrue(relationship.eval(Pair(2, 0)));
    final Sets.FactorSet<Integer> factorSet = new Sets.FactorSet<>(set, relationship);
    Set<Set<Integer>> actual = factorSet.factorset();
    mustBeEqual("Something does not work; eq classes are " + factorSet.equivalenceClasses, expected, actual);
  }

  @SuppressWarnings("unchecked")
  public void testFactorset2() {
    Set<String> set = Sets.Set("a", "b", "c", "d");
    Set<Set<String>> factorset = Sets.Set(Sets.Set("a", "b"), Sets.Set("c", "d"));
    Functions.Function<String, Set<String>> factoring = Sets.factorset(set, factorset);
    assertEquals(Sets.Set("a", "b"), factoring.apply("a"));
    assertEquals(Sets.Set("a", "b"), factoring.apply("b"));
    assertEquals(Sets.Set("c", "d"), factoring.apply("c"));
    assertEquals(Sets.Set("c", "d"), factoring.apply("d"));
  }

  @SuppressWarnings("unchecked")
  public void testFactorset_emptyComponent() {
    Set<String> set = Sets.Set("a", "b", "c", "d");
    Set<Set<String>> factorset = Sets.Set(Sets.Set("a", "b"), Sets.Set("c", "d"), new HashSet<>());
    try {
      Sets.factorset(set, factorset);
      fail("This is a bad factorset, should have thrown an error");
    } catch (Throwable t) {
      // as expected
    }
  }

  @SuppressWarnings("unchecked")
  public void testFactorset_overlappingComponents() {
    Set<String> set = Sets.Set("a", "b", "c", "d");
    Set<Set<String>> factorset = Sets.Set(Sets.Set("a", "b"), Sets.Set("b", "c", "d"));
    try {
      Sets.factorset(set, factorset);
      fail("This is a bad factorset, should have thrown an error");
    } catch (Throwable t) {
      // as expected
    }
  }

  @SuppressWarnings("unchecked")
  public void testFactorset_missingElements() {
    Set<String> set = Sets.Set("a", "b", "c", "d");
    Set<Set<String>> factorset = Sets.Set(Sets.Set("a", "b"), Sets.Set("d"));
    try {
      Sets.factorset(set, factorset);
      fail("This is a bad factorset, should have thrown an error");
    } catch (Throwable t) {
      // as expected
    }
  }

  public <X> void mustBeEqual(String msg, Set<X> expected, Set<X> actual) {
    for (X x : actual) {
      assertTrue(msg + ", this one is unexpected: " + x, expected.contains(x));
    }
    for (X x : expected) {
      assertTrue(msg + ", this one is missing: " + x, actual.contains(x));
    }
  }

  public void testFactorSet_tricky() {
    Set<Integer> set = Sets.Set(1, 2, 3, 4, 5);
    Set<Set<Integer>> expected = new HashSet<>();
    expected.add(Sets.Set(1, 2, 3, 4, 5));
    Set<Set<Integer>> actual = new Sets.FactorSet<>(set, new BinaryRelation<Integer, Integer>() {
      @Override
      public boolean eval(Pair<Integer, Integer> p) {
        return (p.x() >= 3 && p.x() - 1 == p.y()) ||
            (p.x() <= 3 && p.x() + 1 == p.y());
      }
    }).factorset();
    mustBeEqual("factorsets", expected, actual);
  }

  @SuppressWarnings("unchecked")
  public void testFactorSet_incremental() {
    Set<Integer> set = Sets.Set(1, 2, 3, 4, 5);
    Set<Set<Integer>> expected = Sets.Set(Sets.Set(1, 3, 5), Sets.Set(2, 4));
    Sets.FactorSet<Integer> factorset = new Sets.FactorSet<>(set);
    factorset.merge(1, 3);
    factorset.merge(3, 5);
    factorset.merge(4, 2);
    Set<Set<Integer>> actual = factorset.factorset();
    mustBeEqual("factorset", expected, actual);
  }

  @SuppressWarnings("unchecked")
  public void testPowerset() {
    Set<Set<String>> actual = Sets.powerset(Sets.parseSet("[a, b]"));
    Set<Set<String>> expected = Sets.Set(Sets.parseSet("[]"), Sets.parseSet("[a]"), Sets.parseSet("[b]"), Sets.parseSet("[a, b]"));
    assertEquals(expected, actual);
  }

  public void testGroupBy() {
    Map<Integer, Set<String>> actual =
        Sets.groupBy(
                Sets.Set("once", "upon", "a", "midnight", "dreary"),
                Sets.numbers(10),
                new Functions.Function<String, Integer>() {
                  @Override
                  public Integer apply(String s) {
                    return s.length();
                  }
                });
    assertEquals(Sets.Set("a"), actual.get(1));
    assertEquals(Sets.Set(), actual.get(2));
    assertEquals(Sets.Set("once", "upon"), actual.get(4));
  }

  @SuppressWarnings("unchecked")
  public void testUnion() {
    List<Set<String>> source = Base.List(Sets.parseSet("[a]"), Sets.parseSet("[b]"), Sets.parseSet("[a, b]"));
    Sets.DisjointUnion<String> du = new Sets.DisjointUnion<>(source);
    Set<Pair<Integer, String>> expected = Sets.Set(BasePair.Pair(0, "a"), BasePair.Pair(1, "b"), BasePair.Pair(2, "a"), BasePair.Pair(2, "b"));
    assertEquals("check2", BasePair.Pair(1, "b"), BasePair.Pair(du.unionSet(), du.injections()).y().get(1).apply("b"));
    assertEquals("check3", BasePair.Pair(2, "b"), BasePair.Pair(du.unionSet(), du.injections()).y().get(2).apply("b"));
    assertEquals("check1", expected, BasePair.Pair(du.unionSet(), du.injections()).x());
  }
}