package java.math.cat;

import static java.math.cat.Base.List;
import static java.math.cat.Base.disjointUnion;
import static java.math.cat.Base.setProduct;
import static java.math.cat.BasePair.Pair;
import static java.math.cat.Sets.Map;
import static java.math.cat.Sets.Set;
import static java.math.cat.Sets.factorset;
import static java.math.cat.Sets.groupBy;
import static java.math.cat.Sets.numbers;
import static java.math.cat.Sets.parseSet;
import static java.math.cat.Sets.powerset;

import java.math.cat.Functions.Function;
import java.math.cat.Pair;
import java.math.cat.Sets;
import java.math.cat.Sets.Cartesian;
import java.math.cat.Sets.DisjointUnion;
import java.math.cat.Sets.FactorSet;

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
 * Tests for java.math.cat.Sets class
 * 
 * @author Vlad Patryshev
 * All source code is stored on <a href="http://code.google.com/p/categories/">http://code.google.com/p/categories/</a>
 * 
 */
public class SetsTest extends TestCase {

  @SuppressWarnings("serial")
  public void testSet_plain() {
    assertEquals(new HashSet<String>() {
      {
        add("a");
        add("bc");
        add("def");
      }
    },
        Set("bc", "a", "def"));
  }

  public void testSet_none() {
    assertEquals(Collections.EMPTY_SET, Set());
  }

  @SuppressWarnings("serial")
  public void testSet_duplicates() {
    assertEquals(new HashSet<String>() {
      {
        add("a");
        add("bc");
        add("def");
      }
    },
        Set("bc", "a", "def", "bc", "a", "a"));
  }

  public void testParse() {
    assertEquals(Set("abc", "def"), parseSet("[abc, def]"));
  }

  public void testParse_empty() {
    assertEquals(Collections.EMPTY_SET, parseSet("[]"));
  }

  public void testParse_toStringAndBack() {
    Set<String> testSet = Set("abc", "def");
    assertEquals(testSet, parseSet(testSet.toString()));
  }

  public void testParse_evilWithSpaces() {
    Set<String> testSet = Set("a b c", "d e f");
    assertEquals(testSet, parseSet(testSet.toString()));
  }

  @SuppressWarnings("unchecked")
  public void testDisjointUnion_plain() {
    Set<String> actual = disjointUnion(Set("ab", "ac", "ad", "bd", "cd"), Set("a", "b", "c", "d"));
    Set<String> expected = Set("a", "b", "c", "d", "ab", "ac", "ad", "bd", "cd");
    int count = 0;
    for (@SuppressWarnings("unused") String s : actual) {
      count++;
    }
    assertEquals(expected.size(), count);
    assertEquals(expected, actual);
  }

  @SuppressWarnings("unchecked")
  public void testDisjointUnion_withEmpty() {
    Set<String> actual = disjointUnion(new HashSet<String>(), Set("ab", "ac", "ad", "bd", "cd"), new HashSet(), Set("a", "b", "c", "d"), new HashSet());
    Set<String> expected = Set("a", "b", "c", "d", "ab", "ac", "ad", "bd", "cd");
    assertEquals(expected, actual);
  }

  @SuppressWarnings("unchecked")
  public void testDisjointUnion_empty() {
    assertEquals(Collections.EMPTY_SET, disjointUnion());
  }

  @SuppressWarnings("unchecked")
  public void testSetProduct_plain() {
    assertEquals(
        Set(Pair("a", "0"), Pair("a", "1"), Pair("a", "2"), Pair("b", "0"), Pair("b", "1"), Pair("b", "2")),
        setProduct(Set("a", "b"), Set("0", "1", "2")));
  }

  public void testSetProduct_emptyFirst() {
    assertTrue(setProduct(new HashSet<String>(), Set("0", "1", "2")).isEmpty());
  }

  public void testSetProduct_emptySecond() {
    assertTrue(setProduct(Set("0", "1", "2"), new HashSet<String>()).isEmpty());
  }

  public void testRange_oneParam() {
    assertTrue(numbers(-1).isEmpty());
    assertTrue(numbers(0).isEmpty());
    assertEquals(Set(0), numbers(1));
    assertEquals(Set(0, 1, 2), numbers(3));
  }

  public void testRange_twoParam() {
    assertTrue(java.math.cat.Sets.numbers(-1, -3).isEmpty());
    assertTrue(java.math.cat.Sets.numbers(0, 0).isEmpty());
    assertEquals(Set(3), numbers(3, 4));
    assertEquals(Set(4, 5, 6), numbers(4, 7));
  }

  public void testRange_threeParamPositiveStep() {
    assertTrue(numbers(-1, -3, 3).isEmpty());
    assertTrue(numbers(0, 0, 1).isEmpty());
    assertEquals(Set(3), numbers(3, 4, 6));
    assertEquals(Set(4, 7, 10), numbers(4, 11, 3));
  }

  public void testRange_threeParamNonPositiveStep() {
    assertTrue(numbers(-1, -3, 0).isEmpty());
    assertTrue(numbers(0, 0, 0).isEmpty());
    assertTrue(numbers(0, 1, 0).isEmpty());
    assertTrue(numbers(0, 1, -1).isEmpty());
    assertEquals(Set(3), numbers(3, 2, -1));
    assertEquals(Set(4, 7, 10), numbers(10, 2, -3));
  }

  @SuppressWarnings("unchecked")
  public void testCartesianProduct_plain() {
    Iterable<? extends Iterable<Integer>> actual = Cartesian.product(Arrays.asList(Set(1, 2, 3), Set(4, 5, 6)));
    assertEquals(9, Set(actual).size());
  }

  @SuppressWarnings("unchecked")
  public void testProduct_oneSet() {
    Iterable<? extends Iterable<Integer>> actual = Cartesian.product(Arrays.asList(Set(1, 2, 3)));
    assertEquals(3, Set(actual).size());
  }

  @SuppressWarnings("unchecked")
  public void testProduct_none() {
    Iterable<? extends List<Integer>> actual = Cartesian.product(new ArrayList<Set>());
    assertEquals(1, Set(actual).size());
  }

  @SuppressWarnings("unchecked")
  public void testProduct_singleEmpty() {
    List<Set<Long>> oneEmptyComponent = new ArrayList<Set<Long>>();
    oneEmptyComponent.add(Collections.EMPTY_SET);
    Iterable<? extends Iterable<Long>> actual = Cartesian.product(oneEmptyComponent);
    assertTrue(Set(actual).isEmpty());
  }

  @SuppressWarnings("unchecked")
  public void testProduct_emptyAndNot() {
    Set<Long> longButEmpty = Collections.EMPTY_SET;
    Iterable<? extends Iterable<Long>> actual =
        Cartesian.product(longButEmpty, Set(42L));
    Set<? extends Iterable<Long>> set = Set(actual);
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

    Map<String, Integer> actual = Map(Set(
        Pair("morning", 4),
        Pair("day", 2),
        Pair("evening", 3)));

    assertEquals(expected, actual);
  }

  public void testAllMaps_emptyBase() {
    assertEquals(0, java.math.cat.Sets.allMaps(Set("a", "b", "c"), Set()).size());
  }

  public void testAllMaps_emptyExponent() {
    assertEquals(1, java.math.cat.Sets.allMaps(Set(), Set("a", "b", "c")).size());
  }

  @SuppressWarnings({ "serial", "unchecked" })
  public void testAllMaps_plain() {
    Set<? extends Map<String, Integer>> expected = Set(
        new HashMap<String, Integer>() {
          {
            put("a", 1);
            put("b", 1);
            put("c", 1);
          }
        },
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
    assertEquals(expected, java.math.cat.Sets.allMaps(Set("a", "b", "c"), Set(1, 2)));
  }

  @SuppressWarnings("unchecked")
  public void testFactorset() {
    Set<String> set = Set("a", "b", "c", "d");
    Set<Set<String>> factorset = Set(Set("a", "b"), Set("c", "d"));
    Function<String, Set<String>> factoring = factorset(set, factorset);
    assertEquals(Set("a", "b"), factoring.apply("a"));
    assertEquals(Set("a", "b"), factoring.apply("b"));
    assertEquals(Set("c", "d"), factoring.apply("c"));
    assertEquals(Set("c", "d"), factoring.apply("d"));
  }

  @SuppressWarnings("unchecked")
  public void testFactorset_emptyComponent() {
    Set<String> set = Set("a", "b", "c", "d");
    Set<Set<String>> factorset = Set(Set("a", "b"), Set("c", "d"), new HashSet<String>());
    try {
      factorset(set, factorset);
      fail("This is a bad factorset, should have thrown an error");
    } catch (Throwable t) {
      // as expected
    }
  }

  @SuppressWarnings("unchecked")
  public void testFactorset_overlappingComponents() {
    Set<String> set = Set("a", "b", "c", "d");
    Set<Set<String>> factorset = Set(Set("a", "b"), Set("b", "c", "d"));
    try {
      factorset(set, factorset);
      fail("This is a bad factorset, should have thrown an error");
    } catch (Throwable t) {
      // as expected
    }
  }

  @SuppressWarnings("unchecked")
  public void testFactorset_missingElements() {
    Set<String> set = Set("a", "b", "c", "d");
    Set<Set<String>> factorset = Set(Set("a", "b"), Set("d"));
    try {
      factorset(set, factorset);
      fail("This is a bad factorset, should have thrown an error");
    } catch (Throwable t) {
      // as expected
    }
  }

  @SuppressWarnings("unchecked")
  public void testFactorSet() {
    Set<Integer> set = Set(1, 2, 3, 4, 5);
    Set<Set<Integer>> expected = Set(Set(1, 3, 5), Set(2, 4));
    Set<Set<Integer>> actual = new FactorSet<Integer>(set, new java.math.cat.BinaryRelationship<Integer, Integer>() {
      @Override
      public boolean eval(java.math.cat.Pair<Integer, Integer> p) {
        return p.x() % 2 == p.y() % 2;
      }
    }).factorset();
    assertEquals(expected, actual);
  }

  public void testFactorSet_tricky() {
    Set<Integer> set = Set(1, 2, 3, 4, 5);
    Set<Set<Integer>> expected = new HashSet<Set<Integer>>();
    expected.add(Set(1, 2, 3, 4, 5));
    Set<Set<Integer>> actual = new FactorSet<Integer>(set, new java.math.cat.BinaryRelationship<Integer, Integer>() {
      @Override
      public boolean eval(Pair<Integer, Integer> p) {
        return (p.x() >= 3 && p.x() - 1 == p.y()) ||
            (p.x() <= 3 && p.x() + 1 == p.y());
      }
    }).factorset();
    assertEquals(expected, actual);
  }

  @SuppressWarnings("unchecked")
  public void testFactorSet_incremental() {
    Set<Integer> set = Set(1, 2, 3, 4, 5);
    Set<Set<Integer>> expected = Set(Set(1, 3, 5), Set(2, 4));
    FactorSet<Integer> factorset = new FactorSet<Integer>(set);
    factorset.merge(1, 3);
    factorset.merge(3, 5);
    factorset.merge(4, 2);
    Set<Set<Integer>> actual = factorset.factorset();
    assertEquals(expected, actual);
  }

  @SuppressWarnings("unchecked")
  public void testPowerset() {
    Set<Set<String>> actual = powerset(parseSet("[a, b]"));
    Set<Set<String>> expected = Set(parseSet("[]"), parseSet("[a]"), parseSet("[b]"), parseSet("[a, b]"));
    assertEquals(expected, actual);
  }

  public void testGroupBy() {
    Map<Integer, Set<String>> actual =
        groupBy(
            java.math.cat.Sets.Set("once", "upon", "a", "midnight", "dreary"),
            Sets.numbers(10),
            new Function<String, Integer>() {
              @Override
              public Integer apply(String s) {
                return s.length();
              }
            });
    assertEquals(Set("a"), actual.get(1));
    assertEquals(Set(), actual.get(2));
    assertEquals(Set("once", "upon"), actual.get(4));
  }

  @SuppressWarnings("unchecked")
  public void testUnion() {
    List<Set<String>> source = List(parseSet("[a]"), parseSet("[b]"), parseSet("[a, b]"));
    DisjointUnion<String> du = new DisjointUnion<String>(source);
    Set<java.math.cat.Pair<Integer, String>> expected = Set(Pair(0, "a"), Pair(1, "b"), Pair(2, "a"), Pair(2, "b"));
    assertEquals(expected, Pair(du.unionSet(), du.injections()).x());
    assertEquals(Pair(1, "b"), Pair(du.unionSet(), du.injections()).y().get(1).apply("b"));
    assertEquals(Pair(2, "b"), Pair(du.unionSet(), du.injections()).y().get(2).apply("b"));
  }
}