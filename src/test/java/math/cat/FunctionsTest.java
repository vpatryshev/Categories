package math.cat;

import static java.math.cat.Functions.compose;
import static java.math.cat.Functions.forMap;
import static java.math.cat.Functions.id;
import static java.math.cat.Sets.Set;
import java.math.cat.Functions.Function;
import java.math.cat.Functions.Injection;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import junit.framework.TestCase;

/**
 * Tests for java.math.cat.Base class
 * 
 * @author Vlad Patryshev
 * All source code is stored on <a href="http://code.google.com/p/categories/">http://code.google.com/p/categories/</a>
 * 
 */
public class FunctionsTest extends TestCase {

  @SuppressWarnings("serial")
  public void testId_plain() {
    Map<Integer, Integer> expected = new HashMap<Integer, Integer>() {
      {
        put(0, 0);
        put(1, 1);
        put(2, 2);
      }
    };

    assertEquals(expected, id(Set(0, 1, 2)));
  }

  @SuppressWarnings({ "serial", "unchecked" })
  public void testId_distinctResults() {
    Set<Map.Entry<Integer, Integer>> expected = new HashMap<Integer, Integer>() {
      {
        put(0, 0);
        put(1, 1);
        put(2, 2);
      }
    }.entrySet();

    Iterator<Map.Entry<Integer, Integer>> i = id(Set(0, 1, 2)).entrySet().iterator();
    final Map.Entry<Integer, Integer> e1 = i.next();
    final Map.Entry<Integer, Integer> e2 = i.next();
    final Map.Entry<Integer, Integer> e3 = i.next();
    Set<Map.Entry<Integer, Integer>> actual = Set(e1, e2, e3);

    assertEquals(expected, actual);
  }

  @SuppressWarnings("unchecked")
  public void testId_empty() {
    assertEquals(Collections.EMPTY_MAP, id(Collections.EMPTY_SET));
  }

  <T> Function<T, String> functionNamed(final String name) {
    return new Function<T, String>() {
      @Override
      public String apply(T t) {
        return name + "(" + t + ")";
      }
    };
  }

  public void testMap_iterator() {
    Function<Integer, String> f = functionNamed("f");
    Iterator<String> iter = f.map(Arrays.asList(1, 2, 3).iterator());
    for (int i = 1; i < 4; i++) {
      assertEquals("f(" + i + ")", iter.next());
    }
    assertFalse(iter.hasNext());
  }

  public void testMap_iterable() {
    Function<Integer, String> f = functionNamed("f");
    Iterator<String> iter = f.map(((Iterable<Integer>) Arrays.asList(1, 2, 3))).iterator();
    for (int i = 1; i < 4; i++) {
      assertEquals("f(" + i + ")", iter.next());
    }
    assertFalse(iter.hasNext());
  }

  public void testMap_collection() {
    Function<Integer, String> f = functionNamed("f");
    Collection<String> c = f.map(((Collection<Integer>) Arrays.asList(1, 2, 3)));
    assertEquals(3, c.size());
    Iterator<String> iter = c.iterator();
    for (int i = 1; i < 4; i++) {
      assertEquals("f(" + i + ")", iter.next());
    }
    assertFalse(iter.hasNext());
  }

  public void testMap_list() {
    Function<Integer, String> f = functionNamed("f");
    List<String> list = f.map(Arrays.asList(1, 2, 3));
    assertEquals(3, list.size());
    assertEquals("f(3)", list.get(2));
  }

  public void testToMap() {
    Function<Integer, String> f = functionNamed("f");
    Map<Integer, String> actual = f.toMap(Set(1, 2, 3));
    assertEquals(3, actual.size());
    assertEquals("f(2)", actual.get(2));
  }

  public void testForMap() {
    Function<Integer, String> f = functionNamed("f");
    Map<Integer, String> map = f.toMap(Set(1, 2, 3));
    Function<Integer, String> f1 = forMap(map);
    assertEquals("f(2)", f1.apply(2));
    assertEquals(null, f1.apply(4));
  }

  public void testCompose() {
    Function<Integer, String> f = functionNamed("f");
    Function<String, String> g = functionNamed("g");
    Function<Integer, String> fg = compose(f, g);
    assertEquals("g(f(42))", fg.apply(42));
  }

  public void testComposeInjections() {
    Injection<Integer, Integer> injection1 = new Injection<Integer, Integer>() {
      @Override
      public Integer apply(Integer i) {
        return i + 1;
      }
    };
    Injection<Integer, Integer> injection2 = new Injection<Integer, Integer>() {
      @Override
      public Integer apply(Integer i) {
        return i * 2;
      }
    };

    Function<Integer, Integer> composition = injection1.then(injection2);
    assertTrue(composition instanceof Injection);
  }
}