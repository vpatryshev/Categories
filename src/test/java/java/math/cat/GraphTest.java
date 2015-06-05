package java.math.cat;

import static java.math.cat.Base.Map;
import static java.math.cat.Base.array;
import static java.math.cat.BasePair.Pair;
import static java.math.cat.Graph.Graph;
import static java.math.cat.Sets.Set;

import java.util.Map;
import java.util.Set;

import junit.framework.TestCase;

/**
 * Unittest for Graph class
 * 
 * @author Vlad Patryshev
 * All source code is stored on <a href="http://code.google.com/p/categories/">http://code.google.com/p/categories/</a>
 * 
 */
public class GraphTest extends TestCase {
  @SuppressWarnings("unchecked")
  public void testParse() {
    Set<String> nodes = Set("0", "1", "2");
    Map<String, java.math.cat.Pair<String, String>> arrows =
        Map(array("0.id", "0.1", "0.2", "1.id", "a", "b", "2.1", "2.id", "2.a", "2.b", "2.swap"),
            array(Pair("0", "0"), Pair("0", "1"), Pair("0", "2"), Pair("1", "1"), Pair("1", "2"), Pair("1", "2"), Pair("2", "1"), Pair("2", "2"), Pair("2", "2"), Pair("2", "2"), Pair("2", "2"))
        );
    Graph<String, String> testGraph = Graph(nodes, arrows);
    Graph<String, String> actual = Graph(testGraph.toString());
    assertEquals(testGraph, actual);
  }

  public void testSingleton() {
    Graph<String, String> singleton = Graph("([.], {})");
    assertEquals(Set("."), singleton.nodes());
    assertTrue(singleton.arrows().isEmpty());
  }

  public void testSquare() {
    Graph<String, String> SQUARE = Graph("([a,b,c,d], {ab: a -> b, ac: a -> c, bd: b -> d, cd: c -> d, ad: a -> d})");
    assertEquals(5, SQUARE.arrows().size());
    assertEquals("d", SQUARE.d1("ad"));
    assertEquals("a", SQUARE.d0("ab"));
  }
}