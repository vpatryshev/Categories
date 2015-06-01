package math.cat

import org.specs2._
import math.cat.Graph;
import math.cat.Pair;

import math.cat.Base._
import static math.cat.Graph.*;
import static math.cat.Pair.Pair;

import java.util.Set;
import java.util.Map;

public class GraphTest extends TestCase {
  public void testParse() {
    Set<String> nodes = Set("0", "1", "2");
    Map<String, Pair<String, String>> arrows =
        Map(array(    "0.id",    "0.1",     "0.2",     "1.id",      "a",       "b",     "2.1",    "2.id",     "2.a",     "2.b",     "2.swap"),
            array(Pair("0","0"), Pair("0","1"), Pair("0","2"), Pair("1","1"), Pair("1","2"), Pair("1","2"), Pair("2","1"), Pair("2","2"), Pair("2","2"), Pair("2","2"), Pair("2","2"))
        );
    Graph<String, String> testGraph = Graph(nodes, arrows);
    assertEquals(testGraph, Graph(testGraph.toString()));
  }

  public void testSingleton() {
    Graph<String, String> singleton = Graph("([.], {})");
    assertEquals(Set("."), singleton.nodes());
    assertTrue(singleton.arrows().isEmpty());
  }
}