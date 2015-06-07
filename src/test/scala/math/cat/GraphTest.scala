package math.cat

import org.specs2.mutable._

class GraphTest extends Specification { /*
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

    def testConstructor_plain_withmap {
    val objects = Set(1, 2, 3)
    val map = Map("1a" -> (1, 1), "1b" -> (1, 1), "2to1" -> (2, 1), "3to2" -> (3, 2), "1to3" -> (1, 3))
    val sut = Graph(objects, map)

    assert(sut.nodes == Set(3, 1, 2))
    assert(sut.d0("2to1") == 2)
    assert(sut.d1("2to1") == 1)
    assert(sut.d0("1to3") == 1)
    assert(sut.d1("1to3") == 3)
    assert(sut.d0("3to2") == 3)
    assert(sut.d1("3to2") == 2)
  }

   def testConstructor_plain_withFunctions {
     val sut = Graph(Set(1, 2, 3), Set(11, 111, 21, 32, 13), (x:Int) => x / 10 % 10, (x:Int) => x % 10)
     assert(sut.d0(111) == 1)
     assert(sut.d0(13) == 1)
     assert(sut.d1(13) == 3)
     assert(sut.d1(32) == 2)
  }

  def testCopyConstructor {
    val source = Graph(Set(1, 2, 3), Set(11, 111, 21, 32, 13), (x:Int) => x / 10 % 10, (x:Int) => x % 10)
    val sut = new Graph(source)
    assert(source == sut)
  }

  def testConstructor_negativeBadD0 {
    try {
      val sut = Graph(Set(1, 3), Set(11, 111, 21, 32, 13), (x:Int) => x / 10 % 10, (x:Int) => x % 10)
      failure("Validator should have thrown an exception")
    } catch {
      case e: IllegalArgumentException => // as expected
    }
  }

  def testConstructor_negativeBadD1 {
    try {
      val sut = Graph[Int, Int](Set(1, 2), Set(11, 111, 21, 13), (x:Int) => x / 10 % 10, (x:Int) => x % 10)
      failure("Validator should have thrown an exception")
    } catch {
      case e: IllegalArgumentException => // as expected
    }
  }

  def testEquals_positive {
    val map = Map(11 -> (1, 1), 111 -> (1, 1), 21 -> (2, 1), 32 -> (3, 2), 13 -> (1, 3))
    val sut1 = Graph(Set(1, 2, 3), map)
    val sut2 = Graph(Set(1, 2, 3), Set(11, 111, 21, 32, 13), (x:Int) => x / 10 % 10, (x:Int) => x % 10)
    assert(sut1 == sut2)
  }

  def testEquals_negative {
    val sut1 = Graph(Set(1, 2, 3), Set(11, 21, 32, 13), (x:Int) => x / 10 % 10, (x:Int) => x % 10)
    val sut2 = Graph(Set(1, 2, 3), Set(11, 111, 21, 32, 13), (x:Int) => x / 10 % 10, (x:Int) => x % 10)
    assert(sut1 != sut2)
  }

   def testUnaryOp {
     val sut = Graph(Set(1, 2, 3), Set(11, 21, 32, 13), (x:Int) => x / 10 % 10, (x:Int) => x % 10)
     assert(sut.d0(32) == 3)
     val opsut = ~sut
     val expected = Graph(Set(1, 2, 3), Set(11, 21, 32, 13), (x:Int) => x % 10, (x:Int) => x / 10 % 10)

     assert(expected == opsut)
     assert(sut == ~opsut)
  }

  def testDiscrete {
    val sut = Graph(Set(1, 2, 3))
    val expected = Graph(Set(1, 2, 3), Set[Int](), (x:Int) => x % 10, (x:Int) => x / 10 % 10)
    assert(sut == expected)
    assert(sut == ~sut)
  }

  def testFromPoset {
    val nodes = Set("a", "b", "c")
    val sut = Graph(PoSet(nodes, (a: String, b: String) => a <= b))
    val arrows = Sets.idMap(Set(("a", "a"), ("a", "b"), ("a", "c"), ("b", "b"), ("b", "c"), ("c", "c")))
    val expected = Graph(nodes, arrows)
    assert(sut.nodes == expected.nodes)
    assert(sut.arrows == expected.arrows)
    assert(sut == expected)
  }

  def testParser_empty1 {
    val sut = Graph("({}, {})")
    assert(Graph(Set[String]()) == sut);
  }

  def testParser_1 {
    var sut = Graph("({0}, {})")
    assert(Graph(Set("0")) == sut);
  }

  def testParser_discrete1 {
    val sut = Graph("({1, 2, 3}, {})")
    assert(Graph(Set("1", "2", "3")) == sut)
  }

  def testParser {
    val objects = Set("1", "2", "3")
    val map = Map("1a" -> ("1", "1"), "1b" -> ("1", "1"), "2to1" -> ("2", "1"), "3to2" -> ("3", "2"), "1to3" -> ("1", "3"))
    val expected = Graph(objects, map)
    val sut = Graph("({1, 2, 3}, {1a: 1 -> 1, 1b: 1 -> 1, 2to1: 2 -> 1, 3to2: 3 -> 2, 1to3: 1 -> 3})")

    assert(sut.nodes == expected.nodes)
    assert(sut.arrows == expected.arrows)
    assert(expected == sut)
  }

  def testHom {
    val sut = Graph("({1, 2, 3}, {1a: 1 -> 1, 1b: 1 -> 1, 2to1: 2 -> 1, 3to2: 3 -> 2, 1to3: 1 -> 3})")
    val hom = sut.hom("1", "1")
    assert(hom == Sets.parse("{1a, 1b}"), sut.hom("1", "1"))
    assert(sut.hom("3", "2") == Sets.parse("{3to2}"))
  }


  */
}