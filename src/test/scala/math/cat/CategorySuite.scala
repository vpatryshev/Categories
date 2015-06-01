package math.cat

import scala.collection.Set
import org.scalatest.junit.JUnit3Suite
import org.scalatest.prop.Checkers
//import org.scalacheck.Prop.forAll
//import Sets._
import Category._

/**
 * Test suite for Graph class
 * @author vpatryshev
 */
class CategorySuite extends JUnit3Suite with Checkers {

  def assertNone(x: Option[_]):Unit = assertNone(x, "expected None, got " + x)

  def assertNone(x: Option[_], message: String):Unit = assert(x == None, message)

  def assertSome[T](x: T, y: Option[T]):Unit = assertSome(x, y, "expected Some(" + x + "), got " + y)

  def assertSome[T](x: T, y: Option[T], message: String):Unit = assert(Some(x) == y, message)

  def assertEquals[T](x: T, y: T): Unit = assertEquals(x, y, "")

  def assertEquals[T](x: T, y: T, message: String):Unit = assert(x == y, message + "\n" + "expected: " + x + ", actual: " + y)

  lazy val halfSimplicial: Category[String, String] = Category(Set("0", "1", "2"),
    Map("0_1" -> "0", "0_2" -> "0", "2_1" -> "2", "2_a" -> "2", "2_b" -> "2", "a" -> "1", "b" -> "1", "2_swap" -> "2"), // d0
    Map("0_1" -> "1", "0_2" -> "2", "2_1" -> "1", "2_a" -> "2", "2_b" -> "2", "a" -> "2", "b" -> "2", "2_swap" -> "2"), // d1
    Map(("0_1", "a") -> "0_2",
        ("0_1", "b") -> "0_2",
        ("2_1", "a") -> "2_a",
        ("2_1", "b") -> "2_b",
        ("a", "2_swap") -> "b",
        ("a", "2_a") -> "a",
        ("a", "2_b") -> "b",
        ("b", "2_swap") -> "a",
        ("b", "2_a") -> "a",
        ("b", "2_b") -> "b",
        ("2_swap", "2_swap") -> "2",
        ("2_swap", "2_a") -> "2_a",
        ("2_swap", "2_b") -> "2_b",
        ("2_a", "2_a") -> "2_a",
        ("2_b", "2_b") -> "2_b",
        ("2_a", "2_swap") -> "2_b",
        ("2_b", "2_swap") -> "2_a"
      )
    )

  def testConstructor_halfSimplicial
    {
      assertEquals(3, halfSimplicial.objects.size)
    }

  def testConstructor_1_bare {
    val sut: Category[String, String] = Category(Set("1"),
      Map(), // d0
      Map(), // d1
      Map()
      )
    assert(sut.arrows.size == 1, "expected one arrow, got " + sut.arrows)
  }

  def testConstructor_1_full {
    val sut: Category[String, String] = Category(Set("1"),
      Map("1" -> "1"), // d0
      Map("1" -> "1"), // d1
      Map(("1", "1") -> "1")
      )
    assert(sut.arrows.size == 1, "expected one arrow, got " + sut.arrows)
  }

  def testConstructor_plain_withmap {
    val objects = Set(1, 2, 3)
    val map = Map("1a" -> (1, 1), "1b" -> (1, 1), "2to1" -> (2, 1), "3to2" -> (3, 2), "1to3" -> (1, 3))
    val sut = Graph(objects, map)

    assertEquals(Set(3, 1, 2), sut.nodes)
    assertEquals(2, sut.d0("2to1"))
    assertEquals(1, sut.d1("2to1"))
    assertEquals(1, sut.d0("1to3"))
    assertEquals(3, sut.d1("1to3"))
    assertEquals(3, sut.d0("3to2"))
    assertEquals(2, sut.d1("3to2"))
  }

  def testParse_1 {
    val sut = Category("({0}, {}, {})")
    assertEquals(Set("0"), sut.objects)
  }

  def testParse_1_1 {
    val sut = Category("({1, 0}, {}, {})")
    assertEquals(Set("0", "1"), sut.objects)
  }

  def testParse_2 {
    val sut = Category("({1, 0}, {a: 0 -> 1}, {})")
    assertEquals(Set("0", "1"), sut.objects)
  }

  def testParse_3 {
    val sut = Category("({0}, {0: 0 -> 0, 1: 0 -> 0, 2: 0 -> 0}, {1 o 1 = 2, 1 o 2 = 0, 2 o 1 = 0, 2 o 2 = 1})")
    assertEquals(Set("0", "1", "2"), sut.arrows)
  }

  def testParse_negative {
    try {
      val expected: Category[String, String] = Category(Set("0", "1", "2"),
        Map("0_1" -> "0", "0_2" -> "0", "a" -> "1", "b" -> "1", "2_1" -> "2", "2_a" -> "2", "2_b" -> "2", "2_swap" -> "2"), // d0
        Map("0_1" -> "1", "0_2" -> "2", "a" -> "1", "b" -> "1", "2_1" -> "1", "2_a" -> "2", "2_b" -> "2", "2_swap" -> "2"), // d1
        Map(("0_1", "a") -> "0_2", ("0_1", "b") -> "0_2", ("2_1", "a") -> "2_a", ("2_1", "b") -> "2_b", ("a", "2_swap") -> "b", ("b", "2_swap") -> "a", ("2_swap", "2_swap") -> "2"))
      //      Category(expected.toString())
      fail("should have thrown an exception")
    } catch
    {
      case e: Exception => // System.out.println(e) // as expected
      case _ => fail("should have thrown an exception exception")
    }
  }

  def testToString_1 {
    val sut: Category[String, String] = Category(Set("1"),
      Map(), // d0
      Map(), // d1
      Map()
      )
    val actual = sut.toString()
    assertEquals("({1}, {1: 1->1}, {1 o 1 = 1})", actual);
  }

  def testParse_positive_0 {
    val expected: Category[String, String] = Category(Set("1"),
      Map(), // d0
      Map(), // d1
      Map()
      )
    val string = expected.toString()
    val parsed = Category(string)
    assertEquals(expected.objects, parsed.objects)
    assertEquals(expected.arrows, parsed.arrows)
    assertEquals(expected, parsed)
  }

  def testParse_positive_3 {
    val source = "({1, 2}, {1: 1->1, 2: 2->2, 2_1: 2->1}, {2 o 2_1 = 2_1})"
    val parsed = Category(source)
    assertEquals(2, parsed.objects.size)
  }

  def testParse_positive_4 {
    val source = "({1, 2}, {1: 1->1, 2: 2->2, 2_1: 2->1, 2_a: 2->2}, {2 o 2_1 = 2_1, 2_a o 2_a = 2_a, 2 o 2_a = 2_a, 2_a o 2_1 = 2_1, 2_a o 2 = 2_a, 2 o 2 = 2, 1 o 1 = 1, 2_1 o 1 = 2_1})"
    val parsed = Category(source)
    assertEquals(2, parsed.objects.size)
  }

  def testParse_positive_5 {
    val expected: Category[String, String] = Category(Set("1", "2"),
      Map("2_1" -> "2"), // d0
      Map("2_1" -> "1"), // d1
      Map()
      )
    val string = expected.toString()
    val parsed = Category(string)
    assertEquals(expected.objects, parsed.objects)
    assertEquals(expected.arrows, parsed.arrows)
    assertEquals(expected, parsed)
  }

  def testParse_positive_6 {
    val expected: Category[String, String] = Category(Set("1", "2"),
      Map("2_1" -> "2", "2_a" -> "2"), // d0
      Map("2_1" -> "1", "2_a" -> "2"), // d1
      Map(("2_a", "2_a") -> "2_a")
      )
    val string = expected.toString()
    val parsed = Category(string)
    assertEquals(expected.objects, parsed.objects)
    assertEquals(expected.arrows, parsed.arrows)
    assertEquals(expected, parsed)
  }

  def testParse_positive_7 {
    val expected: Category[String, String] = Category(Set("0", "1", "2"),
      Map("0_1" -> "0", "0_2" -> "0", "2_1" -> "2", "2_a" -> "2"), // d0
      Map("0_1" -> "1", "0_2" -> "2", "2_1" -> "1", "2_a" -> "2"), // d1
      Map(("0_1", "a") -> "0_2",
        ("2_1", "a") -> "2_a",
        ("2_a", "2_a") -> "2_a"
        )
      )
    val string = expected.toString()
    val parsed = Category(string)
    assertEquals(expected.objects, parsed.objects)
    assertEquals(expected.arrows, parsed.arrows)
    assertEquals(expected, parsed)
  }

  def testParse_positive_8 {
    val expected: Category[String, String] = Category(Set("0", "1", "2"),
      Map("0_1" -> "0", "0_2" -> "0", "2_1" -> "2", "2_a" -> "2"), // d0
      Map("0_1" -> "1", "0_2" -> "2", "2_1" -> "1", "2_a" -> "2"), // d1
      Map(("0_1", "a") -> "0_2",
        ("2_1", "a") -> "2_a",
        ("2_a", "2_a") -> "2_a"
        )
      )
    val string = expected.toString()
    val parsed = Category(string)
    assertEquals(expected.objects, parsed.objects)
    assertEquals(expected.arrows, parsed.arrows)
    assertEquals(expected, parsed)
  }

  def testParse_positive {
    val expected = halfSimplicial
    val string = expected.toString()
    val parsed = Category(string)
    assertEquals(expected.objects, parsed.objects)
    assertEquals(expected.arrows, parsed.arrows)
    assertEquals(expected, parsed)
  }

  def testD0_positive() {
    assertEquals("0", halfSimplicial.d0("0"))
    assertEquals("1", halfSimplicial.d0("1"))
    assertEquals("1", halfSimplicial.d0("a"))
    assertEquals("2", halfSimplicial.d0("2_swap"))
  }

  def testD0_negative() {
    try {
      val unknown = halfSimplicial.d0("qq")
      fail("Should have failed")
    } catch
    {
      case e: Exception => // println("Got expected exception " + e) // as expected
      case _ => fail("should have thrown a NoSuchElementException")
    }
  }

  def testD1_positive() {
    assertEquals("0", halfSimplicial.d1("0"))
    assertEquals("1", halfSimplicial.d1("1"))
    assertEquals("2", halfSimplicial.d1("a"))
    assertEquals("2", halfSimplicial.d1("2_swap"))
  }

  def testEquals_positive_arrows() {
    val c1 = Category("({0, 1}, {a: 0 -> 1, b: 0 -> 1}, {})");
    val c2 = Category("({1, 0}, {b: 0 -> 1, a: 0 -> 1}, {})");
    assertEquals(Set("0", "1"), c1.objects)
    assertEquals(c1.objects, c2.objects)
    assertEquals(c1.arrows, c2.arrows)
    assertEquals(c1, c2)
  }

  def testEquals_negative_arrows() {
    val c1 = Category("({0, 1}, {a: 0 -> 1, b: 0 -> 1}, {})");
    val c3 = Category("({1, 0}, {a: 0 -> 1, c: 0 -> 1}, {})");
    assert(c1 != c3)
  }

  def testEquals_positive_mult() {
    val c1 = Category("({0, 1}, {a: 0 -> 1, b: 1 -> 1, c: 1 -> 1}, {a o b = a, a o c = c, b o b = b, b o c = c, c o b = b, c o c = c})");
    val c2 = Category("({1, 0}, {b: 1 -> 1, c: 1 -> 1, a: 0 -> 1}, {b o b = b, b o c = c, a o b = a, a o c = c, c o b = b, c o c = c})");
    assertEquals(c1, c2)
  }

  def testAreInverse() {
    val sut = Category("({0}, {0: 0 -> 0, 1: 0 -> 0, 2: 0 -> 0}, {1 o 1 = 2, 1 o 2 = 0, 2 o 1 = 0, 2 o 2 = 1})")
    assert(sut.areInverse("1", "2"))
    assert(sut.areInverse("2", "1"))
    assert(!sut.areInverse("2", "2"))
  }

  def testInverse() {
    val sut = Category("({0}, {0: 0 -> 0, 1: 0 -> 0, 2: 0 -> 0}, {1 o 1 = 2, 1 o 2 = 0, 2 o 1 = 0, 2 o 2 = 1})")
    assertSome("1", sut.inverse("2"), "Actually, " + sut.inverse("2"))
    assertSome("2", sut.inverse("1"), "Actually, " + sut.inverse("1"))
  }

  def testIsIsomorphism_positive() {
    val sut = Category("({0}, {0: 0 -> 0, 1: 0 -> 0, 2: 0 -> 0}, {1 o 1 = 2, 1 o 2 = 0, 2 o 1 = 0, 2 o 2 = 1})")
    assert(sut.isIsomorphism("2"))
  }

  def testIsIsomorphism_negative() {
    val sut = Category("({0}, {0: 0 -> 0, 1: 0 -> 0, 2: 0 -> 0}, {1 o 1 = 2, 1 o 2 = 2, 2 o 1 = 2, 2 o 2 = 2})")
    assert(!sut.isIsomorphism("2"))
  }

  def testIsMonomorphism_positive() {
    val sut = Category("({0}, {0: 0 -> 0, 1: 0 -> 0, 2: 0 -> 0}, {1 o 1 = 2, 1 o 2 = 0, 2 o 1 = 0, 2 o 2 = 1})")
    assert(sut.isMonomorphism("0"))
    assert(sut.isMonomorphism("1"))
    assert(sut.isMonomorphism("2"))
  }

  def testIsMonomorphism_negative() {
    val sut = Category("({0}, {0: 0 -> 0, 1: 0 -> 0, 2: 0 -> 0}, {1 o 1 = 2, 1 o 2 = 2, 2 o 1 = 2, 2 o 2 = 2})")
    assert(sut.isMonomorphism("0"))
    assert(!sut.isMonomorphism("1"))
    assert(!sut.isMonomorphism("2"))
  }

  def testIsEpimorphism() {
    val sut = Category("({0}, {0: 0 -> 0, 1: 0 -> 0, 2: 0 -> 0}, {1 o 1 = 1, 1 o 2 = 2, 2 o 1 = 2, 2 o 2 = 2})")
    assert(sut.isEpimorphism("0"))
    assert(!sut.isEpimorphism("1"))
    assert(!sut.isEpimorphism("2"))
  }

  def testFactorsOnLeft {
    val sut = halfSimplicial
    val predicate = sut.factorsOnLeft(("a", "b"), ("2_a", "2_b"))

    assert(predicate("2_1"))
    assert(!predicate("1"))
    assert(!predicate("0_1"))
    assert(!predicate("2"))
  }

  def testFactorsOnRight {
    val predicate = halfSimplicial.factorsOnRight(("2", "b"), ("2_b", "b"))
    assert(predicate("2_b"))
    assert(!predicate("2_a"))
  }

  def testFactorsUniquelyOnLeft {
    assert(halfSimplicial.factorsUniquelyOnLeft("0_2")("2_swap"))
  }

  def testFactorsUniquelyOnRight {
    val sut = halfSimplicial
    assert(sut.factorsUniquelyOnRight("0_1")("0_2"))
    assert(!sut.factorsUniquelyOnRight("2_swap")("2_a"))
    assert(!sut.factorsUniquelyOnRight("2_swap")("0_2"))
  }

  def testEqualizes {
    val sut = halfSimplicial
    assert(sut.equalizes("2_a", "2_b")("0_2"))
    assert(!sut.equalizes("2_a", "2_b")("2_1"))
    assert(!sut.equalizes("2_a", "2_b")("2"))
    assert(!sut.equalizes("0_1", "2_a")("0"))
  }

  def testCoqualizes {
    val sut = halfSimplicial
    assert(sut.coequalizes("2_a", "2_b")("2_1"))
    assert(!sut.coequalizes("2_a", "2_b")("0_2"))
    assert(!sut.coequalizes("2_a", "2_b")("2_swap"))
    assert(!sut.coequalizes("2_a", "2_b")("2"))
  }

  def testAllEqualisingArrows {
    assertEquals(Set("0_2"), halfSimplicial.allEqualizingArrows("2_a", "2_b"))
  }

  def testIsEqualizer_positive {
    assert(halfSimplicial.isEqualizer("2_a", "2_b")("0_2"))
  }

  def testIsEqualizer_negative {
    assert(!halfSimplicial.isEqualizer("2_a", "2_b")("2"))
  }

  def testEqualizer_positive {
    assertSome("0_2", halfSimplicial.equalizer("2_a", "2_b"))
  }

  def testEqualizer_negative {
    assertNone(PARALLEL_PAIR.equalizer("a", "b"))
  }

  def testAllCoequalizingArrows {
    assertEquals(Set("2_a", "2_b", "2_1"),
      halfSimplicial.allCoequalizingArrows("2_a", "2_b"))
    }

  def testIsCoequalizer_positive {
    assert(halfSimplicial.isCoequalizer("2_a", "2_b")("2_1"));
  }

  def testIsCoequalizer_negative {
    assert(!halfSimplicial.isCoequalizer("2_a", "2_b")("2"));
  }

  def testCoequalizer_positive {
    assertSome("2_1", halfSimplicial.coequalizer("2_a", "2_b"));
  }

  def testCoequalizer_negative {
    assertNone(PARALLEL_PAIR.coequalizer("a", "b"));
  }

  def testPairsEqualizing {
    val actual = halfSimplicial.pairsEqualizing("a", "2_swap")
    val expected = Set(("0_1","0_2"), ("2_1","2_b"), ("1","b"))
    for (p <- actual) {
      assert(expected contains p, "should not contain " + p)
    }
    assertEquals(expected, actual)
  }

  def testPairsCoequalizing {
    val actual = halfSimplicial.pairsCoequalizing("2_1", "2_swap")
    val expected = Set(("a","2_a"), ("1","2_1"), ("b","2_b"))
    assertEquals(expected, actual)
  }

  def testPairsCoequalizing_SQUARE {
    assertEquals(Set(("bd", "cd")), SQUARE.pairsCoequalizing("ab", "ac"))
  }

  def testPairsWithTheSameDomain {
    val actual = halfSimplicial.pairsWithTheSameDomain("1", "2")
    val expected = Set(("1","b"), ("2_1","2_b"), ("2_1","2"), ("2_1","2_swap"), ("0_1","0_2"), ("2_1","2_a"), ("1","a"))
    assertEquals(expected, actual)
  }

  def testPairsWithTheSameCodomain {
    val actual = halfSimplicial.pairsWithTheSameCodomain("0", "2")
    val expected = Set(("0_2","2"), ("0_1","2_1"), ("0_2","2_swap"), ("0_2","2_b"), ("0_2","2_a"))
    assertEquals(expected, actual)
  }

  def testProduct_none {
    assertNone(PARALLEL_PAIR.product("0", "1"))
  }

  def testProduct_plain {
    assertSome(("ab", "ac"), SQUARE.product("b", "c"))
  }

  def testUnion_none {
    assertNone(PARALLEL_PAIR.union("0", "1"))
  }

  def testUnion_plain {
    assertSome(("bd", "cd"), SQUARE.union("b", "c"))
  }

  def testIsPullback {
    assert(!SQUARE.isPullback("bd", "cd")(("ab", "ab")) )
    assert(SQUARE.isPullback("bd", "cd")(("ab", "ac")) )
  }

  def testPullback_none {
    assertNone(PARALLEL_PAIR.pullback("a", "b"))
  }

  def testPullback_same {
    assertSome(("0", "0"), PARALLEL_PAIR.pullback("a", "a"))
  }

  def testPullback_plain {
    assertSome(("ab", "ac"), SQUARE.pullback("bd", "cd"))
  }

  def testPushout_none {
    assertNone(PARALLEL_PAIR.pushout("a", "b"))
  }

  def testPushout_same {
    assertSome(("1", "1"), PARALLEL_PAIR.pushout("a", "a"))
  }

  def testIsPushout_square {
    assert(SQUARE.isPushout("ab", "ac")(("bd", "cd")))
  }

  def testPushout_plain {
    assertSome(("bd", "cd"), SQUARE.pushout("ab", "ac"))
  }

  def testTerminal_none {
    assertNone(Z2.terminal)
    assertNone(PARALLEL_PAIR.terminal)
  }

  def testIsTerminal_positive {
    assert(SQUARE.isTerminal("d"))
    assert(_4_.isTerminal(3))
  }

  def testIsTerminal_negative {
    assert(!SQUARE.isTerminal("a"))
    assert(!SQUARE.isTerminal("b"))
    assert(!_4_.isTerminal(0))
    assert(!_4_.isTerminal(1))
    assert(!PARALLEL_PAIR.isTerminal("0"))
    assert(!PARALLEL_PAIR.isTerminal("1"))
  }

  def testTerminal_misc {
    assertSome("d", SQUARE.terminal)
    assertSome(3, _4_.terminal)
  }

  def testInitial_none {
    assertNone(Z2.initial)
    assertNone(PARALLEL_PAIR.initial)
  }

  def testInitial_misc {
    assertSome("a", SQUARE.initial)
    assertSome(0, _4_.initial)
  }

  def testAllInitialObjects_byDefinition {
    assertEquals(Set("0"), PARALLEL_PAIR.allInitialObjects_byDefinition)
    assertEquals(Set("a"), SQUARE.allInitialObjects_byDefinition)
    assertEquals(Set("a", "b"), PULLBACK.allInitialObjects_byDefinition)
    assertEquals(Set("b", "d"), M.allInitialObjects_byDefinition)
    assertEquals(Set("a", "c", "e"), W.allInitialObjects_byDefinition)
  }

  def testAllInitialObjects_programmersShortcut {
    assertEquals(Set("0"), PARALLEL_PAIR.allInitialObjects_programmersShortcut)
    assertEquals(Set("a"), SQUARE.allInitialObjects_programmersShortcut)
    assertEquals(Set("a", "b"), PULLBACK.allInitialObjects_programmersShortcut)
    assertEquals(Set("b", "d"), M.allInitialObjects_programmersShortcut)
    assertEquals(Set("a", "c", "e"), W.allInitialObjects_programmersShortcut)
  }

  def testAllInitialObjects {
    assertEquals(Set("0"), PARALLEL_PAIR.allInitialObjects)
    assertEquals(Set("a"), SQUARE.allInitialObjects)
    assertEquals(Set("a", "b"), PULLBACK.allInitialObjects)
    assertEquals(Set("b", "d"), M.allInitialObjects)
    assertEquals(Set("a", "c", "e"), W.allInitialObjects)
  }

  def testAllInitialObjects_forKnownCategories {
    for (c <- KNOWN_CATEGORIES) {
      assertEquals(c.allInitialObjects_byDefinition, c.allInitialObjects_programmersShortcut, "Oops, " + c)
    }
  }
  // following are tests for accompanying object

  def test0 {
    val expected = "({}, {}, {})"
    val actual = _0_.toString
    assertEquals(expected, actual)
  }

  def test1 {
    assertEquals(Set(0), _1_.objects)
    assertEquals(Set((0, 0)), _1_.arrows)
  }

  def test2 {
    assertEquals(Set(0, 1), _2_.objects)
    assertEquals(Set((0, 0), (1, 1), (0, 1)), _2_.arrows)
  }

  def test3 {
    assertEquals(Set(0, 1, 2), _3_.objects)
    assertEquals(Set((0, 0), (1, 1), (2, 2), (0, 1), (0, 2), (1, 2)), _3_.arrows)
  }

  def testZ2 {
    assertEquals(Set("1", "a"), Z2.arrows)
    assertEquals("1", Z2.m("a", "a"))
  }

  def testSplitMono {
    val SPLIT_MONO =
      Category("({a,b}, {ab: a -> b, ba: b -> a, bb: b -> b}, {ba o ab = bb, ab o ba = a, ab o bb = ab, bb o ba = ba, bb o bb = bb})")
    assertEquals(Set("a", "b"), SPLIT_MONO.objects)
  }

  def testM {
    assertEquals(5, M.objects.size)
  }

  def testImplicitSegment {
    def sut: Category[Int, (Int, Int)] = 3
    assertEquals(_3_, sut)
  }
}