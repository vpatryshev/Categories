package math.cat

import math.Base._
import math.Test
import math.cat.Categories._
import math.cat.Graph.GraphParser
import math.cat.SetCategory._
import math.cat.construction._
import math.sets.Sets
import math.sets.Sets._
import scalakittens.{Good, Result}

import scala.language.implicitConversions
import scala.language.postfixOps

/**
  * Tests for Category class
  */
class CategoryTest extends Test:

  type SUT = Category
  
  val EmptyComposition: Map[(String, String), String] = Map()
  val EmptyMap: Map[String, String] = Map()

  private val defineComposition = Category.arrowBuilder
  
  "Category" should:

    "calculate composablePairs" in :
      M.composablePairs must be_==(Set(
        ("d", "d"), ("b", "ba"), ("d", "de"), ("d", "dc"), ("a", "a"),
        ("b", "bc"), ("bc", "c"), ("c", "c"), ("e", "e"), ("de", "e"),
        ("b", "b"), ("dc", "c"), ("ba", "a")))

      Square.composablePairs must be_==(Set(
        ("d", "d"), ("ac", "c"), ("ab", "b"), ("ac", "cd"), ("cd", "d"),
        ("a", "ac"), ("b", "bd"), ("ab", "bd"), ("a", "ad"), ("ad", "d"),
        ("bd", "d"), ("a", "a"),
        ("c", "c"), ("b", "b"), ("a", "ab"), ("c", "cd")))

    "degree" in :
      val sut = fromSegment(10)
      sut.degree("4", 0) must be_==(Good("9", Nil))
      sut.degree("4", 1) must be_==(Good("4", List("4.4")))
      sut.degree("4", 5) must be_==(Good("4", List("4.4", "4.4", "4.4", "4.4", "4.4")))

    "id case 1" in :
      `𝟛`.id("2") must be_==("2.2")
      `𝟛`.name must be_==("𝟛")

    "id case 2" in :
      ParallelPair.id("1") must be_==("1")

    "id case 3" in :
      import NaturalNumbers._

      id(BigInt(42)) must be_==((BigInt(42), BigInt(42)))

    "toString_1" in :
      expect(_.toString must be_==("sample: ({1}, {}, {})")
      )(
        Category("sample", Set("1"),
          EmptyMap, // d0
          EmptyMap, // d1
          EmptyComposition,
          defineComposition
        ))

    "D0_positive()" in :
      Simplicial3.d0("0") must be_==("0")
      Simplicial3.d0("1") must be_==("1")
      Simplicial3.d0("a") must be_==("1")
      Simplicial3.d0("swap") must be_==("2")

    "D0_negative()" in :
      try
        val unknown = Simplicial3.d0("qq")
        failure("Should have failed")
      catch
        case e: NoSuchElementException => // as expected
        case _: Throwable => failure("should have thrown a NoSuchElementException")

      ok

    "D1_positive()" in :
      Simplicial3.d1("0") must be_==("0")
      Simplicial3.d1("1") must be_==("1")
      Simplicial3.d1("a") must be_==("2")
      Simplicial3.d1("swap") must be_==("2")

    "equals_positive_arrows()" in :
      val c1 = category"({0, 1}, {a: 0 -> 1, b: 0 -> 1}, {})"
      val c2 = category"({1, 0}, {b: 0 -> 1, a: 0 -> 1}, {})"
      c1.objects must be_==(Set("0", "1"))
      c2.objects must be_==(c1.objects)
      c2.arrows must be_==(c1.arrows)
      c1 must be_==(c2)

    "equals_negative_arrows()" in :
      val c1 = category"({0, 1}, {a: 0 -> 1, b: 0 -> 1}, {})"
      val c3 = category"({1, 0}, {a: 0 -> 1, c: 0 -> 1}, {})"
      (c1 == c3) must beFalse

    "equals_positive_mult()" in :
      val c1 = category"({0, 1}, {a: 0 -> 1, b: 1 -> 1, c: 1 -> 1}, {b ∘ a = a, c ∘ a = c, b ∘ b = b, c ∘ b = c, b ∘ c = b, c ∘ c = c})"
      val c2 = category"({1, 0}, {b: 1 -> 1, c: 1 -> 1, a: 0 -> 1}, {b ∘ b = b, c ∘ b = c, b ∘ a = a, c ∘ a = c, b ∘ c = b, c ∘ c = c})"
      c1 must be_==(c2)

    "AreInverse()" in :
      Z3.areInverse("1", "2") must beTrue
      Z3.areInverse("2", "1") must beTrue
      Z3.areInverse("2", "2") must beFalse

    "Inverse()" in :
      Z3.inverse("2") must be_==(Good("1"))
      Z3.inverse("1") must be_==(Good("2"))

    "isIsomorphism_positive()" in :
      Z3.isIsomorphism("2") must beTrue
      `𝟛`.arrows.foreach:
         case a@PairRegex(x, y) => (x == y) must be_==(`𝟛`.isIsomorphism(a))
         case s                 => failure(s"$s does not look like an arrow in a poset")

      ok

      Simplicial3.arrows.filter(Simplicial3.isIsomorphism) must be_==
        (Set("0", "1", "2", "swap"))

    "isIsomorphism_negative()" in :
      val sut = category"({0}, {0: 0 -> 0, 1: 0 -> 0, 2: 0 -> 0}, {1 ∘ 1 = 2, 1 ∘ 2 = 2, 2 ∘ 1 = 2, 2 ∘ 2 = 2})"
      sut.isIsomorphism("2") must beFalse

    "isMonomorphism_positive()" in :
      val sut = Z3
      sut.isMonomorphism("0") must beTrue
      sut.isMonomorphism("1") must beTrue
      sut.isMonomorphism("2") must beTrue

    "isMonomorphism_negative()" in :
      val sut = category"({0}, {0: 0 -> 0, 1: 0 -> 0, 2: 0 -> 0}, {1 ∘ 1 = 2, 1 ∘ 2 = 2, 2 ∘ 1 = 2, 2 ∘ 2 = 2})"
      sut.isMonomorphism("0") must beTrue
      sut.isMonomorphism("1") must beFalse
      sut.isMonomorphism("2") must beFalse

    "isEpimorphism()" in :
      val sut = category"({0}, {0: 0 -> 0, 1: 0 -> 0, 2: 0 -> 0}, {1 ∘ 1 = 1, 1 ∘ 2 = 2, 2 ∘ 1 = 2, 2 ∘ 2 = 2})"
      sut.isEpimorphism("0") must beTrue
      sut.isEpimorphism("1") must beFalse
      sut.isEpimorphism("2") must beFalse

    "isEndomorphism" in :
      `𝟛`.arrows.foreach:
        case a@PairRegex(x, y) => (x == y) must be_==(`𝟛`.isEndomorphism(a))
        case s => failure(s"$s does not look like an arrow in a poset")

      Simplicial3.arrows.filter(Simplicial3.isEndomorphism) must be_==
        (Set("0", "1", "2", "2_a", "2_b", "swap"))

    "factorsOnLeft" in :
      val sut = Simplicial3
      val predicate = sut.factorsOnLeft(("a", "b"), ("2_a", "2_b"))

      predicate("2_1") must beTrue
      predicate("1") must beFalse
      predicate("0_1") must beFalse
      predicate("2") must beFalse

    "factorsOnRight" in :
      val predicate = Simplicial3.factorsOnRight(("2", "b"), ("2_b", "b"))
      predicate("2_b") must beTrue
      predicate("2_a") must beFalse

    "factorsUniquelyOnLeft" in :
      Simplicial3.factorsUniquelyOnLeft("0_2")("swap") must beTrue
      Simplicial3.factorsUniquelyOnLeft("a")("swap") must beTrue
      Simplicial3.factorsUniquelyOnLeft("a")("2_b") must beFalse

    "factorsUniquelyOnRight" in :
      val sut = Simplicial3
      val mustDo = sut.factorsUniquelyOnRight("0_1")("0_2")
      mustDo must beTrue
      sut.factorsUniquelyOnRight("swap")("2_a") must beFalse
      sut.factorsUniquelyOnRight("swap")("0_2") must beFalse
      Simplicial3.factorsUniquelyOnRight("2_1")("swap") must beTrue
      Simplicial3.factorsUniquelyOnLeft("a")("b") must beFalse

    "factor uniquely on left" in :
      Simplicial3.factorUniquelyOnLeft("0_2", "swap")("0_2", "swap") must beTrue
      Simplicial3.factorUniquelyOnLeft("a", "swap")("0_2", "swap") must beFalse

    "factor uniquely on right" in :
      val sut = Simplicial3
      sut.factorUniquelyOnRight("0_1", "0_2")("0_1", "0_2") must beTrue
      sut.factorUniquelyOnRight("swap", "2_a")("0_1", "0_2") must beFalse

    "equalize" in :
      val sut = Simplicial3
      sut.equalizes("2_a", "2_b")("0_2") must beTrue
      sut.equalizes("2_a", "2_b")("2_1") must beFalse
      sut.equalizes("2_a", "2_b")("2") must beFalse
      sut.equalizes("0_1", "2_a")("0") must beFalse

    "coequalize" in :
      val sut = Simplicial3
      sut.coequalizes("2_a", "2_b")("2_1") must beTrue
      sut.coequalizes("2_a", "2_b")("0_2") must beFalse
      sut.coequalizes("2_a", "2_b")("swap") must beFalse
      sut.coequalizes("2_a", "2_b")("2") must beFalse

    "allEqualizingArrows" in :
      Simplicial3.allEqualizingArrows("2_a", "2_b") must be_==(Set("0_2"))

    "isEqualizer positive" in :
      Simplicial3.isEqualizer("2_a", "2_b")("0_2") must beTrue

    "isEqualizer negative" in :
      Simplicial3.isEqualizer("2_a", "2_b")("2") must beFalse

    "equalizer positive" in :
      Simplicial3.equalizer("2_a", "2_b") must be_==(Good("0_2"))

    "equalizer negative" in :
      ParallelPair.equalizer("a", "b").isBad must beTrue

    "allCoequalizingArrows" in :
      Simplicial3.allCoequalizingArrows("2_a", "2_b") must be_==(Set("2_a", "2_b", "2_1"))

    "isCoequalizer_positive" in :
      Simplicial3.isCoequalizer("2_a", "2_b")("2_1") must beTrue

    "isCoequalizer_negative" in :
      Simplicial3.isCoequalizer("2_a", "2_b")("2") must beFalse

    "Coequalizer_positive" in :
      Simplicial3.coequalizer("2_a", "2_b") must be_==(Good("2_1"))

    "Coequalizer_negative" in :
      ParallelPair.coequalizer("a", "b").isBad must beTrue

    "pairsEqualizing" in :
      val actual = Simplicial3.pairsEqualizing("a", "swap")
      val expected = Set(("0_1", "0_2"), ("2_1", "2_b"), ("1", "b"))
      actual must be_==(expected)

    "pairsCoequalizing" in :
      val actual = Simplicial3.pairsCoequalizing("2_1", "swap")
      val expected = Set(("a", "2_a"), ("1", "2_1"), ("b", "2_b"))
      actual must be_==(expected)

    "pairsCoequalizing_SQUARE" in :
      Square.pairsCoequalizing("ab", "ac") must be_==(Set(("bd", "cd")))

    "pairsWithTheSameDomain" in :
      val actual = Simplicial3.pairsWithTheSameDomain("1", "2")
      val expected = Set(("1", "b"), ("2_1", "2_b"), ("2_1", "2"), ("2_1", "swap"), ("0_1", "0_2"), ("2_1", "2_a"), ("1", "a"))
      actual must be_==(expected)

    "pairsWithTheSameCodomain" in :
      val actual = Simplicial3.pairsWithTheSameCodomain("0", "2")
      val expected = Set(
        ("0_2", "2"), ("0_1", "2_1"), ("0_2", "swap"), ("0_2", "2_b"), ("0_2", "2_a"))
      actual must be_==(expected)

    "isProduct" in :
      Square.isProduct("a", "c")(("ab", "ac")) must beFalse
      Square.isProduct("a", "b")(("ab", "ac")) must beFalse
      Square.isProduct("a", "a")(("ab", "ac")) must beFalse
      Square.isProduct("b", "c")(("ab", "ac")) must beTrue

    "product_none" in :
      ParallelPair.product("0", "1").isBad must beTrue

    "product_plain" in :
      Square.product("b", "c") must be_==(Good(("ab", "ac")))

    "isUnion" in :
      Square.isUnion("b", "c")(("bd", "cd")) must beTrue
      Square.isUnion("a", "c")(("ac", "c")) must beTrue
      Square.isUnion("a", "c")(("ac", "c")) must beTrue
      Square.isUnion("a", "c")(("ac", "a")) must beFalse

    "union_none" in :
      ParallelPair.union("0", "1").isBad must beTrue

    "union_plain" in :
      val actual = Square.union("b", "c")
      actual must be_==(Good(("bd", "cd")))

    "isPullback" in :
      val actual1 = Square.isPullback("bd", "cd")(("ab", "ab"))
      actual1 must beFalse
      val actual2 = Square.isPullback("bd", "cd")(("ab", "ac"))
      actual2 must beTrue

    "Pullback_none" in :
      ParallelPair.pullback("a", "b").isBad must beTrue

    "Pullback_same" in :
      val actual = ParallelPair.pullback("a", "a")
      actual must be_==(Good(("0", "0")))

    "Pullback_plain" in :
      val actual = Square.pullback("bd", "cd")
      actual must be_==(Good(("ab", "ac")))

    "Pushout_none" in :
      ParallelPair.pushout("a", "b").isBad must beTrue

    "Pushout_same" in :
      val actual = ParallelPair.pushout("a", "a")
      actual must be_==(Good(("1", "1")))

    "isPushout_square" in :
      val actual = Square.isPushout("ab", "ac")(("bd", "cd"))
      actual must beTrue

    "Pushout_plain" in :
      val actual = Square.pushout("ab", "ac")
      actual must be_==(Good(("bd", "cd")))

    "Terminal_none" in :
      Z2.terminal.isBad must beTrue
      ParallelPair.terminal.isBad must beTrue

    "isTerminal_positive_Square" in :
      Square.isTerminal("d") must beTrue

    "isTerminal_positive 𝟜" in :
      `𝟜`.isTerminal("3") must beTrue

    "isTerminal_negative Square" in :
      Square.isTerminal("a") must beFalse
      Square.isTerminal("b") must beFalse

    "isTerminal_negative 𝟜" in :
      `𝟜`.isTerminal("0") must beFalse
      `𝟜`.isTerminal("1") must beFalse

    "isTerminal_negative ParallelPair" in :
      ParallelPair.isTerminal("0") must beFalse
      ParallelPair.isTerminal("1") must beFalse

    "Terminal_misc" in :
      Square.terminal must be_==(Good("d"))
      `𝟜`.terminal must be_==(Good("3"))

    "Initial_none" in :
      Z2.initial.isBad must beTrue
      ParallelPair.initial.isBad must beTrue

    "Initial_misc" in :
      Square.initial must be_==(Good("a"))
      `𝟜`.initial must be_==(Good("0"))

    "foreach" in :
      `𝟜` foreach (i => (i.toInt >= 0 && i.toInt < 4) must beTrue)
      ok

    "map" in :
      val actual = Square map (x => x.toUpperCase)
      actual.iterator.toSet must be_==(Set("A", "B", "C", "D"))

    "allRootObjects_byDefinition" in :
      ParallelPair.allRootObjects_byDefinition must be_==(Set("0"))
      Square.allRootObjects_byDefinition must be_==(Set("a"))
      Pullback.allRootObjects_byDefinition must be_==(Set("a", "b"))
      M.allRootObjects_byDefinition must be_==(Set("b", "d"))
      W.allRootObjects_byDefinition must be_==(Set("a", "c", "e"))

    "allRootObjects_programmersShortcut" in :
      ParallelPair.allRootObjects_programmersShortcut must be_==(Set("0"))
      Square.allRootObjects_programmersShortcut must be_==(Set("a"))
      Pullback.allRootObjects_programmersShortcut must be_==(Set("a", "b"))
      M.allRootObjects_programmersShortcut must be_==(Set("b", "d"))
      W.allRootObjects_programmersShortcut must be_==(Set("a", "c", "e"))

    "allRootObjects" in :
      ParallelPair.allRootObjects must be_==(Set("0"))
      Square.allRootObjects must be_==(Set("a"))
      Pullback.allRootObjects must be_==(Set("a", "b"))
      M.allRootObjects must be_==(Set("b", "d"))
      W.allRootObjects must be_==(Set("a", "c", "e"))

    "allRootObjects_forKnownCategories" in :
      KnownCategories.filter(c => c.finiteObjects).foreach:
        c => c.allRootObjects_programmersShortcut must be_==(c.allRootObjects_byDefinition)

      ok

    "arrowsFromRootObjects" in :
      M.arrowsFromRootObjects must be_==(Set("b", "ba", "bc", "d", "dc", "de"))
      W.arrowsFromRootObjects must be_==(Set("a", "ab", "c", "cb", "cd", "e", "ed"))
      ParallelPair.arrowsFromRootObjects must be_==(Set("0", "a", "b"))
      Pullback.arrowsFromRootObjects must be_==(Set("a", "ac", "b", "bc"))
      Pushout.arrowsFromRootObjects must be_==(Set("a", "ab", "ac"))
      Square.arrowsFromRootObjects must be_==(Set("a", "ab", "ac", "ad"))

    "buildBundles M" in :
      M.buildBundles(Set("a", "d"), Set("a", "d", "dc", "de")) must be_==
        (Map("a" -> Set("a"), "d" -> Set("d", "dc", "de")))

    "buildBundles W" in :
      W.buildBundles(Set("a", "d"), Set("a", "ab")) must be_==
        (Map("a" -> Set("a", "ab"), "d" -> Set()))

    "buildBundles ParallelPair" in :
      ParallelPair.buildBundles(Set("0"), Set("0", "a", "b")) must be_==
        (Map("0" -> Set("0", "a", "b")))

      ParallelPair.buildBundles(Set("1"), Set("0", "a", "b")) must throwA[InstantiationException]

    "buildBundles Pullback" in :
      Pullback.buildBundles(Set("a", "c"), Set("a", "ac", "c")) must be_==
        (Map("a" -> Set("a", "ac"), "c" -> Set("c")))

    "buildBundles Pushout" in :
      Pushout.buildBundles(Set("a", "b"), Set("a", "ab", "ac", "b")) must be_==
        (Map("a" -> Set("a", "ab", "ac"), "b" -> Set("b")))

    "buildBundles Square" in :
      Square.buildBundles(Set("a"), Set("a", "ab", "ac", "ad")) must be_==
        (Map("a" -> Set("a", "ab", "ac", "ad")))

      Square.buildBundles(Set("b", "c"), Set()) must be_==
        (Map("b" -> Set(), "c" -> Set()))

    "isInitial 𝟜" in :
      `𝟜`.isInitial("0") must beTrue
      `𝟜`.isInitial("1") must beFalse

    "isInitial in Simplicial3" in :
      Simplicial3.isInitial("0") must beTrue
      Simplicial3.isInitial("1") must beFalse
      Simplicial3.isInitial("2") must beFalse

    "isInitial in ParallelPair" in :
      ParallelPair.isInitial("0") must beFalse

    "isInitial in Pullback" in :
      Pullback.isInitial("a") must beFalse

    "op" in :
      val op3 = `𝟛`.op
      import op3._
      op3.arrows.asInstanceOf[AnyRef] must be_==(`𝟛`.arrows.asInstanceOf[AnyRef])
      op3.objects.asInstanceOf[AnyRef] must be_==(`𝟛`.objects.asInstanceOf[AnyRef])
      op3.d0("1.2") must be_==("2")
      op3.factory.iHope
      ok

  "AAAAAA" should:

    "pass a regression test of 7/7/19" in :
      val cd = AAAAAA.asArrow("23")
      AAAAAA.d0(cd) must be_==("2")
      AAAAAA.d1(cd) must be_==("3")

    "be one cluster" in :
      val sut = AAAAAA.clusters
      val clusters = sut.d1
      clusters.size must be_==(1)
      val cluster = clusters.head
      cluster.size must be_==(6)

  "complete subcategory" should:
    "be ok for Square" in :
      Square.completeSubcategory("abd", Set("a", "b", "d")) must be_==
        (Good(category"abd:({a,b,d}, {ab: a -> b, bd: b -> d, ad: a -> d}, {bd ∘ ab = ad})"))

      Square.completeSubcategory("diagonal", Set("a", "c")) must be_==
        (Good(category"diagonal:({a,c}, {ac: a -> c})"))

      Square.completeSubcategory("abx", Set("a", "b", "x")).errorDetails must beSome("Unknown nodes: x")

  "components" should:
    "build 0" in :
      `𝟘`.connectedComponents.isEmpty must beTrue

    "build 3" in :
      `𝟛`.connectedComponents.map(_.name) must be_==(Set("𝟛.1"))

    "build 1+1" in :
      val c1: Category = category"Discrete_2.1:({a}, {a:a->a})"
      val c2: Category = category"Discrete_2.2:({b}, {b:b->b})"
      `𝟙+𝟙`.connectedComponents must be_==(Set(c1, c2))

    "build Z2+ParallelPair" in :
      val sut = category"A:({1, 2, 3}, {1: 1 -> 1, a: 1 -> 1, b: 2 -> 3, c: 2 -> 3}, {1 ∘ 1 = 1, 1 ∘ a = a, a ∘ 1 = a, a ∘ a = 1})"
      sut.connectedComponents must be_==(Set(
        category"A.1:({1}, {1: 1 -> 1, a: 1 -> 1}, {1 ∘ 1 = 1, 1 ∘ a = a, a ∘ 1 = a, a ∘ a = 1})",
        category"A:({2, 3}, {b: 2 -> 3, c: 2 -> 3})"))

  "baseGraph" should:
    import Graph._

    "good for 𝟛" in :
      `𝟛`.baseGraph must be_==(graph"({0,1,2}, {0.1: 0 -> 1, 1.2: 1 -> 2})")

    "good for 𝟜" in :
      `𝟜`.baseGraph must be_==(graph"({0,1,2,3}, {0.1: 0 -> 1, 1.2: 1 -> 2, 2.3: 2 -> 3})")

    "good for Pullback" in :
      Pullback.baseGraph must be_==(graph"({a,b,c}, {ac: a -> c, bc: b -> c})")

    "good for Square" in :
      Square.baseGraph must be_==(graph"({a,b,c,d}, {ab: a -> b, ac: a -> c, bd: b -> d, cd: c -> d})")

    "good for SplitMono" in :
      SplitMono.baseGraph must be_==(graph"({a,b}, {ab: a -> b, ba: b -> a, bb: b -> b})")

    "good for ParallelPair" in :
      ParallelPair.baseGraph must be_==(graph"({0, 1}, {a:0->1, b:0->1})")

    "good for Z3" in :
      Z3.baseGraph must be_==(graph"({0}, {1: 0 -> 0, 2: 0 -> 0})")

    "good for Simplicial3" in :
      Simplicial3.baseGraph must be_==
        (graph"({0, 1, 2}, {0_1: 0->1, 2_b: 2->2, 2_a: 2->2, swap: 2->2, 2_1: 2->1, b: 1->2, a: 1->2})")

    "good for AAAAAA" in :
      AAAAAA.baseGraph must be_==
        (graph"({1,2,3,4,5,6}, {12:1->2, 23:2->3, 34:3->4, 45:4->5, 56:5->6, 61:6->1})")

  "SetCategory" should:
    "have products" in :
      val first: set = setOf.elements("a", "b")
      val second: set = setOf.elements(1, 2, 3)
      val product = Setf.product(first, second)

      product match
        case Good((x, y)) =>
          x.d0 must be_==(setOf.elements(
            ("a", 1), ("a", 2), ("a", 3),
            ("b", 1), ("b", 2), ("b", 3)))
        case otherwise => failure("product not found")

      ok
