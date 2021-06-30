package math.cat

import math.Test
import math.cat.Categories._
import math.cat.Graph.GraphParser
import math.cat.SetCategory._
import math.cat.construction._
import math.sets.Sets
import math.sets.Sets._
import org.specs2.control.eff.Evaluate.fail
import org.specs2.matcher.MatchResult
import scalakittens.{Good, Result}
import math.Base._

import scala.language.postfixOps

/**
  * Tests for Category class
  */
class CategoryTest extends Test with CategoryFactory {

  type SUT = Category
  
  val EmptyComposition: Map[(String, String), String] = Map()
  val EmptyMap: Map[String, String] = Map()

  private val defineComposition = Category.arrowBuilder
  
  "Category" >> {

    "have segments" >> {
      for {
        i <- 0 until 10
      } {
        Category.segment(i).arrows.size === i * (i + 1) / 2
      }
      ok
    }

    "parsing example1" >> {
      val d0d1 = Map(
        "0.1" -> ("0", "1"),
        "0.2" -> ("0", "2"),
        "a" -> ("1", "2"),
        "b" -> ("1", "2"),
        "2.1" -> ("2", "1"),
        "2.a" -> ("2", "2"),
        "2.b" -> ("2", "2"),
        "2.swap" -> ("2", "2")
      )

      lazy val sutOpt = Category("example1",
        Set("0", "1", "2"), // objects
        d0d1.view.mapValues(_._1).toMap, // d0
        d0d1.view.mapValues(_._2).toMap, // d1
        Map(
          ("0.1", "a") -> "0.2",
          ("0.1", "b") -> "0.2",
          ("2.1", "a") -> "2.a",
          ("2.1", "b") -> "2.b",
          ("a", "2.a") -> "a",
          ("a", "2.b") -> "b",
          ("a", "2.swap") -> "b",
          ("b", "2.a") -> "a",
          ("b", "2.b") -> "b",
          ("b", "2.swap") -> "a",
          ("2.a", "2.a") -> "2.a",
          ("2.a", "2.b") -> "2.b",
          ("2.b", "2.a") -> "2.a",
          ("2.b", "2.b") -> "2.b",
          ("2.swap", "2.a") -> "2.a",
          ("2.swap", "2.b") -> "2.b",
          ("2.swap", "2.swap") -> "2") // composition map
      )

      sutOpt match {
        case Good(cat) =>
          val string = cat.toString
          Category.read(string) === sutOpt
        case oops => failure(oops.toString)
      }
      ok
    }

    "composablePairs" >> {
      M.composablePairs === Set(("d", "d"), ("b", "ba"), ("d", "de"), ("d", "dc"), ("a", "a"),
        ("b", "bc"), ("bc", "c"), ("c", "c"), ("e", "e"), ("de", "e"), ("b", "b"), ("dc", "c"), ("ba", "a"))
      Square.composablePairs === Set(("d", "d"), ("ac", "c"), ("ab", "b"), ("ac", "cd"), ("cd", "d"),
        ("a", "ac"), ("b", "bd"), ("ab", "bd"), ("a", "ad"), ("ad", "d"), ("bd", "d"), ("a", "a"),
        ("c", "c"), ("b", "b"), ("a", "ab"), ("c", "cd"))
    }

    "degree" >> {
      val sut = segment(10)
      sut.degree("4", 0) === Good("9", Nil)
      sut.degree("4", 1) === Good("4", List("4.4"))
      sut.degree("4", 5) === Good("4", List("4.4", "4.4", "4.4", "4.4", "4.4"))
    }

    "id case 1" >> {
      _3_.id("2") === "2.2"
      _3_.name === "_3_"
    }

    "id case 2" >> {
      ParallelPair.id("1") === "1"
    }

    "id case 3" >> {
      import NaturalNumbers._
      
      id(NaturalNumbers.obj(BigInt(42))) === (BigInt(42), BigInt(42))
    }

    "regression from 6/9/15" >> {
      val expected = Category("regression from 6/9/15",
        objects = Set("0", "1", "2"),
        domain = Map("0" -> "0", "1" -> "1", "2" -> "2", "a" -> "0", "b" -> "1"),
        codomain = Map("0" -> "0", "1" -> "1", "2" -> "2", "a" -> "2", "b" -> "2"),
        composition = EmptyComposition,
        defineComposition
      ).iHope

      val sample1 = category"sample1:({0,1,2}, {a: 0 -> 2, b: 1 -> 2}, {a ∘ 0 = a})"
      sample1 === expected

      val sample2 = category"sample2:({0,1,2}, {a: 0 -> 2, b: 1 -> 2})"
      sample2 === expected

    }

    "constructor_Simplicial3" >> {
      Simplicial3.objects must haveSize(3)
    }

    "constructor_1_bare" >> {
      val sutOpt = Category("constructor_1_bare",
        objects = Set("1"),
        domain = EmptyMap,
        codomain = EmptyMap,
        composition = EmptyComposition,
        defineComposition
      )
      checkOpt(sutOpt, (sut: Category) => {
        sut.arrows must haveSize(1)
      })
      ok
    }

    "constructor_1_full" >> {
      expect(sut => {
        sut.arrows must haveSize(1)
      })(
        Category("constructor_1_full", Set("1"),
          Map("1" -> "1"), // d0
          Map("1" -> "1"), // d1
          Map(("1", "1") -> "1"),
          defineComposition
        )
      )
    }

    "parse_1" >> {
      val sut = category"({0}, {}, {})"
      sut.objects === Set("0")
    }

    "parse_1_1" >> {
      val sut = category"({1, 0}, {}, {})"
      sut.objects === Set("0", "1")
    }

    "parse_2" >> {
      val sut = category"({1, 0}, {a: 0 -> 1}, {})"
      sut.objects === Set("0", "1")
    }

    "parse_Z3" >> {
      Z3.arrows === Set("0", "1", "2")
    }

    "parse_nonsense" >> {
      try {
        category"(bs)"
        failure("should not have worked")
      } catch {
        case x: Exception => ok
      }
      ok
    }
    
    "parse_negative" >> {
      val actual = Category("must fail", Set("0", "1", "2"),
        Map("0_1" -> "0", "0_2" -> "0", "a" -> "1", "b" -> "1", "2_1" -> "2", "2_a" -> "2", "2_b" -> "2", "swap" -> "2"), // d0
        Map("0_1" -> "1", "0_2" -> "2", "a" -> "1", "b" -> "1", "2_1" -> "1", "2_a" -> "2", "2_b" -> "2", "swap" -> "2"), // d1
        // breaking laws
        Map(("0_1", "a") -> "0_2", ("0_1", "b") -> "0_2", ("2_1", "a") -> "2_a", ("2_1", "b") -> "2_b", ("a", "swap") -> "b", ("b", "swap") -> "a", ("swap", "swap") -> "2"),
        defineComposition
      )
      checkError(_.contains("12 arrows still missing:"), actual)
      actual.isGood === false
    }

    def checkParsing(catOpt: Result[Category]): MatchResult[Any] =
      expect(sut => {
        val string = sut.toString
        val parsed = Category.read(string)
        parsed === catOpt
      })(catOpt)

    "toString_1" >> {
      expect(sut => {
        sut.toString === "sample: ({1}, {}, {})"
      })(
        Category("sample", Set("1"),
          EmptyMap, // d0
          EmptyMap, // d1
          EmptyComposition,
          defineComposition
        ))
    }

    "parse_positive_0" >> {
      val sutOpt = Category("sample0", Set("1"),
        EmptyMap, // d0
        EmptyMap, // d1
        EmptyComposition,
        defineComposition
      )
      checkParsing(sutOpt)
    }

    "parse_positive_3" >> {
      val parsed = category"({1, 2}, {1: 1->1, 2: 2->2, 2_1: 2->1}, {2_1 ∘ 2 = 2_1})"
      parsed.objects.size === 2
    }

    "parse_positive_4" >> {
      val parsed = category"""(
        {1, 2},
        {1: 1->1, 2: 2->2, 2_1: 2->1, 2_a: 2->2}, 
        {2_1 ∘ 2 = 2_1, 2_a ∘ 2_a = 2_a, 2 ∘ 2_a = 2_a, 2_1 ∘ 2_a = 2_1, 2_a ∘ 2 = 2_a, 2 ∘ 2 = 2, 1 ∘ 1 = 1, 1 ∘ 2_1 = 2_1}
      )"""
      parsed.objects.size === 2
    }

    "parse_positive_5" >> {
      val sutOpt = Category("sample5", Set("1", "2"),
        Map("2_1" -> "2"), // d0
        Map("2_1" -> "1"), // d1
        EmptyComposition,
        defineComposition
      )

      checkParsing(sutOpt)
    }

    "parse_positive_6" >> {
      checkParsing(Category("sample6", Set("1", "2"),
        Map("2_1" -> "2", "2_a" -> "2"), // d0
        Map("2_1" -> "1", "2_a" -> "2"), // d1
        Map(("2_a", "2_a") -> "2_a"),
        defineComposition
      ))
    }

    "parse_positive_7" >> {
      val sutOpt = Category("sample7", Set("0", "1", "2"),
        Map("0_1" -> "0", "0_2" -> "0", "2_1" -> "2", "2_a" -> "2"), // d0
        Map("0_1" -> "1", "0_2" -> "2", "2_1" -> "1", "2_a" -> "2"), // d1
        Map(("0_1", "a") -> "0_2",
          ("2_1", "a") -> "2_a",
          ("2_a", "2_a") -> "2_a"
        ),
        defineComposition
      )
      checkParsing(sutOpt)
    }

    "parse_positive_8" >> {
      val sutOpt = Category("sample8", Set("0", "1", "2"),
        Map("0_1" -> "0", "0_2" -> "0", "2_1" -> "2", "2_a" -> "2"), // d0
        Map("0_1" -> "1", "0_2" -> "2", "2_1" -> "1", "2_a" -> "2"), // d1
        Map(("0_1", "a") -> "0_2",
          ("2_1", "a") -> "2_a",
          ("2_a", "2_a") -> "2_a"
        ),
        defineComposition
      )
      checkParsing(sutOpt)
      ok
    }

    "parse_positive" >> {
      checkParsing(Good(Simplicial3))
      ok
    }

    "D0_positive()" >> {
      Simplicial3.d0("0") === "0"
      Simplicial3.d0("1") === "1"
      Simplicial3.d0("a") === "1"
      Simplicial3.d0("swap") === "2"
    }

    "D0_negative()" >> {
      try {
        val unknown = Simplicial3.d0("qq")
        failure("Should have failed")
      } catch {
        case e: Exception => // as expected
        case _: Throwable => failure("should have thrown a NoSuchElementException")
      }
      ok
    }

    "D1_positive()" >> {
      Simplicial3.d1("0") === "0"
      Simplicial3.d1("1") === "1"
      Simplicial3.d1("a") === "2"
      Simplicial3.d1("swap") === "2"
    }

    "equals_positive_arrows()" >> {
      val c1 = category"({0, 1}, {a: 0 -> 1, b: 0 -> 1}, {})"
      val c2 = category"({1, 0}, {b: 0 -> 1, a: 0 -> 1}, {})"
      c1.objects === Set("0", "1")
      c2.objects === c1.objects
      c2.arrows === c1.arrows
      c1 === c2
    }

    "equals_negative_arrows()" >> {
      val c1 = category"({0, 1}, {a: 0 -> 1, b: 0 -> 1}, {})"
      val c3 = category"({1, 0}, {a: 0 -> 1, c: 0 -> 1}, {})"
      (c1 == c3) must beFalse
    }

    "equals_positive_mult()" >> {
      val c1 = category"({0, 1}, {a: 0 -> 1, b: 1 -> 1, c: 1 -> 1}, {b ∘ a = a, c ∘ a = c, b ∘ b = b, c ∘ b = c, b ∘ c = b, c ∘ c = c})"
      val c2 = category"({1, 0}, {b: 1 -> 1, c: 1 -> 1, a: 0 -> 1}, {b ∘ b = b, c ∘ b = c, b ∘ a = a, c ∘ a = c, b ∘ c = b, c ∘ c = c})"
      (c1 == c2) must beTrue
    }

    "AreInverse()" >> {
      Z3.areInverse("1", "2") must beTrue
      Z3.areInverse("2", "1") must beTrue
      Z3.areInverse("2", "2") must beFalse
    }

    "Inverse()" >> {
      Z3.inverse("2") === Good("1")
      Z3.inverse("1") === Good("2")
    }

    "isIsomorphism_positive()" >> {
      Z3.isIsomorphism("2") must beTrue
      _3_.arrows.forall {
        case a@PairRegex(x, y) => (x == y) === _3_.isIsomorphism(a)
        case s => failure(s"$s does not look like an arrow in a poset"); false
      }

      Simplicial3.arrows.filter(Simplicial3.isIsomorphism) ===
        Set("0", "1", "2", "swap")
    }

    "isIsomorphism_negative()" >> {
      val sut = category"({0}, {0: 0 -> 0, 1: 0 -> 0, 2: 0 -> 0}, {1 ∘ 1 = 2, 1 ∘ 2 = 2, 2 ∘ 1 = 2, 2 ∘ 2 = 2})"
      sut.isIsomorphism("2") must beFalse
    }

    "isMonomorphism_positive()" >> {
      val sut = Z3
      sut.isMonomorphism("0") must beTrue
      sut.isMonomorphism("1") must beTrue
      sut.isMonomorphism("2") must beTrue
    }

    "isMonomorphism_negative()" >> {
      val sut = category"({0}, {0: 0 -> 0, 1: 0 -> 0, 2: 0 -> 0}, {1 ∘ 1 = 2, 1 ∘ 2 = 2, 2 ∘ 1 = 2, 2 ∘ 2 = 2})"
      sut.isMonomorphism("0") must beTrue
      sut.isMonomorphism("1") must beFalse
      sut.isMonomorphism("2") must beFalse
    }

    "isEpimorphism()" >> {
      val sut = category"({0}, {0: 0 -> 0, 1: 0 -> 0, 2: 0 -> 0}, {1 ∘ 1 = 1, 1 ∘ 2 = 2, 2 ∘ 1 = 2, 2 ∘ 2 = 2})"
      sut.isEpimorphism("0") must beTrue
      sut.isEpimorphism("1") must beFalse
      sut.isEpimorphism("2") must beFalse
    }

    "isEndomorphism" in {
      _3_.arrows.forall {
        case a@PairRegex(x, y) => (x == y) === _3_.isEndomorphism(a)
        case s => failure(s"$s does not look like an arrow in a poset"); false
      }

      Simplicial3.arrows.filter(Simplicial3.isEndomorphism) ===
        Set("0", "1", "2", "2_a", "2_b", "swap")
    }

    "factorsOnLeft" >> {
      val sut = Simplicial3
      val predicate = sut.factorsOnLeft(("a", "b"), ("2_a", "2_b"))

      predicate("2_1") must beTrue
      predicate("1") must beFalse
      predicate("0_1") must beFalse
      predicate("2") must beFalse
    }

    "factorsOnRight" >> {
      val predicate = Simplicial3.factorsOnRight(("2", "b"), ("2_b", "b"))
      predicate("2_b") must beTrue
      predicate("2_a") must beFalse
    }

    "factorsUniquelyOnLeft" >> {
      Simplicial3.factorsUniquelyOnLeft("0_2")("swap") must beTrue
      Simplicial3.factorsUniquelyOnLeft("a")("swap") must beTrue
      Simplicial3.factorsUniquelyOnLeft("a")("2_b") must beFalse
    }

    "factorsUniquelyOnRight" >> {
      val sut = Simplicial3
      val mustDo = sut.factorsUniquelyOnRight("0_1")("0_2")
      mustDo must beTrue
      sut.factorsUniquelyOnRight("swap")("2_a") must beFalse
      sut.factorsUniquelyOnRight("swap")("0_2") must beFalse
      Simplicial3.factorsUniquelyOnRight("2_1")("swap") must beTrue
      Simplicial3.factorsUniquelyOnLeft("a")("b") must beFalse
    }

    "factorUniquelyOnLeft" >> {
      Simplicial3.factorUniquelyOnLeft("0_2", "swap")("0_2", "swap") must beTrue
      Simplicial3.factorUniquelyOnLeft("a", "swap")("0_2", "swap") must beFalse
    }

    "factorUniquelyOnRight" >> {
      val sut = Simplicial3
      sut.factorUniquelyOnRight("0_1", "0_2")("0_1", "0_2") must beTrue
      sut.factorUniquelyOnRight("swap", "2_a")("0_1", "0_2") must beFalse
    }

    "equalizes" >> {
      val sut = Simplicial3
      sut.equalizes("2_a", "2_b")("0_2") must beTrue
      sut.equalizes("2_a", "2_b")("2_1") must beFalse
      sut.equalizes("2_a", "2_b")("2") must beFalse
      sut.equalizes("0_1", "2_a")("0") must beFalse
    }

    "coqualizes" >> {
      val sut = Simplicial3
      sut.coequalizes("2_a", "2_b")("2_1") must beTrue
      sut.coequalizes("2_a", "2_b")("0_2") must beFalse
      sut.coequalizes("2_a", "2_b")("swap") must beFalse
      sut.coequalizes("2_a", "2_b")("2") must beFalse
    }

    "allEqualizingArrows" >> {
      Simplicial3.allEqualizingArrows("2_a", "2_b") === Set("0_2")
    }

    "isEqualizer_positive" >> {
      Simplicial3.isEqualizer("2_a", "2_b")("0_2") must beTrue
    }

    "isEqualizer_negative" >> {
      Simplicial3.isEqualizer("2_a", "2_b")("2") must beFalse
    }

    "equalizer_positive" >> {
      Simplicial3.equalizer("2_a", "2_b") === Good("0_2")
    }

    "equalizer_negative" >> {
      ParallelPair.equalizer("a", "b").isBad must beTrue
    }

    "allCoequalizingArrows" >> {
      Simplicial3.allCoequalizingArrows("2_a", "2_b") === Set("2_a", "2_b", "2_1")
    }

    "isCoequalizer_positive" >> {
      Simplicial3.isCoequalizer("2_a", "2_b")("2_1") must beTrue
    }

    "isCoequalizer_negative" >> {
      Simplicial3.isCoequalizer("2_a", "2_b")("2") must beFalse
    }

    "Coequalizer_positive" >> {
      Simplicial3.coequalizer("2_a", "2_b") === Good("2_1")
    }

    "Coequalizer_negative" >> {
      ParallelPair.coequalizer("a", "b").isBad must beTrue
    }

    "pairsEqualizing" >> {
      val actual = Simplicial3.pairsEqualizing("a", "swap")
      val expected = Set(("0_1", "0_2"), ("2_1", "2_b"), ("1", "b"))
      actual === expected
    }

    "pairsCoequalizing" >> {
      val actual = Simplicial3.pairsCoequalizing("2_1", "swap")
      val expected = Set(("a", "2_a"), ("1", "2_1"), ("b", "2_b"))
      actual === expected
    }

    "pairsCoequalizing_SQUARE" >> {
      Square.pairsCoequalizing("ab", "ac") === Set(("bd", "cd"))
    }

    "pairsWithTheSameDomain" >> {
      val actual = Simplicial3.pairsWithTheSameDomain("1", "2")
      val expected = Set(("1", "b"), ("2_1", "2_b"), ("2_1", "2"), ("2_1", "swap"), ("0_1", "0_2"), ("2_1", "2_a"), ("1", "a"))
      actual === expected
    }

    "pairsWithTheSameCodomain" >> {
      val actual = Simplicial3.pairsWithTheSameCodomain("0", "2")
      val expected = Set(("0_2", "2"), ("0_1", "2_1"), ("0_2", "swap"), ("0_2", "2_b"), ("0_2", "2_a"))
      actual === expected
    }

    "isProduct" >> {
      Square.isProduct("a", "c")(("ab", "ac")) === false
      Square.isProduct("a", "b")(("ab", "ac")) === false
      Square.isProduct("a", "a")(("ab", "ac")) === false
      Square.isProduct("b", "c")(("ab", "ac")) === true
    }

    "product_none" >> {
      ParallelPair.product("0", "1").isBad must beTrue
    }

    "product_plain" >> {
      Square.product("b", "c") === Good(("ab", "ac"))
    }

    "isUnion" >> {
      Square.isUnion("b", "c")(("bd", "cd")) === true
      Square.isUnion("a", "c")(("ac", "c")) === true
      Square.isUnion("a", "c")(("ac", "c")) === true
      Square.isUnion("a", "c")(("ac", "a")) === false
    }

    "union_none" >> {
      ParallelPair.union("0", "1").isBad must beTrue
    }

    "union_plain" >> {
      val actual = Square.union("b", "c")
      actual === Good(("bd", "cd"))
    }

    "isPullback" >> {
      val actual1 = Square.isPullback("bd", "cd")(("ab", "ab"))
      actual1 must beFalse
      val actual2 = Square.isPullback("bd", "cd")(("ab", "ac"))
      actual2 must beTrue
    }

    "Pullback_none" >> {
      ParallelPair.pullback("a", "b").isBad must beTrue
    }

    "Pullback_same" >> {
      val actual = ParallelPair.pullback("a", "a")
      actual === Good(("0", "0"))
    }

    "Pullback_plain" >> {
      val actual = Square.pullback("bd", "cd")
      actual === Good(("ab", "ac"))
    }

    "Pushout_none" >> {
      ParallelPair.pushout("a", "b").isBad must beTrue
    }

    "Pushout_same" >> {
      val actual = ParallelPair.pushout("a", "a")
      actual === Good(("1", "1"))
    }

    "isPushout_square" >> {
      val actual = Square.isPushout("ab", "ac")(("bd", "cd"))
      actual must beTrue
    }

    "Pushout_plain" >> {
      val actual = Square.pushout("ab", "ac")
      actual === Good(("bd", "cd"))
    }

    "Terminal_none" >> {
      Z2.terminal.isBad must beTrue
      ParallelPair.terminal.isBad must beTrue
    }

    "isTerminal_positive_Square" >> {
      Square.isTerminal("d") must beTrue
    }

    "isTerminal_positive _4_" >> {
      _4_.isTerminal("3") must beTrue
    }

    "isTerminal_negative Square" >> {
      Square.isTerminal("a") must beFalse
      Square.isTerminal("b") must beFalse
    }

    "isTerminal_negative _4_" >> {
      _4_.isTerminal("0") must beFalse
      _4_.isTerminal("1") must beFalse
    }

    "isTerminal_negative ParallelPair" >> {
      ParallelPair.isTerminal("0") must beFalse
      ParallelPair.isTerminal("1") must beFalse
    }

    "Terminal_misc" >> {
      Square.terminal === Good("d")
      _4_.terminal === Good("3")
    }

    "Initial_none" >> {
      Z2.initial.isBad must beTrue
      ParallelPair.initial.isBad must beTrue
    }

    "Initial_misc" >> {
      Square.initial === Good("a")
      _4_.initial === Good("0")
    }

    "foreach" >> {
      _4_ foreach (i => (i.toInt >= 0 && i.toInt < 4) === true)
      ok
    }

    "map" >> {
      val actual = Square map (x => x.toUpperCase)
      actual.iterator.to(Set) === Set("A", "B", "C", "D")
    }

    "allRootObjects_byDefinition" >> {
      ParallelPair.allRootObjects_byDefinition === Set("0")
      Square.allRootObjects_byDefinition === Set("a")
      Pullback.allRootObjects_byDefinition === Set("a", "b")
      M.allRootObjects_byDefinition === Set("b", "d")
      W.allRootObjects_byDefinition === Set("a", "c", "e")
    }

    "allRootObjects_programmersShortcut" >> {
      ParallelPair.allRootObjects_programmersShortcut === Set("0")
      Square.allRootObjects_programmersShortcut === Set("a")
      Pullback.allRootObjects_programmersShortcut === Set("a", "b")
      M.allRootObjects_programmersShortcut === Set("b", "d")
      W.allRootObjects_programmersShortcut === Set("a", "c", "e")
    }

    "allRootObjects" >> {
      ParallelPair.allRootObjects === Set("0")
      Square.allRootObjects === Set("a")
      Pullback.allRootObjects === Set("a", "b")
      M.allRootObjects === Set("b", "d")
      W.allRootObjects === Set("a", "c", "e")
    }

    "allRootObjects_forKnownCategories" >> {
      KnownCategories.filter(c => Sets.isFinite(c.objects)).forall { c =>
        c.allRootObjects_programmersShortcut === c.allRootObjects_byDefinition
      }
    }

    "arrowsFromRootObjects" >> {
      M.arrowsFromRootObjects === Set("b", "ba", "bc", "d", "dc", "de")
      W.arrowsFromRootObjects === Set("a", "ab", "c", "cb", "cd", "e", "ed")
      ParallelPair.arrowsFromRootObjects === Set("0", "a", "b")
      Pullback.arrowsFromRootObjects === Set("a", "ac", "b", "bc")
      Pushout.arrowsFromRootObjects === Set("a", "ab", "ac")
      Square.arrowsFromRootObjects === Set("a", "ab", "ac", "ad")
    }

    "buildBundles M" >> {
      M.buildBundles(Set("a", "d"), Set("a", "d", "dc", "de")) ===
        Map("a" -> Set("a"), "d" -> Set("d", "dc", "de"))
    }

    "buildBundles W" >> {
      W.buildBundles(Set("a", "d"), Set("a", "ab")) ===
        Map("a" -> Set("a", "ab"), "d" -> Set())
    }

    "buildBundles ParallelPair" >> {
      ParallelPair.buildBundles(Set("0"), Set("0", "a", "b")) ===
        Map("0" -> Set("0", "a", "b"))
      ParallelPair.buildBundles(Set("1"), Set("0", "a", "b")) must throwA[IllegalArgumentException]
    }

    "buildBundles Pullback" >> {
      Pullback.buildBundles(Set("a", "c"), Set("a", "ac", "c")) ===
        Map("a" -> Set("a", "ac"), "c" -> Set("c"))
    }

    "buildBundles Pushout" >> {
      Pushout.buildBundles(Set("a", "b"), Set("a", "ab", "ac", "b")) ===
        Map("a" -> Set("a", "ab", "ac"), "b" -> Set("b"))
    }

    "buildBundles Square" >> {
      Square.buildBundles(Set("a"), Set("a", "ab", "ac", "ad")) ===
        Map("a" -> Set("a", "ab", "ac", "ad"))
      Square.buildBundles(Set("b", "c"), Set()) ===
        Map("b" -> Set(), "c" -> Set())
    }

    "isInitial _4_" in {
      _4_.isInitial("0") === true
      _4_.isInitial("1") === false
    }

    "isInitial in Simplicial3" in {
      Simplicial3.isInitial("0") === true
      Simplicial3.isInitial("1") === false
      Simplicial3.isInitial("2") === false
    }

    "isInitial in ParallelPair" in {
      ParallelPair.isInitial("0") === false
    }

    "isInitial in Pullback" in {
      Pullback.isInitial("a") === false
    }

    "op" in {
      val op3 = _3_.op
      import op3._
      op3.arrows === _3_.arrows
      op3.objects === _3_.objects
      op3.d0("1.2".asInstanceOf[op3.Arrow]) === "2"
      op3.validate.iHope
      ok
    }

    // following are tests for accompanying object

    "0" >> {
      val expected = "_0_: ({}, {}, {})"
      val actual = _0_.toString
      actual === expected
      _0_.objects must beEmpty
      _0_.arrows must beEmpty
    }

    "1" >> {
      _1_.objects === Set("0")
      _1_.arrows === Set("0.0")
      _1_.objects.size === 1
      _1_.arrows.size === 1
    }

    "2" >> {
      val sut = _2_
      sut.objects === Set("0", "1")
      val expected = Set("0.0", "0.1", "1.1")
      val arrows = sut.arrows
      arrows === expected
      sut.arrowsBetween("0", "1").size === 1
    }

    "3" >> {
      _3_.objects === Set("0", "1", "2")
      val expected = Set("0.0", "1.1", "2.2", "0.1", "0.2", "1.2")
      expected === _3_.arrows
    }

    "Z2" >> {
      Z2.arrows === Set("1", "a")
      Z2.m("a", "a") === Some("1")
    }

    "SplitMono" >> {
      SplitMono.objects === Set("a", "b")
      SplitMono.arrows === Set("a", "b", "ab", "ba", "bb")
    }

    "M" >> {
      M.objects.size === 5
    }

    "Segment" >> {
      def sut: Cat = segment(3)
      sut === _3_
    }
  }

  "Square" should {
    "pass a regression test of 3/31/19" in {
      val cd = Square.arrow("cd")
      Square.d0(cd) === "c"
      Square.d1(cd) === "d"
    }
  }

  private[cat] def transitiveClosure(
    data: PartialData, previouslyMissing: Int = Int.MaxValue): PartialData = {
    try {
      val stringified = data.toString
    } catch {
      case iae: IllegalArgumentException =>
        throw new IllegalArgumentException("Very bad data", iae)
    }
    val missing = try {
      data.missingCompositions
    } catch {
      case x: Exception =>
        throw new IllegalArgumentException(s"Faled on $data", x)
    }
    if (missing.isEmpty) data else {
      val newData: PartialData = appendArrows(data, missing)
      transitiveClosure(newData, missing.size)
    }
  }

  private def appendArrows(data: PartialData, missing: Iterable[(data.Arrow, data.Arrow)]) = {
    val nonexistentCompositions: Set[(data.Arrow, data.Arrow)] = data.nonexistentCompositions.toSet
    val newArrows: Map[data.Arrow, (data.Obj, data.Obj)] = nonexistentCompositions.flatMap {
      case (f, g) =>
        data.newComposition(f, g).map { h => (h, (data.d0(f), data.d1(g))) }
    }.toMap

    if (newArrows.isEmpty) {
      throw new IllegalArgumentException(s"${data.name}: ${missing.size} arrows still missing: $missing")
    }

    val newGraph: Graph = data.addArrows(newArrows) iHope
    
    val newData = new PartialData(newGraph) {
      override def newComposition(f: Arrow, g: Arrow): Option[Arrow] =
        data.newComposition(f.asInstanceOf[data.Arrow], g.asInstanceOf[data.Arrow]).asInstanceOf[Option[Arrow]]

      override val compositionSource: CompositionTable = data.composition.asInstanceOf[CompositionTable]
    }
    (newData.validateGraph returning newData).orCommentTheError(s"Failed on $newData").iHope
  }

  "Parser, regression test of 6/18/21" should {
    "Parse AAA" in  {
      val source = "AAA: ({1,2,3}, {12: 1 -> 2, 23: 2 -> 3, 31: 3 -> 1})"
      val graph = Graph.read(source)
      graph.isGood === true
      val parser = new CategoryParser

      val data1 = CategoryData.partial[String](graph.iHope)(Map.empty, arrowBuilder)
      val s1 = data1.toString
      val missing1 = data1.missingCompositions
      val data2: PartialData = appendArrows(data1, missing1)

      val nodeStrings = asString(data2.nodes)
      val arrowsSorted: Seq[data2.Arrow] = listSorted(data2.arrows)
      def stringify(a: data2.Arrow) = s"$a: ${data2.d0(a)}->${data2.d1(a)}"
      val arrowStrings =
        arrowsSorted map ((a: data2.Arrow) => stringify(a)) mkString ", "

      val closure = transitiveClosure(data1)

      val raw1 = closure.validate map { validData => validData.newCategory }

      raw1.isGood === true

      val raw2 = data2.build
      raw2.isGood === true

      val category = parser.buildCategory(graph, Map.empty)
      category.isGood === true

      //      val parsed = (new CategoryParser).readCategory(source)
      val parsed = parser.parseAll(parser.category, source)
      parsed match {
        case parser.Success(res, _) => if res.errorDetails.nonEmpty then
          val p = Categories.read(source).iHope
          res.errorDetails === None

        case e: parser.NoSuccess => failure(s"Failed to parse: $e")
      }
      ok
    }

    "Parse AAAAAA" in  {
      val source = "AAAAAA: ({1,2,3,4,5,6}, {12: 1 -> 2, 23: 2 -> 3, 34: 3 -> 4, 45: 4 -> 5, 56: 5 -> 6, 61: 6 -> 1})"
      val graph = Graph.read(source)
      graph.isGood === true
      val parser = new CategoryParser

      val data = CategoryData.partial[String](graph.iHope)(Map.empty, arrowBuilder)

      val missingCompositions: List[(Any, Any)] = data.missingCompositions.toList

      val missing = try {
        data.missingCompositions
      } catch {
        case x: Exception =>
          throw new IllegalArgumentException(s"Faled on $data", x)
      }

      val closure = transitiveClosure(data)

      val raw1 = closure.validate map { validData => validData.newCategory }

      raw1.isGood === true

      val raw2 = data.build
      raw2.isGood === true

      val category = parser.buildCategory(graph, Map.empty)
      category.isGood === true

      val parsed = parser.parseAll(parser.category, source)
      parsed match {
        case parser.Success(res, _) => if (res.errorDetails.nonEmpty) {
          val p = Categories.read(source).iHope
        }
          res.errorDetails === None

        case e: parser.NoSuccess => failure(s"Failed to parse: $e")
      }
      ok
    }

  }
  
  
  // will have to fix the build process
  "AAAAAA" should {
    
    "pass a regression test of 7/7/19" in {
      val cd = AAAAAA.arrow("23")
      AAAAAA.d0(cd) === "2"
      AAAAAA.d1(cd) === "3"
    }
    
    "be one cluster" in {
      val sut = AAAAAA.clusters
      val clusters = sut.d1
      clusters.size === 1
      val cluster = clusters.head
      cluster.size === 6
    }
  }

  "complete subcategory" should {
    "be ok for Square" in {
      Square.completeSubcategory("abd", Set("a", "b", "d")) === Good(category"abd:({a,b,d}, {ab: a -> b, bd: b -> d, ad: a -> d}, {bd ∘ ab = ad})")
      Square.completeSubcategory("diagonal", Set("a", "c")) === Good(category"diagonal:({a,c}, {ac: a -> c})")

      Square.completeSubcategory("abx", Set("a", "b", "x")).errorDetails === Some("Unknown nodes: x")
    }
  }

  "components" should {
    "build 0" in {
      _0_.connectedComponents.isEmpty === true
    }
    "build 3" in {
      _3_.connectedComponents.map(_.name) === Set("_3_.1")
    }
    "build 1+1" in {
      val c1: Category = category"Discrete_2.1:({a}, {a:a->a})"
      val c2: Category = category"Discrete_2.2:({b}, {b:b->b})"
      _1plus1_.connectedComponents === Set(c1, c2)
    }
    "build Z2+ParallelPair" in {
      val sut = category"A:({1, 2, 3}, {1: 1 -> 1, a: 1 -> 1, b: 2 -> 3, c: 2 -> 3}, {1 ∘ 1 = 1, 1 ∘ a = a, a ∘ 1 = a, a ∘ a = 1})"
      sut.connectedComponents === Set(
        category"A.1:({1}, {1: 1 -> 1, a: 1 -> 1}, {1 ∘ 1 = 1, 1 ∘ a = a, a ∘ 1 = a, a ∘ a = 1})",
        category"A:({2, 3}, {b: 2 -> 3, c: 2 -> 3})")
    }
  }
  
  "baseGraph" >> {
    import Graph._

    "good for _3_" >> {
      _3_.baseGraph === graph"({0,1,2}, {0.1: 0 -> 1, 1.2: 1 -> 2})"
    }

    "good for _4_" >> {
      _4_.baseGraph === graph"({0,1,2,3}, {0.1: 0 -> 1, 1.2: 1 -> 2, 2.3: 2 -> 3})"
    }
    
    "good for Pullback" >> {
      Pullback.baseGraph === graph"({a,b,c}, {ac: a -> c, bc: b -> c})"
    }
    
    "good for Square" >> {
      Square.baseGraph === graph"({a,b,c,d}, {ab: a -> b, ac: a -> c, bd: b -> d, cd: c -> d})"
    }

    "good for SplitMono" >> {
      SplitMono.baseGraph === graph"({a,b}, {ab: a -> b, ba: b -> a, bb: b -> b})"
    }
    
    "good for ParallelPair" >> {
      ParallelPair.baseGraph === graph"({0, 1}, {a:0->1, b:0->1})"
    }

    "good for Z3" >> {
      Z3.baseGraph === graph"({0}, {1: 0 -> 0, 2: 0 -> 0})"
    }

    "good for Simplicial3" >> {
//      Simplicial3.canDeduce(Simplicial3.arrows)("0_1") === false
      val baseGraph = Simplicial3.baseGraph
      baseGraph === graph"({0, 1, 2}, {0_1: 0->1, 2_b: 2->2, 2_a: 2->2, swap: 2->2, 2_1: 2->1, b: 1->2, a: 1->2})"
    }

    "good for AAAAAA" >> {
      val actual = AAAAAA.baseGraph
      AAAAAA.baseGraph === graph"({1,2,3,4,5,6}, {12:1->2, 23:2->3, 34:3->4, 45:4->5, 56:5->6, 61:6->1})"
    }
  }

  "SetCategory" >> {
    "have products" >> {
      val first: set = setOf.elements("a", "b")
      val second: set = setOf.elements(1, 2, 3)
      val product = Setf.product(first, second)

      product match {
        case Good((x, y)) =>
          x.d0 === setOf.elements(
            ("a", 1), ("a", 2), ("a", 3),
            ("b", 1), ("b", 2), ("b", 3))
        case otherwise => failure("product not found")
      }
      ok
    }
  }
}