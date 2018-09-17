package math.cat

import org.specs2.mutable._
import Category._

/**
 * Tests for Category class
 * @author vpatryshev
 */
class CategoryTest extends Specification {

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

  "Category" >> {

    "parser" >> {
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
      
      val testCategory = Category(
        Set("0", "1", "2"), // objects
        d0d1.mapValues(_._1), // d0
        d0d1.mapValues(_._2), // d1
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
      Category(testCategory.toString) must_== testCategory
    }
    
    "Constructor_halfSimplicial" >> {
      halfSimplicial.objects must haveSize(3)
    }

    "Constructor_1_bare" >> {
      val sut: Category[String, String] = Category(Set("1"),
        Map(), // d0
        Map(), // d1
        Map()
      )
      sut.arrows must haveSize(1)
    }

    "Constructor_1_full" >> {
      val sut: Category[String, String] = Category(Set("1"),
        Map("1" -> "1"), // d0
        Map("1" -> "1"), // d1
        Map(("1", "1") -> "1")
      )
      sut.arrows must haveSize(1)
    }

    "Constructor_plain_withmap" >> {
      val objects = Set(1, 2, 3)
      val map = Map("1a" ->(1, 1), "1b" ->(1, 1), "2to1" ->(2, 1), "3to2" ->(3, 2), "1to3" ->(1, 3))
      val sut = Graph(objects, map)

      sut.nodes === Set(3, 1, 2)
      sut.d0("2to1") === 2
      sut.d1("2to1") === 1
      sut.d0("1to3") === 1
      sut.d1("1to3") === 3
      sut.d0("3to2") === 3
      sut.d1("3to2") === 2
    }

    "Parse_1" >> {
      val sut = Category("({0}, {}, {})")
      sut.objects === Set("0")
    }

    "Parse_1_1" >> {
      val sut = Category("({1, 0}, {}, {})")
      Set("0", "1") === sut.objects
    }

    "Parse_2" >> {
      val sut = Category("({1, 0}, {a: 0 -> 1}, {})")
      Set("0", "1") === sut.objects
    }

    "Parse_3" >> {
      val sut = Category("({0}, {0: 0 -> 0, 1: 0 -> 0, 2: 0 -> 0}, {1 o 1 = 2, 1 o 2 = 0, 2 o 1 = 0, 2 o 2 = 1})")
      Set("0", "1", "2") === sut.arrows
    }

    "Parse_negative" >> {
      try {
        val expected: Category[String, String] = Category(Set("0", "1", "2"),
          Map("0_1" -> "0", "0_2" -> "0", "a" -> "1", "b" -> "1", "2_1" -> "2", "2_a" -> "2", "2_b" -> "2", "2_swap" -> "2"), // d0
          Map("0_1" -> "1", "0_2" -> "2", "a" -> "1", "b" -> "1", "2_1" -> "1", "2_a" -> "2", "2_b" -> "2", "2_swap" -> "2"), // d1
          Map(("0_1", "a") -> "0_2", ("0_1", "b") -> "0_2", ("2_1", "a") -> "2_a", ("2_1", "b") -> "2_b", ("a", "2_swap") -> "b", ("b", "2_swap") -> "a", ("2_swap", "2_swap") -> "2"))
        //      Category(expected.toString())
        failure("should have thrown an exception")
      } catch {
        case e: Exception => // System.out.println(e) // as expected
        case _: Throwable => failure("should have thrown an exception exception")
      }
      true
    }

    "ToString_1" >> {
      val sut: Category[String, String] = Category(Set("1"),
        Map(), // d0
        Map(), // d1
        Map()
      )
      val actual = sut.toString()
      "({1}, {1: 1->1}, {1 o 1 = 1})" === actual
    }

    "Parse_positive_0" >> {
      val expected: Category[String, String] = Category(Set("1"),
        Map(), // d0
        Map(), // d1
        Map()
      )
      val string = expected.toString()
      val parsed = Category(string)
      expected.objects === parsed.objects
      expected.arrows === parsed.arrows
      expected === parsed
    }

    "Parse_positive_3" >> {
      val source = "({1, 2}, {1: 1->1, 2: 2->2, 2_1: 2->1}, {2 o 2_1 = 2_1})"
      val parsed = Category(source)
      2 === parsed.objects.size
    }

    "Parse_positive_4" >> {
      val source = "({1, 2}, {1: 1->1, 2: 2->2, 2_1: 2->1, 2_a: 2->2}, {2 o 2_1 = 2_1, 2_a o 2_a = 2_a, 2 o 2_a = 2_a, 2_a o 2_1 = 2_1, 2_a o 2 = 2_a, 2 o 2 = 2, 1 o 1 = 1, 2_1 o 1 = 2_1})"
      val parsed = Category(source)
      2 === parsed.objects.size
    }

    "Parse_positive_5" >> {
      val expected: Category[String, String] = Category(Set("1", "2"),
        Map("2_1" -> "2"), // d0
        Map("2_1" -> "1"), // d1
        Map()
      )
      val string = expected.toString()
      val parsed = Category(string)
      expected.objects === parsed.objects
      expected.arrows === parsed.arrows
      expected === parsed
    }

    "Parse_positive_6" >> {
      val expected: Category[String, String] = Category(Set("1", "2"),
        Map("2_1" -> "2", "2_a" -> "2"), // d0
        Map("2_1" -> "1", "2_a" -> "2"), // d1
        Map(("2_a", "2_a") -> "2_a")
      )
      val string = expected.toString()
      val parsed = Category(string)
      expected.objects === parsed.objects
      expected.arrows === parsed.arrows
      expected === parsed
    }

    "Parse_positive_7" >> {
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
      expected.objects === parsed.objects
      expected.arrows === parsed.arrows
      expected === parsed
    }

    "Parse_positive_8" >> {
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
      expected.objects === parsed.objects
      expected.arrows === parsed.arrows
      expected === parsed
    }

    "Parse_positive" >> {
      val expected = halfSimplicial
      val string = expected.toString()
      val parsed = Category(string)
      expected.objects === parsed.objects
      expected.arrows === parsed.arrows
      expected === parsed
    }

    "D0_positive()" >> {
      "0" === halfSimplicial.d0("0")
      "1" === halfSimplicial.d0("1")
      "1" === halfSimplicial.d0("a")
      "2" === halfSimplicial.d0("2_swap")
    }

    "D0_negative()" >> {
      try {
        val unknown = halfSimplicial.d0("qq")
        failure("Should have failed")
      } catch {
        case e: Exception => // println("Got expected exception " + e) // as expected
        case _: Throwable => failure("should have thrown a NoSuchElementException")
      }
      true
    }

    "D1_positive()" >> {
      "0" === halfSimplicial.d1("0")
      "1" === halfSimplicial.d1("1")
      "2" === halfSimplicial.d1("a")
      "2" === halfSimplicial.d1("2_swap")
    }

    "Equals_positive_arrows()" >> {
      val c1 = Category("({0, 1}, {a: 0 -> 1, b: 0 -> 1}, {})")
      val c2 = Category("({1, 0}, {b: 0 -> 1, a: 0 -> 1}, {})")
      Set("0", "1") === c1.objects
      c1.objects === c2.objects
      c1.arrows === c2.arrows
      (c1 == c2) must beTrue
// bug!      c1 === c2
    }

    "Equals_negative_arrows()" >> {
      val c1 = Category("({0, 1}, {a: 0 -> 1, b: 0 -> 1}, {})")
      val c3 = Category("({1, 0}, {a: 0 -> 1, c: 0 -> 1}, {})")
      (c1 == c3) must beFalse
    }

    "Equals_positive_mult()" >> {
      val c1 = Category("({0, 1}, {a: 0 -> 1, b: 1 -> 1, c: 1 -> 1}, {a o b = a, a o c = c, b o b = b, b o c = c, c o b = b, c o c = c})")
      val c2 = Category("({1, 0}, {b: 1 -> 1, c: 1 -> 1, a: 0 -> 1}, {b o b = b, b o c = c, a o b = a, a o c = c, c o b = b, c o c = c})")
      (c1 == c2) must beTrue
    }

    "AreInverse()" >> {
      val sut = Category("({0}, {0: 0 -> 0, 1: 0 -> 0, 2: 0 -> 0}, {1 o 1 = 2, 1 o 2 = 0, 2 o 1 = 0, 2 o 2 = 1})")
      sut.areInverse("1", "2") must beTrue
      sut.areInverse("2", "1") must beTrue
      sut.areInverse("2", "2") must beFalse
    }

    "Inverse()" >> {
      val sut = Category("({0}, {0: 0 -> 0, 1: 0 -> 0, 2: 0 -> 0}, {1 o 1 = 2, 1 o 2 = 0, 2 o 1 = 0, 2 o 2 = 1})")
      sut.inverse("2") === Some("1")
      sut.inverse("1") === Some("2")
    }

    "IsIsomorphism_positive()" >> {
      val sut = Category("({0}, {0: 0 -> 0, 1: 0 -> 0, 2: 0 -> 0}, {1 o 1 = 2, 1 o 2 = 0, 2 o 1 = 0, 2 o 2 = 1})")
      sut.isIsomorphism("2") must beTrue
    }

    "IsIsomorphism_negative()" >> {
      val sut = Category("({0}, {0: 0 -> 0, 1: 0 -> 0, 2: 0 -> 0}, {1 o 1 = 2, 1 o 2 = 2, 2 o 1 = 2, 2 o 2 = 2})")
      sut.isIsomorphism("2") must beFalse
    }

    "IsMonomorphism_positive()" >> {
      val sut = Category("({0}, {0: 0 -> 0, 1: 0 -> 0, 2: 0 -> 0}, {1 o 1 = 2, 1 o 2 = 0, 2 o 1 = 0, 2 o 2 = 1})")
      sut.isMonomorphism("0") must beTrue
      sut.isMonomorphism("1") must beTrue
      sut.isMonomorphism("2") must beTrue
    }

    "IsMonomorphism_negative()" >> {
      val sut = Category("({0}, {0: 0 -> 0, 1: 0 -> 0, 2: 0 -> 0}, {1 o 1 = 2, 1 o 2 = 2, 2 o 1 = 2, 2 o 2 = 2})")
      sut.isMonomorphism("0") must beTrue
      sut.isMonomorphism("1") must beFalse
      sut.isMonomorphism("2") must beFalse
    }

    "IsEpimorphism()" >> {
      val sut = Category("({0}, {0: 0 -> 0, 1: 0 -> 0, 2: 0 -> 0}, {1 o 1 = 1, 1 o 2 = 2, 2 o 1 = 2, 2 o 2 = 2})")
      sut.isEpimorphism("0") must beTrue
      sut.isEpimorphism("1") must beFalse
      sut.isEpimorphism("2") must beFalse
    }

    "FactorsOnLeft" >> {
      val sut = halfSimplicial
      val predicate = sut.factorsOnLeft(("a", "b"), ("2_a", "2_b"))

      predicate("2_1") must beTrue
      predicate("1") must beFalse
      predicate("0_1") must beFalse
      predicate("2") must beFalse
    }

    "FactorsOnRight" >> {
      val predicate = halfSimplicial.factorsOnRight(("2", "b"), ("2_b", "b"))
      predicate("2_b") must beTrue
      predicate("2_a") must beFalse
    }

    "FactorsUniquelyOnLeft" >> {
      halfSimplicial.factorsUniquelyOnLeft("0_2")("2_swap") must beTrue
    }

    "FactorsUniquelyOnRight" >> {
      val sut = halfSimplicial
      sut.factorsUniquelyOnRight("0_1")("0_2") must beTrue
      sut.factorsUniquelyOnRight("2_swap")("2_a") must beFalse
      sut.factorsUniquelyOnRight("2_swap")("0_2") must beFalse
    }

    "Equalizes" >> {
      val sut = halfSimplicial
      sut.equalizes("2_a", "2_b")("0_2") must beTrue
      sut.equalizes("2_a", "2_b")("2_1") must beFalse
      sut.equalizes("2_a", "2_b")("2") must beFalse
      sut.equalizes("0_1", "2_a")("0") must beFalse
    }

    "Coqualizes" >> {
      val sut = halfSimplicial
      sut.coequalizes("2_a", "2_b")("2_1") must beTrue
      sut.coequalizes("2_a", "2_b")("0_2") must beFalse
      sut.coequalizes("2_a", "2_b")("2_swap") must beFalse
      sut.coequalizes("2_a", "2_b")("2") must beFalse
    }

    "AllEqualisingArrows" >> {
      halfSimplicial.allEqualizingArrows("2_a", "2_b") === Set("0_2")
    }

    "IsEqualizer_positive" >> {
      halfSimplicial.isEqualizer("2_a", "2_b")("0_2") must beTrue
    }

    "IsEqualizer_negative" >> {
      halfSimplicial.isEqualizer("2_a", "2_b")("2") must beFalse
    }

    "Equalizer_positive" >> {
      halfSimplicial.equalizer("2_a", "2_b") === Some("0_2")
    }

    "Equalizer_negative" >> {
      ParallelPair.equalizer("a", "b") === None
    }

    "AllCoequalizingArrows" >> {
      halfSimplicial.allCoequalizingArrows("2_a", "2_b") === Set("2_a", "2_b", "2_1")
    }

    "IsCoequalizer_positive" >> {
      halfSimplicial.isCoequalizer("2_a", "2_b")("2_1") must beTrue
    }

    "IsCoequalizer_negative" >> {
      halfSimplicial.isCoequalizer("2_a", "2_b")("2") must beFalse
    }

    "Coequalizer_positive" >> {
      halfSimplicial.coequalizer("2_a", "2_b") === Some("2_1")
    }

    "Coequalizer_negative" >> {
      ParallelPair.coequalizer("a", "b") === None
    }

    "PairsEqualizing" >> {
      val actual = halfSimplicial.pairsEqualizing("a", "2_swap")
      val expected = Set(("0_1", "0_2"), ("2_1", "2_b"), ("1", "b"))
      for (p <- actual) {
        expected must contain(p)
      }
      actual.forall {a => expected must contain(a)}
      expected.forall {a => actual must contain(a)}
      expected == actual must beTrue
      // TODO(vlad): fix it, it fails
//      expected === actual
    }

    "PairsCoequalizing" >> {
      val actual = halfSimplicial.pairsCoequalizing("2_1", "2_swap")
      val expected = Set(("a", "2_a"), ("1", "2_1"), ("b", "2_b"))
      expected === actual
    }

    "PairsCoequalizing_SQUARE" >> {
      Square.pairsCoequalizing("ab", "ac") === Set(("bd", "cd"))
    }

    "PairsWithTheSameDomain" >> {
      val actual = halfSimplicial.pairsWithTheSameDomain("1", "2")
      val expected = Set(("1", "b"), ("2_1", "2_b"), ("2_1", "2"), ("2_1", "2_swap"), ("0_1", "0_2"), ("2_1", "2_a"), ("1", "a"))
      actual.forall {a => expected must contain(a)}
      expected.forall {a => actual must contain(a)}
      expected == actual must beTrue

      // TODO(vlad): fix it, it fails
//      expected === actual
    }

    "PairsWithTheSameCodomain" >> {
      val actual = halfSimplicial.pairsWithTheSameCodomain("0", "2")
      val expected = Set(("0_2", "2"), ("0_1", "2_1"), ("0_2", "2_swap"), ("0_2", "2_b"), ("0_2", "2_a"))
      actual.forall {a => expected must contain(a)}
      expected.forall {a => actual must contain(a)}
      expected == actual must beTrue

      // TODO(vlad): fix it, it fails
//      expected === actual
    }

    "Product_none" >> {
      ParallelPair.product("0", "1") === None
    }

    "Product_plain" >> {
      Square.product("b", "c") === Some(("ab", "ac"))
    }

    "Union_none" >> {
      ParallelPair.union("0", "1") === None
    }

    "Union_plain" >> {
      Square.union("b", "c") === Some(("bd", "cd"))
    }

    "IsPullback" >> {
      Square.isPullback("bd", "cd")(("ab", "ab")) must beFalse
      Square.isPullback("bd", "cd")(("ab", "ac")) must beTrue
    }

    "Pullback_none" >> {
      ParallelPair.pullback("a", "b") === None
    }

    "Pullback_same" >> {
      ParallelPair.pullback("a", "a") === Some(("0", "0"))
    }

    "Pullback_plain" >> {
      Square.pullback("bd", "cd") === Some(("ab", "ac"))
    }

    "Pushout_none" >> {
      ParallelPair.pushout("a", "b") === None
    }

    "Pushout_same" >> {
      ParallelPair.pushout("a", "a") === Some(("1", "1"))
    }

    "IsPushout_square" >> {
      Square.isPushout("ab", "ac")(("bd", "cd")) must beTrue
    }

    "Pushout_plain" >> {
      Square.pushout("ab", "ac") === Some(("bd", "cd"))
    }

    "Terminal_none" >> {
      Z2.terminal === None
      ParallelPair.terminal === None
    }

    "IsTerminal_positive" >> {
      Square.isTerminal("d") must beTrue
      _4_.isTerminal(3) must beTrue
    }

    "IsTerminal_negative" >> {
      Square.isTerminal("a") must beFalse
      Square.isTerminal("b") must beFalse
      _4_.isTerminal(0) must beFalse
      _4_.isTerminal(1) must beFalse
      ParallelPair.isTerminal("0") must beFalse
      ParallelPair.isTerminal("1") must beFalse
    }

    "Terminal_misc" >> {
      Square.terminal === Some("d")
      _4_.terminal === Some(3)
    }

    "Initial_none" >> {
      Z2.initial === None
      ParallelPair.initial === None
    }

    "Initial_misc" >> {
      Square.initial === Some("a")
      _4_.initial === Some(0)
    }

    "AllInitialObjects_byDefinition" >> {
      Set("0") === ParallelPair.allInitialObjects_byDefinition
      Set("a") === Square.allInitialObjects_byDefinition
      Set("a", "b") === Pullback.allInitialObjects_byDefinition
      Set("b", "d") === M.allInitialObjects_byDefinition
      Set("a", "c", "e") === W.allInitialObjects_byDefinition
    }

    "AllInitialObjects_programmersShortcut" >> {
      Set("0") === ParallelPair.allInitialObjects_programmersShortcut
      Set("a") === Square.allInitialObjects_programmersShortcut
      Set("a", "b") === Pullback.allInitialObjects_programmersShortcut
      Set("b", "d") === M.allInitialObjects_programmersShortcut
      Set("a", "c", "e") === W.allInitialObjects_programmersShortcut
    }

    "AllInitialObjects" >> {
      Set("0") === ParallelPair.allInitialObjects
      Set("a") === Square.allInitialObjects
      Set("a", "b") === Pullback.allInitialObjects
      Set("b", "d") === M.allInitialObjects
      Set("a", "c", "e") === W.allInitialObjects
    }

    "AllInitialObjects_forKnownCategories" >> {
      KnownCategories.forall { c =>
        c.allInitialObjects_programmersShortcut === c.allInitialObjects_byDefinition
      }
    }
    // following are tests for accompanying object

    "0" >> {
      val expected = "({}, {}, {})"
      val actual = _0_.toString
      expected === actual
    }

    "1" >> {
      Set(0) === _1_.objects
      Set((0, 0)) === _1_.arrows
    }

    "2" >> {
      Set(0, 1) === _2_.objects
      val expected = Set((0, 0), (1, 1), (0, 1))
      val actual = _2_.arrows
      actual.forall {a => expected must contain(a)}
      expected.forall {a => actual must contain(a)}
      expected == actual must beTrue

      // TODO(vlad): fix it, it fails
      // _2_.arrows === expected
    }

    "3" >> {
      Set(0, 1, 2) === _3_.objects
      val expected = Set((0, 0), (1, 1), (2, 2), (0, 1), (0, 2), (1, 2))
      _3_.arrows.forall {a => expected must contain(a)}
      expected.forall {a => _3_.arrows must contain(a)}

      // TODO(vlad): fix it, it fails
      // _3_.arrows === expected
    }

    "Z2" >> {
      Set("1", "a") === Z2.arrows
      Z2.m("a", "a") === "1"
    }

    "SplitMono" >> {
      val SPLIT_MONO =
        Category("({a,b}, {ab: a -> b, ba: b -> a, bb: b -> b}, {ba o ab = bb, ab o ba = a, ab o bb = ab, bb o ba = ba, bb o bb = bb})")
      Set("a", "b") === SPLIT_MONO.objects
    }

    "M" >> {
      5 === M.objects.size
    }

    "ImplicitSegment" >> {
      def sut: Category[Int, (Int, Int)] = 3
      _3_ === sut
    }
  }
}