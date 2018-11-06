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

    "have segments" >> {
      val sut = new CategoryFactory {}
      
      for {
        i <- 0 until 10
      } {
        sut.segment(i)
      }
      ok
    }
    
    "parsing example" >> {
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
      
      try {
        val c = Category
        println(c)
      } catch {
        case x: Exception =>
          x.printStackTrace()
          failure(x.toString)
      }
      
      
      val testCategory = Category.apply(
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
      val string = testCategory.toString
      Category(string) === testCategory
    }
    
    "regression from 6/9/15" >> {
      val expected = Category(
        units = Set("0", "1", "2"),
        domain = Map("0" -> "0", "1" -> "1", "2" -> "2", "a" -> "0", "b" -> "1"),
        codomain = Map("0" -> "0", "1" -> "1", "2" -> "2", "a" -> "2", "b" -> "2"),
        compositionSource = Map[(String, String), String]()
      )
      val source1 = "({0,1,2}, {a: 0 -> 2, b: 1 -> 2}, {0 o a = a})"
      val sample1 = Category(source1)
      sample1 === expected

      val source2 = "({0,1,2}, {a: 0 -> 2, b: 1 -> 2})"
      val sample2 = Category(source1)
      sample2 === expected

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
      sut.objects === Set("0", "1")
    }

    "Parse_2" >> {
      val sut = Category("({1, 0}, {a: 0 -> 1}, {})")
      sut.objects === Set("0", "1")
    }

    lazy val Z3 = Category("({0}, {0: 0 -> 0, 1: 0 -> 0, 2: 0 -> 0}, {1 o 1 = 2, 1 o 2 = 0, 2 o 1 = 0, 2 o 2 = 1})")
    
    "Parse_3" >> {
      Z3.arrows === Set("0", "1", "2")
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
      actual === "({1}, {1: 1->1}, {1 o 1 = 1})"
    }

    "Parse_positive_0" >> {
      val expected: Category[String, String] = Category(Set("1"),
        Map(), // d0
        Map(), // d1
        Map()
      )
      val string = expected.toString()
      val parsed = Category(string)
      parsed.objects === expected.objects
      parsed.arrows === expected.arrows
      parsed === expected
    }

    "Parse_positive_3" >> {
      val source = "({1, 2}, {1: 1->1, 2: 2->2, 2_1: 2->1}, {2 o 2_1 = 2_1})"
      val parsed = Category(source)
      parsed.objects.size === 2
    }

    "Parse_positive_4" >> {
      val source = "({1, 2}, {1: 1->1, 2: 2->2, 2_1: 2->1, 2_a: 2->2}, {2 o 2_1 = 2_1, 2_a o 2_a = 2_a, 2 o 2_a = 2_a, 2_a o 2_1 = 2_1, 2_a o 2 = 2_a, 2 o 2 = 2, 1 o 1 = 1, 2_1 o 1 = 2_1})"
      val parsed = Category(source)
      parsed.objects.size === 2
    }

    "Parse_positive_5" >> {
      val expected: Category[String, String] = Category(Set("1", "2"),
        Map("2_1" -> "2"), // d0
        Map("2_1" -> "1"), // d1
        Map()
      )
      val string = expected.toString()
      val parsed = Category(string)
      parsed.objects === expected.objects
      parsed.arrows === expected.arrows
      parsed === expected
    }

    "Parse_positive_6" >> {
      val expected: Category[String, String] = Category(Set("1", "2"),
        Map("2_1" -> "2", "2_a" -> "2"), // d0
        Map("2_1" -> "1", "2_a" -> "2"), // d1
        Map(("2_a", "2_a") -> "2_a")
      )
      val string = expected.toString()
      val parsed = Category(string)
      parsed.objects === expected.objects
      parsed.arrows === expected.arrows
      parsed === expected
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
      parsed.objects === expected.objects
      parsed.arrows === expected.arrows
      parsed === expected
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
      parsed.objects === expected.objects
      parsed.arrows === expected.arrows
      parsed === expected
    }

    "Parse_positive" >> {
      val expected = halfSimplicial
      val string = expected.toString()
      val parsed = Category(string)
      parsed.objects === expected.objects
      parsed.arrows === expected.arrows
      parsed === expected
    }

    "D0_positive()" >> {
      halfSimplicial.d0("0") === "0"
      halfSimplicial.d0("1") === "1"
      halfSimplicial.d0("a") === "1"
      halfSimplicial.d0("2_swap") === "2"
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
      halfSimplicial.d1("0") === "0"
      halfSimplicial.d1("1") === "1"
      halfSimplicial.d1("a") === "2"
      halfSimplicial.d1("2_swap") === "2"
    }

    "Equals_positive_arrows()" >> {
      val c1 = Category("({0, 1}, {a: 0 -> 1, b: 0 -> 1}, {})")
      val c2 = Category("({1, 0}, {b: 0 -> 1, a: 0 -> 1}, {})")
      c1.objects === Set("0", "1")
      c2.objects === c1.objects
      c2.arrows === c1.arrows
      c1 === c2
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
      Z3.areInverse("1", "2") must beTrue
      Z3.areInverse("2", "1") must beTrue
      Z3.areInverse("2", "2") must beFalse
    }

    "Inverse()" >> {
      Z3.inverse("2") === Some("1")
      Z3.inverse("1") === Some("2")
    }

    "IsIsomorphism_positive()" >> {
      Z3.isIsomorphism("2") must beTrue
    }

    "IsIsomorphism_negative()" >> {
      val sut = Category("({0}, {0: 0 -> 0, 1: 0 -> 0, 2: 0 -> 0}, {1 o 1 = 2, 1 o 2 = 2, 2 o 1 = 2, 2 o 2 = 2})")
      sut.isIsomorphism("2") must beFalse
    }

    "IsMonomorphism_positive()" >> {
      val sut = Z3
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
      val mustDo = sut.factorsUniquelyOnRight("0_1")("0_2")
      mustDo must beTrue
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

    "AllEqualizingArrows" >> {
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
      actual === expected
    }

    "PairsCoequalizing" >> {
      val actual = halfSimplicial.pairsCoequalizing("2_1", "2_swap")
      val expected = Set(("a", "2_a"), ("1", "2_1"), ("b", "2_b"))
      actual === expected
    }

    "PairsCoequalizing_SQUARE" >> {
      Square.pairsCoequalizing("ab", "ac") === Set(("bd", "cd"))
    }

    "PairsWithTheSameDomain" >> {
      val actual = halfSimplicial.pairsWithTheSameDomain("1", "2")
      val expected = Set(("1", "b"), ("2_1", "2_b"), ("2_1", "2"), ("2_1", "2_swap"), ("0_1", "0_2"), ("2_1", "2_a"), ("1", "a"))
      actual === expected
    }

    "PairsWithTheSameCodomain" >> {
      val actual = halfSimplicial.pairsWithTheSameCodomain("0", "2")
      val expected = Set(("0_2", "2"), ("0_1", "2_1"), ("0_2", "2_swap"), ("0_2", "2_b"), ("0_2", "2_a"))
      actual === expected
    }

    "Product_none" >> {
      ParallelPair.product("0", "1") === None
    }

    "Product_plain" >> {
      val actual = Square.product("b", "c")
      actual === Some(("ab", "ac"))
    }

    "Union_none" >> {
      ParallelPair.union("0", "1") === None
    }

    "Union_plain" >> {
      val actual = Square.union("b", "c")
      actual === Some(("bd", "cd"))
    }

    "IsPullback" >> {
      val actual1 = Square.isPullback("bd", "cd")(("ab", "ab"))
      actual1 must beFalse
      val actual2 = Square.isPullback("bd", "cd")(("ab", "ac"))
      actual2 must beTrue
    }

    "Pullback_none" >> {
      ParallelPair.pullback("a", "b") === None
    }

    "Pullback_same" >> {
      val actual = ParallelPair.pullback("a", "a")
      actual === Some(("0", "0"))
    }

    "Pullback_plain" >> {
      val actual = Square.pullback("bd", "cd")
      actual === Some(("ab", "ac"))
    }

    "Pushout_none" >> {
      ParallelPair.pushout("a", "b") === None
    }

    "Pushout_same" >> {
      val actual = ParallelPair.pushout("a", "a")
      actual === Some(("1", "1"))
    }

    "IsPushout_square" >> {
      val actual = Square.isPushout("ab", "ac")(("bd", "cd"))
      actual must beTrue
    }

    "Pushout_plain" >> {
      val actual = Square.pushout("ab", "ac")
      actual === Some(("bd", "cd"))
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
      ParallelPair.allRootObjects_byDefinition === Set("0")
      Square.allRootObjects_byDefinition === Set("a")
      Pullback.allRootObjects_byDefinition === Set("a", "b")
      M.allRootObjects_byDefinition === Set("b", "d")
      W.allRootObjects_byDefinition === Set("a", "c", "e")
    }

    "AllInitialObjects_programmersShortcut" >> {
      ParallelPair.allRootObjects_programmersShortcut === Set("0")
      Square.allRootObjects_programmersShortcut === Set("a")
      Pullback.allRootObjects_programmersShortcut === Set("a", "b")
      M.allRootObjects_programmersShortcut === Set("b", "d")
      W.allRootObjects_programmersShortcut === Set("a", "c", "e")
    }

    "AllInitialObjects" >> {
      ParallelPair.allRootObjects === Set("0")
      Square.allRootObjects === Set("a")
      Pullback.allRootObjects === Set("a", "b")
      M.allRootObjects === Set("b", "d")
      W.allRootObjects === Set("a", "c", "e")
    }

    "AllInitialObjects_forKnownCategories" >> {
      KnownCategories.forall { c =>
        c.allRootObjects_programmersShortcut === c.allRootObjects_byDefinition
      }
    }
    // following are tests for accompanying object

    "0" >> {
      val expected = "({}, {}, {})"
      val actual = _0_.toString
      actual === expected
      _0_.objects must beEmpty
      _0_.arrows must beEmpty
    }

    "1" >> {
      _1_.objects === Set(0)
      _1_.arrows === Set((0, 0))
        _1_.objects.size === 1
      _1_.arrows.size === 1
    }

    "2" >> {
      val sut = _2_
      sut.objects === Set(0, 1)
      val expected = Set((0, 0), (1, 1), (0, 1))
      val arrows = sut.arrows
      arrows === expected
      sut.hom(0, 1).size === 1
    }

    "3" >> {
      _3_.objects === Set(0, 1, 2)
      val expected = Set((0, 0), (1, 1), (2, 2), (0, 1), (0, 2), (1, 2))
      expected === _3_.arrows
    }

    "Z2" >> {
      Z2.arrows === Set("1", "a")
      Z2.m("a", "a") === Some("1")
    }

    "SplitMono" >> {
      val SPLIT_MONO =
        Category("({a,b}, {ab: a -> b, ba: b -> a, bb: b -> b}, {ba o ab = bb, ab o ba = a, ab o bb = ab, bb o ba = ba, bb o bb = bb})")
      SPLIT_MONO.objects === Set("a", "b")
    }

    "M" >> {
      M.objects.size === 5
    }

    "Segment" >> {
      def sut: Category[Int, (Int, Int)] = segment(3)
      sut === _3_
    }
  }
}