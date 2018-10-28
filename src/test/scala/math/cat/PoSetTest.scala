package math.cat

import org.specs2.mutable._

class PoSetTest extends Specification {

  "PoSet" >> {
    "parse" >> {
      val expected = new PoSet[String](Set("abc", "def", "ab", "defgh"), (x,y) => y contains x)
      val actual: PoSet[String] =
        PoSet("({abc, def, defgh, ab},{abc<=abc, def<=def, def<=defgh, defgh<=defgh, ab<=abc, ab<=ab})")

      actual.underlyingSet === expected.underlyingSet
      val product = Sets.product2(expected.underlyingSet, expected.underlyingSet)

      for {p <- product} {
        // could not use `===`, something wrong with it in combination with `aka`
        (actual.le(p) aka s"@<<$p>>: ${actual.le(p)}") must_== expected.le(p)
      }
      expected.equal(actual) must beTrue // here we check equality
      actual.equal(expected) must beTrue // here we check equality again
    }

    "plain pairs" >> {
      val sut = PoSet(Set("a", "b", "c"), Set(("a", "b"), ("a", "c"), ("b", "c")))
      sut.le("a", "c") must beTrue
    }

    "plain comparator" >> {
      val sut = PoSet(Set("a", "b", "c"), (a: String, b: String) => a <= b)
      sut.le("a", "c") must beTrue
    }

    "Constructor_negativeTransitivity" >> {
      PoSet(Set("a", "b", "c"), Set(("a", "b"), ("b", "c"))) should throwA[IllegalArgumentException]
    }

    "Constructor_negativeAntireflexivity" >> {
        PoSet(Set("a", "b", "c"), Set(("a", "b"), ("b", "a"))) should throwA[IllegalArgumentException]
    }

    "Equals_positive" >> {
      val sut1 = PoSet(Set("a", "b", "c"), Set(("a", "b"), ("a", "c"), ("b", "c")))
      val sut2 = PoSet(Set("c", "a", "b"), (x: String, y: String) => x <= y)
      (sut2 == sut1) must beTrue
      (sut1 == sut2) must beTrue
    }

    "Equals_negative" >> {
      val sut1 = PoSet(Set("a", "b", "c"), Set(("a", "b"), ("a", "c"), ("c", "b")))
      def naturalOrder(p: (String, String)) = p._1 <= p._2
      val sut2 = PoSet(Set("c", "a", "b"), (x: String, y: String) => x <= y)
      (sut1 == sut2) must beFalse
    }

    "Equals_differentTypes" >> {
      val sut1 = PoSet(Set("1", "2", "3"), Set(("1", "2"), ("1", "3"), ("2", "3")))
      val sut2 = PoSet(Set(1, 2, 3), Set((1, 2), (1, 3), (2, 3)))
      (sut1 == sut2) must beFalse
    }

    "UnaryOp" >> {
      val sut = PoSet(Set("a", "b", "c"), (a: String, b: String) => a <= b)
      val opsut = ~sut

      opsut.le("c", "a") must beTrue
      ~opsut === sut
    }

    "Discrete" >> {
      val sut: PoSet[Int] = PoSet(Set(1, 2, 3))
      sut.le(3, 3) must beTrue
      sut.le(2, 3) must beFalse
      sut.le(3, 2) must beFalse
      sut == ~sut must beTrue
    }

    "UnderlyingSet" >> {
      val sut = PoSet(Set("a", "b", "c"), (a: String, b: String) => a <= b)
      sut.underlyingSet === Set("a", "b", "c")
    }

    "Paarser" >> {
      val expected = PoSet(Set("a", "b", "c"), (a: String, b: String) => a <= b)
      val actual: PoSet[String] = PoSet("( { a, b, c} , { a <= b, b <= c, a <= c})")
      actual.size === 3
      actual.underlyingSet === Set("a", "b", "c")
      actual.le("a", "b") must beTrue
      actual.le("b", "c") must beTrue
      actual.le("a", "c") must beTrue
      expected === actual
    }

    "Range" >> {
      val sut = PoSet.range(0, 3, 1)
      sut contains 2 must beTrue
      sut.le(1, 2) must beTrue
      sut.le(0, 2) must beTrue
      sut.le(0, 1) must beTrue
    }
  }
}
