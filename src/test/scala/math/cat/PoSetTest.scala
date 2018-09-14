package math.cat

import org.specs2.mutable._

class PoSetTest extends Specification {

  "PoSet" >> {
    "parse" >> {
      val expected = new PoSet[String](Set("abc", "def", "ab", "defgh"), (x,y) => y contains x)
      val actual: PoSet[String] = try {
        PoSet("({abc, def, defgh, ab},{abc<=abc, def<=def, def<=defgh, defgh<=defgh, ab<=abc, ab<=ab})")
      } catch {
        case x: Exception =>
          failure(s"Baaad: $x")
          ???
      }
      val isEqual = expected.underlyingSet == actual.underlyingSet
      val product = Sets.product2(expected.underlyingSet, expected.underlyingSet)
      val fe = (isEqual /: product) ((bool, p) => bool && (expected.le(p) == actual.le(p)))

      isEqual must beTrue
      for {p <- product} {
        expected.le(p) aka s"@$p" must_== actual.le(p)
      }
      fe must beTrue
      expected.equal(actual) must beTrue
      actual.equal(expected) must beTrue
      (expected == actual) aka actual.toString must beTrue
      (actual == expected) aka actual.toString must beTrue
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
      try {
        PoSet(Set("a", "b", "c"), Set(("a", "b"), ("b", "c")))
        failure("Validator should have thrown an exception")
      } catch {
        case e: IllegalArgumentException => true
      }
      true
    }

    "Constructor_negativeAntireflexivity" >> {
      try {
        val sut = PoSet(Set("a", "b", "c"), Set(("a", "b"), ("b", "a")))
        failure("Validator should have thrown an exception")
      } catch {
        case e: IllegalArgumentException => true
      }
      true
    }

    "Equals_positive" >> {
      val sut1 = PoSet(Set("a", "b", "c"), Set(("a", "b"), ("a", "c"), ("b", "c")))
      val sut2 = PoSet(Set("c", "a", "b"), (x: String, y: String) => x <= y)
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
      sut.underlyingSet == Set("a", "b", "c") must beTrue
    }

    "Paarser" >> {
      val expected = PoSet(Set("a", "b", "c"), (a: String, b: String) => a <= b)
      val actual: PoSet[String] = PoSet("( { a, b, c} , { a <= b, b <= c, a <= c})")
      3 == actual.size must beTrue
      println(actual)
      actual.underlyingSet == Set("a", "b", "c") must beTrue
      actual.le("a", "b") must beTrue
      actual.le("b", "c") must beTrue
      actual.le("a", "c") must beTrue
      expected == actual must beTrue
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
