package math.cat

import math.cat.Functions._
import org.specs2.mutable._

/**
 * Test suite for Functions object
 * @author vpatryshev
 */
class FunctionsTest extends Specification {

  "Functions" >> {
    "your test sucks" >> {
      var n = 0
      def f(x: Int) = { n += 1; x+1}
      n === 0
    }

    "lazy pair should not call its function until asked" >> {
      var n = 0
      def f(x: Int) = {n += 1; x+1; throw new RuntimeException("If the pair were lazy, this exception would not happen")}
      val p = LazyPair(123, f)
      n === 0
    }

    "lazy pair should call its function once" >> {
      var n = 0
      def f(x: Int) = { n += 1; x+1}
      val p = LazyPair(123, f)
      n === 0
      p._1 === 123
      n === 0
      p._2 === 124
      n === 1
      p._2 === 124
      n === 1
    }

    "lazy pair should call its function just once" >> {
      var n = 0
      def f(x: Int) = { n += 1; x+1}
      val p = LazyPair(123, f)
      n === 0
      p._2 === 124
      n === 1
      p._2 === 124
      n === 1
    }

    "injection applied to a set should produce a set of the same size" >> {
      val set = Set("a", "b", "cdef")
      val f = injection{(s: String) => s + "!"}
      f.applyTo(set) === Set("a!", "b!", "cdef!")
    }

    "injection after injection is still an injection" >> {
      val f = injection{(s: String) => s + "!"}
      val g = injection{(s: String) => s + "?!"}
      val fg: Injection[String, String] = f andThen g
      true
    }

    "inclusion should be practically usable" >> {
      val f = inclusion[Integer, Number]
      val n:Number = 1
      f(1) === n
    }

    "Schwartzian transform as defined in Wikipedia" >> {
      val f = schwartzianTransform {(s: String) => s.toUpperCase}
      f.applyTo(Set("aX", "mmm")) === Set(("aX", "AX"), ("mmm", "MMM"))
    }

  }

}
