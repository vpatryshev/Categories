package math.cat

import math.cat.Functions._
import org.specs2.mutable._

/**
 * Test suite for Functions object
 */
class FunctionsTest extends Specification {

  "Functions" >> {
    "your test sucks" >> {
      var n = 0
      def f(x: Int) = { n += 1; x+1}
      n === 0
    }

    "injection applied to a set should produce a set of the same size" >> {
      val set = Set("a", "b", "cdef")
      val f = injection{s: String => s + "!"}
      f.applyTo(set) === Set("a!", "b!", "cdef!")
    }

    "injection after injection is still an injection" >> {
      val f = injection{s: String => s + "!"}
      val g = injection{s: String => s + "?!"}
      val fg: Injection[String, String] = f andThen g
      ok
    }

    "inclusion should be practically usable" >> {
      val f = inclusion[Integer, Number]
      val n:Number = 1
      f(1) === n
    }

    "Schwartzian transform as defined in Wikipedia" >> {
      val f = schwartzianTransform {s: String => s.toUpperCase}
      f.applyTo(Set("aX", "mmm")) === Set(("aX", "AX"), ("mmm", "MMM"))
    }

  }

}
