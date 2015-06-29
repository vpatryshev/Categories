package math.cat

import org.specs2.mutable._

/**
 * Tests for math.cat.Pair class
 */
class PairTest extends Specification {
  "Pair" >> {
    "equals_positive" >> {
      val s = new StringBuilder("a").append(if (1 == 2) "cb" else "bc").toString()

      Pair.pair(s, "def") must_== Pair.pair("abc", "def")
    }

    "equals_negativeX" >> {
      val s = new StringBuilder("a").append(if (1 == 1) "cb" else "bc").toString()
      (pair(s, "def") == pair("abc", "def")) must beFalse
    }

    "equals_negativeY" >> {
      val s = new StringBuilder("d").append(if (1 == 2) "ef" else "fe").toString()
      (pair("abc", s) == pair("abc", "def")) must beFalse
    }
  }
}
