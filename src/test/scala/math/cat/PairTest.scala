package math.cat

import org.specs2.mutable._
import Pair._

/**
 * Tests for math.cat.Pair class
 */
class PairTest extends Specification {
  "Pair" >> {
    "equals_positive" >> {
      // have to cheat the compiler
      val s = new StringBuilder("a").append(if (1 == 2) "cb" else "bc").toString()

      Pair(s, "def") must_== Pair("abc", "def")
    }

    "equals_negativeX" >> {
      val s = new StringBuilder("a").append(if (1 == 1) "cb" else "bc").toString()
      (Pair(s, "def") == Pair("abc", "def")) must beFalse
    }

    "equals_negativeY" >> {
      val s = new StringBuilder("d").append(if (1 == 2) "ef" else "fe").toString()
      (Pair("abc", s) == Pair("abc", "def")) must beFalse
    }
    
    "withLeft" >> {
      def sut[Y] = Pair.withLeft[String, Y]("hello")
      sut(42) should_== Pair("hello", 42)
      sut("world") should_== Pair("hello", "world")
    }

    "withRight" >> {
      def sut[X] = Pair.withRight[X, String]("hello")
      sut(42) should_== Pair(42, "hello")
      sut("Kitty") should_== Pair("Kitty", "hello")
    }
    
  }
  
}
