package math

import java.util.Date

import math.Base._
import org.specs2.matcher.MatchResult
import org.specs2.mutable._

class BaseTest extends Specification {

  "Base" should {

    "inverse regular" in {
      val m = Map("I" -> 1, "II" -> 2, "III" -> 3, "IV" -> 4)

      val actual = inverse(m)

      def assertRightInverse[A, B](f: Map[A, B], g: Map[B, A]): MatchResult[Any] = {
        for (a <- f.keys) {
          f.get(a).flatMap(g.get) must beSome(a)
        }
        ok
      }

      assertRightInverse(m, actual)
      assertRightInverse(actual, m)
    }
    
    "inverse empty" in {
      val actual = inverse(Map.empty[String, Date])
      actual === Map.empty[Date, String]
    }

    "inverse bad" in {
      val sut = Map("I" -> 1, "II" -> 2, "III" -> 3, "iii" -> 3)

      inverse(sut) must throwA[IllegalArgumentException]
    }

    "toMap" in {
      toMap(List.empty[Object]) === Map.empty[String, Object]
      
      toMap(List("Nada", "I", "II")) === Map(0 -> "Nada", 1 -> "I", 2 -> "II")
    }

    "concatenate" in {
      concat(1, "+", 2) === "1+2"
      concat(-1, "+", 2) === "-1 + 2"
      concat("n", "+", 42) === "n + 42"
      concat("x y", "and", "z") === "(x y) and z"
      concat("x", "and", " y z") === "x and (y z)"
      concat("x y", "and", "z t  ") === "(x y) and (z t)"
    }
  }
}
