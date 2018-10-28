package math.cat

import java.util.Date

import math.cat.Base._
import org.specs2.matcher.MatchResult
import org.specs2.mutable._

class BaseTest extends Specification {

  "BaseTest" should {

    def assertRightInverse[A, B](f: Map[A, B], g: Map[B, A]): MatchResult[Any] = {
      for (a <- f.keys) {
        f.get(a).flatMap(g.get) must beSome(a)
      }
      ok
    }

    "inverse regular" in {
      val m = Map("I" -> 1, "II" -> 2, "III" -> 3, "IV" -> 4)

      val actual = Base.inverse(m)
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

    "setProduct" in {
      setProduct(Set("a", "b"), Set(1, 2)) ===
        Set(("a", 1), ("a", 2), ("b", 1), ("b", 2))
    }

    "setProduct empty" in {
      setProduct(Set("a", "b"), Set.empty[Int]) === Set.empty[(String, Int)]
      setProduct(Set.empty[String], Set(1, 2)) === Set.empty[(String, Int)]
    }

    "toMap" in {
      toMap(List.empty[Object]) === Map.empty[String, Object]
      
      toMap(List("Nada", "I", "II")) === Map(0 -> "Nada", 1 -> "I", 2 -> "II")
    }

    "id" in {
      id(Set(1, "x", ())) === Map(1 -> 1, "x" -> "x", () -> ())
    }

  }
}
