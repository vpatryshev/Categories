package math.cat

import java.util.Date

import math.cat.Base._
import org.specs2.matcher.MatchResult
import org.specs2.mutable._

class BaseTest extends Specification {

  "BaseTest" should {

    "buildMap" in {
      buildMap(Array("a", "b", "c"), Array(1, 2, 3, 4)) must_== Map("a" -> 1, "b" -> 2, "c" -> 3)
      buildMap(Array("a", "b", "c"), Array(1, 2)) must_== Map("a" -> 1, "b" -> 2)
      buildMap(Array.empty[String], Array(1, 2)) must_== Map.empty[String, Int]
      buildMap(Array("a", "b", "c"), Array.empty[Int]) must_== Map.empty[String, Int]
    }

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
      actual must_== Map.empty[Date, String]
    }

    "inverse bad" in {
      val sut = Map("I" -> 1, "II" -> 2, "III" -> 3, "iii" -> 3)

      inverse(sut) must throwA[IllegalArgumentException]
    }

    "setProduct" in {
      setProduct(Set("a", "b"), Set(1, 2)) must_==
        Set(("a", 1), ("a", 2), ("b", 1), ("b", 2))
    }

    "setProduct empty" in {
      setProduct(Set("a", "b"), Set.empty[Int]) must_== Set.empty[(String, Int)]
      setProduct(Set.empty[String], Set(1, 2)) must_== Set.empty[(String, Int)]
    }

    "toMap" in {
      toMap(List.empty[Object]) must_== Map.empty[String, Object]
      
      toMap(List("Nada", "I", "II")) must_== Map(0 -> "Nada", 1 -> "I", 2 -> "II")
    }

    "id" in {
      id(Set(1, "x", ())) must_== Map(1 -> 1, "x" -> "x", () -> ())
    }

  }
}
