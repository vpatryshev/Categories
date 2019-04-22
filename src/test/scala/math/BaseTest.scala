package math

import java.util.Date

import math.Base._
import org.specs2.matcher.MatchResult
import org.specs2.mutable._

class BaseTest extends Specification {

  "Base" should {

    "inverse regular" in {
      val m = Map("I" → 1, "II" → 2, "III" → 3, "IV" → 4)

      val actual = Base.inverse(m)

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
      val sut = Map("I" → 1, "II" → 2, "III" → 3, "iii" → 3)

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
      
      toMap(List("Nada", "I", "II")) === Map(0 → "Nada", 1 → "I", 2 → "II")
    }

//    "id" in {
//      id(Set(1, "x", ())) === Map(1 → 1, "x" → "x", () → ())
//    }
    
    "range 1 arg" in {
      val r0 = range(0)
      r0.size === 0
      r0.iterator.hasNext === false

      val r1 = range(1)
      r1.size === 1
      r1.iterator.hasNext === true
      r1.iterator.next === 0
      
      range(77).size === 77
    }

    "range 2 args" in {
      val r0 = range(2, 2)
      r0.size === 0
      r0.iterator.hasNext === false

      val r1 = range(11, 12)
      r1.size === 1
      r1.iterator.hasNext === true
      r1.iterator.next === 11

      range(77, 88).size === 11
    }

    "range 3 args" in {
      val r0 = range(2, 2, -1)
      r0.size === 0
      r0.iterator.hasNext === false

      val r1 = range(11, 12, 2)
      r1.size === 1
      r1.iterator.hasNext === true
      r1.iterator.next === 11

      val r2 = range(11, 22, 2)
      r2.size === 6
      r2.iterator.hasNext === true
      r2.iterator.next === 11

      range(77, 121, 11).size === 4
    }

  }
}
