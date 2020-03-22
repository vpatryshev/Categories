package math.sets

import math.sets.Sets._
import org.specs2.mutable._

class BinaryRelationTest extends Specification {

  "BinaryRelation" should {

    "buildFromPairs" in {
      val r = BinaryRelation(setOf.elements(("one", 1), ("two", 2)))
      r("one", 1) === true
      r("one", 2) === false
      r("two", 1) === false
      r("two", 2) === true
    }
    
    "buildFromFunction" in {
      def f(i: Int, s: String): Boolean = s.length == i
      val r = BinaryRelation(f _)
      
      r(3, "ban") === true
      r(4, "oops") === true
      r(2, "three") === false
    }
  }
}
