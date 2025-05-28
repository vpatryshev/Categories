package math.sets

import math.sets.Sets._
import org.specs2.mutable._
import testing.TestBase

class BinaryRelationTest extends TestBase:

  "BinaryRelation" should:

    "buildFromPairs" in :
      val r = BinaryRelation(setOf.elements(("one", 1), ("two", 2)))
      r("one", 1) must beTrue
      r("one", 2) must beFalse
      r("two", 1) must beFalse
      r("two", 2) must beTrue
    
    "buildFromFunction" in :
      def f(i: Int, s: String): Boolean = s.length == i
      val r = BinaryRelation(f)
      
      r(3, "ban") must beTrue
      r(4, "oops") must beTrue
      r(2, "three") must beFalse
