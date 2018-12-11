package math.cat

import math.sets.Sets._
import math.cat.SetFunction._
import math.sets.{BinaryRelation, FactorSet}
import org.specs2.mutable._

/**
 * Test suite for Typeless Set Morphisms object
 */

class SetFunctionTest extends Specification {
  "SetFunction" >> {

    "building TypelessSetMorphism" >> {
      val sut = SetFunction("test", Set(1, 2, "a"), Set("x1", "x2", "xa", 77), (x: Any) => "x" + x)
      sut(1) === "x1"
      sut("a") === "xa"
      try {
        sut(3)
        failure("3 is not in domain")
      } catch {
        case e: Exception => // praise the Lord!
      }
      true
    }
    
    "compositions" >> {
      val x = Set(1, 2, "a")
      val y = Set("x1", "x2", "xa", 77)
      val z = Set(2, 28, x)
      val f = new SetFunction("f", x, y, (x: Any) => "x" + x)
      val g = new SetFunction("g", y, z, (y: Any) => y.toString.length)
      g.compose(f) === None
      f.compose(g).isDefined === true
    }

    "TypelessSetMorphism then another" >> {
      val x = Set(1, 2, "a")
      val y = Set("x1", "x2", "xa", 77)
      val z = Set(2, 28, x)
      val f = new SetFunction("f", x, y, (x: Any) => "x" + x)
      val g = new SetFunction("g", y, z, (y: Any) => y.toString.length)
      val sut = f andThen g
      sut(1) === 2
      sut("a") === 2
      try {
        sut(z)
        failure("3 is not in domain")
      } catch {
        case e: Exception => // praise the Lord
      }
      true
    }

    "TypelessSetMorphism before another" >> {
      val x = Set(1, 2, "a")
      val y = Set("x1", "x2", "xa", 77)
      val z = Set(2, 28, x)
      val f = new SetFunction("f", x, y, (x: Any) => "x" + x)
      val g = new SetFunction("g", y, z, (y: Any) => y.toString.length)
      val sut = g before f
      sut.d0 === x
      sut.d1 === z
      f(1) === "x1"
      sut(1) === 2
      sut("a") === 2
      try {
        sut(z)
        failure("3 is not in domain")
      } catch {
        case e: Exception => // praise the Lord
      }
      true
    }

    "building a constant" >> {
      val s0 = Set(1, 2, "a")
      val s1 = Set("x1", "x2", "xa", 77)
      val sut = SetFunction.constant(s0, s1, 77)
      sut.d0 === s0
      sut.d1 === s1
      for (x <- s0) (sut(x) == 77) must beTrue
      try {
        sut(3)
        failure("3 is not in domain")
      } catch {
        case e: Exception => // praise the Lord!
      }
      true
    }

    "building a nonexistent constant" >> {
      try {
        val sut = SetFunction.constant(Set(1, 2, "a"), Set("x1", "x2", "xa", 77), "xx")
        failure("xx is not in codomain")
      } catch {
        case e: Exception => // praise the Lord!
      }
      true
    }

    "building an inclusion" >> {
      val s0 = Set(1, 2, "a")
      val s1 = Set(0, 1, 2, "b", s0, "a")
      val sut = inclusion(s0, s1)
      sut.d0 === s0
      sut.d1 === s1
      for (x <- s0) (sut(x) == x) must beTrue
      try {
        sut("b")
      } catch {
        case e: Exception => // Hallelujah!
      }
      true
    }

    "building a predicate-based inclusion" >> {
      val s = Set(1, 2, 77, 90, 42, "1xya2")
      def predicate = (x: Any) => x.toString.charAt(0) == '1'

      val sut = inclusion(s, predicate)
      sut.d1 === s
      sut.d0 === (s filter predicate)
      for (x <- List(1, "1xya2")) (sut(x) == x) must beTrue
      try {
        sut(2)
        failure("Must have thrown an exception")
      } catch {
        case e: Exception => // Hallelujah!
      }
      true
    }

    "building ai identity" >> {
      val s = Set(1, 2, "a")
      val sut = id(s)
      sut.d0 === s
      sut.d1 === s
      for (x <- s) (sut(x) == x) must beTrue
      try {
        sut("b")
        failure("Should have thrown an exception")
      } catch {
        case e: Exception => // Hallelujah!
      }
      true
    }

    "for factorset" >> {
      val set0: Set[Int] = setOf(1 to 10)
      val set1 = set0.map(i => i:Any)
      def isOdd(x: Any) = x.toString.charAt(0) % 2 == 0
      val br: BinaryRelation[Any, Any] = (a: Any, b: Any) => isOdd(a) == isOdd(b)
      val factoring = new FactorSet[Any](set1, br)
      val s = Array(Set(2, 4, 6, 8), Set(1, 3, 5, 7, 9, 10))
      val sut = forFactorset(factoring)
      val factor = Set(s(1), s(0))
      set1 === sut.d0
      factor === sut.d1
      s(0) === sut(8)
      s(1) === sut(5)
    }

    "exponent 2->2" >> {
      val set1 = setOf[Any](1, 2)

      val sut = SetFunction.exponent(set1, set1)
      sut.size === 4
      val check1 = sut.contains(SetFunction.id(set1))
      check1 must beTrue
    }

    "exponent 3->5" >> {
      val set1 = setOf[Any](3, 4, 5)
      val set2 = setOf[Any](1, 2, 3, 4, 5)

      val sut = SetFunction.exponent(set1, set2)
      sut.size === 125
      for (i <- set2) {
        val c = SetFunction.constant(set1, set2, 1)
        sut.contains(c) must beTrue
      }
      ok
    }
  }
}