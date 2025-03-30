package math.cat

import math.cat.SetCategory.Setf.asNode
import math.cat.SetFunction._
import math.sets.Sets._
import math.sets.{BinaryRelation, Sets}
import org.specs2.mutable._
import scalakittens.Result
import scalakittens.Result._
import scalakittens.Good
import scalakittens.Good._

import scala.language.postfixOps
import SetFunction.fun

/**
 * Test suite for Typeless Set Morphisms object
 */

class SetFunctionTest extends Specification:
  "SetFunction" >> {

    "building TypelessSetMorphism" >> {
      val sut = fun(Set(1, 2, "a"), Set("x1", "x2", "xa", 77))("test", "x" +)
      sut(1) === "x1"
      sut("a") === "xa"
      
      try
        sut(3)
        failure("3 is not in domain")
      catch
        case e: Exception => // praise the Lord!

      ok
    }
    
    "compositions" >> {
      val x: set = Set(1, 2, "a")
      val y: set = Set("x1", "x2", "xa", 77)
      val z: set = Set(2, 28, x)
      val f = fun(x,y)("f", "x" + )
      val g = fun(y,z)("g", _.length)
      
      g.andThen(f).isDefined === false
      f.andThen(g).isDefined === true
    }

    "TypelessSetMorphism then another" >> {
      val x = Set(1, 2, "a").untyped
      val y = Set("x1", "x2", "xa", 77).untyped
      val z = Set(2, 28, x).untyped
      val f = fun(x,y)("f", "x"+)
      val g = fun(y,z)("g", _.length)
      val sut = Result(f andThen g) iHope
      
      sut(1) === 2
      sut("a") === 2
      try
        sut(z)
        failure("3 is not in domain")
      catch
        case e: Exception => // praise the Lord

      ok
    }

    "building a constant" >> {
      val s0 = Set(1, 2, "a").untyped
      val s1 = Set("x1", "x2", "xa", 77).untyped
      val sut = SetFunction.constant(s0, s1, 77)
      sut.d0 === s0
      sut.d1 === s1
      for x <- s0 do sut(x) === 77

      try
        sut(3)
        failure("3 is not in domain")
      catch
        case e: Exception => // praise the Lord!

      ok
    }

    "building a nonexistent constant" >> {
      try
        val sut = SetFunction.constant(Set(1, 2, "a"), Set("x1", "x2", "xa", 77), "xx")
        failure("xx is not in codomain")
      catch
        case e: Exception => // praise the Lord!

      ok
    }

    "building an inclusion" >> {
      val s0 = Set(1, 2, "a").untyped
      val s1 = Set(0, 1, 2, "b", s0, "a").untyped
      val sut = inclusion(s0, s1).iHope
      sut.d0 === s0
      sut.d1 === s1
      for x <- s0 do sut(x) === x

      try
        sut("b")
      catch
        case e: Exception => // Hallelujah!

      ok
    }

    "building a predicate-based inclusion" >> {
      val s = Set(1, 2, 77, 90, 42, "1xya2").untyped
      def predicate = (x: Any) => x.toString.charAt(0) == '1'

      val sut = filterByPredicate(s)(predicate)
      sut.d1 === s
      sut.d0 === (s filter predicate)
      for x <- List(1, "1xya2") do sut(x) === x
      try
        sut(2)
        failure("Must have thrown an exception")
      catch
        case e: Exception => // Hallelujah!

      ok
    }

    "building ai identity" >> {
      val s: set = Set(1, 2, "a")
      val sut = id(s)
      sut.d0 === s
      sut.d1 === s
      for x <- s do sut(x) === x

      try
        sut("b")
        failure("Should have thrown an exception")
      catch
        case e: Exception => // Hallelujah!

      ok
    }

    "exponent 2->2" >> {
      val set1: set = setOf.elements(1, 2)

      val sut = SetFunction.exponent(set1, set1)
      sut.size === 4
      val check1 = sut.contains(SetFunction.id(set1))
      check1 must beTrue
    }

    "exponent 3->5" >> {
      val set1: set = setOf.elements(3, 4, 5)
      val set2: set = setOf.elements(1, 2, 3, 4, 5)

      val sut = SetFunction.exponent(set1, set2)
      sut.size === 125
      for i <- set2 do
        val c = SetFunction.constant(set1, set2, 1)
        sut.contains(c) must beTrue

      ok
    }

    "check with empty domain or codomain" >> {
      val aset: set = setOf.elements(1, 2, 3, 4, 5)
      val emptyToNonempty: SetFunction = fun(Sets.Empty, aset)("020", _.toString)
      val good: SetFunction = fun(Sets.Empty, Sets.Empty)("good", _.toString)
      good.toString === "good: 0 elements -> 0 elements"
      emptyToNonempty.toString === "020: 0 elements -> 5 elements"
      val bad: Result[SetFunction] = tryBuild(aset, Sets.Empty)("bad", _.toString)
      bad.isBad must beTrue
      bad.errorDetails must beSome
    }

    "check constraint" >> {
      val numbers: set = setOf.elements(1, 2, 3, 4, 5)
      val strings = numbers.map(_.toString)
      val someNumbers = setOf.elements(2, 4)
      val someStrings = someNumbers.map(_.toString)

      val sut: SetFunction = fun(numbers, strings)("good", _.toString)

      val simpleRestriction = sut.restrictTo(someNumbers, strings)
      simpleRestriction.isGood aka s"good: $simpleRestriction" must beTrue

      val anotherGood: Result[SetFunction] = sut.restrictTo(someNumbers, someStrings)
      anotherGood.isGood must beTrue

      anotherGood match
        case Good(sf) => sf.restrictTo(Sets.Empty, Sets.Empty).isGood must beTrue
        case _ => failure("should have been good")

      val emptyToEmpty: Result[SetFunction] = sut.restrictTo(Sets.Empty, Sets.Empty)
      emptyToEmpty.isGood must beTrue

      val emptyToNonempty = sut.restrictTo(Sets.Empty, strings)
      emptyToNonempty.isGood must beTrue
      val bad: Result[SetFunction] = sut.restrictTo(someNumbers, Sets.Empty)
      bad.isBad must beTrue
      bad.errorDetails must beSome

      emptyToEmpty match
        case Good(sf) => sf.restrictTo(numbers, strings).isBad must beTrue
        case _ => failure("should have been good")
        
      ok
    }

    "check constraint" >> {
      val aset: set = setOf.elements(1, 2, 3, 4, 5)
      val emptyToNonempty: SetFunction = fun(Sets.Empty, aset)("020", _.toString)
      val good: SetFunction = fun(Sets.Empty, Sets.Empty)("good", _.toString)
      good.toString === "good: 0 elements -> 0 elements"
      emptyToNonempty.toString === "020: 0 elements -> 5 elements"
      val bad: Result[SetFunction] = tryBuild(aset, Sets.Empty)("bad", _.toString)
      bad.isBad must beTrue
      bad.errorDetails must beSome
    }
  }
