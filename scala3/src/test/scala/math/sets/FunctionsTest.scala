package math.sets

import math.sets.Functions._
import org.specs2.mutable._
import testing.TestBase

/**
 * Test suite for Functions object
 */
class FunctionsTest extends TestBase:

  "Functions" should:
    "id should be an identity" in :
      id[String](Set("Hello world"))("привет медвед") === "привет медвед"
      id[String](Set("Hello world")).unapply("привет медвед") === "привет медвед"
    
    "inclusion should be practically usable" in :
      val f = inclusion[Integer, Number]
      val n:Number = 1
      f(1) must be_==(n)

    "Schwartzian transform as defined in Wikipedia" in :
      val f = schwartzianTransform { (s: String) => s.toUpperCase}
      val actual = Set("aX", "mmm").map(f)
      val expected = Set(("aX", "AX"), ("mmm", "MMM"))
      actual must be_==(expected)
  
  "Bijection" should:
    "composition with a bijection is still an bijection" in :
      val f = bijection[Int, Int](_ + 42, _ - 42)
      val g = bijection[Int, Int](_ + 1, _ - 1)
      val fg: Bijection[Int, Int] = f andThen g
      fg(7) === 50
      fg.unapply(7) === -36
    
    "applyTo" in :
      val f = bijection[Int, Int](_ + 42, _ - 42)
      Set.empty.map(f) must be_==(Set.empty[Int])
      Set(768, 87, 21).map(f) must be_==(Set(810, 129, 63))

    "inverse" in :
      val f = bijection[Int, Int](_ + 2, _ - 2)
      val g = f.inverse
      g(7) === 5

    "unapply" in :
      val f = bijection[Int, Int](_ + 2, _ - 2)
      f.unapply(7) === 5
  
  "Injection" should:
    "applied to a set should produce a set of the same size" in :
      val s = Set("a", "b", "cdef")
      val f = injection{ (s: String) => s + "!"}
      s.map(f) must be_==(Set("a!", "b!", "cdef!"))

    "composition with an injection is still an injection" in :
      val f = injection{ (s: String) => s + "!"}
      val g = injection{ (s: String) => s + "?!"}
      val fg: Injection[String, String] = f andThen g
      ok
