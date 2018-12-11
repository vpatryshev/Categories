package math.sets

import math.sets.Functions._
import org.specs2.mutable._

/**
 * Test suite for Functions object
 */
class FunctionsTest extends Specification {

  "Functions" >> {
    "id should be an identity" >> {
      id[String](Set("Hello world"))("привет медвед") === "привет медвед"
      id[String](Set("Hello world")).unapply("привет медвед") === "привет медвед"
    }
    
    "inclusion should be practically usable" >> {
      val f = inclusion[Integer, Number]
      val n:Number = 1
      f(1) === n
    }

    "Schwartzian transform as defined in Wikipedia" >> {
      val f = schwartzianTransform {s: String => s.toUpperCase}
      f.applyTo(Set("aX", "mmm")) === Set(("aX", "AX"), ("mmm", "MMM"))
    }

    "constant is constant" >> {
      constant[Int, String]("love")(42) === "love"
    }
    
    "forList should work" >> {
      val f = forList[String]("uno"::"dos"::"tres"::Nil)
      f(0) === "uno"
      f(1) === "dos"
      f(2) === "tres"
    }
  }
  
  "Bijection" >> {
    "composition with a bijection is still an bijection" >> {
      val f = bijection[Int, Int](_ + 42, _ - 42)
      val g = bijection[Int, Int](_ + 1, _ - 1)
      val fg: Bijection[Int, Int] = f andThen g
      fg(7) === 50
      fg.unapply(7) == -36
    }
    "applyTo" >> {
      val f = bijection[Int, Int](_ + 42, _ - 42)
      f.applyTo(Set.empty) === Set.empty[Int]
      f.applyTo(Set(768, 87, 21)) == Set(810, 129, 63)
    }
    "inverse" >> {
      val f = bijection[Int, Int](_ + 2, _ - 2)
      val g = f.inverse
      g(7) === 5
    }
    "unapply" >> {
      val f = bijection[Int, Int](_ + 2, _ - 2)
      f.unapply(7) === 5
    }
  }
  
  "Injection" >> {
    "applied to a set should produce a set of the same size" >> {
      val s = Set("a", "b", "cdef")
      val f = injection{s: String => s + "!"}
      f.applyTo(s) === Set("a!", "b!", "cdef!")
    }

    "composition with an injection is still an injection" >> {
      val f = injection{s: String => s + "!"}
      val g = injection{s: String => s + "?!"}
      val fg: Injection[String, String] = f andThen g
      ok
    }
  }
}
