package math.cat

import org.specs2.mutable._
import Category._

class FunctorTest extends Specification {
  
  lazy val categorySquareWithTwoTopLeftCorners: Category[String, String] =
    Category("({a0,a1,b,c,d}, {a0a1: a0 -> a1, a0b: a0 -> b, a0c: a0 -> c, a1b: a1 -> b, a1c: a1 -> c, bd: b -> d, cd: c -> d, a0d: a0 -> d, a1d: a1 -> d}, {bd o a0b = a0d, cd o a0c = a0d, bd o a1b = a1d, cd o a1c = a1d, a1b o a0a1 = a0b, a1c o a0a1 = a0c, a1d o a0a1 = a0d})")

  lazy val functorFromPullbackToDoubleSquare: Functor[String, String, String, String] =
    Functor("From2to1toDoubleSquare",
      Pullback, categorySquareWithTwoTopLeftCorners,
    Map("a" -> "b", "b" -> "c", "c" -> "d"),
    Map("a" -> "b", "b" -> "c", "c" -> "d", "ac" -> "bd", "bc" -> "cd")
  )

  lazy val categorySquareWithTwoRightCorners =
    Category("({a,b,c, d0, d1}"
      + ",{ab: a -> b, ac: a -> c, ad0: a -> d0, bd0: b -> d0, cd0: c -> d0, ad1: a -> d1, bd1: b -> d1, cd1: c -> d1, d0d1: d0 -> d1}"
      +",{bd0 o ab = ad0, cd0 o ac = ad0, bd1 o ab = ad1, cd1 o ac = ad1, d0d1 o ad0 = ad1, d0d1 o bd0 = bd1,d0d1 o bd0 = bd1, d0d1 o cd0 = cd1}"
      + ")")

  lazy val functorFrom1to2toDoubleSquare: Functor[String, String, String, String] =
    Functor("From1to2toDoubleSquare",
      Pushout, categorySquareWithTwoRightCorners,
      Map("a" -> "a", "b" -> "b", "c" -> "c"),
      Map("a" -> "a", "b" -> "b", "c" -> "c", "ab" -> "ab", "ac" -> "ac")
    )
  
  "Constructor" should {

    "report missing object mappings" in {
      Functor("failing test",
        _4_, _4_,
        Map(0 -> 1),
        Map.empty[(Int, Int), (Int, Int)]) should throwA[IllegalArgumentException]
    }

    "report missing arrows mappings" in {
      Functor("failing test",
        _4_, _4_,
        Map(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 3),
        Map.empty[(Int, Int), (Int, Int)]) should throwA[IllegalArgumentException]
    }

    "report missing arrows mappings" in {
      val objectMapping = Map(0 -> 1, 1 -> 2, 2 -> 3 -> 3 -> 3)
      val arrowMapping = Map(
        (0, 0) -> (1, 1),
        (0, 1) -> (1, 2),
        (0, 2) -> (1, 3),
        (0, 3) -> (1, 3),
        (1, 1) -> (2, 3),
        (1, 2) -> (2, 3),
        (1, 3) -> (2, 3),
        (2, 2) -> (3, 3),
        (2, 3) -> (3, 3),
        (3, 3) -> (3, 3)
      )
      Functor("id mapping broken", _4_, _4_, objectMapping, arrowMapping) should
        throwA[IllegalArgumentException]
    }
    
    "report a failure" in {
      val objectMapping = Map(0 -> 1, 1 -> 2, 2 -> 3 -> 3 -> 3)
      val arrowMapping = Map(
        (0, 0) -> (1, 1),
        (0, 1) -> (1, 2),
        (0, 2) -> (1, 3),
        (0, 3) -> (1, 3),
        (1, 1) -> (2, 2),
        (1, 2) -> (2, 3),
        (1, 3) -> (2, 3),
        (2, 2) -> (3, 3),
        (2, 3) -> (3, 3),
        (3, 3) -> (3, 3)
      )
      Functor("something wrong here", _4_, _4_, objectMapping, arrowMapping) should
        throwA[IllegalArgumentException]
    }
    
  }

  "functor" should {
    "produce cartesian product" in {
      val from = Category(Set(0, 1))
      val to = Square
      val map = Map(0 -> "b", 1 -> "c")
      val f = Functor("sample product", from, to, map, map)
      val limitOpt = f.limit
      
      limitOpt match {
        case Some(limit) =>
          limit.arrowTo(0) == "ab"
          limit.arrowTo(1) == "ac"
        case _ => failure(s"Could not build a limit of $f")
      }
      ok
    }
    
    "cones from" in {
      val actual = functorFromPullbackToDoubleSquare.conesFrom("a0")
      val expected = functorFromPullbackToDoubleSquare.Cone("a0", Map(
        "a" -> "a0b",
        "b" -> "a0c",
        "c" -> "a0d")
      )
      actual === Set(expected)
    }
    
    "build all cones" in {
      val allCones = functorFromPullbackToDoubleSquare.allCones
      val c1 = functorFromPullbackToDoubleSquare.Cone("a0", Map(
        "a" -> "a0b",
        "b" -> "a0c",
        "c" -> "a0d")
      )
      
      val c2 = functorFromPullbackToDoubleSquare.Cone("a1", Map(
        "a" -> "a1b",
        "b" -> "a1c",
        "c" -> "a1d")
      )
      
      allCones === Set(c1, c2)
    }
    
    "limit with two candidates" in {
      functorFromPullbackToDoubleSquare.limit match {
        case Some(limit) =>
          limit.vertex == "a1"
          limit.arrowTo("a") == "a1b"
          limit.arrowTo("b") == "a1c"
        case oops => failure("no limit?")
      }
      ok
    }
    
    "cocones to" in {
      val actual = functorFrom1to2toDoubleSquare.coconesTo("d0")
      val expected = functorFrom1to2toDoubleSquare.Cocone("d0", Map(
        "a" -> "ad0",
        "b" -> "bd0",
        "c" -> "cd0"
      ))
      actual === Set(expected)
    }
    
    "all cocones" in {
      val allCocones = functorFrom1to2toDoubleSquare.allCocones
      val expected1 = functorFrom1to2toDoubleSquare.Cocone("d0", Map(
        "a" -> "ad0",
        "b" -> "bd0",
        "c" -> "cd0"
      ))
      val expected2 = functorFrom1to2toDoubleSquare.Cocone("d1", Map(
        "a" -> "ad1",
        "b" -> "bd1",
        "c" -> "cd1"
      ))
      allCocones === Set(expected1, expected2)
    }
    
    "colimit with two candidates" in {
      functorFrom1to2toDoubleSquare.colimit match {
        case Some(colimit) =>
          colimit.vertex == "d1"
          colimit.arrowFrom("a") === "ad1"
          colimit.arrowFrom("b") === "bd1"
          colimit.arrowFrom("c") === "cd1"
        case oops => failure("Could not build a colimit for " + 
                     functorFrom1to2toDoubleSquare.tag)
      }
      ok
    }
  }

}
