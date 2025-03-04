package math.cat

import math.Test
import math.cat.Categories._
import math.cat.SetCategory.Setf
import math.sets.Sets.set
import org.specs2.matcher.MatchResult
import scalakittens.Good

import scala.language.postfixOps
import SetFunction.fun

class FunctorTest extends Test:
  type SUT = Functor
  
  lazy val categorySquareWithTwoTopLeftCorners: Cat =
    category"({a0,a1,b,c,d}, {a0a1: a0 -> a1, a0b: a0 -> b, a0c: a0 -> c, a1b: a1 -> b, a1c: a1 -> c, bd: b -> d, cd: c -> d, a0d: a0 -> d, a1d: a1 -> d}, {bd ∘ a0b = a0d, cd ∘ a0c = a0d, bd ∘ a1b = a1d, cd ∘ a1c = a1d, a1b ∘ a0a1 = a0b, a1c ∘ a0a1 = a0c, a1d ∘ a0a1 = a0d})"

  lazy val functorFromPullbackToDoubleSquare: SUT = {
    Functor(
      "From2to1toDoubleSquare",
      Pullback, categorySquareWithTwoTopLeftCorners)(
      Map("a" -> "b", "b" -> "c", "c" -> "d"),
      Map("a" -> "b", "b" -> "c", "c" -> "d", "ac" -> "bd", "bc" -> "cd")
    ) getOrElse (throw new InstantiationException("Something wrong with `From2to1toDoubleSquare`"))
  }

  lazy val categorySquareWithTwoRightCorners =
    category"""({a,b,c, d0, d1}
      ,{ab: a -> b, ac: a -> c, ad0: a -> d0, bd0: b -> d0, cd0: c -> d0, ad1: a -> d1, bd1: b -> d1, cd1: c -> d1, d0d1: d0 -> d1}
      ,{bd0 ∘ ab = ad0, cd0 ∘ ac = ad0, bd1 ∘ ab = ad1, cd1 ∘ ac = ad1, d0d1 ∘ ad0 = ad1, d0d1 ∘ bd0 = bd1,d0d1 ∘ bd0 = bd1, d0d1 ∘ cd0 = cd1}
      )"""

  lazy val functorFrom1to2toDoubleSquare: SUT = {

    Functor("From1to2toDoubleSquare",
      Pushout, categorySquareWithTwoRightCorners)(
      Map("a" -> "a", "b" -> "b", "c" -> "c"),
      Map("a" -> "a", "b" -> "b", "c" -> "c", "ab" -> "ab", "ac" -> "ac")
    ).iHope
  }
  
  "Constructor" should {

    "report missing object mappings" in:
      expectError("Object mapping fails for 1",
      Functor("failing test",
        _4_, _4_)(
        Map("0" -> "1"),
        Map.empty[String, String]))

    "report incorrect object mappings" in:
      expectError("Object mapping fails for 1",
        Functor("failing test",
          _2_, _2_)(
          Map("0" -> "1", "1" -> "3"),
          Map.empty[String, String]))

    "report missing arrows mappings" in:
      expectError("Missing arrow mappings for 0.1, 1.2, 2.3, 0.3, 1.3, 0.2",
        Functor("failing test",
        _4_, _4_)(
        Map("0" -> "1", "1" -> "2", "2" -> "3", "3" -> "3"),
          Map.empty[String, String]))

    "report inconsistent arrow mappings" in:
      val objectMapping: _4_.Node => _4_.Node =
        Map[_4_.Node, _4_.Node]("0" -> "1", "1" -> "2", "2" -> "1", "3" -> "3")
      val arrowMapping: _4_.Arrow => _4_.Arrow = Map[_4_.Arrow, _4_.Arrow](
        "0.0" -> "1.1",
        "0.1" -> "1.2",
        "0.2" -> "1.3",
        "0.3" -> "1.3",
        "1.1" -> "2.3",
        "1.2" -> "2.3",
        "1.3" -> "2.3",
        "2.2" -> "3.3",
        "2.3" -> "3.3",
        "3.3" -> "3.3"
      )
      expectError("Inconsistent mapping for d1(0.2)",
        Functor("id mapping broken", _4_, _4_)(objectMapping, arrowMapping))

    "report a failure" in:
      val objectMapping: _4_.Node => _4_.Node =
        Map[_4_.Node, _4_.Node]("0" -> "1", "1" -> "2", "2" -> "3", "3" -> "4")
      val arrowMapping = Map(
        "0.0" -> "1.1",
        "0.1" -> "1.2",
        "0.2" -> "1.3",
        "0.3" -> "1.3",
        "1.1" -> "2.2",
        "1.2" -> "2.3",
        "1.3" -> "2.3",
        "2.2" -> "3.3",
        "2.3" -> "3.3",
        "3.3" -> "3.3"
      )

      expectError("Object mapping fails for 3",
        Functor("something wrong here", _4_, _4_)(objectMapping, arrowMapping))
  }

  "functor" should {
    "produce cartesian product" in {
      val from = Category.discrete(Set(0, 1))
      val to = Square

      val mapA: from.Arrow => to.Arrow =
        Map(0 -> "b", 1 -> "c")

      type toO = to.Obj
      val mapO: from.Obj => to.Obj =
        Map(0 -> "b", 1 -> "c")

      val fOpt = Functor("sample product", from, to)(mapO, mapA)
      checkOption[Functor](fOpt,
        (f: Functor) =>
          val limitOpt = f.limit

          limitOpt match
            case Good(limit) =>
              limit.arrowTo(0) == "ab" && limit.arrowTo(1) == "ac"
            case bad => failure(s"Could not build a limit of $f: $bad")
      )
    }
    
    "cones from" in {
      val sut = functorFromPullbackToDoubleSquare
      val obj0 = sut.d0.obj
      val obj1 = sut.d1.obj
      val arr1 = sut.d1.asArrow
      val actual: Set[sut.Cone] = sut.conesFrom(obj1("a0"))
      
      val expected: sut.Cone = sut.Cone(obj1("a0"), Map(
        obj0("a") -> arr1("a0b"),
        obj0("b") -> arr1("a0c"),
        obj0("c") -> arr1("a0d")
      ))
        
      actual === Set(expected)
    }
    
    "build all cones" in {
      val sut = functorFromPullbackToDoubleSquare
      val obj0 = sut.d0.obj
      val obj1 = sut.d1.obj
      val arr1 = sut.d1.asArrow
      val allCones = sut.allCones
      val c1 = sut.Cone(obj1("a0"), Map(
        obj0("a") -> arr1("a0b"),
        obj0("b") -> arr1("a0c"),
        obj0("c") -> arr1("a0d")
      ))
      
      val c2 = sut.Cone(obj1("a1"), Map(
        obj0("a") -> arr1("a1b"),
        obj0("b") -> arr1("a1c"),
        obj0("c") -> arr1("a1d")
      ))
      
      allCones === Set(c1, c2)
    }
    
    "limit with two candidates" in {
      val sut = functorFromPullbackToDoubleSquare
      sut.limit match
        case Good(limit) =>
          limit.vertex === "a1"
          limit.arrowTo("a") === "a1b"
          limit.arrowTo("b") === "a1c"
        case oops => failure("no limit?")

      ok
    }
    
    "cocones to" in {
      val sut = functorFrom1to2toDoubleSquare
      val obj0 = sut.d0.obj
      val obj1 = sut.d1.obj
      val arr1 = sut.d1.asArrow

      val actual = sut.coconesTo(obj1("d0"))
      val expected = sut.Cocone(obj1("d0"), Map(
        obj0("a") -> arr1("ad0"),
        obj0("b") -> arr1("bd0"),
        obj0("c") -> arr1("cd0")
      ))
      actual === Set(expected)
    }
    
    "all cocones" in {
      val sut = functorFrom1to2toDoubleSquare
      val obj0 = sut.d0.obj
      val obj1 = sut.d1.obj
      val arr1 = sut.d1.asArrow
      val allCocones = sut.allCocones
      val expected1 = sut.Cocone(obj1("d0"), Map(
        obj0("a") -> arr1("ad0"),
        obj0("b") -> arr1("bd0"),
        obj0("c") -> arr1("cd0")
      ))
      val expected2 = sut.Cocone(obj1("d1"), Map(
        obj0("a") -> arr1("ad1"),
        obj0("b") -> arr1("bd1"),
        obj0("c") -> arr1("cd1")
      ))
      allCocones === Set(expected1, expected2)
    }
    
    "colimit with two candidates" in {
      val sut = functorFrom1to2toDoubleSquare
      val obj0 = sut.d0.obj
      val obj1 = sut.d1.obj
      val arr1 = sut.d1.asArrow

      sut.colimit match
        case Good(colimit) =>
          colimit.vertex === "d1"
          colimit.arrowFrom(obj0("a")) === "ad1"
          colimit.arrowFrom(obj0("b")) === "bd1"
          colimit.arrowFrom(obj0("c")) === "cd1"
        case oops => failure("Could not build a colimit for " + 
                     functorFrom1to2toDoubleSquare.tag)

      ok
    }

    "validate with Set as domain" in {
      val a: set = Set(1, 2, 3)
      val b: set = Set(2, 3, 4)
      val c: set = Set(0, 1)
      val ac = fun(a,c)("f", _.toInt % 2)
      val bc = fun(b,c)("g", x => (x.toInt + 1) % 2)
      val sutOpt = Functor(
        "pullback", Pullback, SetCategory.Setf)(
        Map("a" -> a, "b" -> b, "c" -> c),
        Map("ac" -> ac, "bc" -> bc)
      )
      
      checkOption[Functor](sutOpt,
        sut => {
          sut.d0 === Pullback
          sut.d1 === Setf
          sut.objectsMapping("a") === a
          sut.objectsMapping("b") === b
          sut.objectsMapping("c") === c
          sut.arrowsMapping("ac") === ac
        }
      )
    }
  }
