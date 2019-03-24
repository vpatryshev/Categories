package math.cat

import Category._
import math.Test
import math.cat.SetCategory.Setf
import math.sets.Sets.set

class FunctorTest extends Test {
  type SUT = Functor
  
  lazy val categorySquareWithTwoTopLeftCorners: Cat =
    category"({a0,a1,b,c,d}, {a0a1: a0 -> a1, a0b: a0 -> b, a0c: a0 -> c, a1b: a1 -> b, a1c: a1 -> c, bd: b -> d, cd: c -> d, a0d: a0 -> d, a1d: a1 -> d}, {bd o a0b = a0d, cd o a0c = a0d, bd o a1b = a1d, cd o a1c = a1d, a1b o a0a1 = a0b, a1c o a0a1 = a0c, a1d o a0a1 = a0d})"

  lazy val functorFromPullbackToDoubleSquare: SUT = {
    import categorySquareWithTwoTopLeftCorners._
    Functor.build(
      "From2to1toDoubleSquare",
      Pullback, categorySquareWithTwoTopLeftCorners)(
      Map("a" -> "b", "b" -> "c", "c" -> "d"),
      Map("a" -> "b", "b" -> "c", "c" -> "d", "ac" -> "bd", "bc" -> "cd")
    ) getOrElse (throw new InstantiationException("Something wrong with `From2to1toDoubleSquare`"))
  }

  lazy val categorySquareWithTwoRightCorners =
    category"""({a,b,c, d0, d1}
      ,{ab: a -> b, ac: a -> c, ad0: a -> d0, bd0: b -> d0, cd0: c -> d0, ad1: a -> d1, bd1: b -> d1, cd1: c -> d1, d0d1: d0 -> d1}
      ,{bd0 o ab = ad0, cd0 o ac = ad0, bd1 o ab = ad1, cd1 o ac = ad1, d0d1 o ad0 = ad1, d0d1 o bd0 = bd1,d0d1 o bd0 = bd1, d0d1 o cd0 = cd1}
      )"""

  lazy val functorFrom1to2toDoubleSquare: SUT = {
    import categorySquareWithTwoRightCorners._

    Functor.build("From1to2toDoubleSquare",
      Pushout, categorySquareWithTwoRightCorners)(
      Map("a" -> "a", "b" -> "b", "c" -> "c"),
      Map("a" -> "a", "b" -> "b", "c" -> "c", "ab" -> "ab", "ac" -> "ac")
    ).iHope
  }
  
  "Constructor" should {

    "report missing object mappings" in {
      import _4_._
      checkError(_ contains "Object mapping fails for 1", 
      Functor.build("failing test",
        _4_, _4_)(
        Map("0" -> "1"),
        Map.empty[String, String]))
    }

    "report incorrect object mappings" in {
      import _2_._
      checkError(_ contains "Object mapping fails for 1",
        Functor.build("failing test",
          _2_, _2_)(
          Map("0" -> "1", "1" -> "3"),
          Map.empty[String, String]))
    }

    "report missing arrows mappings" in {
      import _4_._
      checkError(_ contains "failing test: arrow mapping not found for 0.2: 0 -> 2",
        Functor.build("failing test",
        _4_, _4_)(
        Map("0" -> "1", "1" -> "2", "2" -> "3", "3" -> "3"),
          Map.empty[String, String]))
    }

    "report missing arrows mappings" in {
      import _4_._
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
      checkError(_ contains "Inconsistent mapping for d1(0.2)",
        Functor.build("id mapping broken", _4_, _4_)(objectMapping, arrowMapping))
    }
    
    "report a failure" in {
      import _4_._
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

      checkError(_ contains "Object mapping fails for 3",
        Functor.build("something wrong here", _4_, _4_)(objectMapping, arrowMapping))
    }
  }

  "functor" should {
    "produce cartesian product" in {
      val from = Category.discrete(Set(0, 1))
      val to = Square

      val mapA: from.Arrow => to.Arrow =
        Map(from.arrow(0) -> to.arrow("b"), from.arrow(1) -> to.arrow("c"))

      type toO = to.Obj
      val mapO: from.Obj => to.Obj =
        Map(from.obj(0) -> to.obj("b"), from.obj(1) -> to.obj("c"))
      
      val fOpt = Functor.build("sample product", from, to)(mapO, mapA)
      check[Functor](fOpt,
        (f:Functor) => {
        val limitOpt = f.limit

        limitOpt match {
          case Some(limit) =>
            limit.arrowTo(f.d0.obj(0)) == "ab"
            limit.arrowTo(f.d0.obj(1)) == "ac"
          case _ => failure(s"Could not build a limit of $f")
        }
      })
    }
    
    "cones from" in {
      val sut = functorFromPullbackToDoubleSquare
      val obj0 = sut.d0.obj _
      val obj1 = sut.d1.obj _
      val arr1 = sut.d1.arrow _
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
      val obj0 = sut.d0.obj _
      val obj1 = sut.d1.obj _
      val arr1 = sut.d1.arrow _
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
      sut.limit match {
        case Some(limit) =>
          limit.vertex == "a1"
          limit.arrowTo(sut.d0.obj("a")) == "a1b"
          limit.arrowTo(sut.d0.obj("b")) == "a1c"
        case oops => failure("no limit?")
      }
      ok
    }
    
    "cocones to" in {
      val sut = functorFrom1to2toDoubleSquare
      val obj0 = sut.d0.obj _
      val obj1 = sut.d1.obj _
      val arr1 = sut.d1.arrow _

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
      val obj0 = sut.d0.obj _
      val obj1 = sut.d1.obj _
      val arr1 = sut.d1.arrow _
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
      val obj0 = sut.d0.obj _
      val obj1 = sut.d1.obj _
      val arr1 = sut.d1.arrow _

      sut.colimit match {
        case Some(colimit) =>
          colimit.vertex == "d1"
          colimit.arrowFrom(obj0("a")) === "ad1"
          colimit.arrowFrom(obj0("b")) === "bd1"
          colimit.arrowFrom(obj0("c")) === "cd1"
        case oops => failure("Could not build a colimit for " + 
                     functorFrom1to2toDoubleSquare.tag)
      }
      ok
    }

    "validate with Set as domain" in {
      val a: set = Set(1, 2, 3)
      val b: set = Set(2, 3, 4)
      val c: set = Set(0, 1)
      val ac = SetFunction("f", a, c, _.toString.toInt % 2)
      val bc = SetFunction("g", b, c, x => (x.toString.toInt + 1) % 2)
      val sutOpt = Functor.build(
        "pullback", Category.Pullback, SetCategory.Setf)(
        Map("a" -> a, "b" -> b, "c" -> c),
        Map("ac" -> ac, "bc" -> bc)
      )
      
      check[Functor](sutOpt,
        sut => {
          sut.d0 === Category.Pullback
          sut.d1 === Setf
          sut.objectsMapping(sut.d0.obj("a")) === a
          sut.objectsMapping(sut.d0.obj("b")) === b
          sut.objectsMapping(sut.d0.obj("c")) === c
          sut.arrowsMapping(sut.d0.arrow("ac")) === ac
        }
      )
    }
  }

}
