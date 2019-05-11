package math.cat.topos

import math.Test
import math.cat.Category._
import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import math.sets.Sets
import math.sets.Sets.set
import org.specs2.matcher.MatchResult
import scalakittens.Result
import scalakittens.Result._

class GrothendieckToposTest extends Test with TestDiagrams {

  type SUT = Diagram

  def representable(topos: CategoryOfDiagrams) =
    (obj: topos.domain.Obj) ⇒ topos.Representable(obj)
  
//  "Power" should {
//    "exist for representables in Set^_2_" in {
//      val topos = new Diagrams(_2_)
//      import topos.domain._
//      val sut0 = representable(topos)("0")
//      val pow0 = sut0.power
//      val sut1 = representable(topos)("1")
//      val pow1 = sut1.power
//      ok
//    }
//
//    "exist for reps in Set^_2_" in {
//      ok
//    }    
//  }
  
  "Subobject classifier" should {
    "exist for _0_" in {
      val topos = new CategoryOfDiagrams(_0_)
      val omega = topos.Ω
      val points = omega.points
      points.size === 1
    }

    "exist for _1_" in {
      val topos = new CategoryOfDiagrams(_1_)
      val omega = topos.Ω
      val omega0 = omega("0").toList
      omega0.size === 2
      val omega00::omega01::Nil = omega0
      omega00.asInstanceOf[Diagram]("0").isEmpty must beTrue
      omega01.asInstanceOf[Diagram]("0").isEmpty must beFalse
      val points = omega.points
      points.size === 2

      points.map(_.toString) ===
        List("Point0(0→(0→{}))", "Point1(0→(0→{0.0}))")  
    }

    "exist for _2_" in {
      val topos = new CategoryOfDiagrams(_2_)
      
      val omega = topos.Ω
      val omega0 = omega("0")
      omega0.size === 3
      val omega1 = omega("1")
      omega1.size === 2
      val points = omega.points.toList
      points.size === 3
      points(0).toString ===
        "Point0(0→(0→{}, 1→{}), 1→(0→{}, 1→{}))"

      points(1).toString ===
        "Point1(0→(0→{}, 1→{0.1}), 1→(0→{}, 1→{1.1}))"

      points(2).toString ===
        "Point2(0→(0→{0.0}, 1→{0.1}), 1→(0→{}, 1→{1.1}))"
    }

    "exist for _3_" in {
      val topos = new CategoryOfDiagrams(_3_)

      val omega = topos.Ω
      val omega0 = omega("0")
      omega0.size === 4
      val omega1 = omega("1")
      omega1.size === 3
      val points = omega.points.toList
      points.size === 4
      points(0).toString ===
        "Point0(0→(0→{}, 1→{}, 2→{}), 1→(0→{}, 1→{}, 2→{}), 2→(0→{}, 1→{}, 2→{}))"

      points(1).toString ===
        "Point1(0→(0→{}, 1→{}, 2→{0.2}), 1→(0→{}, 1→{}, 2→{1.2}), 2→(0→{}, 1→{}, 2→{2.2}))"

      points(2).toString ===
        "Point2(0→(0→{}, 1→{0.1}, 2→{0.2}), 1→(0→{}, 1→{1.1}, 2→{1.2}), 2→(0→{}, 1→{}, 2→{2.2}))"

      points(3).toString ===
        "Point3(0→(0→{0.0}, 1→{0.1}, 2→{0.2}), 1→(0→{}, 1→{1.1}, 2→{1.2}), 2→(0→{}, 1→{}, 2→{2.2}))"
    }

    "exist for ParallelPair" in {
      val topos = new CategoryOfDiagrams(ParallelPair)

      val omega = topos.Ω
      val points = omega.points.toList
      points.size === 3  // out of 5 possible candidates, 2 split by a or by b, so they are not points
      points(0).toString ===
        "Point0(0→(0→{}, 1→{}), 1→(0→{}, 1→{}))"

      points(1).toString ===
        "Point1(0→(0→{}, 1→{a,b}), 1→(0→{}, 1→{1}))"

      points(2).toString ===
        "Point2(0→(0→{0}, 1→{a,b}), 1→(0→{}, 1→{1}))"
    }

    "exist for Pullback" in {
      val topos = new CategoryOfDiagrams(Pullback)

      val omega = topos.Ω
      val points = omega.points.toList
      points.size === 5
      points(0).toString ===
        "Point0(a→(a→{}, b→{}, c→{}), b→(a→{}, b→{}, c→{}), c→(a→{}, b→{}, c→{}))"

      points(1).toString ===
        "Point1(a→(a→{}, b→{}, c→{ac}), b→(a→{}, b→{}, c→{bc}), c→(a→{}, b→{}, c→{c}))"

      points(2).toString ===
        "Point2(a→(a→{}, b→{}, c→{ac}), b→(a→{}, b→{b}, c→{bc}), c→(a→{}, b→{}, c→{c}))"

      points(3).toString ===
        "Point3(a→(a→{a}, b→{}, c→{ac}), b→(a→{}, b→{}, c→{bc}), c→(a→{}, b→{}, c→{c}))"

      points(4).toString ===
        "Point4(a→(a→{a}, b→{}, c→{ac}), b→(a→{}, b→{b}, c→{bc}), c→(a→{}, b→{}, c→{c}))"
    }

    "exist for Pushout" in {
      val topos = new CategoryOfDiagrams(Pushout)

      val omega = topos.Ω
      val points = omega.points.toList
      points.size === 5
      points(0).toString ===
        "Point0(a→(a→{}, b→{}, c→{}), b→(a→{}, b→{}, c→{}), c→(a→{}, b→{}, c→{}))"

      points(1).toString ===
        "Point1(a→(a→{}, b→{}, c→{ac}), b→(a→{}, b→{}, c→{}), c→(a→{}, b→{}, c→{c}))"

      points(2).toString ===
        "Point2(a→(a→{}, b→{ab}, c→{}), b→(a→{}, b→{b}, c→{}), c→(a→{}, b→{}, c→{}))"

      points(3).toString ===
        "Point3(a→(a→{}, b→{ab}, c→{ac}), b→(a→{}, b→{b}, c→{}), c→(a→{}, b→{}, c→{c}))"

      points(4).toString ===
        "Point4(a→(a→{a}, b→{ab}, c→{ac}), b→(a→{}, b→{b}, c→{}), c→(a→{}, b→{}, c→{c}))"
    }

    "exist for Z3" in {
      val topos = new CategoryOfDiagrams(Z3)

      val omega = topos.Ω
      val points = omega.points.toList
      points.size === 2
      points(0).toString === "Point0(0→(0→{}))"

      points(1).toString === "Point1(0→(0→{0,1,2}))"
    }
  }
  
  "True and False" should {
    "exist for _0_" in {
      val topos = new CategoryOfDiagrams(_0_)
      val omega = topos.Ω
      omega.True === omega.False // that's a degenerate topos
    }
    
    def checkAt(point0: Any)(mappings: (String, set)*): MatchResult[Any] = {
      point0 match {
        case d: Diagram => 
          (traverse {
            for {(k, v) <- mappings } yield OKif(d(k) == v, s"Failed on $k, expected $v, got ${d(k)}")
          } andThen OK) === OK
        case trash => failure(s"Expected a diagram, got $trash")
      }
      ok
    }
    
    
    "exist for _1_" in {
      val topos = new CategoryOfDiagrams(_1_)
      val omega = topos.Ω
      checkAt(omega.False("0"))("0" → Sets.Empty)
      checkAt(omega.True("0"))("0" → Set("0.0"))
    }
    
    "exist for _2_" in {
      val topos = new CategoryOfDiagrams(_2_)
      val omega = topos.Ω
      checkAt(omega.False("0"))("0" → Sets.Empty, "1" → Sets.Empty)
      checkAt(omega.False("1"))("0" → Sets.Empty, "1" → Sets.Empty)
      
      checkAt(omega.True("0"))("0" → Set("0.0"), "1" → Set("0.1"))
      checkAt(omega.True("1"))("0" → Sets.Empty, "1" → Set("1.1"))
    }
    
    "exist for _3_" in {
      val topos = new CategoryOfDiagrams(_3_)
      val omega = topos.Ω
      checkAt(omega.False("0"))("0" → Sets.Empty, "1" → Sets.Empty, "2" → Sets.Empty)
      checkAt(omega.False("1"))("1" → Sets.Empty, "2" → Sets.Empty)
      checkAt(omega.False("2"))("2" → Sets.Empty)

      checkAt(omega.True("0"))("0" → Set("0.0"), "1" → Set("0.1"), "2" → Set("0.2"))
      checkAt(omega.True("1"))("0" → Sets.Empty, "1" → Set("1.1"), "2" → Set("1.2"))
      checkAt(omega.True("2"))("0" → Sets.Empty, "1" → Sets.Empty, "2" → Set("2.2"))
    }

    "exist for ParallelPair" in {
      val topos = new CategoryOfDiagrams(ParallelPair)
      val omega = topos.Ω
      checkAt(omega.False("0"))("0" → Sets.Empty, "1" → Sets.Empty)
      checkAt(omega.False("1"))("0" → Sets.Empty, "1" → Sets.Empty)

      checkAt(omega.True("0"))("0" → Set("0"), "1" → Set("a", "b"))
      checkAt(omega.True("1"))("1" → Set("1"))
    }

    "exist for Pullback" in {
      val topos = new CategoryOfDiagrams(Pullback)
      val omega = topos.Ω
      val False = omega.False
      checkAt(False("a"))("a" → Sets.Empty, "b" → Sets.Empty, "c" → Sets.Empty)
      checkAt(False("b"))("a" → Sets.Empty, "b" → Sets.Empty, "c" → Sets.Empty)
      checkAt(False("c"))("a" → Sets.Empty, "b" → Sets.Empty, "c" → Sets.Empty)

      val True = omega.True
      checkAt(True("a"))("a" → Set("a"), "b" → Sets.Empty, "c" → Set("ac"))
      checkAt(True("b"))("a" → Sets.Empty, "b" → Set("b"), "c" → Set("bc"))
      checkAt(True("c"))("a" → Sets.Empty, "b" → Sets.Empty, "c" → Set("c"))
    }

    "exist for Pushout" in {
      val topos = new CategoryOfDiagrams(Pushout)
      val omega = topos.Ω
      val False = omega.False
      checkAt(False("a"))("a" → Sets.Empty, "b" → Sets.Empty, "c" → Sets.Empty)
      checkAt(False("b"))("a" → Sets.Empty, "b" → Sets.Empty, "c" → Sets.Empty)
      checkAt(False("c"))("a" → Sets.Empty, "b" → Sets.Empty, "c" → Sets.Empty)

      val True = omega.True
      checkAt(True("a"))("a" → Set("a"), "b" → Set("ab"), "c" → Set("ac"))
      checkAt(True("b"))("a" → Sets.Empty, "b" → Set("b"), "c" → Sets.Empty)
      checkAt(True("c"))("a" → Sets.Empty, "b" → Sets.Empty, "c" → Set("c"))
    }

    "exist for Z3" in {
      val topos = new CategoryOfDiagrams(Z3)
      val omega = topos.Ω
      checkAt(omega.False("0"))("0" → Sets.Empty)
      checkAt(omega.True("0"))("0" → Set("0", "1", "2"))
    }
  }
  
  "Classifying map" should {
    
    "exist for ParallelPair" in {
      val topos = new CategoryOfDiagrams(ParallelPair)
      val base = CategoryOfDiagrams.BaseCategory
      val i1: topos.Arrow = topos.inclusionOf(SampleParallelPairSubdiagram1, SampleParallelPairDiagram1) iHope

      val chi1: DiagramArrow = topos.classifyingMap(i1)
      val chi10 = topos.asFunction(chi1("0"))
      chi10(1).toString === "Diagram[ParallelPair](0→{0}, 1→{a,b})"
      chi10(2).toString === "Diagram[ParallelPair](0→{0}, 1→{a,b})"
      chi10(3).toString === "Diagram[ParallelPair](0→{0}, 1→{a,b})"
      chi10(4).toString === "Diagram[ParallelPair](0→{}, 1→{a,b})"
      chi10(5).toString === "Diagram[ParallelPair](0→{}, 1→{a,b})"
      val chi11 = topos.asFunction(chi1("1"))
      chi11(1).toString === "Diagram[ParallelPair](0→{}, 1→{1})"
      chi11(2).toString === "Diagram[ParallelPair](0→{}, 1→{1})"
      chi11(3).toString === "Diagram[ParallelPair](0→{}, 1→{})"
    }
  }
}
