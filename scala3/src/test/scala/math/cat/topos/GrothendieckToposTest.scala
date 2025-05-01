package math.cat.topos

import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import math.cat.SetFunction
import org.specs2.matcher.MatchResult

import scala.language.postfixOps
import SetFunction.*

class GrothendieckToposTest extends Fixtures:

  def checkPoints(topos: GrothendieckTopos, expected: String*): MatchResult[Any] =
    val points = topos.Ω.points
    points.map(_.toShortString.replaceFirst("p\\d+\\(", "").dropRight(1)) === expected.toList

  "Subobject classifier" should:
    "be good for`𝟘`" in:
      checkPoints(`Set^𝟘`, "")

    "exist for 𝟙" in {
      val topos = new CategoryOfDiagrams(`𝟙`)
      import topos._
      val points = Ω.points
      points.size === 2
      val omega0 = Ω("0").toList
      omega0 match
        case omega00 :: omega01 :: Nil =>
          omega00("0").isEmpty must beTrue
          omega01("0").isEmpty must beFalse
        case bad => failure(s"Expected 2 points in $bad")

      points.map(_.toShortString) === List("p0(0->())", "p1(0->(0->{0.0}))")
    }

    "exist for 𝟚" in {
      val topos = new CategoryOfDiagrams(`𝟚`)
      import topos._
      val points = Ω.points
      val omega0 = Ω("0")
      omega0.size === 3
      val omega1 = Ω("1")
      omega1.size === 2
      points.map(_.toShortString) === 
        "p0(0->(), 1->())"::"p1(0->(1->{0.1}), 1->(1->{1.1}))"::"p2(0->(0->{0.0}, 1->{0.1}), 1->(1->{1.1}))"::Nil
    }

    "exist for " in {
      val topos = new CategoryOfDiagrams(`𝟛`)
      import topos._
      val points = Ω.points
      val omega0 = Ω("0")
      omega0.size === 4
      val omega1 = Ω("1")
      omega1.size === 3
      points.map(_.toShortString) ===
        "p0(0->(), 1->(), 2->())"::
        "p1(0->(2->{0.2}), 1->(2->{1.2}), 2->(2->{2.2}))"::
        "p2(0->(1->{0.1}, 2->{0.2}), 1->(1->{1.1}, 2->{1.2}), 2->(2->{2.2}))"::
        "p3(0->(0->{0.0}, 1->{0.1}, 2->{0.2}), 1->(1->{1.1}, 2->{1.2}), 2->(2->{2.2}))"::Nil
    }

    "exist for ParallelPair" in {
      val topos = new CategoryOfDiagrams(ParallelPair)
      import topos._
      val points = Ω.points
      points.map(_.toShortString) === "p0(0->(), 1->())"::
                                      "p1(0->(1->{a,b}), 1->(1->{1}))"::
                                      "p2(0->(0->{0}, 1->{a,b}), 1->(1->{1}))"::Nil
    }

    "exist for Pullback" in {
      val topos = new CategoryOfDiagrams(Pullback)
      import topos._
      val points = Ω.points
      points.map(_.toShortString) === "p0(a->(), b->(), c->())"::
                                      "p1(a->(c->{ac}), b->(c->{bc}), c->(c->{c}))"::
                                      "p2(a->(c->{ac}), b->(b->{b}, c->{bc}), c->(c->{c}))"::
                                      "p3(a->(a->{a}, c->{ac}), b->(c->{bc}), c->(c->{c}))"::
                                      "p4(a->(a->{a}, c->{ac}), b->(b->{b}, c->{bc}), c->(c->{c}))"::Nil
    }

    "exist for Pushout" in {
      val topos = new CategoryOfDiagrams(Pushout)
      import topos._
      val points = Ω.points
      points.map(_.toShortString) === "p0(a->(), b->(), c->())"::
                                      "p1(a->(c->{ac}), b->(), c->(c->{c}))"::
                                      "p2(a->(b->{ab}), b->(b->{b}), c->())"::
                                      "p3(a->(b->{ab}, c->{ac}), b->(b->{b}), c->(c->{c}))"::
                                      "p4(a->(a->{a}, b->{ab}, c->{ac}), b->(b->{b}), c->(c->{c}))"::Nil
    }

    "exist for Z3" in {
      val topos = new CategoryOfDiagrams(Z3)
      import topos._
      val points = Ω.points
      points.map(_.toShortString) === "p0(0->())"::"p1(0->(0->{0,1,2}))"::Nil
    }
  }

    "be good for`𝟙`" in:
      checkPoints(`Set^𝟙`, "0->()", "0->(0->{0.0})")

    "be good for `𝟚`" in:
      checkPoints(`Set^𝟚`,
        "0->(), 1->()",
        "0->(1->{0.1}), 1->(1->{1.1})",
        "0->(0->{0.0}, 1->{0.1}), 1->(1->{1.1})")

    "be good for `𝟛`" in:
      checkPoints(`Set^𝟛`,
        "0->(), 1->(), 2->()",
        "0->(2->{0.2}), 1->(2->{1.2}), 2->(2->{2.2})",
        "0->(1->{0.1}, 2->{0.2}), 1->(1->{1.1}, 2->{1.2}), 2->(2->{2.2})",
        "0->(0->{0.0}, 1->{0.1}, 2->{0.2}), 1->(1->{1.1}, 2->{1.2}), 2->(2->{2.2})")
      
    "be good for ParallelPair" in:
      checkPoints(`Set^ParallelPair`,
      "0->(), 1->()",
      "0->(1->{a,b}), 1->(1->{1})",
      "0->(0->{0}, 1->{a,b}), 1->(1->{1})")

    "be good for Pullback" in:
      checkPoints(`Set^Pullback`,
        "a->(), b->(), c->()",
        "a->(c->{ac}), b->(c->{bc}), c->(c->{c})",
        "a->(c->{ac}), b->(b->{b}, c->{bc}), c->(c->{c})",
        "a->(a->{a}, c->{ac}), b->(c->{bc}), c->(c->{c})",
        "a->(a->{a}, c->{ac}), b->(b->{b}, c->{bc}), c->(c->{c})")

    "be good for Pushout" in:
      checkPoints(`Set^Pushout`,
        "a->(), b->(), c->()",
        "a->(c->{ac}), b->(), c->(c->{c})",
        "a->(b->{ab}), b->(b->{b}), c->()",
        "a->(b->{ab}, c->{ac}), b->(b->{b}), c->(c->{c})",
        "a->(a->{a}, b->{ab}, c->{ac}), b->(b->{b}), c->(c->{c})"
      )

    "be good for Z3" in:
      checkPoints(`Set^Z3`, "0->()","0->(0->{0,1,2})")

  "Classifying map" should:

    "be good for ParallelPair" in:
      val topos = `Set^ParallelPair`
      import topos._
      val omega = topos.Ω

      val i1: topos.Arrow = inclusionOf(SampleParallelPairSubdiagram1) in SampleParallelPairDiagram1.asOldDiagram iHope
      val χ1: DiagramArrow = topos.χ(i1)
      val χ10: SetFunction = asFunction(χ1("0"))
      val sample1: χ1.d1.d1.Arrow = χ1("0")
      val sample2 = χ10(1)
      def short(x: Any) = (x match {
        case tD: topos.Diagramme => tD.toShortString
        case d: Diagram => d.source.toShortString
        case other => 
          other.toString
      }).replaceAll("\\s+", "")

      short(χ10(1)) === "Diagram[ParallelPair](0->{0},1->{a,b})"
      short(χ10(2)) === "Diagram[ParallelPair](0->{0},1->{a,b})"
      short(χ10(3)) === "Diagram[ParallelPair](0->{0},1->{a,b})"
      short(χ10(4)) === "Diagram[ParallelPair](1->{a,b})"
      short(χ10(5)) === "Diagram[ParallelPair](1->{a,b})"
      val χ11 = asFunction(χ1("1"))
      short(χ11(1)) === "Diagram[ParallelPair](1->{1})"
      short(χ11(2)) === "Diagram[ParallelPair](1->{1})"
      short(χ11(3)) === "Diagram[ParallelPair]()"

