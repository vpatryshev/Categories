package math.cat.topos

import math.Test
import math.cat.Categories.*
import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import math.cat.{Category, SetFunction}
import math.sets.Sets
import math.sets.Sets.{itsaset, set}
import org.specs2.matcher.MatchResult
import scalakittens.Result.*

import scala.language.postfixOps
import SetFunction.*

class GrothendieckToposTest extends Fixtures:

  "Subobject classifier" should {
    "exist for`𝟘`" in:
      val topos = `Set^𝟘`
      import topos._
      val points = Ω.points
      points.size === 1

    "exist for`𝟙`" in {
      val topos = `Set^𝟙`
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

    "exist for _2_" in {
      val topos = `Set^_2_`
      import topos._
      val points = Ω.points
      val omega0 = Ω("0")
      omega0.size === 3
      val omega1 = Ω("1")
      omega1.size === 2
      points.map(_.toShortString) === 
        "p0(0->(), 1->())"::"p1(0->(1->{0.1}), 1->(1->{1.1}))"::"p2(0->(0->{0.0}, 1->{0.1}), 1->(1->{1.1}))"::Nil
    }

    "exist for _3_" in {
      val topos = `Set^_3_`
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
      val topos = `Set^ParallelPair`
      import topos._
      val points = Ω.points
      val expected =
        "p0(0->(), 1->())" ::
        "p1(0->(1->{a,b}), 1->(1->{1}))" ::
        "p2(0->(0->{0}, 1->{a,b}), 1->(1->{1}))" ::
        Nil
      points.map(_.toShortString) === expected
    }

    "exist for Pullback" in:
      val topos = `Set^Pullback`
      import topos._
      val points = Ω.points
      val expected =
        "p0(a->(), b->(), c->())"::
          "p1(a->(c->{ac}), b->(c->{bc}), c->(c->{c}))"::
          "p2(a->(c->{ac}), b->(b->{b}, c->{bc}), c->(c->{c}))"::
          "p3(a->(a->{a}, c->{ac}), b->(c->{bc}), c->(c->{c}))"::
          "p4(a->(a->{a}, c->{ac}), b->(b->{b}, c->{bc}), c->(c->{c}))"::
          Nil

      points.size === expected.size
      val actual = points.map(_.toShortString)

      (actual zip expected) foreach { case (a, e) => a must_== e }

      points.map(_.toShortString) === expected

    "exist for Pushout" in {
      val topos = `Set^Pushout`
      import topos._

      val actualMappings = Ω.objMappings.map(_.toShortString).sorted

      val points = Ω.points
      val actual = points.map(_.toShortString)

      val expected = 
        "p0(a->(), b->(), c->())" ::
        "p1(a->(c->{ac}), b->(), c->(c->{c}))" ::
        "p2(a->(b->{ab}), b->(b->{b}), c->())" ::
        "p3(a->(b->{ab}, c->{ac}), b->(b->{b}), c->(c->{c}))" ::
        "p4(a->(a->{a}, b->{ab}, c->{ac}), b->(b->{b}), c->(c->{c}))" ::
        Nil

      points.size === expected.size

      (actual zip expected) foreach { case (a, e) => a must_== e }
      points.map(_.toShortString) === expected
    }

    "exist for Z3" in {
      val topos = `Set^Z3`
      import topos._
      val points = Ω.points
      points.map(_.toShortString) === "p0(0->())"::"p1(0->(0->{0,1,2}))"::Nil
    }
  }

  "Classifying map" should {

    "exist for ParallelPair" in {
      val topos = `Set^ParallelPair`
      import topos._
      val omega = topos.Ω

      try
        val aboutOmega = omega.toString
        println(aboutOmega)
        ok
      catch
        case e: Throwable =>
          System.err.println(e)
          e.printStackTrace()
          ok
          
      val i1: topos.Arrow = inclusionOf(SampleParallelPairSubdiagram1) in SampleParallelPairDiagram1.asOldDiagram iHope
      val χ1: DiagramArrow = topos.χ(i1)
      val χ10: SetFunction = asFunction(χ1("0"))
      def short(x: Any) = x.toShortString
      short(χ10(1)) === "Diagram[ParallelPair](0->{0}, 1->{a,b})"
      short(χ10(2)) === "Diagram[ParallelPair](0->{0}, 1->{a,b})"
      short(χ10(3)) === "Diagram[ParallelPair](0->{0}, 1->{a,b})"
      short(χ10(4)) === "Diagram[ParallelPair](1->{a,b})"
      short(χ10(5)) === "Diagram[ParallelPair](1->{a,b})"
      val χ11 = asFunction(χ1("1"))
      short(χ11(1)) === "Diagram[ParallelPair](1->{1})"
      short(χ11(2)) === "Diagram[ParallelPair](1->{1})"
      short(χ11(3)) === "Diagram[ParallelPair]()"
    }
  }

