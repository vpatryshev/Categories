package math.cat.topos

import scala.language.reflectiveCalls
import math.cat.Categories._
import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import scalakittens.Result

class TopologyTest extends Fixtures {

  "Topologies" should {
//    "exist for _0_" in {
//      val topos: GrothendieckTopos = new CategoryOfDiagrams(_0_)
//      import topos._
//      val points = Ω.points
//      val sut = for {
//        point <- Result.forValue(points.head)
//        inclusion <- topos inclusionOf point in Ω
//        topology <- Topology.forInclusion[Diagram, DiagramArrow](topos, inclusion)
//      } yield topology
//
//      sut.isGood aka sut.toString must beTrue
//    }

    "exist for _1_" in {
      val topos = new CategoryOfDiagrams(_1_)
      import topos._
      val points = Ω.points
      val omega0 = Ω("0").toList
      omega0.size === 2
      val omega00 :: omega01 :: Nil = omega0
      omega00.asInstanceOf[Diagram]("0").isEmpty must beTrue
      omega01.asInstanceOf[Diagram]("0").isEmpty must beFalse
      points.size === 2

      points.map(_.toShortString) === List("p0(0→())", "p1(0→(0→{0.0}))")
    }

    "exist for _2_" in {
      val topos = new CategoryOfDiagrams(_2_)
      import topos._
      val points = Ω.points
      val omega0 = Ω("0")
      omega0.size === 3
      val omega1 = Ω("1")
      omega1.size === 2
      points.map(_.toShortString) === 
        "p0(0→(), 1→())"::"p1(0→(1→{0.1}), 1→(1→{1.1}))"::"p2(0→(0→{0.0}, 1→{0.1}), 1→(1→{1.1}))"::Nil
    }

    "exist for _3_" in {
      val topos = new CategoryOfDiagrams(_3_)
      import topos._
      val points = Ω.points
      val omega0 = Ω("0")
      omega0.size === 4
      val omega1 = Ω("1")
      omega1.size === 3
      points.map(_.toShortString) === "p0(0→(), 1→(), 2→())"::
                                      "p1(0→(2→{0.2}), 1→(2→{1.2}), 2→(2→{2.2}))"::
                                      "p2(0→(1→{0.1}, 2→{0.2}), 1→(1→{1.1}, 2→{1.2}), 2→(2→{2.2}))"::
                                      "p3(0→(0→{0.0}, 1→{0.1}, 2→{0.2}), 1→(1→{1.1}, 2→{1.2}), 2→(2→{2.2}))"::Nil
    }

    "exist for ParallelPair" in {
      val topos = new CategoryOfDiagrams(ParallelPair)
      import topos._
      val points = Ω.points
      points.map(_.toShortString) === "p0(0→(), 1→())"::
                                      "p1(0→(1→{a,b}), 1→(1→{1}))"::
                                      "p2(0→(0→{0}, 1→{a,b}), 1→(1→{1}))"::Nil
    }

    "exist for Pullback" in {
      val topos = new CategoryOfDiagrams(Pullback)
      import topos._
      val points = Ω.points
      points.map(_.toShortString) === "p0(a→(), b→(), c→())"::
                                      "p1(a→(c→{ac}), b→(c→{bc}), c→(c→{c}))"::
                                      "p2(a→(c→{ac}), b→(b→{b}, c→{bc}), c→(c→{c}))"::
                                      "p3(a→(a→{a}, c→{ac}), b→(c→{bc}), c→(c→{c}))"::
                                      "p4(a→(a→{a}, c→{ac}), b→(b→{b}, c→{bc}), c→(c→{c}))"::Nil
    }

    "exist for Pushout" in {
      val topos = new CategoryOfDiagrams(Pushout)
      import topos._
      val points = Ω.points
      points.map(_.toShortString) === "p0(a→(), b→(), c→())"::
                                      "p1(a→(c→{ac}), b→(), c→(c→{c}))"::
                                      "p2(a→(b→{ab}), b→(b→{b}), c→())"::
                                      "p3(a→(b→{ab}, c→{ac}), b→(b→{b}), c→(c→{c}))"::
                                      "p4(a→(a→{a}, b→{ab}, c→{ac}), b→(b→{b}), c→(c→{c}))"::Nil
    }

    "exist for Z3" in {
      val topos = new CategoryOfDiagrams(Z3)
      import topos._
      val points = Ω.points
      points.map(_.toShortString) === "p0(0→())"::"p1(0→(0→{0,1,2}))"::Nil
    }
  }

  "Classifying map" should {

    "exist for ParallelPair" in {
      val topos = new CategoryOfDiagrams(ParallelPair)
      import topos._
      val i1: topos.Arrow = inclusionOf(SampleParallelPairSubdiagram1) in SampleParallelPairDiagram1 iHope

      val chi1: DiagramArrow = topos.χ(i1)
      val chi10 = topos.asFunction(chi1("0"))
      chi10(1).toShortString === "Diagram[ParallelPair](0→{0}, 1→{a,b})"
      chi10(2).toShortString === "Diagram[ParallelPair](0→{0}, 1→{a,b})"
      chi10(3).toShortString === "Diagram[ParallelPair](0→{0}, 1→{a,b})"
      chi10(4).toShortString === "Diagram[ParallelPair](1→{a,b})"
      chi10(5).toShortString === "Diagram[ParallelPair](1→{a,b})"
      val chi11 = topos.asFunction(chi1("1"))
      chi11(1).toShortString === "Diagram[ParallelPair](1→{1})"
      chi11(2).toShortString === "Diagram[ParallelPair](1→{1})"
      chi11(3).toShortString === "Diagram[ParallelPair]()"
    }
  }
}
