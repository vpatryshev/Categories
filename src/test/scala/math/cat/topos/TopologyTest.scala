package math.cat.topos

import scala.language.reflectiveCalls
import math.cat.Categories._
import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import scalakittens.Result

class TopologyTest extends Fixtures {
  
  "Topologies" should {
    "exist for _0_" in {
      val topos = new CategoryOfDiagrams(_0_)
      import topos._
      Ω.True.toString === "⊤"
      val truth = Ω.True.asPredicate
      val ts = truth.toString
      ts === "⊤"
      val sut = LawvereTopology.forPredicate(topos)(truth)

      sut.isGood aka sut.toString must beTrue
      ok
    }

    "exist for _1_" in {
      val topos = new CategoryOfDiagrams(_1_)
      import topos._
      val subs: List[Diagram] = Ω.subobjects.toList
      subs.size === 4
      LawvereTopology.DEBUG = true

      val builder = LawvereTopology.forPredicate(topos)

      val predicates = Result.traverse( for {
        sub <- subs
        fOpt = inclusionOf(sub) in Ω
        predicate = fOpt map predicateFor
      } yield predicate).iHope.toList

//      val t0 = builder(predicates(0))
//      val t1 = builder(predicates(1))
//      val t2 = builder(predicates(2))
//      val t3 = builder(predicates(3))
//      val topologies = predicates map builder
//      
//      val sut = Result traverse topologies

//      sut.isGood aka sut.toString must beTrue
      ok
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
