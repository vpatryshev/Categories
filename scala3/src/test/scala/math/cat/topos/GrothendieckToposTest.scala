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

    "be good for`𝟙`" in:
      checkPoints(`Set^𝟙`, "0->()", "0->(0->{0.0})")

    "be good for _2_" in:
      checkPoints(`Set^_2_`,
        "0->(), 1->()",
        "0->(1->{0.1}), 1->(1->{1.1})",
        "0->(0->{0.0}, 1->{0.1}), 1->(1->{1.1})")

    "be good for _3_" in:
      checkPoints(`Set^_3_`,
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
      def short(x: Any) = x.source.toShortString
      short(χ10(1)) === "Diagram[ParallelPair](0->{0}, 1->{a,b})"
      short(χ10(2)) === "Diagram[ParallelPair](0->{0}, 1->{a,b})"
      short(χ10(3)) === "Diagram[ParallelPair](0->{0}, 1->{a,b})"
      short(χ10(4)) === "Diagram[ParallelPair](1->{a,b})"
      short(χ10(5)) === "Diagram[ParallelPair](1->{a,b})"
      val χ11 = asFunction(χ1("1"))
      short(χ11(1)) === "Diagram[ParallelPair](1->{1})"
      short(χ11(2)) === "Diagram[ParallelPair](1->{1})"
      short(χ11(3)) === "Diagram[ParallelPair]()"

