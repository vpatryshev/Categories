package math.cat.topos.logic

import math.cat.Categories._
import math.cat.topos.{CategoryOfDiagrams, Diagram, Fixtures}
import math.sets.Sets
import math.sets.Sets.set
import org.specs2.matcher.MatchResult
import scalakittens.Result
import scalakittens.Result._

import scala.language.reflectiveCalls

class ConstantsTest extends Fixtures {
  "True and False" should {
    "exist for _0_" in {
      val topos = new CategoryOfDiagrams(_0_)
      import topos._
      Ω.True.toString === "⊤"
      val tTrue = Ω.True.mapping
      val tFalse = Ω.False.mapping
      tTrue === tFalse // that's a degenerate topos, but tags are still distinct
    }

    def checkAt(point: Any)(mappings: (String, set)*): MatchResult[Any] = {
      point match {
        case d: Diagram =>
          Result.check {
            for {(k, v) <- mappings } yield OKif(d(k) == v, s"Failed on $k, expected $v, got ${d(k)}")
          } === OK
        case trash => failure(s"Expected a diagram, got $trash")
      }
      ok
    }


    "exist for _1_" in {
      val topos = new CategoryOfDiagrams(_1_)
      val omega = topos.Ω
      checkAt(omega.False("0"))("0" -> Sets.Empty)
      checkAt(omega.True("0"))("0" -> Set("0.0"))
    }

    "exist for _2_" in {
      val topos = new CategoryOfDiagrams(_2_)
      val omega = topos.Ω
      checkAt(omega.False("0"))("0" -> Sets.Empty, "1" -> Sets.Empty)
      checkAt(omega.False("1"))("0" -> Sets.Empty, "1" -> Sets.Empty)

      checkAt(omega.True("0"))("0" -> Set("0.0"), "1" -> Set("0.1"))
      checkAt(omega.True("1"))("0" -> Sets.Empty, "1" -> Set("1.1"))
    }

    "exist for _3_" in {
      val topos = new CategoryOfDiagrams(_3_)
      val omega = topos.Ω
      checkAt(omega.False("0"))("0" -> Sets.Empty, "1" -> Sets.Empty, "2" -> Sets.Empty)
      checkAt(omega.False("1"))("1" -> Sets.Empty, "2" -> Sets.Empty)
      checkAt(omega.False("2"))("2" -> Sets.Empty)

      checkAt(omega.True("0"))("0" -> Set("0.0"), "1" -> Set("0.1"), "2" -> Set("0.2"))
      checkAt(omega.True("1"))("0" -> Sets.Empty, "1" -> Set("1.1"), "2" -> Set("1.2"))
      checkAt(omega.True("2"))("0" -> Sets.Empty, "1" -> Sets.Empty, "2" -> Set("2.2"))
    }

    "exist for ParallelPair" in {
      val topos = new CategoryOfDiagrams(ParallelPair)
      val omega = topos.Ω
      checkAt(omega.False("0"))("0" -> Sets.Empty, "1" -> Sets.Empty)
      checkAt(omega.False("1"))("0" -> Sets.Empty, "1" -> Sets.Empty)

      checkAt(omega.True("0"))("0" -> Set("0"), "1" -> Set("a", "b"))
      checkAt(omega.True("1"))("1" -> Set("1"))
    }

    "exist for Pullback" in {
      val topos = new CategoryOfDiagrams(Pullback)
      val omega = topos.Ω
      val False = omega.False
      checkAt(False("a"))("a" -> Sets.Empty, "b" -> Sets.Empty, "c" -> Sets.Empty)
      checkAt(False("b"))("a" -> Sets.Empty, "b" -> Sets.Empty, "c" -> Sets.Empty)
      checkAt(False("c"))("a" -> Sets.Empty, "b" -> Sets.Empty, "c" -> Sets.Empty)

      val True = omega.True
      checkAt(True("a"))("a" -> Set("a"), "b" -> Sets.Empty, "c" -> Set("ac"))
      checkAt(True("b"))("a" -> Sets.Empty, "b" -> Set("b"), "c" -> Set("bc"))
      checkAt(True("c"))("a" -> Sets.Empty, "b" -> Sets.Empty, "c" -> Set("c"))
    }

    "exist for Pushout" in {
      val topos = new CategoryOfDiagrams(Pushout)
      val omega = topos.Ω
      val False = omega.False
      checkAt(False("a"))("a" -> Sets.Empty, "b" -> Sets.Empty, "c" -> Sets.Empty)
      checkAt(False("b"))("a" -> Sets.Empty, "b" -> Sets.Empty, "c" -> Sets.Empty)
      checkAt(False("c"))("a" -> Sets.Empty, "b" -> Sets.Empty, "c" -> Sets.Empty)

      val True = omega.True
      checkAt(True("a"))("a" -> Set("a"), "b" -> Set("ab"), "c" -> Set("ac"))
      checkAt(True("b"))("a" -> Sets.Empty, "b" -> Set("b"), "c" -> Sets.Empty)
      checkAt(True("c"))("a" -> Sets.Empty, "b" -> Sets.Empty, "c" -> Set("c"))
    }

    "exist for Z3" in {
      val topos = new CategoryOfDiagrams(Z3)
      val omega = topos.Ω
      checkAt(omega.False("0"))("0" -> Sets.Empty)
      checkAt(omega.True("0"))("0" -> Set("0", "1", "2"))
    }
  }
}
