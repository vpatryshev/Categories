package math.cat.topos.logic

import math.cat.Categories._
import math.cat.topos.{CategoryOfDiagrams, Diagram, Fixtures}
import math.sets.Sets
import math.sets.Sets.set
import org.specs2.matcher.MatchResult
import scalakittens.Result
import scalakittens.Result._

import scala.language.reflectiveCalls

class ConstantsTest extends Fixtures:
  "True and False" should {
    "exist for 𝟘" in {
      val topos = new CategoryOfDiagrams(`𝟘`)
      import topos._
      Ω.True.toString === "⊤"
      val tTrue = Ω.True.mapping
      val tFalse = Ω.False.mapping
      tTrue === tFalse // that's a degenerate topos, but tags are still distinct
    }

    def checkAt(point: Any)(mappings: (String, set)*): MatchResult[Any] = {
      point match
        case d: Diagram => Result.check {
            for (k, v) <- mappings yield OKif(d(k) == v, s"Failed on $k, expected $v, got ${d(k)}")
          } === OK
        case trash => failure(s"Expected a diagram, got $trash")

      ok
    }

    "exist for 𝟙" in {
      val topos = new CategoryOfDiagrams(`𝟙`)
      val omega = topos.Ω
      checkAt(omega.False("0"))("0" -> Sets.`∅`)
      checkAt(omega.True("0"))("0" -> Set("0.0"))
    }

    "exist for 𝟚" in {
      val topos = new CategoryOfDiagrams(`𝟚`)
      val omega = topos.Ω
      val False = omega.False
      val True = omega.True
      checkAt(False("0"))("0" -> Sets.`∅`, "1" -> Sets.`∅`)
      checkAt(False("1"))("0" -> Sets.`∅`, "1" -> Sets.`∅`)

      checkAt(True("0"))("0" -> Set("0.0"), "1" -> Set("0.1"))
      checkAt(True("1"))("0" -> Sets.`∅`, "1" -> Set("1.1"))
    }

    "exist for " in {
      val topos = new CategoryOfDiagrams(`𝟛`)
      val omega = topos.Ω
      val False = omega.False
      val True = omega.True
      checkAt(False("0"))("0" -> Sets.`∅`, "1" -> Sets.`∅`, "2" -> Sets.`∅`)
      checkAt(False("1"))("1" -> Sets.`∅`, "2" -> Sets.`∅`)
      checkAt(False("2"))("2" -> Sets.`∅`)

      checkAt(True("0"))("0" -> Set("0.0"), "1" -> Set("0.1"), "2" -> Set("0.2"))
      checkAt(True("1"))("0" -> Sets.`∅`, "1" -> Set("1.1"), "2" -> Set("1.2"))
      checkAt(True("2"))("0" -> Sets.`∅`, "1" -> Sets.`∅`, "2" -> Set("2.2"))
    }

    "exist for ParallelPair" in {
      val topos = new CategoryOfDiagrams(ParallelPair)
      val omega = topos.Ω
      val False = omega.False
      val True = omega.True

      checkAt(False("0"))("0" -> Sets.`∅`, "1" -> Sets.`∅`)
      checkAt(False("1"))("0" -> Sets.`∅`, "1" -> Sets.`∅`)

      checkAt(True("0"))("0" -> Set("0"), "1" -> Set("a", "b"))
      checkAt(True("1"))("1" -> Set("1"))
    }

    "exist for Pullback" in {
      val topos = new CategoryOfDiagrams(Pullback)
      val omega = topos.Ω
      val False = omega.False
      val True = omega.True

      checkAt(False("a"))("a" -> Sets.`∅`, "b" -> Sets.`∅`, "c" -> Sets.`∅`)
      checkAt(False("b"))("a" -> Sets.`∅`, "b" -> Sets.`∅`, "c" -> Sets.`∅`)
      checkAt(False("c"))("a" -> Sets.`∅`, "b" -> Sets.`∅`, "c" -> Sets.`∅`)

      checkAt(True("a"))("a" -> Set("a"), "b" -> Sets.`∅`, "c" -> Set("ac"))
      checkAt(True("b"))("a" -> Sets.`∅`, "b" -> Set("b"), "c" -> Set("bc"))
      checkAt(True("c"))("a" -> Sets.`∅`, "b" -> Sets.`∅`, "c" -> Set("c"))
    }

    "exist for Pushout" in {
      val topos = new CategoryOfDiagrams(Pushout)
      val omega = topos.Ω
      val False = omega.False
      checkAt(False("a"))("a" -> Sets.`∅`, "b" -> Sets.`∅`, "c" -> Sets.`∅`)
      checkAt(False("b"))("a" -> Sets.`∅`, "b" -> Sets.`∅`, "c" -> Sets.`∅`)
      checkAt(False("c"))("a" -> Sets.`∅`, "b" -> Sets.`∅`, "c" -> Sets.`∅`)

      val True = omega.True
      checkAt(True("a"))("a" -> Set("a"), "b" -> Set("ab"), "c" -> Set("ac"))
      checkAt(True("b"))("a" -> Sets.`∅`, "b" -> Set("b"), "c" -> Sets.`∅`)
      checkAt(True("c"))("a" -> Sets.`∅`, "b" -> Sets.`∅`, "c" -> Set("c"))
    }

    "exist for Z3" in {
      val topos = new CategoryOfDiagrams(Z3)
      val omega = topos.Ω
      checkAt(omega.False("0"))("0" -> Sets.`∅`)
      checkAt(omega.True("0"))("0" -> Set("0", "1", "2"))
    }
  }
