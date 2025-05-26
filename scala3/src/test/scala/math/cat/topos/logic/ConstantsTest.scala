package math.cat.topos.logic

import math.cat.Categories.*
import math.cat.topos.*
import math.sets.Sets
import math.sets.Sets.set
import org.specs2.matcher.MatchResult
import scalakittens.Result
import scalakittens.Result.*

import scala.language.reflectiveCalls

class ConstantsTest extends Fixtures:
  "True and False" should {
    "exist for 𝟘" in {
      val topos = `Set^𝟘`
      import topos._
      Truth.toString === "⊤"
      val tTrue = Truth.mapping
      val tFalse = Falsehood.mapping
      tTrue === tFalse // that's a degenerate topos, but tags are still distinct
    }

    def checkAt(topos: GrothendieckTopos)(point: Any)(mappings: (String, set)*): MatchResult[Any] = {
      point match
        case d: topos.Diagram => Result.check {
          for (k, v) <- mappings yield OKif(d(k) == v, s"Failed on $k, expected $v, got ${d(k)}")
        } === OK
        case trash => failure(s"Expected a diagram, got $trash")

      ok
    }

    "exist for 𝟙" in {
      val topos = new CategoryOfDiagrams(`𝟙`)
      val omega = topos.Ω
      checkAt(topos)(omega.False("0"))("0" -> Sets.`∅`)
      checkAt(topos)(omega.True("0"))("0" -> Set("0.0"))
    }

    "exist for 𝟚" in {
      val topos = new CategoryOfDiagrams(`𝟚`)
      val omega = topos.Ω
      val False = omega.False
      val True = omega.True
      checkAt(topos)(False("0"))("0" -> Sets.`∅`, "1" -> Sets.`∅`)
      checkAt(topos)(False("1"))("0" -> Sets.`∅`, "1" -> Sets.`∅`)

      checkAt(topos)(True("0"))("0" -> Set("0.0"), "1" -> Set("0.1"))
      checkAt(topos)(True("1"))("0" -> Sets.`∅`, "1" -> Set("1.1"))
    }

    "exist for `𝟛`" in {
      val topos = new CategoryOfDiagrams (`𝟛`)
      val omega = topos.Ω
      val False = omega.False
      val True = omega.True
      checkAt(topos)(False("0"))("0" -> Sets.`∅`, "1" -> Sets.`∅`, "2" -> Sets.`∅`)
      checkAt(topos)(False("1"))("1" -> Sets.`∅`, "2" -> Sets.`∅`)
      checkAt(topos)(False("2"))("2" -> Sets.`∅`)

      checkAt(topos)(True("0"))("0" -> Set("0.0"), "1" -> Set("0.1"), "2" -> Set("0.2"))
      checkAt(topos)(True("1"))("0" -> Sets.`∅`, "1" -> Set("1.1"), "2" -> Set("1.2"))
      checkAt(topos)(True("2"))("0" -> Sets.`∅`, "1" -> Sets.`∅`, "2" -> Set("2.2"))
    }

    "exist for ParallelPair" in {
      val topos = new CategoryOfDiagrams(ParallelPair)
      val omega = topos.Ω
      val False = omega.False
      val True = omega.True

      checkAt(topos)(False("0"))("0" -> Sets.`∅`, "1" -> Sets.`∅`)
      checkAt(topos)(False("1"))("0" -> Sets.`∅`, "1" -> Sets.`∅`)

      checkAt(topos)(True("0"))("0" -> Set("0"), "1" -> Set("a", "b"))
      checkAt(topos)(True("1"))("1" -> Set("1"))
    }

    "exist for Pullback" in {
      val topos = new CategoryOfDiagrams(Pullback)
      val omega = topos.Ω
      val False = omega.False
      val True = omega.True

      checkAt(topos)(False("a"))("a" -> Sets.`∅`, "b" -> Sets.`∅`, "c" -> Sets.`∅`)
      checkAt(topos)(False("b"))("a" -> Sets.`∅`, "b" -> Sets.`∅`, "c" -> Sets.`∅`)
      checkAt(topos)(False("c"))("a" -> Sets.`∅`, "b" -> Sets.`∅`, "c" -> Sets.`∅`)

      checkAt(topos)(True("a"))("a" -> Set("a"), "b" -> Sets.`∅`, "c" -> Set("ac"))
      checkAt(topos)(True("b"))("a" -> Sets.`∅`, "b" -> Set("b"), "c" -> Set("bc"))
      checkAt(topos)(True("c"))("a" -> Sets.`∅`, "b" -> Sets.`∅`, "c" -> Set("c"))
    }

    "exist for Pushout" in {
      val topos = new CategoryOfDiagrams(Pushout)
      val omega = topos.Ω
      val False = omega.False
      checkAt(topos)(False("a"))("a" -> Sets.`∅`, "b" -> Sets.`∅`, "c" -> Sets.`∅`)
      checkAt(topos)(False("b"))("a" -> Sets.`∅`, "b" -> Sets.`∅`, "c" -> Sets.`∅`)
      checkAt(topos)(False("c"))("a" -> Sets.`∅`, "b" -> Sets.`∅`, "c" -> Sets.`∅`)

      val True = omega.True
      checkAt(topos)(True("a"))("a" -> Set("a"), "b" -> Set("ab"), "c" -> Set("ac"))
      checkAt(topos)(True("b"))("a" -> Sets.`∅`, "b" -> Set("b"), "c" -> Sets.`∅`)
      checkAt(topos)(True("c"))("a" -> Sets.`∅`, "b" -> Sets.`∅`, "c" -> Set("c"))
    }

    "exist for Z3" in {
      val topos = new CategoryOfDiagrams(Z3)
      val omega = topos.Ω
      checkAt(topos)(omega.False("0"))("0" -> Sets.`∅`)
      checkAt(topos)(omega.True("0"))("0" -> Set("0", "1", "2"))
    }
  }
