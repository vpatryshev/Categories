package math.cat.topos.logic

import math.cat.Categories.*
import math.cat.topos.*
import math.sets.Sets
import math.sets.Sets.set
import org.specs2.execute.Result as MatchResult
import scalakittens.Result
import scalakittens.Result.*

import scala.language.reflectiveCalls

class ConstantsTest extends Fixtures:

  def checkAt(topos: GrothendieckTopos)(pointValue: Any)(mappings: (String, set)*): MatchResult =
    pointValue match
      case d: topos.Diagram => Result.check {
        for (k, v) <- mappings yield OKif(d(k) == v, s"Failed on $k, expected $v, got ${d(k)}")
      } must be_==(OK)
      case trash => failure(s"Expected a diagram, got $trash")

    ok

  "True and False" should :
    "exist for 𝟘" in :
      val topos = `Set^𝟘`
      import topos.*
      Truth.toString must be_==("⊤")
      val tTrue = Truth.mapping
      val tFalse = Falsehood.mapping
      tTrue must be_==(tFalse) // that's a degenerate topos, but tags are still distinct

    "exist for 𝟙" in :
      val topos = new CategoryOfDiagrams(`𝟙`)
      import topos.*
      Truth.toString must be_==("⊤")
      checkAt(topos)(Falsehood("0"))("0" -> Sets.`∅`)
      checkAt(topos)(Truth("0"))("0" -> Set("0.0"))

    "exist for 𝟚" in :
      val topos = new CategoryOfDiagrams(`𝟚`)
      import topos.*
      val omega = topos.Ω
      val False = omega.False
      val True = omega.True
      checkAt(topos)(Falsehood("0"))("0" -> Sets.`∅`, "1" -> Sets.`∅`)
      checkAt(topos)(Falsehood("1"))("0" -> Sets.`∅`, "1" -> Sets.`∅`)

      checkAt(topos)(Truth("0"))("0" -> Set("0.0"), "1" -> Set("0.1"))
      checkAt(topos)(Truth("1"))("0" -> Sets.`∅`, "1" -> Set("1.1"))

    "exist for `𝟛`" in :
      val topos = new CategoryOfDiagrams (`𝟛`)
      import topos.*

      checkAt(topos)(Falsehood("0"))("0" -> Sets.`∅`, "1" -> Sets.`∅`, "2" -> Sets.`∅`)
      checkAt(topos)(Falsehood("1"))("1" -> Sets.`∅`, "2" -> Sets.`∅`)
      checkAt(topos)(Falsehood("2"))("2" -> Sets.`∅`)

      checkAt(topos)(Truth("0"))("0" -> Set("0.0"), "1" -> Set("0.1"), "2" -> Set("0.2"))
      checkAt(topos)(Truth("1"))("0" -> Sets.`∅`, "1" -> Set("1.1"), "2" -> Set("1.2"))
      checkAt(topos)(Truth("2"))("0" -> Sets.`∅`, "1" -> Sets.`∅`, "2" -> Set("2.2"))

    "exist for ParallelPair" in :
      val topos = new CategoryOfDiagrams(ParallelPair)
      import topos.*

      checkAt(topos)(Falsehood("0"))("0" -> Sets.`∅`, "1" -> Sets.`∅`)
      checkAt(topos)(Falsehood("1"))("0" -> Sets.`∅`, "1" -> Sets.`∅`)

      checkAt(topos)(Truth("0"))("0" -> Set("0"), "1" -> Set("a", "b"))
      checkAt(topos)(Truth("1"))("1" -> Set("1"))

    "exist for Pullback" in :
      val topos = new CategoryOfDiagrams(Pullback)
      import topos.*

      checkAt(topos)(Falsehood("a"))("a" -> Sets.`∅`, "b" -> Sets.`∅`, "c" -> Sets.`∅`)
      checkAt(topos)(Falsehood("b"))("a" -> Sets.`∅`, "b" -> Sets.`∅`, "c" -> Sets.`∅`)
      checkAt(topos)(Falsehood("c"))("a" -> Sets.`∅`, "b" -> Sets.`∅`, "c" -> Sets.`∅`)

      checkAt(topos)(Truth("a"))("a" -> Set("a"), "b" -> Sets.`∅`, "c" -> Set("ac"))
      checkAt(topos)(Truth("b"))("a" -> Sets.`∅`, "b" -> Set("b"), "c" -> Set("bc"))
      checkAt(topos)(Truth("c"))("a" -> Sets.`∅`, "b" -> Sets.`∅`, "c" -> Set("c"))

    "exist for Pushout" in :
      val topos = new CategoryOfDiagrams(Pushout)
      import topos.*

      checkAt(topos)(Falsehood("a"))("a" -> Sets.`∅`, "b" -> Sets.`∅`, "c" -> Sets.`∅`)
      checkAt(topos)(Falsehood("b"))("a" -> Sets.`∅`, "b" -> Sets.`∅`, "c" -> Sets.`∅`)
      checkAt(topos)(Falsehood("c"))("a" -> Sets.`∅`, "b" -> Sets.`∅`, "c" -> Sets.`∅`)

      checkAt(topos)(Truth("a"))("a" -> Set("a"), "b" -> Set("ab"), "c" -> Set("ac"))
      checkAt(topos)(Truth("b"))("a" -> Sets.`∅`, "b" -> Set("b"), "c" -> Sets.`∅`)
      checkAt(topos)(Truth("c"))("a" -> Sets.`∅`, "b" -> Sets.`∅`, "c" -> Set("c"))

    "exist for Z3" in :
      val topos = new CategoryOfDiagrams(Z3)
      import topos.*
      checkAt(topos)(Falsehood("0"))("0" -> Sets.`∅`)
      checkAt(topos)(Truth("0"))("0" -> Set("0", "1", "2"))
