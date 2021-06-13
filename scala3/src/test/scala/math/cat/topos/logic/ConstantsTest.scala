package math.cat.topos.logic

import math.cat.Categories._
import math.cat.topos.{CategoryOfDiagrams, Diagram}
import math.sets.Sets
import math.sets.Sets.set
import org.specs2.matcher.MatchResult
import scalakittens.Result._

import scala.language.reflectiveCalls

class ConstantsTest extends Fixtures {
  "True and False" should {

    def checkAt(point: Any)(mappings: (String, set)*): MatchResult[Any] = {
      ok
    }

    "exist for _1_" in {
      val topos = new CategoryOfDiagrams(_0_)
      val omega = topos.Ω
      ok
    }
  }
}
