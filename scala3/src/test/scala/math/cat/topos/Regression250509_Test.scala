package math.cat.topos

import math.cat.Categories.*
import math.cat.SetFunction
import math.cat.topos.{CategoryOfDiagrams, Fixtures}
import org.specs2.matcher.MatchResult
import scalakittens.Bad

import scala.language.{implicitConversions, reflectiveCalls}

class Regression250509_Test extends Fixtures:

  "Topologies" should:
    "exist for 𝟙" in :
      val candidates = topologyCandidates(`Set^𝟙`)
      candidates.size === 4
      topologyCandidatesContainingTruth(`Set^𝟙`).size === 2

      val topologies = topologiesTested(`Set^𝟙`)
      val goodOnes = topologies.filter(_._2.isGood)
      goodOnes.size === 2

      val errors = topologies collect :
        case (id: String, bad: Bad[_]) => (id, bad.listErrors.mkString(";"))
      errors.size aka s"Oops, $errors" must_== 2
      val haveTruth = errors.filter(_._2.contains("Should contain truth"))
      haveTruth.size === 2

      ok

