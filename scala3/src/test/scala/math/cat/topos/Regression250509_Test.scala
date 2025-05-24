package math.cat.topos

import math.cat.Categories.*
import math.cat.SetFunction
import math.cat.topos.{CategoryOfDiagrams, Fixtures}
import org.specs2.matcher.MatchResult
import scalakittens.{Bad, Good}

import scala.language.{implicitConversions, reflectiveCalls}

class Regression250509_Test extends Fixtures:

  "Topologies" should:
    "exist for 𝟙" in :
      val candidates = topologyCandidates(`Set^𝟙`)
      candidates.size === 4
      
      val predicate_1 = candidates.find(_.tag == "1⊂Ω").get
      val predicate_3 = candidates.find(_.tag == "3⊂Ω").get
      
      val theyContainTruth = topologyCandidatesContainingTruth(`Set^𝟙`)
      theyContainTruth.size aka "number of candidates containing truth" must_== 2

      predicate_1.containsTruth must beTrue
      predicate_3.containsTruth must beTrue

      val topologies = topologiesTested(`Set^𝟙`)
      val expectedToBeGood_1 = topologies("1⊂Ω")
      expectedToBeGood_1 match
        case Good(value) => ok
        case Bad(errors) => failure(errors.toString)

      val expectedToBeGood_3 = topologies("3⊂Ω")
      expectedToBeGood_3 match
        case Good(value) => ok
        case Bad(errors) => failure(errors.toString)

      val goodOnes = topologies.filter(_._2.isGood)
      goodOnes.size aka "number of good topologies" must_== 2

      val errors = topologies collect :
        case (id: String, bad: Bad[_]) => (id, bad.listErrors.mkString(";"))
      errors.size aka s"Oops, $errors" must_== 2
      val haveTruth = errors.filter(_._2.contains("Should contain truth"))
      haveTruth.size === 2

      ok

