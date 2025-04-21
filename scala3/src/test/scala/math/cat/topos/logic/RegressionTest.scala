package math.cat.topos.logic

import math.cat.SetFunction.asFunction
import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import math.cat.topos.{CategoryOfDiagrams, Diagram, Fixtures, GrothendieckTopos}
import math.cat.{Category, SetFunction}
import org.specs2.matcher.MatchResult
import scalakittens.Result.*

import scala.language.reflectiveCalls
import math.cat.Categories.*

/**
 * You may wonder where does this name come from.
 * This is done to make sure this suite starts running as early as possible, because it's very long.
 * We run on all available cores, so other suites will run in parallel.
 */
class RegressionTest extends Fixtures:

  "Logic" should {
    val `Set^𝟙` = CategoryOfDiagrams(`𝟙`)

    "work for adjunctions in set to 𝟙" in:
      val sut = `Set^𝟙`
      val Ω = `Set^𝟙`.Ω
      val True = `Set^𝟙`.Ω.True asPredicateIn `Set^𝟙`
      val False = `Set^𝟙`.Ω.False asPredicateIn `Set^𝟙`
      val False0 = False("0")
      val p = `Set^𝟙`.Ω.points.head asPredicateIn `Set^𝟙`
      val q = `Set^𝟙`.Ω.points.head asPredicateIn `Set^𝟙`
      val p_and_q = p ∧ q
      val pnqAt0 = p.binaryOp(Ω.conjunction)(q) // , "TEST CONJUNCTION OF FALSE AND FALSE")
      val r = `Set^𝟙`.Ω.points.head asPredicateIn `Set^𝟙`
      val q2r = q ⟹ r
      val expectedAt0 = True("0")

      val p_0 = p.mappingAt("0") //, "p, expected false")
      val q_0 = q.mappingAt("0")//, "q, expected false")
      val pq_0 = p_and_q.mappingAt("0")//, "pq, expected false")
      val p_q_true: `Set^𝟙`.Predicate = p_and_q.binaryOp(Ω.implication)(True) //, "TESTING")
      val valueAt0: p_q_true.d1.d1.Arrow = p_q_true.mappingAt("0")//, "p_q_true, expected true")
      val compared = valueAt0 == expectedAt0
      compared must beTrue
      valueAt0 === expectedAt0
      p_q_true === True
      p_and_q ⟹ False === True
      val left = p_and_q ⟹ r
      left === True
      val r1 = False ⟹ q2r
      r1 === True
      val right = p ⟹ q2r
      right === True

      left === right
  }
