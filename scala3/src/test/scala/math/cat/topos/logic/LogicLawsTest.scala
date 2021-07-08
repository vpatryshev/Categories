package math.cat.topos.logic

import math.cat.Category
import math.cat.topos.{CategoryOfDiagrams, Fixtures, GrothendieckTopos}
import org.specs2.matcher.MatchResult

import scala.language.reflectiveCalls

class LogicLawsTest extends Fixtures {
  "Distributivity laws of logic" in {

    // distributivity of conjunction over disjunction
    def conjunctionOverDisjunction(topos: GrothendieckTopos)(
      p: topos.Predicate, q: topos.Predicate, pAndQ: topos.Predicate, r: topos.Predicate)= {
      val p_qr = p ∧ (q ∨ r)
      val p_and_q_pr = pAndQ ∨ (p ∧ r)
      p_qr === p_and_q_pr
    }

    // distributivity of disjunction over conjunction
    def disjunctionOverConjunction(topos: GrothendieckTopos)(
      p: topos.Predicate,
      q: topos.Predicate, pOrQ: topos.Predicate, r: topos.Predicate)= {
      val p_qr = p ∨ (q ∧ r)
      val p_and_q_pr = pOrQ ∧ (p ∨ r)
      p_qr === p_and_q_pr
    }

    def checkDistributivity(cat: Category): MatchResult[Any] = {
      val topos = new CategoryOfDiagrams(cat)
      import topos._
      val points = Ω.points

      val desc = s"Testing distributivity laws over ${cat.name}"
      println(desc)

      for { pt1 <- points } {
        report(cat)(s"distributivity at ${pt1.tag}")
        val p: topos.Predicate = pt1.asPredicateIn(topos)

        for { pt2 <- points } {
          val q = pt2.asPredicateIn(topos)
          val pAndQ: topos.Predicate = p ∧ q
          val pOrQ: topos.Predicate = p ∨ q

          for { pt3 <- points } {
            val r: topos.Predicate = pt3.asPredicateIn(topos)
            conjunctionOverDisjunction(topos)(p, q, pAndQ, r)
            disjunctionOverConjunction(topos)(p, q, pOrQ, r)
          }
        }
      }

      ok
    }

    "hold for all known domains" in {
      categoriesToTest filter (_.isFinite) foreach checkDistributivity

      ok
    }
  }
}
