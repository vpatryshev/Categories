package math.cat.topos.logic

import math.cat.Category
import math.cat.topos.{CategoryOfDiagrams, Fixtures}
import org.specs2.matcher.MatchResult

import scala.language.reflectiveCalls

class NegationTest extends Fixtures {

  "Negation" should {

    "work for all known domains" in {

      def check(cat: Category): MatchResult[Any] = {
        val topos = new CategoryOfDiagrams(cat)
        import topos._
        val rep = report(cat) _
        val desc = s"Testing negation over ${cat.name}"
        println(desc)
        val True = Ω.True.asPredicateIn(topos)
        val False = Ω.False.asPredicateIn(topos)

        ¬(True) === False
        ¬(False) === True

        for { pt1 <- Ω.points } {
          rep(s"that ¬¬¬${pt1.tag} = ¬${pt1.tag}")
          val p: topos.Predicate = pt1.asPredicateIn(topos)
          val not_p = ¬(p)
          ¬(¬(not_p)) === not_p

          rep(s"that ¬(${pt1.tag} ∨ x) =  ¬${pt1.tag} ∧ ¬x")
          for {pt2 <- Ω.points} {
            val q: topos.Predicate = pt2.asPredicateIn(topos)
            ¬(p ∨ q) === not_p ∧ ¬(q)
          }
        }

        ok
      }

      categoriesToTest filter (_.isFinite) foreach check

      ok
    }
  }
}
