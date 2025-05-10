package math.cat.topos.logic

import math.cat.Category
import math.cat.topos.{CategoryOfDiagrams, Fixtures}
import org.specs2.matcher.MatchResult

import scala.language.reflectiveCalls

class NegationTest extends Fixtures:

  "Negation" should {

    "work for all known domains" in {

      val testCase = new TestCase:
        def check(cat: Category, number: Int, total: Int): MatchResult[Any] =
          val topos = new CategoryOfDiagrams(cat)
          import topos._
          val rep = report(_)
          val desc = s"Testing negation over ${cat.name} ($number/$total)"
          println(desc)
          val True = Truth asPredicateIn topos
          val False = Falsehood asPredicateIn topos

          ¬(True) === False
          ¬(False) === True

          for pt1 <- Ω.points do
            rep(s"that ¬¬¬${pt1.tag} = ¬${pt1.tag}")
            val p = pt1 asPredicateIn topos
            val not_p = ¬(p)
            ¬(¬(not_p)) === not_p

            rep(s"that ¬(${pt1.tag} ∨ x) = ¬${pt1.tag} ∧ ¬x")
            for pt2 <- Ω.points do
              val q = pt2 asPredicateIn topos
              ¬(p ∨ q) === not_p ∧ ¬(q)

          ok

        end check

      test(testCase)
    }
  }
