package math.cat.topos.logic

import math.cat.Category
import math.cat.topos.{CategoryOfDiagrams, Fixtures}
import org.specs2.execute.Result as MatchResult

import scala.language.reflectiveCalls

class NegationTest extends Fixtures:

  "Negation" should {

    "work for all known domains" in {

      val testCase = new TestCase:
        def check(cat: Category, number: Int, total: Int): MatchResult =
          val topos = new CategoryOfDiagrams(cat)
          import topos._
          val rep = reportIn(topos)
          val desc = s"Testing negation over ${cat.name} ($number/$total)"
          println(desc)
          val True = Truth asPredicateIn topos
          val False = Falsehood asPredicateIn topos

          ¬(True) must be_==(False)
          ¬(False) must be_==(True)

          for pt1 <- Ω.points do
            rep(s"check that ¬¬¬${pt1.tag} = ¬${pt1.tag}")
            val p = pt1 asPredicateIn topos
            val not_p = ¬(p)
            ¬(¬(not_p)) must be_==(not_p)

            rep(s"check that ¬(${pt1.tag} ∨ x) = ¬${pt1.tag} ∧ ¬x")
            for pt2 <- Ω.points do
              val q = pt2 asPredicateIn topos
              ¬(p ∨ q) must be_==(not_p ∧ ¬(q))

          ok

        end check

      test(testCase)
    }
  }
