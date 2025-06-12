package math.cat.topos.logic

import math.cat.topos.{CategoryOfDiagrams, Fixtures}
import math.cat.Category
import org.specs2.execute.Result as MatchResult

import scala.language.reflectiveCalls

class DisjunctionTest extends Fixtures:

  "Disjunction" should {

    "work for all known domains" in {

      val testCase = new TestCase:
        def check(cat: Category, number: Int, total: Int): MatchResult =
          val topos = new CategoryOfDiagrams(cat)
          import topos._
          val desc = s"Testing disjunction over ${cat.name} ($number/$total)"
          val rep = reportIn(topos)
          println(desc)
          val True = Truth.asPredicateIn(topos)
          val False = Falsehood.asPredicateIn(topos)
          checkThatIn(topos, number, total).mustBeMonoid[Predicate](
            "disjunction",
            False,
            (p: Predicate, q: Predicate) => p ∨ q
          )

          for pt <- Ω.points do
            rep(s"disjunction with False for ${pt.tag}")
            val p = pt.asPredicateIn(topos)
            (True ∨ p) must be_==(True)

          ok

      test(testCase)
    }
  }
