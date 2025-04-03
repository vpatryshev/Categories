package math.cat.topos.logic

import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import math.cat.topos.{CategoryOfDiagrams, Fixtures, GrothendieckTopos}
import math.cat.{Category, SetFunction}
import org.specs2.matcher.MatchResult
import scalakittens.Result._

import scala.language.reflectiveCalls

/**
 * You may wonder where does this name come from.
 * This is done to make sure this suite starts running as early as possible, because it's very long.
 * We run on all available cores, so other suites will run in parallel.
 */
class AnImplicationTest extends Fixtures:

  "Implication" should {

    def check(cat: Category, number: Int, total: Int): MatchResult[Any] =
      val topos = new CategoryOfDiagrams(cat)
      import topos._
      val desc = s"Testing implication over ${cat.name} ($number/$total)"
      println(desc)
      val True = Ω.True asPredicateIn topos
      val False = Ω.False asPredicateIn topos

      for pt1 <- Ω.points do
        report(s"True ⟹ ${pt1.tag} = ${pt1.tag}")
        val p = pt1 asPredicateIn topos
        (True ⟹ p) === p
        report(s"False ⟹ ${pt1.tag} = True")
        (False ⟹ p) === True
        report(s"${pt1.tag} ⟹ ${pt1.tag}")
        (p ⟹ p) === True
        report(s"${pt1.tag} ⟹ True = True")
        (p ⟹ True) === True

        report(s"adjunction for ${pt1.tag}")
        for pt2 <- Ω.points do
          val q = pt2 asPredicateIn topos
          val p_and_q = p ∧ q

          for pt3 <- Ω.points do
            val r = pt3 asPredicateIn topos
            val q2r = q ⟹ r
            val left = p_and_q ⟹ r
            val right = p ⟹ q2r
            left === right

        report(s"conjunction distributivity for ${pt1.tag}")
        for pt2 <- Ω.points do
          val q = pt2 asPredicateIn topos
          val p_and_q = p ∧ q

          for pt3 <- Ω.points do
            val r = pt3 asPredicateIn topos
            val r2p = r ⟹ p
            val r2q = r ⟹ q
            val left = r2p ∧ r2q
            val right = r ⟹ p_and_q
            left === right

        report(s"disjunction distributivity for ${pt1.tag}")
        for pt2 <- Ω.points do
          val q = pt2 asPredicateIn topos
          val p_or_q = p ∨ q

          for pt3 <- Ω.points do
            val r = pt3 asPredicateIn topos
            val p2r = p ⟹ r
            val q2r = q ⟹ r
            val left = p2r ∧ q2r
            val right = p_or_q ⟹ r
            left === right

      ok

    end check

    def checkAt(i: Int): MatchResult[Any] =
      groupedCategoriesToTest(i) foreach:
        case (cat, index) => check(cat, index, totalOfGrouped)
      ok

    def nameThem(i: Int): String =
      groupedCategoriesToTest(i).map{_._1.name} mkString ", "

    s"work for domains: ${nameThem(0)}" in checkAt(0)
    s"work for domains: ${nameThem(1)}" in checkAt(1)
    s"work for domains: ${nameThem(2)}" in checkAt(2)
  }
