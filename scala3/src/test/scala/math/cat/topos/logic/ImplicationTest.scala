package math.cat.topos.logic

import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import math.cat.topos.{CategoryOfDiagrams, Fixtures, GrothendieckTopos}
import math.cat.{Category, SetFunction}
import org.specs2.matcher.MatchResult
import scalakittens.Result._

import scala.language.reflectiveCalls

class ImplicationTest extends Fixtures:

  "Implication" should {

    def check(cat: Category, number: Int, total: Int): MatchResult[Any] =
      val topos = new CategoryOfDiagrams(cat)
      import topos._
      val desc = s"Testing implication over ${cat.name} ($number/$total)"
      val rep = report(_)
      println(desc)
      val True = Ω.True.asPredicateIn(topos)
      val False = Ω.False.asPredicateIn(topos)

      for pt1 <- Ω.points do
        rep(s"True ⟹ ${pt1.tag}")
        val p = pt1.asPredicateIn(topos)
        (True ⟹ p) === p
        rep(s"False ⟹ ${pt1.tag}")
        (False ⟹ p) === True
        rep(s"${pt1.tag} ⟹ ${pt1.tag}")
        (p ⟹ p) === True
        rep(s"${pt1.tag} ⟹ True")
        (p ⟹ True) === True

        rep(s"adjunction for ${pt1.tag}")
        for pt2 <- Ω.points do
          val q = pt2.asPredicateIn(topos)
          val p_and_q = p ∧ q

          for pt3 <- Ω.points do
            val r = pt3.asPredicateIn(topos)
            val q2r = q ⟹ r
            val left = p_and_q ⟹ r
            val right = p ⟹ q2r
            left === right

        rep(s"adjunction for ${pt1.tag}")
        for pt2 <- Ω.points do
          val q = pt2.asPredicateIn(topos)
          val p_and_q = p ∧ q

          for pt3 <- Ω.points do
            val r = pt3.asPredicateIn(topos)
            val q2r = q ⟹ r
            val left = p_and_q ⟹ r
            val right = p ⟹ q2r
            left === right

        rep(s"conjunction distributivity for ${pt1.tag}")
        for pt2 <- Ω.points do
          val q = pt2.asPredicateIn(topos)
          val p_and_q = p ∧ q

          for pt3 <- Ω.points do
            val r = pt3.asPredicateIn(topos)
            val r2p = r ⟹ p
            val r2q = r ⟹ q
            val left = r2p ∧ r2q
            val right = r ⟹ p_and_q
            left === right

        rep(s"disjunction distributivity for ${pt1.tag}")
        for pt2 <- Ω.points do
          val q = pt2.asPredicateIn(topos)
          val p_or_q = p ∨ q

          for pt3 <- Ω.points do
            val r = pt3.asPredicateIn(topos)
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
