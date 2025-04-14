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

    def check(topos: CategoryOfDiagrams, number: Int, total: Int): MatchResult[Any] =
      val cat = topos.domain
      val report = reportIn(topos)
      import topos._
      val desc = s"Testing implication in ${topos.name} ($number/$total), ${Ω.points.size} points in Ω"
      println(desc)
      val True = Ω.True asPredicateIn topos
      val False = Ω.False asPredicateIn topos
      val context = s"${Ω.tag} in ${topos.name}"
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
        checkAdjunction(topos)(p, context)

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

    def checkAdjunction(topos: GrothendieckTopos)(p: topos.Predicate, context: String): MatchResult[Any] =
      val testname = s"$context, adjunction for ${p.tag}"
      report(s"adjunction for ${p.tag}")

      for pt2 <- topos.Ω.points do
        val q = pt2 asPredicateIn topos
        checkAdjunction2(topos)(p, q, context)

      ok

    def checkAdjunction2(topos: GrothendieckTopos)(
                        p: topos.Predicate, q: topos.Predicate, context: String
    ): MatchResult[Any] =
      val p_and_q = p ∧ q
      for pt3 <- topos.Ω.points do
        val r = pt3 asPredicateIn topos
        checkAdjunctionpqr(topos)(p, q, p_and_q, r, context)
      ok

    def checkAdjunctionpqr(topos: GrothendieckTopos)(
      p: topos.Predicate, q: topos.Predicate, p_and_q: topos.Predicate,r: topos.Predicate, context: String
    ): MatchResult[Any] =
      val q2r = q ⟹ r
      val left = p_and_q ⟹ r
      val right = p ⟹ q2r
      left.equalsWithDetails(right, printDetails = true, context) aka context must beTrue

    def checkAt(i: Int): MatchResult[Any] =
      groupedCategoriesToTest(i) foreach:
        case (cat, index) =>
          val topos = new CategoryOfDiagrams(cat)
          check(topos, index, totalOfGrouped)
      ok

    def nameThem(i: Int): String =
      groupedCategoriesToTest(i).map{_._1.name} mkString ", "

    "work for adjunctions in set^𝟙" in:
      val sut = `Set^𝟙`
      val True = `Set^𝟙`.Ω.True asPredicateIn `Set^𝟙`
      val False = `Set^𝟙`.Ω.False asPredicateIn `Set^𝟙`
      val p = `Set^𝟙`.Ω.points.head asPredicateIn `Set^𝟙`
      p === False
      val q = `Set^𝟙`.Ω.points.head asPredicateIn `Set^𝟙`
      q === False
      val p_and_q = p ∧ q
      p_and_q === False
      False ∧ False === False
      val r = `Set^𝟙`.Ω.points.head asPredicateIn `Set^𝟙`
      r === False
      val q2r = q ⟹ r
      q2r === True
      False ⟹ r === True
      False ⟹ False === True
      False ⟹ True === True
      p ⟹ True === True
      p_and_q === p
      val p_q_true = p_and_q ⟹ True
      p_q_true === True
      p_and_q ⟹ False === True
      val left = p_and_q ⟹ r
      left === True
      val r1 = False ⟹ q2r
      r1 === True
      val right = p ⟹ q2r
      right === True

      left === right
//      checkAdjunctionpqr(`Set^𝟙`)(p, q, p_and_q, r, "adjunction in set^𝟙")

//    "work for set^𝟙" in check(`Set^𝟙`, -1, 1)

//    s"work for domains: ${nameThem(0)}" in checkAt(0)
//    s"work for domains: ${nameThem(1)}" in checkAt(1)
//    s"work for domains: ${nameThem(2)}" in checkAt(2)
  }
