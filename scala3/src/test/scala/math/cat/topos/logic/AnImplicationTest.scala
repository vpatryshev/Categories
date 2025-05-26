package math.cat.topos.logic

import math.cat.SetFunction.asFunction
import math.cat.topos.CategoryOfDiagrams.DiagramArrow
import math.cat.topos.{CategoryOfDiagrams, Fixtures, GrothendieckTopos}
import math.cat.{Category, SetFunction}
import org.specs2.matcher.MatchResult
import scalakittens.Result.*

import scala.language.reflectiveCalls

/**
 * You may wonder where does this name come from.
 * This is done to make sure this suite starts running as early as possible, because it's very long.
 * We run on all available cores, so other suites may run in parallel.
 */
class AnImplicationTest extends Fixtures:

  def check(topos: CategoryOfDiagrams, number: Int, total: Int): MatchResult[Any] =
    val cat = topos.domain
    val report = reportIn(topos)
    import topos._
    val desc = s"Testing implication in ${topos.name} ($number/$total), ${Ω.points.size} points in Ω"
    println(desc)
    val True = Truth asPredicateIn topos
    val False = Falsehood asPredicateIn topos
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
    reportIn(topos)(s"adjunction for ${p.tag}")

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
    p: topos.Predicate, q: topos.Predicate, p_and_q: topos.Predicate, r: topos.Predicate, context: String
  ): MatchResult[Any] =
    val q2r = q ⟹ r
    val left = p_and_q ⟹ r
    val right = p ⟹ q2r
    if (left != equals)
      left.equalsWithDetails(right, printDetails = true, context) aka context must beTrue
    ok

  "Implication" should:
    def checkAt(i: Int): MatchResult[Any] =
      groupedCategoriesToTest(i) foreach:
        case (cat, index) =>
          val topos = new CategoryOfDiagrams(cat)
          check(topos, index, totalOfGrouped)
      ok

    def nameThem(i: Int): String =
      groupedCategoriesToTest(i).map{_._1.name} mkString ", "

//    "run all" in:
//      s"work for domains: ${nameThem(0)}" in checkAt(0)
//      s"work for domains: ${nameThem(1)}" in checkAt(1)
//      s"work for domains: ${nameThem(2)}" in checkAt(2)

    "do M" in:
      val topos = `Set^M`
      check(topos, 0, 1)

