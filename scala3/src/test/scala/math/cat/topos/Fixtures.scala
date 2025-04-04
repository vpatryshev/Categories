package math.cat.topos

import math.Test
import math.cat.Categories._
import math.cat.Category
import org.specs2.execute.{Result => TestResult}
import org.specs2.matcher.MatchResult
import scalakittens.Result
import scalakittens.Result._

import scala.language.postfixOps

class Fixtures extends Test with math.cat.topos.TestDiagrams:
  type SUT = Diagram

  def report(what: String, where: String = ""): Unit =
    println(s"  checking $what $where")

  def reportIn(topos: GrothendieckTopos): String => Unit =
    report(_, s"in ${topos.tag}")

  trait TestCase:
    def check(cat: Category, number: Int, total: Int): MatchResult[Any]

  case class checkThatIn(topos: GrothendieckTopos, number: Int, total: Int):

    def reportIn(topos: GrothendieckTopos): String => Unit =
      report(_, s"in ${topos.tag}")

    def mustBeMonoid[P](what: String,
      unit: P,
      binop: (P, P) => P): MatchResult[Any] =
      import topos._
      val report = reportIn(topos)
      val points: Seq[Point] = Ω.points
      println(s"Testing $what monoidal properties (${points.size} points in Ω) in ${domain.name} ($number/$total)")
      def predicate(p: Point): P = p.asPredicateIn(topos).asInstanceOf[P]

      for pt1 <- points do
        report(s"monoidal props at ${pt1.tag}")
        val p: P = predicate(pt1)
        val actual = binop(unit, p)
// different classes in scala 3        actual.getClass === p.getClass
        actual === p
        // idempotence
        binop(p, p) === p

        for pt2 <- points do
          val q = predicate(pt2)
          val p_q = binop(p, q)

          // commutativity
          p_q === binop(q, p)

          for pt3 <- points do
            val r = predicate(pt3)
            // associativity
            binop(p_q, r) === binop(p, binop(q, r))

      ok
    
  end checkThatIn
  
  val categoriesToTest: List[Cat] = SomeKnownCategories
  val allFiniteCategories = categoriesToTest.filter(_.isFinite)

  def test(testCase: TestCase): MatchResult[Any] =
    allFiniteCategories .zipWithIndex foreach:
      case (c, i) => testCase.check(c, i, allFiniteCategories.size)

    ok

  val categoriesToTestSpeed: List[Cat] = LessSimpleCategories

  val batchSize = 8
  val allButWM = LessSimpleCategories filterNot (c => c == W || c == M)
  val totalOfGrouped = SimpleCategories.length + 1 + allButWM.length

  // Moved W to a separate group; it's the hardest to deal with
  val groupedCategoriesToTest: List[List[(Cat, Int)]] =
    SimpleCategories.zipWithIndex ::
    List((W, SimpleCategories.length), (M, SimpleCategories.length + 1)) ::
    allButWM.zipWithIndex.map(x => (x._1, x._2 + SimpleCategories.length + 2)) ::Nil

//  println(groupedCategoriesToTest.map(_.map(entry => s"${entry._2}. ${entry._1.name}")).mkString("\n"))
//  println(":)")
