package math.cat.topos

import math.Test
import math.cat.Categories._
import math.cat.Category
import org.specs2.execute.Result as MatchResult
import scalakittens.Result
import scalakittens.Result._

import scala.language.postfixOps

class Fixtures extends Test with math.cat.topos.TestDiagrams:
  lazy val titleWidth = toposes.keys.map(_.length).max + 4

  def header(name: String): String =
    s"  $name:" + (" " * (titleWidth - name.length))

  def report(what: String, where: String = ""): Unit =
    println(s"  ${header(where)} $what")

  def reportIn(topos: GrothendieckTopos): String => Unit =
    report(_, topos.tag)

  trait TestCase:
    def check(cat: Category, number: Int, total: Int): MatchResult

  case class checkThatIn(topos: GrothendieckTopos, number: Int, total: Int):
    val report = reportIn(topos)
    def mustBeMonoid[P](what: String,
      unit: P,
      binop: (P, P) => P): MatchResult =
      import topos._
      val report = reportIn(topos)
      val points: Seq[Point] = Ω.points
      println(s"Testing $what monoidal properties (${points.size} points in Ω) in ${domain.name} ($number/$total)")
      def predicate(p: Point): P = p.asPredicateIn(topos).asInstanceOf[P]

      for pt1 <- points do
        report(s"monoidal props at ${pt1.tag}")
        val p: P = predicate(pt1)
        val actual = binop(unit, p)
// different classes in scala 3        actual.getClass must be_==(p).getClass
        actual must be_==(p)
        // idempotence
        binop(p, p) must be_==(p)

        for pt2 <- points do
          val q = predicate(pt2)
          val p_q = binop(p, q)

          // commutativity
          p_q must be_==(binop(q, p))

          for pt3 <- points do
            val r = predicate(pt3)
            // associativity
            binop(p_q, r) must be_==(binop(p, binop(q, r)))

      ok
    
  end checkThatIn
  
  val categoriesToTest: List[Cat] = SomeKnownCategories
  val allFiniteCategories = categoriesToTest.filter(_.isFinite)

  def test(testCase: TestCase): MatchResult =
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
