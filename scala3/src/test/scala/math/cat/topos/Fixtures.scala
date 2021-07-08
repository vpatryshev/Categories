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

  def report(cat: Category)(what: String): Unit =
    println(s"  checking $what over ${cat.name}")
  
  def expectOk(r: Result[_]): TestResult =
    r.isGood aka r.toString must beTrue
  
  def expectError(r: Result[_], messages: String*): TestResult =
    r.isBad must beTrue
    r.errorDetails match
      case Some(things) => 
        val matches = messages map { message => OKif(things contains message) }
        expectOk(Result.traverse(matches))
        
      case None => failure(s"Expected errors in $r")

  case class checkThatIn(topos: GrothendieckTopos):
    def mustBeMonoid[P](what: String,
      unit: P,
      binop: (P, P) => P): MatchResult[Any] = {
      import topos._
      val rep = report(topos.domain) _
      val points: Seq[Point] = Ω.points
      println(s"Testing <<${domain.name}>> $what monoidal properties (${points.size} points in Ω)")
      def predicate(p: Point): P = p.asPredicateIn(topos).asInstanceOf[P]

      for {pt1 <- points } {
        rep(s"monoidal at ${pt1.tag}")
        val p: P = predicate(pt1)
        val actual = binop(unit, p)
// different classes in scala 3        actual.getClass === p.getClass
        actual === p
        // idempotence
        binop(p, p) === p

        for {pt2 <- points } {
          val q = predicate(pt2)
          val p_q = binop(p, q)

          // commutativity
          p_q === binop(q, p)

          for {pt3 <- points } {
            val r = predicate(pt3)
            // associativity
            binop(p_q, r) === binop(p, binop(q, r))
          }
        }
      }
      ok
    }

  val categoriesToTest: List[Cat] = SomeKnownCategories

  val batchSize = 8

  // Moved W to a separate group; it's the hardest to deal with
  val groupedCategoriesToTest: List[List[Cat]] = SimpleCategories :: List(W) :: {
    val allButW = LessSimpleCategories filterNot (W ==)
    (for {
      i <- 0 until batchSize -1
      indices = i until allButW.length by (batchSize - 1)
    } yield indices map (allButW(_)) toList) toList
  }
