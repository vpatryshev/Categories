package math.cat.topos

import math.Test
import math.cat.Categories.{Cat, LessSimpleCategories, SimpleCategories, SomeKnownCategories}
import math.cat.Category
import org.specs2.matcher.MatchResult
import org.specs2.execute.{Result => TestResult}
import scalakittens.Result
import scalakittens.Result._

class Fixtures extends Test with TestDiagrams {
  type SUT = Diagram

  def report(cat: Category)(what: String): Unit =
    println(s"  checking $what over ${cat.name}")
  
  def expectOk(r: Result[_]): TestResult = {
    r.isGood aka r.toString must beTrue
  }
  
  def expectError(r: Result[_], messages: String*): TestResult = {
    r.isBad must beTrue
    r.errorDetails match {
      case Some(things) => 
        val matches = messages map { message => OKif(things contains message) }
        expectOk(Result.traverse(matches))
        
      case None => failure(s"Expected errors in $r")
    }
  }

  case class checkThatIn(topos: GrothendieckTopos) {
    def mustBeMonoid[P](what: String,
      unit: P,
      binop: (P, P) => P): MatchResult[Any] = {
      import topos._
      val rep = report(topos.domain) _
      val points = Ω.points
      println(s"Testing <<${domain.name}>> $what monoidal properties (${points.size} points in Ω)")
      def predicate(p: Point): P = p.asPredicate.asInstanceOf[P]

      for {pt1 <- points } {
        rep(s"monoidal at ${pt1.tag}")
        val p = predicate(pt1)
        binop(unit, p) === p
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
  }

  val categoriesToTest: List[Cat] = SomeKnownCategories

  val batchSize = 8

  val groupedCategoriesToTest: List[List[Cat]] = SimpleCategories :: {
    (for {
      i <- 0 until batchSize
      indices = i until LessSimpleCategories.length by batchSize
    } yield indices map (LessSimpleCategories(_)) toList) toList
  }

}

object Fixtures extends Fixtures {
  println("Fixtures ok")
}
