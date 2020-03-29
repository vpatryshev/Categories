package math.cat.topos

import math.Test
import org.specs2.matcher.MatchResult
import scalakittens.Result

class Fixtures extends Test with TestDiagrams {
  type SUT = Diagram
  
  def expectOk(r: Result[_]): Unit = {
    r.isGood aka r.toString must beTrue
  }
  
  def expectError(r: Result[_], messages: String*): Unit = {
    r.isBad must beTrue
    r.errorDetails match {
      case Some(things) => messages foreach { message => things must contain(message) }
        
      case None => failure(s"Expected errors in $r")
    }
  }

  case class checkThatIn(topos: GrothendieckTopos) {
    def mustBeMonoid[P](what: String,
      unit: P,
      binop: (P, P) ⇒ P): MatchResult[Any] = {
      import topos._
      val points = Ω.points
      println(s"Testing <<${domain.name}>> $what monoidal properties (${points.size} points in Ω)")
      def predicate(p: Point): P = p.asPredicate.asInstanceOf[P]

      for {pt1 ← points } {
        println(s"  monoidal at ${pt1.tag}")
        val p = predicate(pt1)
        binop(unit, p) === p
        // idempotence
        binop(p, p) === p

        for {pt2 ← points } {
          val q = predicate(pt2)
          val p_q = binop(p, q)

          // commutativity
          p_q === binop(q, p)

          for {pt3 ← points } {
            val r = predicate(pt3)
            // associativity
            binop(p_q, r) === binop(p, binop(q, r))
          }
        }
      }
      ok
    }
  }

}
