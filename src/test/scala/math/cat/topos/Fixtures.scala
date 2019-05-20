package math.cat.topos

import math.Test
import org.specs2.matcher.MatchResult

class Fixtures extends Test with TestDiagrams {
  type SUT = Diagram

  case class checkThatIn(topos: GrothendieckTopos) {
    def mustBeMonoid[P](what: String,
      unit: P,
      binop: (P, P) => P): MatchResult[Any] = {
      import topos._
      val points = Ω.points
      println(s"Testing <<${domain.name}>> $what monoidal properties (${points.size} points in Ω)")
      def predicate(p: Point): P = predicateFor(p).asInstanceOf[P]

      for { p <- points } {
        println(s"  monoidal at ${p.tag}")
        val pp = predicate(p)
        binop(unit, pp) === pp
        // idempotence
        binop(pp, pp) === pp

        for { q <- points } {
          val pq = predicate(q)
          val ppq = binop(pp, pq)

          // commutativity
          ppq === binop(pq, pp)

          for { r <- points } {
            val pr = predicate(r)
            // associativity
            binop(ppq, pr) === binop(pp, binop(pq, pr))
          }
        }
      }
      ok
    }
  }

}
