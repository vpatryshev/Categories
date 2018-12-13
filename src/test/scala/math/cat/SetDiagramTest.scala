package math.cat

import math.cat.SetCategory.Setf
import math.sets.Sets.set
import org.specs2.mutable._

/**
  * Prototype for all tests
  */
class SetDiagramTest extends Specification {

  "SetDiagram" should {
    "have a limit" in {
      val a: set = Set(1, 2, 3)
      val b: set = Set(2, 3, 4)
      val c: set = Set(0, 1)
      val ac = SetFunction("f", a, c, _.toString.toInt % 2)
      val bc = SetFunction("g", b, c, x => (x.toString.toInt + 1) % 2)
      val sut = SetDiagram(
        "pullback", Category.Pullback,
        Map("a" -> a, "b" -> b, "c" -> c),
        Map("ac" -> ac, "bc" -> bc)
      )
      sut.domain === Category.Pullback
      sut.codomain === Setf
      sut.limit match {
        case None => failure("We expected a limit")
        case Some(sut.Cone(vertex, arrowTo)) =>
          vertex.size === 5
          val ara = arrowTo("a")
          val arb = arrowTo("b")

          for {i <- 1 to 3; j <- 2 to 4} {
            val element = i::j::Nil
            (i, j, vertex(element)) === (i, j, (i+j) %2 == 1)
            if (vertex(element)) {
              (i, j, ara(element)) === (i, j, i)
              (i, j, arb(element)) === (i, j, j)
            }
          }
      }
      ok
    }
  }
}
