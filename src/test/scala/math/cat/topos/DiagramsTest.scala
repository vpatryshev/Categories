package math.cat.topos

import math.cat.{Category, TestDiagrams}
import org.specs2.mutable.Specification
import Category._
import math.Test
import math.cat.topos.Diagrams.Diagram

class DiagramsTest extends Test with TestDiagrams {

  type SUT = SmallDiagram
  
  "DiagramsTest" should {
    "id" in {
      val topos = new Diagrams(W)
      expect { d =>
        val idtrans = topos.id(d)
        idtrans.d0 === d
        idtrans.d1 === d
        idtrans.compose(idtrans) === idtrans
      } (SampleWDiagram.asDiagram)
    }

    "m" in {
      // todo: test composition
      val topos = new Diagrams(M)
      expect { d =>
        val idtrans = topos.id(d)
        idtrans.d0 === d
        idtrans.d1 === d
      } (SampleMDiagram.asDiagram)
    }

//    "graphOfDiagrams" in {
//      ok
//    }

    "have a terminal" in {
      val topos = new Diagrams(W)
      val terminalOpt = topos.terminal
      terminalOpt match {
        case None => failure(s"Could not build a terminal in $topos")
        case Some(terminal) =>
          import terminal._
          for {
            x <- W.objects
          } {
            val setAtx: Set[_] = terminal @@ x
            setAtx.size === 1
          }
      }
      ok
    }
  }
}
