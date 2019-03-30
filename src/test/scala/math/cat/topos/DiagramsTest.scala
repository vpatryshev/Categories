package math.cat.topos

import math.Test
import math.cat.Category._
import math.cat.{Category, TestDiagrams}

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

    def checkConstSize(topos: Diagrams)(obj: topos.Obj, expected: Int): Unit = {
      for {
        x <- topos.site.objects
      } {
        val setAtx: Set[_] = obj apply x
        setAtx.size === expected
      }
    }

    "have an initial" in {
      val topos = new Diagrams(M)

      val terminalOpt = topos.initial
      terminalOpt match {
        case None => failure(s"Could not build an initial in $topos")
        case Some(terminal) => checkConstSize(topos)(terminal, 0)
      }
      checkConstSize(topos)(topos._0, 0)
      ok
    }
    
    "have a terminal" in {
      val topos = new Diagrams(W)
      
      val terminalOpt = topos.terminal
      terminalOpt match {
        case None => failure(s"Could not build a terminal in $topos")
        case Some(terminal) => checkConstSize(topos)(terminal, 1)
      }
      checkConstSize(topos)(topos._1, 1)
      ok
    }
  }
}
