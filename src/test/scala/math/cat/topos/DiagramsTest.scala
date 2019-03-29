package math.cat.topos

import math.cat.{Category, TestDiagrams}
import org.specs2.mutable.Specification
import Category._
import math.Test
import math.Base._
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

    def checkConstSize(topos: Diagrams)(obj: topos.Obj, expected: Int): Unit = {
      import obj._
      for {
        x <- topos.site.objects
      } {
        val setAtx: Set[_] = obj @@ x
        setAtx.size === expected
      }
      
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
