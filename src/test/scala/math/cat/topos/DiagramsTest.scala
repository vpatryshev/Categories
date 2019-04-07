package math.cat.topos

import math.Test
import math.cat.Category._
import math.cat.TestDiagrams

class DiagramsTest extends Test with TestDiagrams {

  type SUT = SmallDiagram

  "Diagrams" should {
    "have id W" in {
      val topos = new Diagrams(W)
      val d = SampleWDiagram
      val idtrans = topos.id(d)
      idtrans.d0 === d
      idtrans.d1 === d
      idtrans.compose(idtrans) === idtrans
    }

    "have id in M" in {
      // todo: test composition
      val topos = new Diagrams(M)
      val d = SampleMDiagram
      val idtrans = topos.id(d)
      idtrans.d0 === d
      idtrans.d1 === d
    }

    def checkConstSize(topos: Diagrams)(obj: topos.Obj, expected: Int): Unit = {
      for {
        x <- topos.site.objects
      } {
        val setAtx: Set[_] = obj apply x
        setAtx.size === expected
      }
    }

    "have an initial in M" in {
      val topos = new Diagrams(M)

      val terminalOpt = topos.initial
      terminalOpt match {
        case None => failure(s"Could not build an initial in $topos")
        case Some(terminal) => checkConstSize(topos)(terminal, 0)
      }
      checkConstSize(topos)(topos._0, 0)
      ok
    }

    "have a terminal in W" in {
      val topos = new Diagrams(W)

      val terminalOpt = topos.terminal
      terminalOpt match {
        case None => failure(s"Could not build a terminal in $topos")
        case Some(terminal) =>
          terminal === topos._1
          checkConstSize(topos)(terminal, 1)
          val ab = terminal.arrowsMapping(terminal.d0.arrow("ab"))
          terminal.asFunction(ab).d0 === Set(Set())
      }
      ok
    }

    "list subterminals in square" in {
      val topos = new Diagrams(Square)
      val subterminals = topos.subterminals
      subterminals.size === 6
      subterminals must contain(topos._0)
      subterminals must contain(topos._1)
    }
  }
}
