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

    "produce representables in W" in {
      import Diagrams._
      val topos = new Diagrams(W)
      import topos.site._
      val at = (obj: topos.site.Obj) => topos.representable(obj)
      
      case class table(data: List[String] = Nil) {
        def |(x: String): table = table(x::data)
        def |(d: Diagrams.Diagram) = check(d, data.reverse, data.reverse)
      }

      case class check(d: Diagram, data: List[String], fullList: List[String]) {
        def |(x: String): check = {
          val y = data.head
          val expected = x.split(",").toSet.filter(_.nonEmpty)
          val actual = d(y)
          actual aka s"${d.tag} @$y" must_== expected
          check(d, data.tail, fullList)
        }
        def |(d: Diagram): check = check(d, fullList, fullList)
      }

      val applyTo = new table

      applyTo | "a" | "b"  | "c" | "d"  | "e" |
      at("a") | "a" | "ab" | ""  | ""   | ""  |
      at("b") | ""  | "b"  | ""  | ""   | ""  |
      at("c") | ""  | "cb" | "c" | "cd" | ""  |
      at("d") | ""  | ""   | ""  | "d"  | ""  |
      at("e") | ""  | ""   | ""  | "ed" | "e" 

      ok
    }
  }
}
