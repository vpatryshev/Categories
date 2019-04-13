package math.cat.topos

import math.Test
import math.cat.Category._
import math.cat.TestDiagrams
import math.cat.topos.Diagrams.Diagram
import scalakittens.Good

class DiagramsTest extends Test with TestDiagrams {

  type SUT = SmallDiagram

  def checkConstSize(topos: Diagrams)(obj: topos.Obj, expected: Int): Unit = {
    for {
      x <- topos.site.objects
    } {
      val setAtx: Set[_] = obj apply x
      setAtx.size === expected
    }
  }

  "Diagrams" should {

    "have a terminal in W" in {
      val topos = new Diagrams(W)

      val terminalOpt = topos.terminal
      terminalOpt match {
        case Good(terminal) =>
          terminal === topos._1
          checkConstSize(topos)(terminal, 1)
          val ab = terminal.arrowsMapping(terminal.d0.arrow("ab"))
          terminal.asFunction(ab).d0 === Set(Set())
        case none => failure(s"Could not build a terminal in $topos: $none")
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

    case class diagramTable(data: List[String] = Nil) {
      def |(x: String): diagramTable = diagramTable(x::data)
      def |(d: Diagrams.Diagram) = checkDiagram(d, data.reverse, data.reverse)
    }

    case class checkDiagram(d: Diagram, data: List[String], fullList: List[String]) {
      private def check1(x: String, y: String) = {
        val expected = x.split(",").toSet.filter(_.nonEmpty)
        val actual = d(y)
        actual aka s"${d.tag} @$y" must_== expected
      }
      
      def |(x: String): checkDiagram = {
        check1(x, data.head)
        checkDiagram(d, data.tail, fullList)
      }

      def |(d: Diagram): checkDiagram = checkDiagram(d, fullList, fullList)
    }
    
    val applyTo = new diagramTable
    
    def representable(topos: Diagrams) =
      (obj: topos.site.Obj) => topos.representable(obj)
    
    "produce representables in W" in {
      import Diagrams._
      val topos = new Diagrams(W)
      import topos.site._
      val at = representable(topos)

      applyTo | "a" | "b"  | "c" | "d"  | "e" |
      at("a") | "a" | "ab" | ""  | ""   | ""  |
      at("b") | ""  | "b"  | ""  | ""   | ""  |
      at("c") | ""  | "cb" | "c" | "cd" | ""  |
      at("d") | ""  | ""   | ""  | "d"  | ""  |
      at("e") | ""  | ""   | ""  | "ed" | "e"

      ok
    }

    "produce representables in M" in {
      import Diagrams._
      val topos = new Diagrams(M)
      import topos.site._
      val at = representable(topos)

      applyTo | "a"  | "b" | "c"  | "d" | "e"  |
      at("a") | "a"  | ""  | ""   | ""  | ""   |
      at("b") | "ba" | "b" | "bc" | ""  | ""   |
      at("c") | ""   | ""  | "c"  | ""  | ""   |
      at("d") | ""   | ""  | "dc" | "d" | "de" |
      at("e") | ""   | ""  | ""   | ""  | "e"

      ok
    }

    "produce representables in Z3" in {
      import Diagrams._
      val topos = new Diagrams(Z3)
      import topos.site._
      val at = representable(topos)

      applyTo | "0"     |
      at("0") | "0,1,2"

      ok
    }
  }
  
  "Identity arrow" should {
    "exist in Set^W" in {
      val topos = new Diagrams(W)
      val d = SampleWDiagram
      val idtrans = topos.id(d)
      idtrans.d0 === d
      idtrans.d1 === d
      idtrans.compose(idtrans) === idtrans
    }

    "exist in Set^M" in {
      // todo: test composition
      val topos = new Diagrams(M)
      val d = SampleMDiagram
      val idtrans = topos.id(d)
      idtrans.d0 === d
      idtrans.d1 === d
    }
  }

  "Initial object" should {
    "exist in Set^M" in {
      val topos = new Diagrams(M)

      val terminalOpt = topos.initial
      terminalOpt match {
        case Good(terminal) => checkConstSize(topos)(terminal, 0)
        case none => failure(s"Could not build an initial in $topos: $none")
      }
      checkConstSize(topos)(topos._0, 0)
      ok
    }
  }
}
