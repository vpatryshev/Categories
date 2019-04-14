package math.cat.topos

import math.Test
import math.cat.Category._
import math.cat.{SetDiagram, TestDiagrams}
import math.cat.topos.Diagrams.Diagram
import math.sets.Sets.set
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
  
  "representables" should {
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

    "be good in Set^W" in {
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

    "be good in Set^M" in {
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

    "be good in Set^Z3" in {
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
  
  "Terminal object" should {

    "exist in Set^W" in {
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

    "exist in Set^Square" in {
      val topos = new Diagrams(Square)
      val subterminals = topos.subterminals
      subterminals.size === 6
      subterminals must contain(topos._0)
      subterminals must contain(topos._1)
    }
  }
  
  "List of subobjects" should {
    "be good for empty diagram" in {
      val actual = EmptyDiagram.subobjects
      actual.size === 1
      actual.head === EmptyDiagram
    }
    
    "be good for pullback diagram" in {
      val sample = SamplePullbackDiagram.d0.objects map (ob => SamplePullbackDiagram.objectsMapping(ob))
      
      def canonical(s: set) = s.map(_.toString).toList.sorted.mkString(",")

      def fullSet(d: SetDiagram): List[String] = {
        d.d0.objects.toList map ((o:d.d0.Obj) => d.objectsMapping(o))
      } map d.asSet map canonical

      val listOfSubobjects = SamplePullbackDiagram.subobjects.toList
      val actual =
        listOfSubobjects.sortBy(fullSet)(Ordering.by(_.mkString(";"))).reverse
      actual.size === 20
      val objects = actual map fullSet

      actual.head.points.size === 0
      val last = actual.last
      SamplePullbackDiagram.d0 === last.d0
      SamplePullbackDiagram.d1 === last.d1

      SamplePullbackDiagram.sameNodes(last) must beTrue
      SamplePullbackDiagram.sameArrows(last) must beTrue
      val comp = actual.last == SamplePullbackDiagram

      actual.last === SamplePullbackDiagram
    }
  }
}
