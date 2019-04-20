package math.cat.topos

import math.Test
import math.cat.Category._
import math.cat.{SetDiagram, TestDiagrams}
import math.cat.topos.Diagrams.Diagram
import math.sets.Sets.set
import scalakittens.Good
import math.Base._

class DiagramsTest extends Test with TestDiagrams {

  type SUT = SmallDiagram

  def representable(topos: Diagrams) =
    (obj: topos.site.Obj) ⇒ topos.Representable(obj)

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
      def |(f: String ⇒ Any) = check(f)(data.reverse, data.reverse)
    }

    case class check(f: String ⇒ Any)(data: List[String], fullList: List[String]) {
      private def check1(x: String, y: String) = {
        val expected = x.split(",").toSet.filter(_.nonEmpty)
        val actual = f(y)
        actual.toString aka s"@$y" must_== expected.toString
      }

      def |(x: String): check = {
        check1(x, data.head)
        check(f)(data.tail, fullList)
      }

      def |(f: String ⇒ Any): check = check(f)(fullList, fullList)
    }

    val appliesTo = new diagramTable

    "be good in Set^W" in {
      val topos = new Diagrams(W)
      import topos.site._
      val ob = (o: String) ⇒ {
        val r = representable(topos)(obj(o))
        name: String ⇒ r(name)
      }

      appliesTo | "a" | "b"  | "c" | "d"  | "e" |
        ob("a") | "a" | "ab" | ""  | ""   | ""  |
        ob("b") | ""  | "b"  | ""  | ""   | ""  |
        ob("c") | ""  | "cb" | "c" | "cd" | ""  |
        ob("d") | ""  | ""   | ""  | "d"  | ""  |
        ob("e") | ""  | ""   | ""  | "ed" | "e"

      ok
    }

    "be good in Set^M" in {
      val topos = new Diagrams(M)
      import topos.site._
      val rep = representable(topos)
      val ob = (o: Obj) ⇒ {
        val r = rep(o)
        name: String ⇒ r(name)
      }

      appliesTo | "a"  | "b" | "c"  | "d" | "e"  |
        ob("a") | "a"  | ""  | ""   | ""  | ""   |
        ob("b") | "ba" | "b" | "bc" | ""  | ""   |
        ob("c") | ""   | ""  | "c"  | ""  | ""   |
        ob("d") | ""   | ""  | "dc" | "d" | "de" |
        ob("e") | ""   | ""  | ""   | ""  | "e"

      val mults = for {
        x <- topos.site.objects
        a <- topos.site.arrows
      } {
        val r = rep(x)
        val arrow = r.d0.arrow(a)
        val ra = r.arrowsMapping(arrow)
        ra should not be null
      }
      
      ok
    }

    "be good in Set^Z3" in {
      val topos = new Diagrams(Z3)
      import topos.site._
      val ob = (o: Obj) ⇒ {
        val r = representable(topos)(o)
        name: String ⇒ r(obj(name)).toString
      }
      val ar = (o: Obj) ⇒ {
        val r = representable(topos)(o)
        a: String ⇒ r.arrowsMapping(r.d0.arrow(a)).toString
      }

      appliesTo | "0"     |
        ob("0") | "0,1,2"

      ok
    }
    
  }
  
//  "Power" should {
//    "exist for representables in Set^_2_" in {
//      val topos = new Diagrams(_2_)
//      import topos.site._
//      val sut0 = representable(topos)("0")
//      val pow0 = sut0.power
//      val sut1 = representable(topos)("1")
//      val pow1 = sut1.power
//      ok
//    }
//
//    "exist for reps in Set^_2_" in {
//      ok
//    }    
//  }
  
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
        case Good(terminal) ⇒ checkConstSize(topos)(terminal, 0)
        case none ⇒ failure(s"Could not build an initial in $topos: $none")
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
        case Good(terminal) ⇒
          terminal === topos._1
          checkConstSize(topos)(terminal, 1)
          val ab = terminal.arrowsMapping(terminal.d0.arrow("ab"))
          terminal.asFunction(ab).d0 === Set(Set())
        case none ⇒ failure(s"Could not build a terminal in $topos: $none")
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
      val sample = SamplePullbackDiagram.d0.objects map (ob ⇒ SamplePullbackDiagram.objectsMapping(ob))

      def canonical(s: set) = s.map(_.toString).toList.sorted.mkString(",")

      def fullSet(d: SetDiagram): List[String] = {
        d.d0.objects.toList map ((o:d.d0.Obj) ⇒ d.objectsMapping(o))
      } map d.asSet map canonical

      val listOfSubobjects = SamplePullbackDiagram.subobjects.toList
      val actual =
        listOfSubobjects.sortBy(fullSet)(Ordering.by(_.mkString(";"))).reverse
      actual.size === 85
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
    
    "exist for representables in Set^_2_" in {
      val topos = new Diagrams(_2_)
      val r0 = topos.Representable(topos.objectNamed("0"))

      val obj0 = topos.objectNamed("0")
      val obj1 = topos.objectNamed("1")
      
      val sor = topos.subobjectsOfRepresentables
      sor.size === 2
      sor(obj0).size === 3
      sor(obj1).size === 2
    }
  }
  
  "Subobject classifier" should {
    "exist for _0_" in {
      val topos = new Diagrams(_0_)
      val omega = topos.Ω
      val points = omega.points
      points.size === 1
    }

    "exist for _1_" in {
      val topos = new Diagrams(_1_)
      val omega = topos.Ω
      val omega0 = omega("0").toList
      omega0.size === 2
      val omega00::omega01::Nil = omega0
      omega00.asInstanceOf[Diagram]("0").isEmpty must beTrue
      omega01.asInstanceOf[Diagram]("0").isEmpty must beFalse
      val points = omega.points
      points.size === 2
    }

    "exist for _2_" in {
      val topos = new Diagrams(_2_)
      
      val omega = topos.Ω
//      omega("0") === Nil
//      omega("1") === Nil
      val points = omega.points
      points.size === 3
    }
  }
}
