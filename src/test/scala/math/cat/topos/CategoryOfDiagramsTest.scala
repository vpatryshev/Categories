package math.cat.topos

import math.Test
import math.cat.Category._
import math.sets.Sets.set
import scalakittens.Good

class CategoryOfDiagramsTest extends Test with TestDiagrams {

  type SUT = Diagram

  def representable(topos: CategoryOfDiagrams) =
    (obj: topos.domain.Obj) ⇒ topos.Representable(obj)

  def checkConstSize(topos: CategoryOfDiagrams)(obj: topos.Obj, expected: Int): Unit = {
    for {
      x <- topos.domain.objects
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
      val topos = new CategoryOfDiagrams(W)
      import topos.domain._
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
      val topos = new CategoryOfDiagrams(M)
      import topos.domain._
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
        x <- topos.domain.objects
        a <- topos.domain.arrows
      } {
        val r = rep(x)
        val arrow = r.d0.arrow(a)
        val ra = r.arrowsMapping(arrow)
        ra should not be null
      }
      
      ok
    }

    "be good in Set^Z3" in {
      val topos = new CategoryOfDiagrams(Z3)
      import topos.domain._
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
  
  "Identity arrow" should {
    "exist in Set^W" in {
      val topos = new CategoryOfDiagrams(W)
      val d = SampleWDiagram
      val idtrans = topos.id(d)
      idtrans.d0 === d
      idtrans.d1 === d
      idtrans.compose(idtrans) === idtrans
    }

    "exist in Set^M" in {
      // todo: test composition
      val topos = new CategoryOfDiagrams(M)
      val d = SampleMDiagram
      val idtrans = topos.id(d)
      idtrans.d0 === d
      idtrans.d1 === d
    }
  }

  "Initial object" should {
    "exist in Set^M" in {
      val topos = new CategoryOfDiagrams(M)

      val initialOpt = topos.initial
      initialOpt match {
        case Good(initial) ⇒ checkConstSize(topos)(initial, 0)
        case none ⇒ failure(s"Could not build an initial in $topos: $none")
      }
      checkConstSize(topos)(topos._0, 0)
      ok
    }
  }
  
  "Terminal object" should {

    "exist in Set^W" in {
      val topos = new CategoryOfDiagrams(W)

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
      val topos = new CategoryOfDiagrams(Square)
      val subterminals = topos.subterminals
      subterminals.size === 6
      subterminals must contain(topos._0)
      subterminals must contain(topos._1)
    }
  }
  
  "List of subobjects" should {
    "be good for empty diagram" in {
      val sut = EmptyDiagram
      val actual = sut.subobjects
      actual.size === 1
      actual.head === sut
      actual.forall(_ ⊂ sut)
    }

    "be good for pullback diagram" in {
      val sut = SamplePullbackDiagram
      val topos = new CategoryOfDiagrams(Pullback)
      val sample = sut.d0.objects map (ob ⇒ sut.objectsMapping(ob))

      def canonical(s: set) = s.map(_.toString).toList.sorted.mkString(",")

      def fullSet(d: Diagram): List[String] = {
        d.d0.objects.toList map ((o:d.d0.Obj) ⇒ d.objectsMapping(o))
      } map d.asSet map canonical

      val listOfSubobjects = sut.subobjects.toList
      val actual =
        listOfSubobjects.sortBy(fullSet)(Ordering.by(_.mkString(";"))).reverse
      actual.size === 85
      val objects = actual map fullSet

      actual.head.points.size === 0
      val last = actual.last
      sut.d0 === last.d0
      sut.d1 === last.d1

      sut.sameNodes(last) must beTrue
      sut.sameArrows(last) must beTrue
      val comp = actual.last == sut

      actual.last === sut

      actual.forall(_ ⊂ sut)

    }
    
    "exist for representables in Set^_2_" in {
      val topos = new CategoryOfDiagrams(_2_)
      val r0 = topos.Representable(topos.objectNamed("0"))

      val obj0 = topos.objectNamed("0")
      val obj1 = topos.objectNamed("1")
      
      val sor = topos.subobjectsOfRepresentables
      sor.size === 2
      sor(obj0).size === 3
      sor(obj1).size === 2
    }
  }
  
  "Cartesian product" should {
    "exist in Set^ParallelPair" in {
      val topos = new CategoryOfDiagrams(ParallelPair)
      val d1 = SampleParallelPairDiagram1
      val d2 = SampleParallelPairDiagram2
      val sample = topos.product2builder(d1, d2)
      
      // no need to check f1 and f2 for now
      val actual = topos.product2(d1, d2)
      actual("0").size === 15
      actual("1") === Set((0,0), (1,0), (2,0), (3,0), (0,1), (1,1), (2,1), (3,1))
      
    }
    "exist in Set^M" in {
      val topos = new CategoryOfDiagrams(M)

      val actual = topos.product2(SampleMDiagram, SampleMDiagram)
      for {
        x <- topos.domain.objects
      } {
        actual(x).size == SampleMDiagram(x).size*SampleMDiagram(x).size
      }
      ok
    }
  }

  "Diagram points" should {

    def checkPoint(sut: Diagram)(point: Point, values: List[Int]) = {
      val objects = sut.d0.objects.toList
      val actual = objects map point.apply
      val expected = values // map Sets.singleton
      actual must_== expected
    }

    "exist in paralel pair" in {
      val sut = SampleParallelPairDiagram1
      val topos = new CategoryOfDiagrams(ParallelPair)
      import topos._
      val actual = sut.points
      actual.size === 3
      val check = checkPoint(sut) _
      val p1 :: p2 :: p3 :: Nil = actual
      check(p1, 1 :: 1 :: Nil)
      check(p2, 2 :: 2 :: Nil)
      check(p3, 5 :: 2 :: Nil)
    }

    "exist in pullback" in {
      val sut = SamplePullbackDiagram
      val topos = new CategoryOfDiagrams(Pullback)
      import topos._
      val actual = sut.points
      actual.size === 5
      val p1 :: p2 :: p3 :: p4 :: p5 :: Nil = actual
      val check = checkPoint(sut) _
      check(p1, 1 :: 2 :: 1 :: Nil)
      check(p2, 1 :: 4 :: 1 :: Nil)
      check(p3, 2 :: 3 :: 0 :: Nil)
      check(p4, 3 :: 2 :: 1 :: Nil)
      check(p5, 3 :: 4 :: 1 :: Nil)
    }

    "exist in Z3 diagram" in {
      val sut = SampleZ3Diagram
      val actual = sut.points
      actual.size === 1
      val p1 :: Nil = actual
      val check = checkPoint(sut) _

      ok
    }
  }
}
