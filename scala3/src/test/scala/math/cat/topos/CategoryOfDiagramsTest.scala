package math.cat.topos

import math.Base._
import math.Test
import math.cat.Categories._
import math.cat.SetFunction
import math.sets.Sets.set
import scalakittens.Good

import scala.language.implicitConversions
import scala.annotation.targetName

class CategoryOfDiagramsTest extends Test with TestDiagrams:

  type SUT = Diagram

  def representable(topos: CategoryOfDiagrams): topos.domain.Obj => topos.Representable =
    (obj: topos.domain.Obj) => topos.Representable(obj)

  def checkConstSize(topos: CategoryOfDiagrams)(obj: topos.Obj, expected: Int): Unit =
    for
      x <- topos.domain.objects
    do
      val setAtx: Set[?] = obj(x)
      setAtx.size === expected

  
  "representables" should {
    case class diagramTable(data: List[String] = Nil) {
      @targetName("add_column")
      def |(x: String): diagramTable = diagramTable(x::data)
      @targetName("add_column")
      def |(f: String => Any): check = check(f)(data.reverse, data.reverse)
    }

    case class check(f: String => Any)(data: List[String], fullList: List[String]) {
      private def check1(x: String, y: String) = {
        val expected = x.split(",").toSet.filter(_.nonEmpty)
        val actual = f(y)
        actual.toString aka s"@$y" must_== expected.toString
      }

      def |(x: String): check = {
        check1(x, data.head)
        check(f)(data.tail, fullList)
      }

      def |(f: String => Any): check = check(f)(fullList, fullList)
    }

    val appliesTo = new diagramTable

    "be good in Set to W" in {
      val topos = new CategoryOfDiagrams(W)
      import topos.domain._
      val ob = (o: String) => {
        val r = representable(topos)(o)
        (name: String) => r(name)
      }

      appliesTo | "a" | "b" | "c" | "d" | "e" |
        ob("a") | "a" | "ab"|  "" |  "" |  "" |
        ob("b") |  "" | "b" |  "" |  "" |  "" |
        ob("c") |  "" | "cb"| "c" | "cd"|  "" |
        ob("d") |  "" |  "" |  "" | "d" |  "" |
        ob("e") |  "" |  "" |  "" | "ed"| "e"

      ok
    }

    "be good in Set to M" in {
      val topos = new CategoryOfDiagrams(M)
      import topos.domain._
      val ob = (o: String) => {
        val r = representable(topos)(o)
        (name: String) => r(name)
      }

      appliesTo | "a" | "b" | "c" | "d" | "e" |
        ob("a") | "a" | ""  | ""  | ""  | ""  |
        ob("b") | "ba"| "b" | "bc"| ""  | ""  |
        ob("c") | ""  |  "" | "c" | ""  | ""  |
        ob("d") | ""  |  "" | "dc"| "d" | "de"|
        ob("e") | ""  |  "" | ""  | ""  | "e"

      val mults: Unit = for {
        x <- topos.domain.objects
        a <- topos.domain.arrows
      } {
        val r = representable(topos)(x)
        val ra = r.arrowsMapping(a)
      }

      ok
    }

    "be good in Set to Z3" in {
      val topos = new CategoryOfDiagrams(Z3)
      import topos.domain._
      def ob(o: String) = {
        val r = representable(topos)(o)
        ((name: String) => r(name).toString): (String => String)
      }

      val stringToString: String => String = ob("0")
      
      appliesTo | "0"     |
        ob("0") | "0,1,2"

      ok
    }

  }

  "Identity arrow" should {
    "exist in Set to W" in {
      val topos = new CategoryOfDiagrams(W)
      val d = SampleWDiagram
      val identity_transformation = topos.id(d)
      identity_transformation.d0 === d
      identity_transformation.d1 === d
      identity_transformation ∘ identity_transformation === identity_transformation
    }

    "exist in Set to M" in {
      // todo: test composition
      val topos = new CategoryOfDiagrams(M)
      val d = SampleMDiagram
      val idtrans = topos.id(d)
      idtrans.d0 === d
      idtrans.d1 === d
    }
  }

  "Initial object" should {
    "exist in Set to M" in {
      val topos = new CategoryOfDiagrams(M)

      val initialOpt = topos.initialT
      initialOpt match
        case Good(initial) => checkConstSize(topos)(initial, 0)
        case none => failure(s"Could not build an initial in $topos: $none")

      checkConstSize(topos)(topos._0, 0)
      ok
    }
  }

  "Terminal object" should {

    "exist in Set to W" in {
      val topos = new CategoryOfDiagrams(W)

      val terminalOpt = topos.terminalT
      terminalOpt match
        case Good(terminal) =>
          terminal === topos._1
          checkConstSize(topos)(terminal, 1)
          val ab = terminal.arrowsMapping("ab")
          terminal.asFunction(ab).d0 === Set(Set())
        case none => failure(s"Could not build a terminal in $topos: $none")

      ok
    }

    "exist in Set to Square" in {
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
      val sample = sut.d0.objects map (ob => sut.objectsMapping(ob))

      def fullSet(d: Diagram): List[String] = {
        d.d0.objects.toList map ((o: d.d0.Obj) => d.objectsMapping(o))
      } map d.setOf map asString

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

    "exist for representables in Set to _2_" in {
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
    "exist in Set to ParallelPair" in {
      val topos = new CategoryOfDiagrams(ParallelPair)
      val d1 = SampleParallelPairDiagram1
      val d2 = SampleParallelPairDiagram2

      // no need to check f1 and f2 for now
      val actual = topos.product2(d1, d2)
      actual("0").size === 15
      actual("1") === Set((0, 0), (1, 0), (2, 0), (3, 0), (0, 1), (1, 1), (2, 1), (3, 1))
    }

    "exist in Set to M" in {
      val topos = new CategoryOfDiagrams(M)

      val actual = topos.product2(SampleMDiagram, SampleMDiagram)
      for {
        x <- topos.domain.objects
      } {
        actual(x).size == SampleMDiagram(x).size * SampleMDiagram(x).size
      }
      ok
    }
  }

  "Cartesian product of arrows" should {
    "exist in Set to _1_" in {
      val topos = new CategoryOfDiagrams(_1_)
      val d01 = build(s"d01", _1_)(
        Map[String, set]("0" -> Set(11, 12)),
        Map[String, SetFunction]()
      )
      val d02 = build(s"d02", _1_)(
        Map[String, set]("0" -> Set(21, 22)),
        Map[String, SetFunction]()
      )
      val d11 = build(s"d11", _1_)(
        Map[String, set]("0" -> Set("a11", "a12")),
        Map[String, SetFunction]()
      )
      val d12 = build(s"d12", _1_)(
        Map[String, set]("0" -> Set("b21", "b22")),
        Map[String, SetFunction]()
      )

      val f = topos.buildArrow("f",
        d01, d11,
        x => i => "a" + i)

      val g = topos.buildArrow("g",
        d02, d12,
        x => i => "b" + i)

      val expected = topos.buildArrow("f×g",
        topos.product2(d01, d02),
        topos.product2(d11, d12),
        x => {case (i, j) => ("a" + i, "b" + j)})

      val actual = topos.productOfArrows(f, g)

      actual === expected
    }
    "exist in Set to ParallelPair" in {
      val topos = new CategoryOfDiagrams(ParallelPair)

      val dom1a = Set[Any](101, 102)
      val dom1b = Set[Any](111, 112, 121, 122)
      val d01 = build(s"d01", ParallelPair)(
        Map[String, set]("0" -> dom1a, "1" -> dom1b),
        Map[String, SetFunction](
          "a" -> SetFunction("_a_", dom1a, dom1b, x => x.toString.toInt + 10),
          "b" -> SetFunction("_b_", dom1a, dom1b, x => x.toString.toInt + 20)
        ))

      val dom2a = Set[Any](201, 202)
      val dom2b = Set[Any](211, 212, 221, 222)

      val d02 = build(s"d02", ParallelPair)(
        Map[String, set]("0" -> dom2a, "1" -> dom2b),
        Map[String, SetFunction](
          "a" -> SetFunction("_a_", dom2a, dom2b, x => x.toString.toInt + 10),
          "b" -> SetFunction("_b_", dom2a, dom2b, x => x.toString.toInt + 20)
        )
      )
      val codom1a = Set[Any]("a101", "a102")
      val codom1b = Set[Any]("a111", "a112", "a121", "a122")
      val d11 = build(s"d11", ParallelPair)(
        Map[String, set]("0" -> codom1a, "1" -> codom1b),
        Map[String, SetFunction](
          "a" -> SetFunction("_a_", codom1a, codom1b,
            s => s.toString.replace("0", "1")),
          "b" -> SetFunction("_b_", codom1a, codom1b,
            s => s.toString.replace("0", "2"))
        )
      )

      val codom2a = Set[Any]("b201", "b202")
      val codom2b = Set[Any]("b211", "b212", "b221", "b222")

      val d12 = build(s"d12", ParallelPair)(
        Map[String, set]("0" -> codom2a, "1" -> codom2b),
        Map[String, SetFunction](
          "a" -> SetFunction("_a_", codom2a, codom2b,
            s => s.toString.replace("0", "1")),
          "b" -> SetFunction("_b_", codom2a, codom2b,
            s => s.toString.replace("0", "2"))
        )
      )

      val f = topos.buildArrow("f",
        d01, d11,
        x => i => "a" + i)

      val g = topos.buildArrow("g",
        d02, d12,
        x => i => "b" + i)

      val expected = topos.buildArrow("f×g",
        topos.product2(d01, d02),
        topos.product2(d11, d12),
        x => {
          case (i, j) => ("a" + i, "b" + j)
        })

      val actual = topos.productOfArrows(f, g)

      actual === expected
    }
  }

  "Diagram points" should {

    def checkPoint(sut: Diagram)(point: Point, expected: List[Int]) = {
      val objects = sut.d0.objects.toList
      val actual = objects map point.apply
      actual must_== expected
    }

    "exist in paralel pair" in {
      val sut = SampleParallelPairDiagram1
      val topos = new CategoryOfDiagrams(ParallelPair)
      val check = checkPoint(sut)
      sut.points match
        case p1 :: p2 :: p3 :: Nil =>
          check(p1, 1 :: 1 :: Nil)
          check(p2, 2 :: 2 :: Nil)
          check(p3, 5 :: 2 :: Nil)
        case bad => failure("Expected 3 points, got: $bad")
        
      ok
    }

    "exist in pullback" in {
      val sut = SamplePullbackDiagram
      val topos = new CategoryOfDiagrams(Pullback)
      val actual = sut.points
      actual match
        case p1 :: p2 :: p3 :: p4 :: p5 :: Nil =>
          val check = checkPoint(sut)
          check(p1, 1 :: 2 :: 1 :: Nil)
          check(p2, 1 :: 4 :: 1 :: Nil)
          check(p3, 2 :: 3 :: 0 :: Nil)
          check(p4, 3 :: 2 :: 1 :: Nil)
          check(p5, 3 :: 4 :: 1 :: Nil)
        case _ => failure("Expected 5 points from $actual")  
        
      ok
    }

    "exist in Z3 diagram" in {
      val sut = SampleZ3Diagram
      val actual = sut.points
      actual match
        case p1 :: Nil =>
          val check = checkPoint(sut)
          check(p1, 2223 :: Nil)
        case oops => failure("Expected 1 point in $actual: $oops")

      ok
    }
  }
