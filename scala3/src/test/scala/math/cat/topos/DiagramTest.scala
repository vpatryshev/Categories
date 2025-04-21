package math.cat.topos

import scala.language.implicitConversions

import math.Test
import math.cat.Categories._
import math.cat.SetCategory.Setf
import math.cat.{Category, Functor, SetFunction}
import SetFunction.fun
import math.sets.Sets
import math.sets.Sets.set
import scalakittens.{Good, Result}

/**
  * Test for individual diagrams (functors with codomain=sets)
  */
class DiagramTest extends Test with TestDiagrams:
  type SUT = Diagram

  "Diagram" should {

    "validate as a functor with Set as domain" in {
      val sut = BuildPullbackDiagram.asFunctor.iHope
      sut.d0 === Pullback
      sut.d1 === Setf
    }

    "test build" in {
      val dom = Pullback
      val topos = new CategoryOfDiagrams(dom)
      val sut1 = BuildPullbackDiagram.asFunctor.iHope
      sut1.objectsMapping("b") === BuildPullbackDiagram.sb

      val diagram: Diagram =
        new Diagram("Test", topos):
          
          override def objectsMapping(x: d0.Obj): d1.Obj =
            BuildPullbackDiagram.om(x.toString)
          
          override protected def arrowsMappingCandidate(a: d0.Arrow): d1.Arrow =
            BuildPullbackDiagram.am(a.toString)

      val res = Functor.validateFunctor(diagram)
      res.isGood must beTrue
    }

    "get validated - positive" in {
      val sut = SamplePullbackDiagram
      sut.d0 === Pullback
      sut.d1 === Setf
    }

    "get validated with inconsistent mappings - negative" in {
      val a: set = Set(1, 2, 3, 4, 5)
      val b: set = Set(0, 1, 2)

      val f = fun(a,b)("f", x => Math.min(2, x.toInt))
      val g = fun(b,b)("g", x => x.toInt % 3)
      val topos = new CategoryOfDiagrams(ParallelPair)
      expectError(_.
        matches(raw"Inconsistent mapping for d0\(b\) - Set\(.*\) vs .*Set\(5, 1, 2, 3, 4\).*"),
        Diagram.tryBuild(topos)(
          "Bad Bad Bad",
          Map("0" -> a, "1" -> b),
          Map("a" -> f, "b" -> g)
        )
      )
    }

    "get validated with inconsistent domains - negative" in {
      val a: set = Set(1, 2, 3, 4, 5)
      val b: set = Sets.Empty

      // The following two things are actually not set functions, they are broken
      val f = new SetFunction("f", a, b, x => Math.min(2, x.toString.toInt)) // Note: this i
      val g = new SetFunction("g", b, b, x => x.toString.toInt % 3)
      val topos = new CategoryOfDiagrams(ParallelPair)

      expectError(_.
        matches(raw"Inconsistent mapping for d0\(b\) - Set\(.*\) vs HashSet\(5, 1, 2, 3, 4\).*"),
        Diagram.tryBuild(topos)(
          "Bad Bad Bad",
          Map("0" -> a, "1" -> b),
          Map("a" -> f, "b" -> g)
        )
      )
    }

    "validate empty diagram" in {
      EmptyDiagram.d0 === `𝟘`
      EmptyDiagram.d1 === Setf
    }
  }

  "Diagram limit" should {
    "exist for an empty diagram" in {
      val sut = const(Set("a", "b"))
          sut.d0 === `𝟙`
          sut.d1 === Setf
          sut.objectsMapping("0") === Set("a", "b")
    }

    "exist for a point" in {
      val x: set = Set("x", "y", "z")
      val sut = const(x)
      sut.d0 === `𝟙`
      sut.d1 === Setf

      sut.limit match
        case Good(sut.Cone(vertex, arrowTo)) =>
          sut.setOf(vertex).size === x.size
        case none => failure(s"We expected a limit, got $none")
      
      ok
    }

    "exist for a pullback" in {
      val sut = SamplePullbackDiagram
      val limit = sut.limit.iHope
      val vertex = sut.setOf(limit.vertex)
      vertex.size === 5
      val ara = limit.arrowTo("a")
      val arb = limit.arrowTo("b")

      for
        i <- 1 to 3
        j <- 2 to 4
        element = i :: j :: Nil
      do
        (i, j, vertex(element)) === (i, j, (i + j) % 2 == 1)
        if vertex(element) then
          (i, j, ara(element)) === (i, j, i)
          (i, j, arb(element)) === (i, j, j)

      ok
    }

    "exist for an equalizer" in {
      val sut = SampleParallelPairDiagram1
      val limit = sut.limit.iHope

      val vertex = sut.setOf(limit.vertex)
      vertex.size === 3
      vertex === Set(1 :: Nil, 2 :: Nil, 5 :: Nil)
      
      val ar0 = limit.arrowTo("0")
      val ar1 = limit.arrowTo("1")

      for element <- vertex do
        val i = (element match
          case n :: Nil => n;
          case other => Integer.MAX_VALUE
        ).toString.toInt
        (element, ar0(element)) === (element, i)
        (element, ar1(element)) === (element, i % 3)
      
      ok
    }

      "exist for a monoid Z3" in {
        val sut = SampleZ3Diagram
        val limit = sut.limit.iHope
        limit.vertex === Set(List(2223))
        val ar0 = limit.arrowTo("0")
        ar0(List(2223)) === 2223
      }

      "exist for a W, regular data" in {
        val sut = SampleWDiagram
        val limit = sut.limit.iHope

        val vertex = sut.setOf(limit.vertex)
        vertex.size === 16
        val ara = limit.arrowTo("a")
        val arb = limit.arrowTo("b")
        val arc = limit.arrowTo("c")
        val ard = limit.arrowTo("d")
        val are = limit.arrowTo("e")

        for
          i <- SampleWDiagramContent.a
          j <- SampleWDiagramContent.c
          k <- SampleWDiagramContent.e
        do
          val element = i :: j :: k :: Nil
          if vertex(element) then
            (i, j, k, ara(element)) === (i, j, k, i)
            (i, j, k, arc(element)) === (i, j, k, j)
            (i, j, k, are(element)) === (i, j, k, k)
            (i, j, k, arb(element)) === (i, j, k, SampleWDiagramContent.ab(i))
            (i, j, k, arb(element)) === (i, j, k, SampleWDiagramContent.cb(j))
            (i, j, k, ard(element)) === (i, j, k, SampleWDiagramContent.cd(j))
            (i, j, k, ard(element)) === (i, j, k, SampleWDiagramContent.ed(k))

        ok
      }

      "exist for a W, weird data" in {
        val topos = new CategoryOfDiagrams(W)
        val a: set = Set(1, 2, 3)
        val b: set = Set(2, 3, 4)
        val c: set = Set(0, 1, 2, 3, 4, 5)
        val d: set = Set(0, 1, 2)
        val e: set = Set(1, 2, 3, 4)
        val ab = fun(a,b)("ab", _.toInt + 1)
        val cb = fun(c,b)("cb", x => Math.max(2, Math.min(4, x.toInt)))
        val cd = fun(c,d)("cd", _.toInt % 3)
        val ed = fun(e,d)("ed", x => (x.toInt + 1) % 2)
        val sut = Diagram.tryBuild(topos)(
          "W",
          Map("a" -> a, "b" -> b, "c" -> c, "d" -> d, "e" -> e),
          Map("ab" -> ab, "cb" -> cb, "cd" -> cd, "ed" -> ed)
        ).iHope

        val limit = sut.limit.iHope

        val vertex = sut.setOf(limit.vertex)
        vertex.size === 8
        val ara = limit.arrowTo("a")
        val arb = limit.arrowTo("b")
        val arc = limit.arrowTo("c")
        val ard = limit.arrowTo("d")
        val are = limit.arrowTo("e")
        val points = sut.limitBuilder

        for
          i <- a
          j <- c
          k <- e
        do
          val element = i :: j :: k :: Nil
          val eq1 = ab(i) == cb(j)
          val eq2 = cd(j) == ed(k)
          (i, j, k, eq1, eq2, points.isPoint(element)) === (i, j, k, eq1, eq2, eq1 && eq2)
          (i, j, k, eq1, eq2, vertex(element)) === (i, j, k, eq1, eq2, eq1 && eq2)

          if vertex(element) then
            (i, j, k, ara(element)) === (i, j, k, i)
            (i, j, k, arc(element)) === (i, j, k, j)
            (i, j, k, are(element)) === (i, j, k, k)
            (i, j, k, arb(element)) === (i, j, k, ab(i))
            (i, j, k, arb(element)) === (i, j, k, cb(j))
            (i, j, k, ard(element)) === (i, j, k, cd(j))
            (i, j, k, ard(element)) === (i, j, k, ed(k))
        
        ok
      }
    }

  "Diagram colimit" should {
    "exist for a empty diagram" in {
      val sut = EmptyDiagram
      sut.d0 === `𝟘`
      sut.d1 === Setf
      sut.colimit match
        case Good(sut.Cocone(Sets.Empty, arrowFrom)) => ok
        case none => failure(s"no colimit? $none")

      ok
    }

    "exist for a point" in {
      val expected: set = Set("x", "y", "z")
      val sut = const(expected)
      sut.d0 === `𝟙`
      sut.d1 === Setf
      val colimit = sut.colimit.iHope
      val vertex = sut.setOf(colimit.vertex)
      vertex.size === expected.size
    }

    "exist for a pushout" in {
      val topos = new CategoryOfDiagrams(Pushout)
      val a: set = Set(1, 2, 3)
      val b: set = Set(2, 3, 4)
      val c: set = Set(0, 1)
      val ab = fun(a,b)("f", _.toInt + 1)
      val ac = fun(a,c)("g", _.toInt % 2)
      val sut: SUT = Diagram.tryBuild(topos)(
        "pushout",
        Map("a" -> a, "b" -> b, "c" -> c),
        Map("ab" -> ab, "ac" -> ac)
      ).iHope
      val colimit = sut.colimit.iHope

      val v0 = Set((0, 4), (0, 2), (1, 1))
      val v1 = Set((0, 3), (1, 0))
      val ExpectedVertex: set = Set(v0, v1)
      val list = v0 :: v1 :: v0 :: Nil
      val ara = colimit.arrowFrom("a")
      val arb = colimit.arrowFrom("b")

      for i <- 1 to 3 do ara(i) == list(i - 1)
      for i <- 2 to 4 do arb(i) == list(i - 2)
        
      ok
    }


    "exist for a coequalizer" in {
      val topos = new CategoryOfDiagrams(ParallelPair)
      val a: set = Set(1, 2, 3, 4)
      val b: set = Set(0, 1, 2)
      val f = fun(a,b)("f", x => Math.min(2, x.toInt))
      val g = fun(a,b)("g", x => x.toInt % 3)
      val sut: Diagram = Diagram.tryBuild(topos)(
        "coEq",
        Map("0" -> a, "1" -> b),
        Map("a" -> f, "b" -> g)
      ).iHope
      val colimit = sut.colimit.iHope
      
      val element = Set((0, 0), (0, 1), (0, 2))

      val vertex = sut.setOf(colimit.vertex)
      val ar0 = colimit.arrowFrom("0")
      val ar1 = colimit.arrowFrom("1")
      a.foreach(ar0(_) === element)
      b.foreach(ar1(_) === element)
      
      ok
    }

    "exist for a monoid Z3" in {
      val sut = SampleZ3Diagram
      sut.colimit match
        case Good(sut.Cocone(vertex, arrowFrom)) =>
          vertex === Set(Set((0, 2220), (0, 2221), (0, 2222)), Set((0, 2223)))
        case x => failure(s"We expected a colimit, got $x")
      ok
    }

    "exist for M, regular data" in {
      val sut = SampleMDiagram
      val colimit = sut.colimit.iHope
      val vertex = sut.setOf(colimit.vertex)
      vertex.size === 8
      val ara = colimit.arrowFrom("a")
      val arb = colimit.arrowFrom("b")
      val arc = colimit.arrowFrom("c")
      val ard = colimit.arrowFrom("d")
      val are = colimit.arrowFrom("e")

      for
        i <- SampleMDiagramContent.a
        j <- SampleMDiagramContent.c
        k <- SampleMDiagramContent.e
      do
        val element = i :: j :: k :: Nil
        if vertex(element) then
          (i, j, k, ara(element)) === (i, j, k, i)
          (i, j, k, arc(element)) === (i, j, k, j)
          (i, j, k, are(element)) === (i, j, k, k)
          (i, j, k, arb(element)) === (i, j, k, SampleMDiagramContent.ba(i))
          (i, j, k, arb(element)) === (i, j, k, SampleMDiagramContent.bc(j))
          (i, j, k, ard(element)) === (i, j, k, SampleMDiagramContent.dc(j))
          (i, j, k, ard(element)) === (i, j, k, SampleMDiagramContent.de(k))

      ok
    }

  }
