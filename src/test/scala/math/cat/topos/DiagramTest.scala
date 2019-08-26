package math.cat.topos

import math.Test
import math.cat.SetCategory.Setf
import math.cat.{Category, Functor, SetFunction}
import math.sets.Sets
import math.sets.Sets.set
import scalakittens.{Good, Result}
import math.cat.Categories._

/**
  * Test for individual diagrams (functors with codomain=sets)
  */
class DiagramTest extends Test with TestDiagrams {
  type SUT = Diagram

  "Diagram" should {

    "validate as a functor with Set as domain" in {

      checkOpt[Functor](BuildPullbackDiagram.asFunctor,
        sut ⇒ {
          sut.d0 === Pullback
          sut.d1 === Setf
        }
      )
    }

    "test build" in {
      val dom = Pullback
      val topos = new CategoryOfDiagrams(dom)
      val sut1 = BuildPullbackDiagram.asFunctor.iHope
      sut1.objectsMapping(sut1.d0.obj("b")) === BuildPullbackDiagram.sb

      val diagram: Diagram =
        new Diagram("Test", topos) {
          override val objectsMapping: d0.Obj ⇒ d1.Obj =
            (x: d0.Obj) ⇒ d1.obj(BuildPullbackDiagram.om(x.toString))
          override val arrowsMappingCandidate: d0.Arrow ⇒ d1.Arrow =
            (a: d0.Arrow) ⇒ d1.arrow(BuildPullbackDiagram.am(a.toString))
        }
      val res = Functor.validateFunctor(diagram)
      res.isGood must beTrue
    }

    "get validated - positive" in {
      val sut = SamplePullbackDiagram
      sut.d0 === Pullback
      sut.d1 === Setf
    }

    "get validated - negative" in {
      val a: set = Set(1, 2, 3, 4, 5)
      val b: set = Set(0, 1, 2)
      val f = SetFunction.build("f", a, b, x ⇒ Math.min(2, x.toString.toInt)).iHope
      val g = SetFunction.build("g", b, b, x ⇒ x.toString.toInt % 3).iHope
      val topos = new CategoryOfDiagrams(ParallelPair)
      checkError("Inconsistent mapping for d0(b) - Set(0, 1, 2) vs Set(5, 1, 2, 3, 4)" ==,
        Diagram.build(
          "Bad Bad Bad", topos)(
          Map("0" → a, "1" → b),
          Map("a" → f, "b" → g)
        )
      )
    }

    "validate empty diagram" in {
      EmptyDiagram.d0 === _0_
      EmptyDiagram.d1 === Setf
    }
  }

  "Diagram limit" should {
    "exist for an empty diagram" in {
      check[Diagram](const(Set("a", "b")),
        sut ⇒ {
          sut.d0 === _1_
          sut.d1 === Setf
          sut.objectsMapping(sut.d0.obj("0")) === Set("a", "b")
        }
      )
    }

    "exist for a point" in {
      val x: set = Set("x", "y", "z")
      check[Diagram](const(x),
        sut ⇒ {
          sut.d0 === _1_
          sut.d1 === Setf
          sut.limit match {
            case Good(sut.Cone(vertex, arrowTo)) ⇒
              sut.asSet(sut.d1.obj(vertex)).size === x.size
            case none ⇒ failure(s"We expected a limit, got $none")
          }
        }
      )
    }

    "exist for a pullback" in {
      val sut = SamplePullbackDiagram
      sut.limit match {
        case Good(cone) ⇒
          val vertex = sut.asSet(cone.vertex)
          vertex.size === 5
          val ara = sut.asFunction(sut.d1.arrow(cone.arrowTo(sut.d0.obj("a"))))
          val arb = sut.asFunction(sut.d1.arrow(cone.arrowTo(sut.d0.obj("b"))))

          for {
            i ← 1 to 3
            j ← 2 to 4
          } {
            val element = i :: j :: Nil
            (i, j, vertex(element)) === (i, j, (i + j) % 2 == 1)
            if (vertex(element)) {
              (i, j, ara(element)) === (i, j, i)
              (i, j, arb(element)) === (i, j, j)
            }
          }
        case none ⇒ failure(s"We expected a limit, got $none")
      }
      ok
    }

    "exist for an equalizer" in {
      val sut = SampleParallelPairDiagram1
      sut.limit match {
        case Good(cone) ⇒
          val vertex = sut.asSet(cone.vertex)
          vertex.size === 3
          vertex === Set(1 :: Nil, 2 :: Nil, 5 :: Nil)
          val ar0 = sut.asFunction(sut.d1.arrow(cone.arrowTo(sut.d0.obj("0"))))
          val ar1 = sut.asFunction(sut.d1.arrow(cone.arrowTo(sut.d0.obj("1"))))

          for {
            element ← vertex
          } {
            val i = (element match {
              case n :: Nil ⇒ n;
              case other ⇒ Integer.MAX_VALUE
            }).toString.toInt
            (element, ar0(element)) === (element, i)
            (element, ar1(element)) === (element, i % 3)
          }
        case none ⇒ failure(s"We expected a limit, got $none")
      }

      "exist for a monoid Z3" in {
        val sut = SampleZ3Diagram
        sut.limit match {
          case Good(cone) ⇒
            cone.vertex === Set(List(3))
            val ar0 = sut.asFunction(sut.d1.arrow(cone.arrowTo(sut.d0.obj("0"))))
            ar0(List(3)) === 3
          case none ⇒ failure(s"We expected a limit, got $none")
        }
        ok
      }

      "exist for a W, regular data" in {
        val sut = SampleWDiagram
        sut.limit match {
          case Good(cone) ⇒
            val vertex = sut.asSet(cone.vertex)
            vertex.size === 16
            val ara = sut.asFunction(sut.d1.arrow(cone.arrowTo(sut.d0.obj("a"))))
            val arb = sut.asFunction(sut.d1.arrow(cone.arrowTo(sut.d0.obj("b"))))
            val arc = sut.asFunction(sut.d1.arrow(cone.arrowTo(sut.d0.obj("c"))))
            val ard = sut.asFunction(sut.d1.arrow(cone.arrowTo(sut.d0.obj("d"))))
            val are = sut.asFunction(sut.d1.arrow(cone.arrowTo(sut.d0.obj("e"))))

            for {
              i ← SampleWDiagramContent.a
              j ← SampleWDiagramContent.c
              k ← SampleWDiagramContent.e
            } {
              val element = i :: j :: k :: Nil
              if (vertex(element)) {
                (i, j, k, ara(element)) === (i, j, k, i)
                (i, j, k, arc(element)) === (i, j, k, j)
                (i, j, k, are(element)) === (i, j, k, k)
                (i, j, k, arb(element)) === (i, j, k, SampleWDiagramContent.ab(i))
                (i, j, k, arb(element)) === (i, j, k, SampleWDiagramContent.cb(j))
                (i, j, k, ard(element)) === (i, j, k, SampleWDiagramContent.cd(j))
                (i, j, k, ard(element)) === (i, j, k, SampleWDiagramContent.ed(k))
              }
            }
          case none ⇒ failure(s"We expected a limit, got $none")
        }
        ok
      }

      "exist for a W, weird data" in {
        val topos = new CategoryOfDiagrams(W)
        val a: set = Set(1, 2, 3)
        val b: set = Set(2, 3, 4)
        val c: set = Set(0, 1, 2, 3, 4, 5)
        val d: set = Set(0, 1, 2)
        val e: set = Set(1, 2, 3, 4)
        val ab = SetFunction.build("ab", a, b, _.toString.toInt + 1).iHope
        val cb = SetFunction.build("cb", c, b, x ⇒ Math.max(2, Math.min(4, x.toString.toInt))).iHope
        val cd = SetFunction.build("cd", c, d, _.toString.toInt % 3).iHope
        val ed = SetFunction.build("ed", e, d, x ⇒ (x.toString.toInt + 1) % 2).iHope
        val sutOpt = Diagram.build(
          "W", topos)(
          Map("a" → a, "b" → b, "c" → c, "d" → d, "e" → e),
          Map("ab" → ab, "cb" → cb, "cd" → cd, "ed" → ed)
        )

        expect(sut ⇒
          sut.limit match {
            case Good(cone) ⇒
              val vertex = sut.asSet(cone.vertex)
              vertex.size === 8
              val ara = sut.asFunction(sut.d1.arrow(cone.arrowTo(sut.d0.obj("a"))))
              val arb = sut.asFunction(sut.d1.arrow(cone.arrowTo(sut.d0.obj("b"))))
              val arc = sut.asFunction(sut.d1.arrow(cone.arrowTo(sut.d0.obj("c"))))
              val ard = sut.asFunction(sut.d1.arrow(cone.arrowTo(sut.d0.obj("d"))))
              val are = sut.asFunction(sut.d1.arrow(cone.arrowTo(sut.d0.obj("e"))))
              val points = sut.limitBuilder

              for {
                i ← a
                j ← c
                k ← e
              } {
                val element = i :: j :: k :: Nil
                val eq1 = ab(i) == cb(j)
                val eq2 = cd(j) == ed(k)
                (i, j, k, eq1, eq2, points.isPoint(element)) === (i, j, k, eq1, eq2, eq1 && eq2)

                (i, j, k, eq1, eq2, vertex(element)) === (i, j, k, eq1, eq2, eq1 && eq2)
                if (vertex(element)) {
                  (i, j, k, ara(element)) === (i, j, k, i)
                  (i, j, k, arc(element)) === (i, j, k, j)
                  (i, j, k, are(element)) === (i, j, k, k)
                  (i, j, k, arb(element)) === (i, j, k, ab(i))
                  (i, j, k, arb(element)) === (i, j, k, cb(j))
                  (i, j, k, ard(element)) === (i, j, k, cd(j))
                  (i, j, k, ard(element)) === (i, j, k, ed(k))
                }
              }
            case none ⇒ failure(s"We expected a limit, got $none")
          })(sutOpt)
      }
    }
  }

  "Diagram colimit" should {
    "exist for a empty diagram" in {
      val sut = EmptyDiagram
      sut.d0 === _0_
      sut.d1 === Setf
      sut.colimit match {
        case Good(sut.Cocone(Sets.Empty, arrowFrom)) ⇒ ok
        case none ⇒ failure(s"no colimit? $none")
      }
      ok
    }

    "exist for a point" in {
      val expected: set = Set("x", "y", "z")
      check[Diagram](const(expected),
        sut ⇒ {
          sut.d0 === _1_
          sut.d1 === Setf
          sut.colimit match {
            case Good(cocone) ⇒
              val vertex = sut.asSet(cocone.vertex)
              vertex.size === expected.size
            case x ⇒ failure(s"We expected a colimit, got $x")
          }
        })
    }

    "exist for a pushout" in {
      val topos = new CategoryOfDiagrams(Pushout)
      val a: set = Set(1, 2, 3)
      val b: set = Set(2, 3, 4)
      val c: set = Set(0, 1)
      val ab = SetFunction.build("f", a, b, _.toString.toInt + 1).iHope
      val ac = SetFunction.build("g", a, c, _.toString.toInt % 2).iHope
      val sutOpt: Result[SUT] = Diagram.build(
        "pushout", topos)(
        Map("a" → a, "b" → b, "c" → c),
        Map("ab" → ab, "ac" → ac)
      )

      val v0 = Set((0, 4), (0, 2), (1, 1))
      val v1 = Set((0, 3), (1, 0))
      val ExpectedVertex: set = Set(v0, v1)
      val list = v0 :: v1 :: v0 :: Nil
      expect(sut ⇒
        sut.colimit match {
          case Good(cocone) ⇒
            val vertex = sut.asSet(cocone.vertex)
            val ara = sut.asFunction(sut.d1.arrow(cocone.arrowFrom(sut.d0.obj("a"))))
            val arb = sut.asFunction(sut.d1.arrow(cocone.arrowFrom(sut.d0.obj("b"))))
            for {
              i ← 1 to 3
            } {
              ara(i) == list(i - 1)
            }
            for {
              i ← 2 to 4
            } {
              arb(i) == list(i - 2)
            }
          case x ⇒ failure(s"We expected a good colimit, got $x")
        }
      )(sutOpt)
    }


    "exist for a coequalizer" in {
      val topos = new CategoryOfDiagrams(ParallelPair)
      val a: set = Set(1, 2, 3, 4)
      val b: set = Set(0, 1, 2)
      val f = SetFunction.build("f", a, b, x ⇒ Math.min(2, x.toString.toInt)).iHope
      val g = SetFunction.build("g", a, b, x ⇒ x.toString.toInt % 3).iHope
      val sutOpt: Result[Diagram] = Diagram.build(
        "coEq", topos)(
        Map("0" → a, "1" → b),
        Map("a" → f, "b" → g)
      )
      val element = Set((0, 0), (0, 1), (0, 2))
      val Vertex: set = Set(element)

      expect(sut ⇒
        sut.colimit match {
          case Good(cocone) ⇒
            val vertex = sut.asSet(cocone.vertex)
            val ar0 = sut.asFunction(sut.d1.arrow(cocone.arrowFrom(sut.d0.obj("0"))))
            val ar1 = sut.asFunction(sut.d1.arrow(cocone.arrowFrom(sut.d0.obj("1"))))
            a.foreach(ar0(_) === element)
            b.foreach(ar1(_) === element)
          case bad ⇒ failure(s"We expected a colimit, got $bad")
        }
      )(sutOpt)
    }

    "exist for a monoid Z3" in {
      val sut = SampleZ3Diagram
      sut.colimit match {
        case Good(sut.Cocone(vertex, arrowFrom)) ⇒
          vertex === Set(Set((0, 0), (0, 1), (0, 2)), Set((0, 3)))
        case x ⇒ failure(s"We expected a colimit, got $x")
      }
      ok
    }

    "exist for M, regular data" in {
      val sut = SampleMDiagram
      sut.colimit match {
        case Good(cocone) ⇒
          val vertex = sut.asSet(cocone.vertex)
          vertex.size === 8
          val ara = sut.asFunction(sut.d1.arrow(cocone.arrowFrom(sut.d0.obj("a"))))
          val arb = sut.asFunction(sut.d1.arrow(cocone.arrowFrom(sut.d0.obj("b"))))
          val arc = sut.asFunction(sut.d1.arrow(cocone.arrowFrom(sut.d0.obj("c"))))
          val ard = sut.asFunction(sut.d1.arrow(cocone.arrowFrom(sut.d0.obj("d"))))
          val are = sut.asFunction(sut.d1.arrow(cocone.arrowFrom(sut.d0.obj("e"))))

          for {
            i ← SampleMDiagramContent.a
            j ← SampleMDiagramContent.c
            k ← SampleMDiagramContent.e
          } {
            val element = i :: j :: k :: Nil
            if (vertex(element)) {
              (i, j, k, ara(element)) === (i, j, k, i)
              (i, j, k, arc(element)) === (i, j, k, j)
              (i, j, k, are(element)) === (i, j, k, k)
              (i, j, k, arb(element)) === (i, j, k, SampleMDiagramContent.ba(i))
              (i, j, k, arb(element)) === (i, j, k, SampleMDiagramContent.bc(j))
              (i, j, k, ard(element)) === (i, j, k, SampleMDiagramContent.dc(j))
              (i, j, k, ard(element)) === (i, j, k, SampleMDiagramContent.de(k))
            }
          }
        case x ⇒ failure(s"We expected a colimit, got $x")
      }
      ok
    }

  }

}
