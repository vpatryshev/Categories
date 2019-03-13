package math.cat

import math.Test
import math.cat.SetCategory.Setf
import math.sets.Sets
import math.sets.Sets.set

/**
  * Test for set diagrams (functors with codomain=sets)
  */
class SetDiagramTest extends Test with TestDiagrams {
  type SUT = SmallDiagram
  
  "SetDiagram" should {

    "validate as a functor with Set as domain" in {

      check[Functor[Category[String, String], SetCategory]](SamplePullbackDiagram.asFunctor,
        sut => {
          sut.d0 === Category.Pullback
          sut.d1 === Setf
        }
      )
    }

    "test build" in {
      val dom = Category.Pullback
      val sut1 = SamplePullbackDiagram.asFunctor.
        getOrElse(throw new InstantiationException("alas..."))
      sut1.objectsMapping("b") === SamplePullbackDiagram.sb

      val diagram: SetDiagram[Category[String, String]] =
        new SetDiagram[Category[String, String]]("Test", dom) {
          override val objectsMapping: d0.O => d1.O =
            (x: d0.O) => SamplePullbackDiagram.om(x)
          override val arrowsMappingCandidate: d0.Arrow => YArrow =
            (a: d0.Arrow) => SamplePullbackDiagram.am(a)
        }
      val res = Functor.validate(diagram)
      res.isGood must beTrue
    }

    "get validated - positive" in {
      expect(sut => {
        sut.d0 === Category.Pullback
        sut.d1 === Setf
      })(SamplePullbackDiagram.asDiagram)
    }

    "get validated - negative" in {
      val a: set = Set(1, 2, 3, 4, 5)
      val b: set = Set(0, 1, 2)
      val f = SetFunction("f", a, b, x => Math.min(2, x.toString.toInt))
      val g = SetFunction("g", b, b, x => x.toString.toInt % 3)
      checkError("Inconsistent mapping for d0(b) - Set(0, 1, 2) vs Set(5, 1, 2, 3, 4)" ==,
        SetDiagram.build(
          "ParallelPair", Category.ParallelPair)(
          Map("0" -> a, "1" -> b),
          Map("a" -> f, "b" -> g)
        )
      )
    }
    "validate empty diagram" in {
      EmptyDiagram.d0 === Category._0_
      EmptyDiagram.d1 === Setf
    }
  }

  "SetDiagram limit" should {
    "exist for an empty diagram" in {
      check[SmallDiagram](point(Set("a", "b")),
        sut => {
          sut.d0 === Category._1_
          sut.d1 === Setf
          sut.objectsMapping("0") === Set("a", "b")
        }
      )
    }

    "exist for a point" in {
      val x: set = Set("x", "y", "z")
      check[SmallDiagram](point(x),
        sut => {
          sut.d0 === Category._1_
          sut.d1 === Setf
          sut.limit match {
            case None => failure("We expected a limit")
            case Some(sut.Cone(vertex, arrowTo)) =>
              vertex.size === x.size
          }
        }
      )
    }

    "exist for a pullback" in {
      expect(sut =>
        sut.limit match {
          case None => failure("We expected a limit")
          case Some(sut.Cone(vertex, arrowTo)) =>
            vertex.size === 5
            val ara = arrowTo("a")
            val arb = arrowTo("b")

            for {
              i <- 1 to 3
              j <- 2 to 4
            } {
              val element = i :: j :: Nil
              (i, j, vertex(element)) === (i, j, (i + j) % 2 == 1)
              if (vertex(element)) {
                (i, j, ara(element)) === (i, j, i)
                (i, j, arb(element)) === (i, j, j)
              }
            }
        })(SamplePullbackDiagram.asDiagram)
    }

    "exist for an equalizer" in {
      expect(sut =>
        sut.limit match {
          case None => failure("We expected a limit")
          case Some(sut.Cone(vertex, arrowTo)) =>
            vertex.size === 3
            vertex === Set(1 :: Nil, 2 :: Nil, 5 :: Nil)
            val ar0 = arrowTo("0")
            val ar1 = arrowTo("1")

            for {
              element <- vertex
            } {
              val i = (element match {
                case n :: Nil => n;
                case other => Integer.MAX_VALUE
              }).toString.toInt
              (element, ar0(element)) === (element, i)
              (element, ar1(element)) === (element, i % 3)
            }
        })(SampleParallelPairDiagram.asDiagram)
    }

    "exist for a monoid Z3" in {
      expect(sut =>
        sut.limit match {
          case None => failure("We expected a limit")
          case Some(sut.Cone(vertex, arrowTo)) =>
            vertex === Set(List(3))
            arrowTo("0")(List(3)) === 3
        })(SampleZ3Diagram.asDiagram)
    }

    "exist for a W, regular data" in {

      expect(sut =>
        sut.limit match {
          case None => failure("We expected a limit")
          case Some(sut.Cone(vertex, arrowTo)) =>
            vertex.size === 16
            val ara = arrowTo("a")
            val arb = arrowTo("b")
            val arc = arrowTo("c")
            val ard = arrowTo("d")
            val are = arrowTo("e")

            for {
              i <- SampleWDiagram.a
              j <- SampleWDiagram.c
              k <- SampleWDiagram.e
            } {
              val element = i :: j :: k :: Nil
              if (vertex(element)) {
                (i, j, k, ara(element)) === (i, j, k, i)
                (i, j, k, arc(element)) === (i, j, k, j)
                (i, j, k, are(element)) === (i, j, k, k)
                (i, j, k, arb(element)) === (i, j, k, SampleWDiagram.ab(i))
                (i, j, k, arb(element)) === (i, j, k, SampleWDiagram.cb(j))
                (i, j, k, ard(element)) === (i, j, k, SampleWDiagram.cd(j))
                (i, j, k, ard(element)) === (i, j, k, SampleWDiagram.ed(k))
              }
            }
        }
      )(SampleWDiagram.asDiagram)
    }

    "exist for a W, weird data" in {
      val a: set = Set(1, 2, 3)
      val b: set = Set(2, 3, 4)
      val c: set = Set(0, 1, 2, 3, 4, 5)
      val d: set = Set(0, 1, 2)
      val e: set = Set(1, 2, 3, 4)
      val ab = SetFunction("ab", a, b, _.toString.toInt + 1)
      val cb = SetFunction("cb", c, b, x => Math.max(2, Math.min(4, x.toString.toInt)))
      val cd = SetFunction("cd", c, d, _.toString.toInt % 3)
      val ed = SetFunction("ed", e, d, x => (x.toString.toInt + 1) % 2)
      val sutOpt = SetDiagram.build(
        "W", Category.W)(
        Map("a" -> a, "b" -> b, "c" -> c, "d" -> d, "e" -> e),
        Map("ab" -> ab, "cb" -> cb, "cd" -> cd, "ed" -> ed)
      )
      expect(sut =>
        sut.limit match {
          case None => failure("We expected a limit")
          case Some(sut.Cone(vertex, arrowTo)) =>
            vertex.size === 8
            val ara = arrowTo("a")
            val arb = arrowTo("b")
            val arc = arrowTo("c")
            val ard = arrowTo("d")
            val are = arrowTo("e")
            val points = sut.limitBuilder

            for {
              i <- a
              j <- c
              k <- e
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
        })(sutOpt)
    }
  }

  "SetDiagram colimit" should {
    "exist for a empty diagram" in {
      val sut = EmptyDiagram
      sut.d0 === Category._0_
      sut.d1 === Setf
      sut.colimit match {
        case Some(sut.Cocone(Sets.Empty, arrowTo)) => ok
        case x => failure(s"We expected a colimit, got $x")
      }
      ok
    }

    "exist for a point" in {
      val x: set = Set("x", "y", "z")
      check[SmallDiagram](point(x),
        sut => {
          sut.d0 === Category._1_
          sut.d1 === Setf
          sut.colimit match {
            case None => failure("We expected a colimit")
            case Some(sut.Cocone(vertex, arrowFrom)) =>
              vertex.size === x.size
          }
        })
    }

    "exist for a pushout" in {
      val a: set = Set(1, 2, 3)
      val b: set = Set(2, 3, 4)
      val c: set = Set(0, 1)
      val ab = SetFunction("f", a, b, _.toString.toInt + 1)
      val ac = SetFunction("g", a, c, _.toString.toInt % 2)
      val sutOpt = SetDiagram.build(
        "pushout", Category.Pushout)(
        Map("a" -> a, "b" -> b, "c" -> c),
        Map("ab" -> ab, "ac" -> ac)
      )

      val v0 = Set((0, 4), (0, 2), (1, 1))
      val v1 = Set((0, 3), (1, 0))
      val ExpectedVertex: set = Set(v0, v1)
      expect(sut =>
        sut.colimit match {
          case Some(sut.Cocone(ExpectedVertex, arrowFrom)) =>
            val list = v0 :: v1 :: v0 :: Nil
            val ara = arrowFrom("a")
            val arb = arrowFrom("b")
            for {
              i <- 1 to 3
            } {
              ara(i) == list(i - 1)
            }
            for {
              i <- 2 to 4
            } {
              arb(i) == list(i - 2)
            }
          case x => failure(s"We expected a good colimit, got $x")
        }
      )(sutOpt)
    }


    "exist for a coequalizer" in {
      val a: set = Set(1, 2, 3, 4)
      val b: set = Set(0, 1, 2)
      val f = SetFunction("f", a, b, x => Math.min(2, x.toString.toInt))
      val g = SetFunction("g", a, b, x => x.toString.toInt % 3)
      val sutOpt = SetDiagram.build(
        "ParallelPair", Category.ParallelPair)(
        Map("0" -> a, "1" -> b),
        Map("a" -> f, "b" -> g)
      )
      val element = Set((0, 0), (0, 1), (0, 2))
      val Vertex: set = Set(element)

      expect(sut =>
        sut.colimit match {
          case Some(sut.Cocone(Vertex, arrowFrom)) =>
            val ar0 = arrowFrom("0")
            val ar1 = arrowFrom("1")
            a.foreach(ar0(_) === element)
            b.foreach(ar1(_) === element)
          case bad => failure(s"We expected a colimit, got $bad")
        }
      )(sutOpt)
    }

    "exist for a monoid Z3" in {
      expect(sut =>
        sut.colimit match {
          case None => failure("We expected a colimit")
          case Some(sut.Cocone(vertex, arrowFrom)) =>
            vertex === Set(Set((0, 0), (0, 1), (0, 2)), Set((0, 3)))
        }
      )(SampleZ3Diagram.asDiagram)
    }

    "exist for M, regular data" in {

      expect(sut =>
        sut.colimit match {
          case None => failure("We expected a colimit")
          case Some(sut.Cocone(vertex, arrowFrom)) =>
            vertex.size === 8
            val ara = arrowFrom("a")
            val arb = arrowFrom("b")
            val arc = arrowFrom("c")
            val ard = arrowFrom("d")
            val are = arrowFrom("e")

            for {
              i <- SampleMDiagram.a
              j <- SampleMDiagram.c
              k <- SampleMDiagram.e
            } {
              val element = i :: j :: k :: Nil
              if (vertex(element)) {
                (i, j, k, ara(element)) === (i, j, k, i)
                (i, j, k, arc(element)) === (i, j, k, j)
                (i, j, k, are(element)) === (i, j, k, k)
                (i, j, k, arb(element)) === (i, j, k, SampleMDiagram.ba(i))
                (i, j, k, arb(element)) === (i, j, k, SampleMDiagram.bc(j))
                (i, j, k, ard(element)) === (i, j, k, SampleMDiagram.dc(j))
                (i, j, k, ard(element)) === (i, j, k, SampleMDiagram.de(k))
              }
            }
        }
      )(SampleMDiagram.asDiagram)
    }

  }
}
