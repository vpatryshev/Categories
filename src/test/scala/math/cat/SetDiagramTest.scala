package math.cat

import math.cat.SetCategory.Setf
import math.sets.Sets.set
import org.specs2.mutable._

/**
  * Prototype for all tests
  */
class SetDiagramTest extends Specification {

  "SetDiagram" should {
    "get validated - positive" in {
      val a: set = Set(1, 2, 3)
      val b: set = Set(2, 3, 4)
      val c: set = Set(0, 1)
      val ac = SetFunction("f", a, c, _.toString.toInt % 2)
      val bc = SetFunction("g", b, c, x => (x.toString.toInt + 1) % 2)
      val sut = SetDiagram(
        "pullback", Category.Pullback,
        Map("a" -> a, "b" -> b, "c" -> c),
        Map("ac" -> ac, "bc" -> bc)
      )
      sut.domain === Category.Pullback
      sut.codomain === Setf
    }

    "get validated - negative" in {
      val a: set = Set(1, 2, 3, 4, 5)
      val b: set = Set(0, 1, 2)
      val f = SetFunction("f", a, b, x => Math.min(2, x.toString.toInt))
      val g = SetFunction("g", b, b, x => x.toString.toInt % 3)
      SetDiagram(
        "ParallelPair", Category.ParallelPair,
        Map("0" -> a, "1" -> b),
        Map("a" -> f, "b" -> g)
      ) must throwA(new IllegalArgumentException(
        "requirement failed: Inconsistent mapping for d0(b)"))
      ok
    }
    
    "have a limit of empty diagram" in {
      val sut = SetDiagram(
        "terminal", Category._0_,
        Map(),
        Map()
      )
      sut.domain === Category._0_
      sut.codomain === Setf
      sut.limit match {
        case None => failure("We expected a limit")
        case Some(sut.Cone(vertex, arrowTo)) =>
          vertex.size === 1
      }
      ok
    }

    "have a limit of a point" in {
      val x: set = Set("x", "y", "z")
      val sut = SetDiagram(
        "terminal", Category._1_,
        Map(0 -> x),
        Map()
      )
      sut.domain === Category._1_
      sut.codomain === Setf
      sut.limit match {
        case None => failure("We expected a limit")
        case Some(sut.Cone(vertex, arrowTo)) =>
          vertex.size === x.size
      }
      ok
    }
    
    "have a limit that is a pullback" in {
      val a: set = Set(1, 2, 3)
      val b: set = Set(2, 3, 4)
      val c: set = Set(0, 1)
      val ac = SetFunction("f", a, c, _.toString.toInt % 2)
      val bc = SetFunction("g", b, c, x => (x.toString.toInt + 1) % 2)
      val sut = SetDiagram(
        "pullback", Category.Pullback,
        Map("a" -> a, "b" -> b, "c" -> c),
        Map("ac" -> ac, "bc" -> bc)
      )
      sut.limit match {
        case None => failure("We expected a limit")
        case Some(sut.Cone(vertex, arrowTo)) =>
          vertex.size === 5
          val ara = arrowTo("a")
          val arb = arrowTo("b")

          for {i <- 1 to 3; j <- 2 to 4} {
            val element = i :: j :: Nil
            (i, j, vertex(element)) === (i, j, (i + j) % 2 == 1)
            if (vertex(element)) {
              (i, j, ara(element)) === (i, j, i)
              (i, j, arb(element)) === (i, j, j)
            }
          }
      }
      ok
    }

    "have a limit that is an equalizer" in {
      val a: set = Set(1, 2, 3, 4, 5)
      val b: set = Set(0, 1, 2)
      val f = SetFunction("f", a, b, x => Math.min(2, x.toString.toInt))
      val g = SetFunction("g", a, b, x => x.toString.toInt % 3)
      val sut = SetDiagram(
        "ParallelPair", Category.ParallelPair,
        Map("0" -> a, "1" -> b),
        Map("a" -> f, "b" -> g)
      )
      sut.limit match {
        case None => failure("We expected a limit")
        case Some(sut.Cone(vertex, arrowTo)) =>
          vertex.size === 3
          vertex === Set(1 :: Nil, 2 :: Nil, 5 :: Nil)
          val ar0 = arrowTo("0")
          val ar1 = arrowTo("1")

          for {element <- vertex} {
            val i = (element match {
              case n :: Nil => n;
              case other => Integer.MAX_VALUE
            }).toString.toInt
            (element, ar0(element)) === (element, i)
            (element, ar1(element)) === (element, i % 3)
          }
      }
      ok
    }

    "have a limit of a monoid Z3" in {
      val a: set = Set(0, 1, 2)
      val f1 = SetFunction("f1", a, a, x => (x.toString.toInt + 1) % 3)
      val f2 = SetFunction("f2", a, a, x => (x.toString.toInt + 2) % 3)
      val sut = SetDiagram(
        "ParallelPair", Category.Z3,
        Map("0" -> a),
        Map("1" -> f1, "2" -> f2)
      )
      sut.limit match {
        case None => failure("We expected a limit")
        case Some(sut.Cone(vertex, arrowTo)) =>
          vertex.size === 0 // yes, the limit has to be empty
      }
      ok
    }

    "have a limit of W, regular data" in {
      val a: set = Set("a00", "a01", "a10", "a11")
      val b: set = Set("0", "1")
      val c: set = Set("c00", "c01", "c10", "c11")
      val d: set = Set("0", "1")
      val e: set = Set("e00", "e01", "e10", "e11")
      val ab = SetFunction("ab", a, b, _.toString.substring(2))
      val cb = SetFunction("cb", c, b, _.toString.substring(2))
      val cd = SetFunction("cd", c, d, _.toString.substring(1, 2))
      val ed = SetFunction("ed", e, d, _.toString.substring(1, 2))
      val sut = SetDiagram(
        "W", Category.W,
        Map("a" -> a, "b" -> b, "c" -> c, "d" -> d, "e" -> e),
        Map("ab" -> ab, "cb" -> cb, "cd" -> cd, "ed" -> ed)
      )
      sut.limit match {
        case None => failure("We expected a limit")
        case Some(sut.Cone(vertex, arrowTo)) =>
          vertex.size === 16
          val ara = arrowTo("a")
          val arb = arrowTo("b")
          val arc = arrowTo("c")
          val ard = arrowTo("d")
          val are = arrowTo("e")

          for {i <- a; j <- c; k <- e} {
            val element = i :: j :: k :: Nil
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
      }
      ok
    }

    "have a limit of W, weird data" in {
      val a: set = Set(1, 2, 3)
      val b: set = Set(2, 3, 4)
      val c: set = Set(0, 1, 2, 3, 4, 5)
      val d: set = Set(0, 1, 2)
      val e: set = Set(1, 2, 3, 4)
      val ab = SetFunction("ab", a, b, _.toString.toInt + 1)
      val cb = SetFunction("cb", c, b, x => Math.max(2, Math.min(4, x.toString.toInt)))
      val cd = SetFunction("cd", c, d, _.toString.toInt % 3)
      val ed = SetFunction("ed", e, d, x => (x.toString.toInt + 1) % 2)
      val sut = SetDiagram(
        "W", Category.W,
        Map("a" -> a, "b" -> b, "c" -> c, "d" -> d, "e" -> e),
        Map("ab" -> ab, "cb" -> cb, "cd" -> cd, "ed" -> ed)
      )

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

          for {i <- a; j <- c; k <- e} {
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
      }
      ok
    }
    
    
  }

}
