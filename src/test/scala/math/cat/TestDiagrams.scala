package math.cat

import math.Test
import math.cat.Category.Cat
import math.sets.Sets.set
import scalakittens.Result

trait TestDiagrams extends Test {
  type SmallDiagram = SetDiagram[Cat]
  
  lazy val EmptyDiagram: SmallDiagram = SetDiagram.build("empty", Category._0_)(
    Map[String, set](),
    Map[String, SetFunction]()
  ).iHope

  def point(x: set): Result[SmallDiagram] =
    SetDiagram.build(s"point $x", Category._1_)(
      Map[String, set]("0" -> x),
      Map[String, SetFunction]()
    )

  object SamplePullbackDiagram {
    val sa: set = Set(1, 2, 3)
    val sb: set = Set(2, 3, 4)
    val sc: set = Set(0, 1)
    val ac = SetFunction("f", sa, sc, _.toString.toInt % 2)
    val bc = SetFunction("g", sb, sc, x => (x.toString.toInt + 1) % 2)

    val om = Map("a" -> sa, "b" -> sb, "c" -> sc)
    val am = Map("ac" -> ac, "bc" -> bc)

    lazy val asFunctor: Result[Functor[Cat, SetCategory]] = Functor.build[Cat, SetCategory](
      "pullback", Category.Pullback, SetCategory.Setf)(
      om,
      am
    )
    lazy val asDiagram: Result[SmallDiagram] = SetDiagram.build(
      "pullback", Category.Pullback)(
      om,
      am
    )
  }

  object SamplePushoutDiagram {
    
  }
  
  object SampleParallelPairDiagram {
    val a: set = Set(1, 2, 3, 4, 5)
    val b: set = Set(0, 1, 2)
    val f = SetFunction("f", a, b, x => Math.min(2, x.toString.toInt))
    val g = SetFunction("g", a, b, x => x.toString.toInt % 3)
    lazy val asDiagram: Result[SmallDiagram] = SetDiagram.build(
      "ParallelPair", Category.ParallelPair)(
      Map("0" -> a, "1" -> b),
      Map("a" -> f, "b" -> g)
    )
  }

  object SampleZ3Diagram {
    val a: set = Set(0, 1, 2, 3)

    def f(i: Int): Int => Int = (n: Int) => if (n == 3) n else (n + i) % 3

    val f1 = SetFunction("f1", a, a, x => f(1)(x.toString.toInt))
    val f2 = SetFunction("f2", a, a, x => f(2)(x.toString.toInt))
    val asDiagram: Result[SmallDiagram] = SetDiagram.build(
      "Z3", Category.Z3)(
      Map("0" -> a),
      Map("1" -> f1, "2" -> f2)
    )
  }
  
  object SampleWDiagram {
    val a: set = Set("a00", "a01", "a10", "a11")
    val b: set = Set("0", "1")
    val c: set = Set("c00", "c01", "c10", "c11")
    val d: set = Set("0", "1")
    val e: set = Set("e00", "e01", "e10", "e11")
    val ab = SetFunction("ab", a, b, _.toString.substring(2))
    val cb = SetFunction("cb", c, b, _.toString.substring(2))
    val cd = SetFunction("cd", c, d, _.toString.substring(1, 2))
    val ed = SetFunction("ed", e, d, _.toString.substring(1, 2))
    lazy val asDiagram: Result[SmallDiagram] = SetDiagram.build(
      "W", Category.W)(
      Map("a" -> a, "b" -> b, "c" -> c, "d" -> d, "e" -> e),
      Map("ab" -> ab, "cb" -> cb, "cd" -> cd, "ed" -> ed)
    )
  }
  
  object SampleMDiagram {
    val a: set = Set("a00", "a01", "a10", "a11")
    val b: set = Set("0", "1")
    val c: set = Set("c00", "c01", "c10", "c11")
    val d: set = Set("0", "1")
    val e: set = Set("e00", "e01", "e10", "e11")
    val ba = SetFunction("ba", b, a, "a0" +)
    val bc = SetFunction("bc", b, c, "c0" +)
    val dc = SetFunction("dc", d, c, "c1" +)
    val de = SetFunction("de", d, e, "e1" +)
    val asDiagram: Result[SmallDiagram] = SetDiagram.build(
      "M", Category.M)(
      Map("a" -> a, "b" -> b, "c" -> c, "d" -> d, "e" -> e),
      Map("ba" -> ba, "bc" -> bc, "dc" -> dc, "de" -> de)
    )
  }
}
