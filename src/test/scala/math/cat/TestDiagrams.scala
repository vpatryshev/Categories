package math.cat

import math.Base._
import math.Test
import math.sets.Sets.set
import scalakittens.Result

trait TestDiagrams extends Test {
  type SmallDiagram = SetDiagram

  implicit def translateObjectMapping(f: Functor)(om: String => set): f.d0.Obj => f.d1.Obj =
    (x: f.d0.Obj) => f.d1.obj(om(f.toString))

  implicit def translateArrowMapping(f: Functor)(am: String => SetFunction): f.d0.Obj => f.d1.Obj =
    (x: f.d0.Obj) => f.d1.obj(am(f.toString))

  lazy val EmptyDiagram: SmallDiagram = SetDiagram.build("empty", Category._0_)(
    Map[String, set](),
    Map[String, SetFunction]()
  ).iHope
  val SamplePullbackDiagram: SmallDiagram = BuildPullbackDiagram.asDiagram iHope
  val SamplePushoutDiagram: SmallDiagram = {
    null
    // TODO: implement
  }
  val SampleParallelPairDiagram: SmallDiagram = {
    val a: set = Set(1, 2, 3, 4, 5)
    val b: set = Set(0, 1, 2)
    val f = SetFunction.build("f", a, b, x => Math.min(2, x.toString.toInt)).iHope
    val g = SetFunction.build("g", a, b, x => x.toString.toInt % 3).iHope
    SetDiagram.build(
      "ParallelPair", Category.ParallelPair)(
      Map("0" -> a, "1" -> b),
      Map("a" -> f, "b" -> g)
    ) iHope
  }
  val SampleZ3Diagram: SmallDiagram = {
    val a: set = Set(0, 1, 2, 3)

    def f(i: Int): Int => Int = (n: Int) => if (n == 3) n else (n + i) % 3

    val f1 = SetFunction.build("f1", a, a, x => f(1)(x.toString.toInt)).iHope
    val f2 = SetFunction.build("f2", a, a, x => f(2)(x.toString.toInt)).iHope
    SetDiagram.build(
      "Z3", Category.Z3)(
      Map("0" -> a),
      Map("1" -> f1, "2" -> f2)
    ) iHope
  }

  def const(x: set): Result[SmallDiagram] =
    SetDiagram.build(s"point $x", Category._1_)(
      Map[String, set]("0" -> x),
      Map[String, SetFunction]()
    )

  object BuildPullbackDiagram {
    val sa: set = Set(1, 2, 3)
    val sb: set = Set(2, 3, 4)
    val sc: set = Set(0, 1)
    val ac = SetFunction.build("_ac", sa, sc, _.toString.toInt % 2).iHope
    val bc = SetFunction.build("_bc", sb, sc, x => (x.toString.toInt + 1) % 2).iHope
    val om = Map("a" -> sa, "b" -> sb, "c" -> sc)
    val am = Map("ac" -> ac, "bc" -> bc)
    
    lazy val asFunctor: Result[Functor] = Functor.apply(
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

  object SampleWDiagramContent {
    val a: set = Set("a00", "a01", "a10", "a11")
    val b: set = Set("0", "1")
    val c: set = Set("c00", "c01", "c10", "c11")
    val d: set = Set("0", "1")
    val e: set = Set("e00", "e01", "e10", "e11")
    val ab = SetFunction.build("ab", a, b, _.toString.substring(2)).iHope
    val cb = SetFunction.build("cb", c, b, _.toString.substring(2)).iHope
    val cd = SetFunction.build("cd", c, d, _.toString.substring(1, 2)).iHope
    val ed = SetFunction.build("ed", e, d, _.toString.substring(1, 2)).iHope
  }
  
  val SampleWDiagram: SmallDiagram = {
    import SampleWDiagramContent._
    SetDiagram.build(
      "W", Category.W)(
      Map("a" -> a, "b" -> b, "c" -> c, "d" -> d, "e" -> e),
      Map("ab" -> ab, "cb" -> cb, "cd" -> cd, "ed" -> ed)
    ) iHope
  }

  object SampleMDiagramContent {
    val a: set = Set("a00", "a01", "a10", "a11")
    val b: set = Set("0", "1")
    val c: set = Set("c00", "c01", "c10", "c11")
    val d: set = Set("0", "1")
    val e: set = Set("e00", "e01", "e10", "e11")
    val ba = SetFunction.build("ba", b, a, "a0" +).iHope
    val bc = SetFunction.build("bc", b, c, "c0" +).iHope
    val dc = SetFunction.build("dc", d, c, "c1" +).iHope
    val de = SetFunction.build("de", d, e, "e1" +).iHope
  }

  val SampleMDiagram: SmallDiagram = {
    import SampleMDiagramContent._
    SetDiagram.build(
      "M", Category.M)(
      Map("a" -> a, "b" -> b, "c" -> c, "d" -> d, "e" -> e),
      Map("ba" -> ba, "bc" -> bc, "dc" -> dc, "de" -> de)
    ) iHope
  }
}
