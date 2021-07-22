package math.cat.topos

import math.Test
import math.cat.Categories._
import math.cat.{Category, Functor, SetCategory, SetFunction}
import math.sets.Sets.set
import scalakittens.Result

import scala.collection.mutable
import scala.language.{implicitConversions, postfixOps}
import SetFunction._

trait TestDiagrams extends Test:
  val toposes: mutable.Map[String, GrothendieckTopos] = mutable.Map[String, GrothendieckTopos]()
  
  private def toposOver(domain: Category): GrothendieckTopos =
    toposes.getOrElseUpdate(domain.name, new CategoryOfDiagrams(domain))

  def build(name: String, domain: Category)(
    objectsMap: String => set,
    arrowMap: String => SetFunction): Diagram =
    val topos = toposOver(domain)
    def om(o: topos.domain.Obj): set = objectsMap(o.toString)
    def am(o: topos.domain.Arrow): SetFunction = arrowMap(o.toString)
    Diagram.tryBuild(topos)(name, om, am) iHope

  implicit def translateObjectMapping(f: Functor)(om: String => set): f.d0.Obj => f.d1.Obj =
    (x: f.d0.Obj) => f.d1.obj(om(f.toString))

  implicit def translateArrowMapping(f: Functor)(am: String => SetFunction): f.d0.Obj => f.d1.Obj =
    (x: f.d0.Obj) => f.d1.obj(am(f.toString))
  
  lazy val EmptyDiagram: Diagram = build("empty", _0_)(
    Map.empty[String, set],
    Map.empty[String, SetFunction]
  )
  
  val SamplePullbackDiagram: Diagram = BuildPullbackDiagram.asDiagram

    // TODO: implement
  lazy val SamplePushoutDiagram: Diagram = ???
  
  val SampleParallelPairDiagram1: Diagram =
    val a: set = Set(1, 2, 3, 4, 5)
    val b: set = Set(0, 1, 2, 3)
      
    val f = fun(a,b)("f", x => Math.min(2, x.toInt))
    val g = fun(a,b)("g", _.toInt % 3)
    
    build(
      "ParallelPair Sample1", ParallelPair)(
      Map("0" -> a, "1" -> b),
      Map("a" -> f, "b" -> g)
    )
  
  val SampleParallelPairSubdiagram1: Diagram =
    val a: set = Set(1, 2, 3)
    val b: set = Set(0, 1, 2)
    val f = fun(a,b)("f", x => Math.min(2, x.toInt))
    val g = fun(a,b)("g", x => x.toInt % 3)
    build(
      "ParallelPair Sample1", ParallelPair)(
      Map("0" -> a, "1" -> b),
      Map("a" -> f, "b" -> g)
    )
  
  val SampleParallelPairSubdiagram2: Diagram =
    val a: set = Set(1, 2, 3)
    val b: set = Set(0, 1, 2)
    val f = fun(a,b)("ParSub2.f", x => Math.min(2, x.toInt))
    val g = fun(a,b)("ParSub2.g", x => x.toInt % 3)
    build(
      "ParallelPair Sample1", ParallelPair)(
      Map("0" -> a, "1" -> b),
      Map("a" -> f, "b" -> g)
    )
  
  val SampleParallelPairDiagram2: Diagram =
    val a: set = Set(1, 2, 3)
    val b: set = Set(0, 1)
    val f = fun(a,b)("f", x => Math.min(1, x.toInt - 1))
    val g = fun(a,b)("g", x => x.toInt % 2)
    build(
      "ParallelPair Sample2", ParallelPair)(
      Map("0" -> a, "1" -> b),
      Map("a" -> f, "b" -> g)
    )
  
  val SampleZ3Diagram: Diagram =
    val a: set = Set(0, 1, 2, 3)

    def f(i: Int): Int => Int = (n: Int) => if (n == 3) n else (n + i) % 3

    val f1 = fun(a,a)("f1", x => f(1)(x.toInt))
    val f2 = fun(a,a)("f2", x => f(2)(x.toInt))
    build(
      "Z3 Sample", Z3)(
      Map("0" -> a),
      Map("1" -> f1, "2" -> f2)
    )

  def const(x: set): Diagram =
    build(s"Point($x)", _1_)(
      Map[String, set]("0" -> x),
      Map[String, SetFunction]()
    )

  object BuildPullbackDiagram:
    val sa: set = Set(1, 2, 3)
    val sb: set = Set(2, 3, 4)
    val sc: set = Set(0, 1)
    private lazy val ac = fun(sa,sc)("_ac", _.toInt % 2)
    private lazy val bc = fun(sb,sc)("_bc", x => (x.toInt + 1) % 2)
    lazy val om = Map("a" -> sa, "b" -> sb, "c" -> sc)
    lazy val am = Map("ac" -> ac, "bc" -> bc)
    
    lazy val asFunctor: Result[Functor] = Functor(
      "pullback", Pullback, SetCategory.Setf)(
      om,
      am
    )
    lazy val asDiagram: Diagram = build(
      "Pullback Sample", Pullback)(
      om,
      am
    )

  object SampleWDiagramContent:
    val a: set = Set("a00", "a01", "a10", "a11")
    val b: set = Set("0", "1")
    val c: set = Set("c00", "c01", "c10", "c11")
    val d: set = Set("0", "1")
    val e: set = Set("e00", "e01", "e10", "e11")
    val ab = fun(a,b)("ab", _.substring(2))
    val cb = fun(c,b)("cb", _.substring(2))
    val cd = fun(c,d)("cd", _.substring(1, 2))
    val ed = fun(e,d)("ed", _.substring(1, 2))
  
  val SampleWDiagram: Diagram =
    import SampleWDiagramContent._
    build(
      "W Sample", W)(
      Map("a" -> a, "b" -> b, "c" -> c, "d" -> d, "e" -> e),
      Map("ab" -> ab, "cb" -> cb, "cd" -> cd, "ed" -> ed)
    )

  object SampleMDiagramContent:
    val a: set = Set("a00", "a01", "a10", "a11")
    val b: set = Set("0", "1")
    val c: set = Set("c00", "c01", "c10", "c11")
    val d: set = Set("0", "1")
    val e: set = Set("e00", "e01", "e10", "e11")
    val ba = fun(b,a)("ba", "a0"+)
    val bc = fun(b,c)("bc", "c0"+)
    val dc = fun(d,c)("dc", "c1"+)
    val de = fun(d,e)("de", "e1"+)

  val SampleMDiagram: Diagram =
    import SampleMDiagramContent._
    build(
      "M Sample", M)(
      Map("a" -> a, "b" -> b, "c" -> c, "d" -> d, "e" -> e),
      Map("ba" -> ba, "bc" -> bc, "dc" -> dc, "de" -> de)
    )
