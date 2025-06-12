package math.cat.topos

import math.Test
import math.cat.Categories._
import math.cat.{Category, Functor, SetCategory, SetFunction}
import math.sets.Sets.set
import scalakittens.Result

import scala.collection.mutable
import scala.language.{implicitConversions, postfixOps}
import SetFunction._

trait TestDiagrams:
  import Constructor.*
  val toposes: mutable.Map[String, CategoryOfDiagrams] = mutable.Map[String, CategoryOfDiagrams]()
  
  private def toposOver(domain: Category): CategoryOfDiagrams =
    toposes.getOrElseUpdate(domain.name, new CategoryOfDiagrams(domain))

  val `Set^𝟘`:            CategoryOfDiagrams = toposOver(`𝟘`)
  val `Set^𝟙`:            CategoryOfDiagrams = toposOver(`𝟙`)
  val `Set^𝟚`:            CategoryOfDiagrams = toposOver(`𝟚`)
  val `Set^𝟛`:            CategoryOfDiagrams = toposOver(`𝟛`)
  val `Set^𝟜`:            CategoryOfDiagrams = toposOver(`𝟜`)
  val `Set^𝟝`:            CategoryOfDiagrams = toposOver(`𝟝`)
  val `Set^𝟙+𝟙`:           CategoryOfDiagrams = toposOver(`𝟙+𝟙`)
  val `Set^M`:            CategoryOfDiagrams = toposOver(M)
  val `Set^W`:            CategoryOfDiagrams = toposOver(W)
  val `Set^Z2`:           CategoryOfDiagrams = toposOver(Z2)
  val `Set^Z3`:           CategoryOfDiagrams = toposOver(Z3)
  val `Set^Z4`:           CategoryOfDiagrams = toposOver(Z4)
  val `Set^Pullback`:     CategoryOfDiagrams = toposOver(Pullback)
  val `Set^Pushout`:      CategoryOfDiagrams = toposOver(Pushout)
  val `Set^ParallelPair`: CategoryOfDiagrams = toposOver(ParallelPair)
  val `Set^Simplicial`:   CategoryOfDiagrams = toposOver(Simplicial3)
  val `Set^Square`:       CategoryOfDiagrams = toposOver(Square)

  implicit def translateObjectMapping(f: Functor)(om: String => set): f.d0.Obj => f.d1.Obj =
    (x: f.d0.Obj) => om(f.toString)

  implicit def translateArrowMapping(f: Functor)(am: String => SetFunction): f.d0.Obj => f.d1.Obj =
    (x: f.d0.Obj) => am(f.toString)
  
  lazy val EmptyDiagram: `Set^𝟘`.Diagram = build("empty", `Set^𝟘`)(
    Map.empty[String, set],
    Map.empty[String, SetFunction]
  )
  
  val SamplePullbackDiagram: `Set^Pullback`.Diagram = BuildPullbackDiagram.asDiagram

    // TODO: implement
  lazy val SamplePushoutDiagram: `Set^Pushout`.Diagram = ???
  
  val SampleParallelPairDiagram1: `Set^ParallelPair`.Diagram =
    val a: set = Set(1, 2, 3, 4, 5)
    val b: set = Set(0, 1, 2, 3)
      
    val f = fun(a,b)("f", x => Math.min(2, x.toInt))
    val g = fun(a,b)("g", _.toInt % 3)
    
    build(
      "ParallelPair Sample1", `Set^ParallelPair`)(
      Map("0" -> a, "1" -> b),
      Map("a" -> f, "b" -> g)
    )
  
  val SampleParallelPairSubdiagram1: `Set^ParallelPair`.Diagram =
    buildPPdiagram(`Set^ParallelPair`)

  val SampleParallelPairSubdiagram2: `Set^ParallelPair`.Diagram =
    buildPPdiagram(`Set^ParallelPair`, "ParSub2.")

  val SampleParallelPairDiagram2: `Set^ParallelPair`.Diagram =
    val a: set = Set(1, 2, 3)
    val b: set = Set(0, 1)
    val f = fun(a,b)("f", x => Math.min(1, x.toInt - 1))
    val g = fun(a,b)("g", x => x.toInt % 2)
    build(
      "ParallelPair Sample2", `Set^ParallelPair`)(
      Map("0" -> a, "1" -> b),
      Map("a" -> f, "b" -> g)
    )
  
  val SampleZ3Diagram: `Set^Z3`.Diagram =
    val dom: set = Set(2220, 2221, 2222, 2223)

    def f(i: Int)(n: Int): Int = if n == 2223 then n else (2220 + (n + i) % 3)

    require(f(1)(2220) == 2221, s"f(1)(2220) should be 2221 but is ${f(1)(2220)}")
    require(f(1)(2221) == 2222, s"f(1)(2221) should be 2222 but is ${f(1)(2221)}")
    require(f(1)(2222) == 2220, s"f(1)(2222) should be 2220 but is ${f(1)(2222)}")
    require(f(1)(2223) == 2223, s"f(1)(2223) should be 2223 but is ${f(1)(2223)}")
    require(f(2)(2220) == 2222, s"f(2)(2220) should be 2222 but is ${f(2)(2220)}")
    require(f(2)(2221) == 2220, s"f(2)(2221) should be 2220 but is ${f(2)(2221)}")
    require(f(2)(2222) == 2221, s"f(2)(2222) should be 2221 but is ${f(2)(2222)}")
    require(f(2)(2223) == 2223, s"f(2)(2223) should be 2223 but is ${f(2)(2223)}")

    val f1 = fun(from = dom, to = dom)("f1", x => f(1)(x.toInt))
    val f2 = fun(from = dom, to = dom)("f2", x => f(2)(x.toInt))
    build("Z3 Sample", `Set^Z3`)(
      objectsMap = Map("0" -> dom),
      arrowMap = Map("1" -> f1, "2" -> f2)
    )

  def const(x: set): `Set^𝟙`.Diagram =
    build(s"Point($x)", `Set^𝟙`)(
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
    lazy val asDiagram: `Set^Pullback`.Diagram = build(
      "Pullback Sample", `Set^Pullback`)(
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
  
  val SampleWDiagram:  `Set^W`.Diagram =
    import SampleWDiagramContent._
    build(
      "W Sample", `Set^W`)(
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

  val SampleMDiagram: `Set^M`.Diagram =
    import SampleMDiagramContent._
    build(
      "M Sample", `Set^M`)(
      Map("a" -> a, "b" -> b, "c" -> c, "d" -> d, "e" -> e),
      Map("ba" -> ba, "bc" -> bc, "dc" -> dc, "de" -> de)
    )

object Constructor:
  def build(name: String, topos: GrothendieckTopos)(
    objectsMap: String => set,
    arrowMap: String => SetFunction): topos.Diagram =
    def om(o: topos.domain.Obj): set = objectsMap(o.toString)
    def am(o: topos.domain.Arrow): SetFunction = arrowMap(o.toString)
    topos.Diagram(name, om, am)

  def buildPPdiagram(topos: CategoryOfDiagrams, prefix: String = "") = {
    val a: set = Set(1, 2, 3)
    val b: set = Set(0, 1, 2)
    val f = fun(a, b)(s"${prefix}f", x => Math.min(2, x.toInt))
    val g = fun(a, b)(s"${prefix}g", x => x.toInt % 3)
    build(
      "ParallelPair Sample1", topos)(
      Map("0" -> a, "1" -> b),
      Map("a" -> f, "b" -> g)
    )
  }

class SetupTest extends Test:
  "diagrams" should :
    "all get instantiated" in :
      try
        val allThat = new TestDiagrams {}
        allThat.`Set^M`.toString === "Set^M: {(infinite category)}"
      catch
        case someShit: Exception => 
          failure(someShit.toString)

      ok

object Publish:
  
    @main def allToposes(): Unit =
      try
        val allThat  = new TestDiagrams{}
        println(allThat.toposes.values)
      catch
        case x: Exception => x.printStackTrace()