package math.cat

import math.Test
import math.cat.Categories._
import scalakittens.Result
import scalakittens.Result._

/**
  * Natural transformations tests
  */
class NaturalTransformationTest extends Test {
  type F = Functor
  type NT = NaturalTransformation
  type SUT = ((((F, F), F), NT), NT)

  "natural transformation" should {
    val c = _2_
    val d = _5_
    def buildFunctor(name: String, op: Int => Int) =
      Functor(name, c, d)(
        { case s => op(s.toInt).toString },
        { case PairRegex(x, y) => s"${op(x.toInt)}.${op(y.toInt)}" })

    val f: F = buildFunctor("f", 2 * _).iHope
    val g: F = buildFunctor("g", 1 + _).iHope
    val h: F = buildFunctor("g", 2 + _).iHope

    val mappingFor_fg: (f.d0.Obj => f.d1.Arrow) =
      Map("0" -> "0.1", "1" -> "2.2")

    val fgOpt: Result[NT] = NaturalTransformation.build("test_fg", f, g)(mappingFor_fg)
    val ghOpt: Result[NT] = NaturalTransformation.build("test_gh", g, h)(
      Map("0" -> "1.2", "1" -> "2.3"))

    "build properly" in {
      val fg = fgOpt.iHope
      val gh = ghOpt.iHope
      
      ok
    }
    
    
    "compose" in {
        val fgh = fgOpt.iHope andThen ghOpt.iHope
        fgh.d0 === f
        fgh.d1 === h
        fgh.transformPerObject("0") === "0.2"
        fgh.transformPerObject("1") === "2.3"
    }

    "have identity" in {
        val idThen_fg = NaturalTransformation.id(f) andThen fgOpt.iHope
        idThen_fg === fgOpt.iHope
        val idThen_gh = NaturalTransformation.id(g) andThen ghOpt.iHope
        idThen_gh === ghOpt.iHope
        val fgThenId = fgOpt.iHope andThen NaturalTransformation.id(g)
        fgThenId === fgOpt.iHope
    }
  }
}
