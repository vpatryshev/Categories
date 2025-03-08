package math.cat

import math.Test
import math.cat.Categories._
import scalakittens.Result
import scalakittens.Result._
import NaturalTransformation.id

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

    val `f->g` = fgOpt.iHope
    val `g->h` = ghOpt.iHope

    "build properly" in {
      // if the test crashes on start, move lines 35-36 here
      ok
    }

    "compose" in {
        val `f->h` = `g->h` ∘ `f->g`
        `f->h`.d0 === f
        `f->h`.d1 === h
        `f->h`.mappingAt("0") === "0.2"
        `f->h`.mappingAt("1") === "2.3"
    }

    "have identity" in {
        `f->g` ∘ id(f) === `f->g`
        `g->h` ∘ id(g) === `g->h`
        id(g) ∘ `f->g` === `f->g`
    }
  }
}
