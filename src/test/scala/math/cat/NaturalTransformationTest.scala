package math.cat

import math.Test
import math.cat.Category._
import scalakittens.Result
import math.Base._

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

    def buildFunctor(name: String, op: Int ⇒ Int) =
      Functor(name, c, d)(
        { case s ⇒ op(s.toInt).toString },
        { case PairRegex(x, y) ⇒ s"${op(x.toInt)}.${op(y.toInt)}" })

    lazy val f: F = buildFunctor("f", 2 *).iHope
    lazy val g: F = buildFunctor("g", 1 +).iHope
    lazy val h: F = buildFunctor("g", 2 +).iHope

    lazy val fg: NT = NaturalTransformation.build(f, g)(Map(
      f.d0.obj("0") → f.d1.arrow("0.1"), f.d0.obj("1") → f.d1.arrow("2.2"))).iHope

    lazy val gh: NT = NaturalTransformation.build(g, h)(Map("0" → g.d1.arrow("1.2"), "1" → g.d1.arrow("2.3"))).iHope

    "compose" in {
        val fgh = fg compose gh
        fgh.d0 === f
        fgh.d1 === h
        fgh.transformPerObject(fgh.domainCategory.obj("0")) === "0.2"
        fgh.transformPerObject(fgh.domainCategory.obj("1")) === "2.3"
    }

    "have identity" in {
        val idThen_fg = NaturalTransformation.id(f) compose fg
        idThen_fg === fg
        val idThen_gh = NaturalTransformation.id(g) compose gh
        idThen_gh === gh
        val fgThenId = fg compose NaturalTransformation.id(g)
        fgThenId === fg
    }
  }
}
