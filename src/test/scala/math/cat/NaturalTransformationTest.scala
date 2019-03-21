package math.cat

import math.Test
import math.cat.Category._
import scalakittens.Result

/**
  * Natural transformations tests
  */
class NaturalTransformationTest extends Test {
  type F = Functor[Cat, Cat]
  type NT = NaturalTransformation[Cat, Cat]
  type SUT = ((((F, F), F), NT), NT)

  "natural transformation" should {
    val c = _2_
    val d = _5_
    
    def buildFunctor(name: String, op: Int => Int) =
      Functor.build(name, c, d)(
        { case s => op(s.toInt).toString },
        { case PairRegex(x, y) => s"${op(x.toInt)}.${op(y.toInt)}" })
    
    lazy val fOpt: Result[F] = buildFunctor("f", 2*)
    lazy val gOpt: Result[F] = buildFunctor("g", 1+)
    lazy val hOpt: Result[F] = buildFunctor("g", 2+)
    
    lazy val fgOpt: Result[NT] = for {
      f <- fOpt
      g <- gOpt
      nt <- NaturalTransformation.build(f, g)(Map(
        "0" -> f.d1.arrow("0.1"), "1" -> f.d1.arrow("2.2")))
    } yield nt

    lazy val ghOpt: Result[NT] = for {
      g <- gOpt
      h <- hOpt
      nt <- NaturalTransformation.build(g, h)(Map("0" -> g.d1.arrow("1.2"), "1" -> g.d1.arrow("2.3")))
    } yield nt

    "compose" in {
      expect { case ((((f, g), h), fg), gh) =>
        val fgh = fg compose gh
        fgh.from === f
        fgh.to === h
        fgh.transformPerObject(fgh.from.d0.obj("0")) === "0.2"
        fgh.transformPerObject(fgh.from.d0.obj("1")) === "2.3"
      }(fOpt andAlso gOpt andAlso hOpt andAlso fgOpt andAlso ghOpt)
    }

    "have identity" in {
      expect { case ((((f, g), h), fg), gh) =>
        val idThen_fg = NaturalTransformation.id(f) compose fg
        idThen_fg === fg
        val idThen_gh = NaturalTransformation.id(g) compose gh
        idThen_gh === gh
        val fgThenId = fg compose NaturalTransformation.id(g)
        fgThenId === fg
      }(fOpt andAlso gOpt andAlso hOpt andAlso fgOpt andAlso ghOpt)
    }
  }
}
