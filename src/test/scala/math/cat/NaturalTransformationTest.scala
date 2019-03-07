package math.cat

import math.Test
import math.cat.Category._
import scalakittens.Result

/**
  * Prototype for all tests
  */
class NaturalTransformationTest extends Test {
  type C = Category[Int, (Int, Int)]
  type F = Functor[C, C]
  type NT = NaturalTransformation[C, C]
  type SUT = ((((F, F), F), NT), NT)

  "natural transformation" should {
    val c = _2_
    val d = _5_
    lazy val fOpt: Result[F] = Functor.build("f", c, d)((i: Int) => i * 2, p => (p._1 * 2, p._2 * 2))
    lazy val gOpt: Result[F] = Functor.build("g", c, d)((i: Int) => i + 1, p => (p._1 + 1, p._2 + 1))
    lazy val hOpt: Result[F] = Functor.build("h", c, d)((i: Int) => i + 2, p => (p._1 + 2, p._2 + 2))
    lazy val fgOpt: Result[NT] = for {
      f <- fOpt
      g <- gOpt
    } yield NaturalTransformation(f, g)(Map(0 -> (0, 1), 1 -> (2, 2)))

    lazy val ghOpt: Result[NT] = for {
      g <- gOpt
      h <- hOpt
    } yield NaturalTransformation(g, h)(Map(0 -> (1, 2), 1 -> (2, 3)))

    "compose" in {
      expect { case ((((f, g), h), fg), gh) =>
        val fgh = fg compose gh
        fgh.f === f
        fgh.g === h
        fgh.transformPerObject(0) === (0, 2)
        fgh.transformPerObject(1) === (2, 3)
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
