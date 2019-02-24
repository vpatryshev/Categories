package math.cat

import org.specs2.mutable._
import Category._
import sun.net.www.protocol.http.ntlm.NTLMAuthentication

/**
  * Prototype for all tests
  */
class NaturalTransformationTest extends Specification {

  "natural transformation" should {
    val c = _2_
    val d = _5_
    type C = Category[Int, (Int, Int)]
    type F = Functor[C, C]
    type NT = NaturalTransformation[C, C]
    lazy val f: F = Functor("f", c, d)((i: Int) => i*2, p => (p._1*2, p._2*2))
    lazy val g: F = Functor("g", c, d)((i: Int) => i+1, p => (p._1+1, p._2+1))
    lazy val h: F = Functor("h", c, d)((i: Int) => i+2, p => (p._1+2, p._2+2))
    lazy val fg: NT = NaturalTransformation(f, g)(Map(0 -> (0, 1), 1 -> (2, 2)))
    lazy val gh: NT = NaturalTransformation(g, h)(Map(0 -> (1, 2), 1 -> (2, 3)))

    "compose" in {
      val fgh = fg compose gh
      fgh.f === f
      fgh.g === h
      fgh.transformPerObject(0) === (0, 2)
      fgh.transformPerObject(1) === (2, 3)
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
