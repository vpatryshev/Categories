package math.cat

import math.sets.PoSet
import scalakittens.Result
import scalakittens.Result.{OK, Outcome}


/**
  * Morphism for posets.
  * It is a kind of bad decision, to redefine composition here. Should use Scala cake pattern instead.
  * See http://scala.sygneca.com/patterns/component-mixins, http://lamp.epfl.ch/~odersky/papers/ScalableComponent.pdf
  */
class PoSetMorphism[X, Y] private(
  tag: String,
  override val d0: PoSet[X],
  override val d1: PoSet[Y],
  function: X => Y)
  extends SetMorphism[X, Y](tag, d0, d1, function) {
  type Domain = PoSet[X]
  type Codomain = PoSet[Y]

  def compose[Z](g: PoSetMorphism[Y, Z]): PoSetMorphism[X, Z] = {
    require(d1 == g.d0, "Composition not defined")
    new PoSetMorphism[X, Z]("a", d0, g.d1, (x: X) => g(this (x)))
  }
}

object PoSetMorphism {
  def check[X, Y](pom: PoSetMorphism[X, Y]): Result[PoSetMorphism[X, Y]] = {
    val r0 = SetMorphism.check[X, Y, PoSetMorphism[X, Y]](pom)
    val results = for {x <- pom.d0
                       y <- pom.d0
                       if pom.d0.le(x, y)
    } yield {
      val fx = pom(x)
      val fy = pom(y)
      Result.OKif(pom.d1.le(fx, fy),
        s"Since $x<=$y in domain, it should be $fx<=$fy in codomain")
    }
    r0 andThen Result.traverse(results) returning pom
  }

  def build[X, Y](tag: String, domain: PoSet[X], codomain: PoSet[Y], function: X => Y): Result[PoSetMorphism[X, Y]] =
    check(new PoSetMorphism[X, Y](tag, domain, codomain, function))

  def build[X, Y](domain: PoSet[X], codomain: PoSet[Y], function: X => Y): Result[PoSetMorphism[X, Y]] =
    check(new PoSetMorphism[X, Y]("", domain, codomain, function))

  def id[X](domain: PoSet[X]) =
    new PoSetMorphism[X, X]("1", domain, domain, x => x)

  def const[X, Y](domain: PoSet[X], codomain: PoSet[Y], value: Y) =
    new PoSetMorphism[X, Y](value.toString, domain, codomain, _ => value)
}
