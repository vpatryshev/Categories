package math.cat

import math.sets.PoSet


/**
 * Morphism for posets.
 * It is a kind of bad decision, to redefine composition here. Should use Scala cake pattern instead.
 * See http://scala.sygneca.com/patterns/component-mixins, http://lamp.epfl.ch/~odersky/papers/ScalableComponent.pdf
 */
class PoSetMorphism[X, Y] (
        tag: String,
        override val d0: PoSet[X],
        override val d1: PoSet[Y],
        f : X => Y)
    extends SetMorphism[X, Y] (tag, d0, d1, f) {
  type Domain = PoSet[X]
  type Codomain = PoSet[Y]

  for {x <- d0
       y <- d0} {
    if (d0.le(x, y)) {
      val fx = f(x)
      val fy = f(y)
      require(d1.le(fx, fy), s"Since $x<=$y in domain, it should be $fx<=$fy in codomain")
    }
  }

  def compose[Z](g: PoSetMorphism[Y, Z]): PoSetMorphism[X, Z] = {
    require(d1 == g.d0, "Composition not defined")
    PoSetMorphism[X, Z]("a", d0, g.d1, (x: X) => g(this(x)))
  }
}

object PoSetMorphism {
  def apply[X, Y](tag: String, domain: PoSet[X], codomain: PoSet[Y], function: X => Y) =
    new PoSetMorphism[X, Y](tag, domain, codomain, function)

  def apply[X, Y](domain: PoSet[X], codomain: PoSet[Y], function: X => Y) =
    new PoSetMorphism[X, Y]("", domain, codomain, function)

  def id[X](domain: PoSet[X]) =
            new PoSetMorphism[X, X]("1", domain, domain, x => x)

  def const[X, Y](domain: PoSet[X], codomain: PoSet[Y], value: Y) =
      new PoSetMorphism[X, Y](value.toString, domain, codomain, _ => value)
}
