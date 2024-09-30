package math.cat

import math.Base._
import math.sets.PoSet
import scalakittens.Result
import scalakittens.Result._

import scala.language.postfixOps

/**
  * Morphism of posets.
  * It is a kind of bad decision, to redefine composition here. Should use cake pattern instead.
  */
class PoSetMorphism[X, Y] private(
  tag: String,
  override val d0: PoSet[X],
  override val d1: PoSet[Y],
  function: X => Y)
  extends SetMorphism[X, Y](tag, d0, d1, function):
  type Domain = PoSet[X]
  type Codomain = PoSet[Y]

  infix def andThen[Z](g: PoSetMorphism[Y, Z]): Option[PoSetMorphism[X, Z]] =
    OKif(d1 == g.d0, "Composition not defined") returning (
      new PoSetMorphism[X, Z]("a", d0, g.d1, (x: X) => g(this (x)))
    ) asOption

  override def removed(key: X): Map[X, Y] = itsImmutable

object PoSetMorphism:
  def check[X, Y](f: PoSetMorphism[X, Y]): Result[PoSetMorphism[X, Y]] =
    val setMappingChecked = SetMorphism.check[X, Y, PoSetMorphism[X, Y]](f)
    
    lazy val posetMappingChecked =
      traverse(for
        x <- f.d0
        y <- f.d0
        if f.d0.le(x, y)
      yield
        val fx = f(x)
        val fy = f(y)
        OKif(f.d1.le(fx, fy), s"Since $x<=$y in domain, we need $fx<=$fy in codomain")
      )
    
    setMappingChecked andThen posetMappingChecked returning f
  end check
  
  def build[X, Y](tag: String, domain: PoSet[X], codomain: PoSet[Y], function: X => Y): Result[PoSetMorphism[X, Y]] =
    check(new PoSetMorphism[X, Y](tag, domain, codomain, function))

  def build[X, Y](domain: PoSet[X], codomain: PoSet[Y], function: X => Y): Result[PoSetMorphism[X, Y]] =
    build("", domain, codomain, function)

  def id[X](domain: PoSet[X]) =
    new PoSetMorphism[X, X]("id", domain, domain, x => x)

  def const[X, Y](domain: PoSet[X], codomain: PoSet[Y], value: Y) =
    new PoSetMorphism[X, Y](value.toString, domain, codomain, _ => value)
