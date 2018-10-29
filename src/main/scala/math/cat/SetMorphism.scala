package math.cat

import Sets._

/**
 * Morphism class for sets, and the accompanying object.
 * @author vpatryshev
 */
class SetMorphism[X, Y] (val tag: String, val d0: Set[X], val d1: Set[Y], f : X => Y)
          extends Morphism[Set[X], Set[Y]] with Map[X, Y] {

  // Validates set morphism.
  // All we need is that each d0 element is mapped to a d1 element.
  for (x <- d0) {
    val y = this(x)
    require(d1 contains y, "Morphism value " + y + " for " + x + " should be in d1 " + d1)
  }

  override def toString: String = tag match {
    case "" => "{" + (d0 map (x => x.toString + " -> " + this(x).toString) mkString(", "))  + "}"
    case _  => tag
  }

  override def hashCode = d0.hashCode * 4/*random number*/ + d1.hashCode
  
  /**
   * Two set morphisms are equal if they have equal d0s and d1s and map d0 elements to the same values.
   * Note that there's no negation in this calculation; there is a deep reason for it, to be disclosed much, much later.
   *
   * @param other set morphism to compare
   * @return true iff they are equal
   */
  def equals(other: SetMorphism[X, Y]) = {
    ((d0.equals(other.d0) && d1.equals(other.d1)) /: d0) ((eq, x) => eq & this(x) == other(x))
  }
  
  def compose[Z](g: SetMorphism[Y, Z]): Option[SetMorphism[X, Z]] = {
    if (d1 == g.d0) Some(SetMorphism[X, Z](d0, g.d1, (x: X) => g(this(x))))
    else None
  }
  
  def revert = {
    val d1AsSet: Set[Y] = d1
    val d0AsSet: Set[X] = d0
    val d0ForRevert: Set[Set[X]] = Sets.powerset(d0AsSet)
    SetMorphism[Y, Set[X]](
      d1AsSet,
      d0ForRevert,
      Sets.groupBy(d0AsSet, d1AsSet, this))
  }  
  
  override def contains(x: X) = d0.contains(x)

  def get(x: X) = if (contains(x)) Some(f(x)) else None

  override def size = d0.size

  def - (x: X) = requireImmutability
  def +[Y1 >: Y] (kv: (X, Y1)) = requireImmutability
  def updates[B1 >: Y](x: X, y: B1) = requireImmutability
  def empty[PY] = throw new RuntimeException("No such thing exists as empty set morphism")
  def iterator = d0.iterator map (x => (x, f(x)))
  def product = exponent(d1, d0) filter(m => d1.forall(y => f(m(y)) == y))
}

object SetMorphism {
  def apply[X, Y](d0: Set[X], d1: Set[Y], function: X => Y) =
    new SetMorphism[X, Y]("", d0, d1, function)

  def apply[X, Y](name: String, d0: Set[X], d1: Set[Y], function: X => Y) =
    new SetMorphism[X, Y](name, d0, d1, function)

//  implicit def include[X, Y >: X](d0: Set[X]) =
//    apply(d0, d1 asInstanceOf Set[Y], source.function)

  def unit[X](s: Set[X]) = new SetMorphism[X, X]("1", s, s, x => x)

  def const[X, Y](d0: Set[X], d1: Set[Y], value: Y) =
    new SetMorphism[X, Y]("`" + value + "`", d0, d1, _ => value)

  def hom[X, Y](xs: Set[X], ys: Set[Y]): Set[SetMorphism[X, Y]] = {
      val maps = Sets.exponent(xs, ys)
      val morphisms = maps map (SetMorphism(xs, ys, _))
      def predicate(m: SetMorphism[X, Y]) = m.d0 == xs && m.d1 == ys && maps.contains(m)
      setOf(morphisms, maps.size, predicate _)
  }

  def pullback[X, Y, Z](f: SetMorphism[X, Z], g: SetMorphism[Y, Z]) = {
    val pullbackSet = Sets.pullback(f.d0, g.d0, f, g)
    val p0 = (p: (X, Y)) => p._1
    val p1 = (p: (X, Y)) => p._2
    (apply(pullbackSet, f.d0, p0),
     apply(pullbackSet, g.d0, p1))
  }
}