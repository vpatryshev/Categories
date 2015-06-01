package math.cat

class SetMorphism[X, PX <: Set[X], Y, PY <: Set[Y]] (domain: PX, codomain: PY, f : X => Y) 
          extends Morphism[PX, PY] (domain, codomain) with Map[X, Y] {
  // how the hell do I call super constructor in default constructor, he?
  validate
  
  override def apply(x: X) = f(x)
  
  /**
   * Validates set morphism.
   * All we need is that each domain element is mapped to a codomain element.
   */
  def validate {
    require(domain != null, "Domain should not be null")
    require(codomain != null, "Codomain should not be null")
    for (x <- domain) {
      val y = this(x)
      require(codomain contains y, "Morphism value " + y + " for " + x + " should be in codomain " + codomain)
    }
  }

  override def toString: String = {
    "{" + (domain map (x => x.toString + " -> " + this(x).toString) mkString(", "))  + "}"
  }

  override def hashCode = domain.hashCode * 4/*random number*/ + codomain.hashCode
  
  /**
   * Two set morphisms are equal if they have equal domains and codomains and map domain elements to the same values.
   * Note that there's no negation in this calculation; there is a deep reason for it, to be disclosed much, much later.
   *
   * @param other set morphism to compare
   * @return true if they are equal
   */
  def equals(other: SetMorphism[X, PX, Y, PY]) = {
    ((domain.equals(other.domain) && codomain.equals(other.codomain)) /: domain) ((eq, x) => eq & this(x) == other(x))
  }
  
  def compose[Z, PZ <: Set[Z]](g: SetMorphism[Y, PY, Z, PZ]): SetMorphism[X, PX, Z, PZ] = {
    require(codomain == g.domain, "Composition not defined")
    SetMorphism[X, PX, Z, PZ](domain, g.codomain, (x: X) => g(this(x)))
  }
  
  def revert = {
    val codomainAsSet: Set[Y] = codomain
    val domainAsSet: Set[X] = domain
    SetMorphism[Y, Set[Y], Set[X], Set[Set[X]]](codomainAsSet, Sets.powerset(domainAsSet), Sets.groupBy(domainAsSet, codomainAsSet, this))
  }  
  
  override def contains(x: X) = domain.contains(x)
  def get(x: X) = if (contains(x)) Some(f(x)) else None
  override def size = domain.size
  def - (x: X) = throw new RuntimeException("TODO: REPLACE THIS EXCEPTION WITH SOMETHING MEANINGFUL")
  def update[B1 >: Y](x: X, y: B1) = throw new RuntimeException("TODO: REPLACE THIS EXCEPTION WITH SOMETHING MEANINGFUL")
  def empty[PY] = throw new RuntimeException("TODO: find a better exception")
  def iterator = domain.iterator map (x => (x, f(x)))
}

object SetMorphism {
  def apply[X, PX <: Set[X], Y, PY <: Set[Y]](domain: PX, codomain: PY, function: X => Y) = {
    new SetMorphism[X, PX, Y, PY](domain, codomain, function)
  } 
  
  def unit[X, PX <: Set[X]](domain: PX) = {
    new SetMorphism[X, PX, X, PX](domain, domain, x => x)
  }

  def constant[X, PX <: Set[X], Y, PY <: Set[Y]](domain: PX, codomain: PY, value: Y) = {
    new SetMorphism[X, PX, Y, PY](domain, codomain, x => value)
  }
  
  def hom[X, PX <: Set[X], Y, PY <: Set[Y]](xs: PX, ys: PY): Set[SetMorphism[X, PX, Y, PY]] = {
    Sets.exponent(ys, xs) map (SetMorphism(xs, ys, _))
  }
}