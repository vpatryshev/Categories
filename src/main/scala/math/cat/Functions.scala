package math.cat

/**
 * Contains some functions and operations.
 *
 * All source code is stored at <a href="https://github.com/vpatryshev/Categories">https://github.com/vpatryshev/Categories</a>
 *
 */

object Functions {
  /**
   * Injection is a function that maps two different arguments to two different values.
   * What is good about an Injection is that when it maps a set, the resulting set
   * has the same number of elements.
   * @tparam X domain type
   * @tparam Y codomain type
   */

  trait Injection[X, Y] extends Function[X, Y] {
    self =>
    /**
     * Composes this Injection with another Injection.
     * (f compose g)(x) ==  f(g(x))
     *
     * @param g another Injection
     * @return a composition
     */
    def compose[T](g: Injection[T, X]): Injection[T, Y] = injection { t: T => apply(g(t))} 

    /**
     * Composes this Injection with another Injection.
     * (f andThen g)(x) ==  g(f(x))
     *
     * @param g another Injection
     * @return a composition
     */
    def andThen[Z](g: Injection[Y, Z]): Injection[X, Z] = g compose this

    def applyTo(set: Set[X]): Set[Y] = {
      val source: Iterable[X] = set
      val target: Iterable[Y] = source.map(this)
      val predicate: Y => Boolean = (y: Y) => set.iterator exists {self(_) == y}
      Sets.setOf(target, set.size, predicate)
    }
  }

  def injection[X, Y] (f: X => Y): Injection[X, Y] = new Injection[X, Y] { def apply(x: X) = f(x) }

/**
   * Injection of values of a certain type T1 to themselves, as a super type T.
   * @tparam T main type
   * @tparam T1 subtype
   */
  class Inclusion[T1, T >: T1] extends Injection[T1, T] {
    def apply(t: T1): T = t
  }

  /**
   * Builds an inclusion of type T1 to type T
   * @tparam T main type
   * @tparam T1 subtype
   * @return an inclusion function that maps every instance of T1 to itself, considered as T
   */
  def inclusion[T1, T >: T1] = new Inclusion[T1, T]

  /**
   * Isomorphic function. Meaning, it has a revert, unapply(), such that
   * apply(unapply(y)) == y, and unapply(apply(x)) == x.
   *
   * @tparam X domain type
   * @tparam Y codomain type
   */
  abstract class Bijection[X, Y] extends Injection[X, Y] {
    /**
     * Operation that is inverse to apply: apply(unapply(y)) == y and unapply(apply(x)) == x
     * @param y an argument
     * @return an x such that apply(x) == y
     */
    def unapply(y: Y): X

    /**
     * Composes this Bijection with another Bijection.
     * (f compose g)(x) ==  f(g(x))
     *
     * @param g another Injection
     * @return a composition
     */
    def compose[T](g: Bijection[T, X]): Bijection[T, Y] = bijection((t:T) => apply(g(t)), (y: Y) => g.unapply(unapply(y)))

    /**
     * Inverse to this bijection
     */
    def inverse: Bijection[Y, X] = bijection(unapply, apply)

    /**
     *  Composes this Bijection with another Bijection.
     * (f andThen g)(x) ==  g(f(x))
     *
     * @param g another Injection
     * @return a composition
     */
    def andThen[Z](g: Bijection[Y, Z]): Bijection[X, Z] = g compose this

    override def applyTo(set: Set[X]): Set[Y] = {
      val target: Iterable[Y] = set.map(this)
      Sets.setOf(target, set.size, (y:Y) => set contains unapply(y))
    }
  }

  def bijection[X, Y](f: X => Y, g: Y => X): Bijection[X, Y] =
    new Bijection[X, Y] {
      def apply(x: X) = f(x)
      def unapply(y: Y) = g(y)
    }

  /**
   * Identity isomporphism on type T.
   * @tparam T domain type
   */
  class Id[T] extends Bijection[T, T] {
    def unapply(t: T): T = t
    def apply(t: T): T = t
  }

  /**
   * Builds an identity function
   * @tparam T domain type
   *
   * @param set domain
   * @return the identity function
   */
  def id[T] (set: Set[T]): Id[T] = new Id[T]

  /**
   * Given a function f, builds another function that for each x
   * builds Map.Entry(x, f(x))
   *
   * @tparam X domain type
   * @tparam Y codomain type
   * @param f function to apply
   * @return a function that builds pairs.
   */
  def schwartzianTransform[X,Y] (f: X => Y): Bijection[X, Product2[X, Y]] = {
    def first(p:Product2[X,Y]) = p._1 // patch for scala 2.8 compiler bug
    bijection(
      (x: X) => (x, f(x)),
      first
    )
  }

  /**
    * Builds a function that, returns list element by its index.
    *
    * @tparam X list element type
    * @param list the list
    * @return the function
    */
  def forList[X](list: List[X]): Int => X = list

  /**
   * Builds constant function
   * @tparam X domain type
   * @tparam Y codomain type
   * @param value the only value it ever returns
   * @return the constant function
   */
  def constant[X, Y] (value: Y): X => Y = (x: X) => value

  // TODO(vlad): figure out if this makes any sense
//  def restrict[X, X1 <: X, Y](fun: X => Y): X1 => Y = (x1: X1) => fun(x1)

  // This should not even exist! What exception?! what extension? Be contravariant.
  /**
   * Extends a function to a wider domain and codomain
   * the result will throw an exception if argument is not right
   */
//  def extend[X0, X1 >: X0, Y0, Y1 >: Y0](f: X0 => Y0): (X1 => Y1) = { case (x0: X0) => f(x0) }
}