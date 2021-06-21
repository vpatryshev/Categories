package math.sets

/**
 * Contains some functions and operations.
 */

object Functions:
  
  /**
   * Injection is a function that maps two different arguments to two different values.
   * What is good about an Injection is that when it maps a set, the resulting set
   * has the same number of elements.
   * @tparam X domain type
   * @tparam Y codomain type
   */
  trait Injection[X, Y] extends Function[X, Y]:

    /**
      * Composes this Injection with another Injection.
      * (f compose g)(x) == f(g(x))
      *
      * @param g another injection
      * @return a composition
      */
    def compose[T](g: Injection[T, X]): Injection[T, Y] =
      injection { (t:T) => apply(g(t)) }

    /**
      * Composes this Injection with another Injection.
      * (f andThen g)(x) ==  g(f(x))
      *
      * @param g another Injection
      * @return a composition
      */
    def andThen[Z](g: Injection[Y, Z]): Injection[X, Z] = g compose this

  /**
    * Builds an injection.
    * We actually don't know whether `f` is monomorphic, but we trust the user.
    * @param f the function that is promoted to Injection
    * @tparam X argument type
    * @tparam Y result type
    * @return an instance of `Injection`
    */
  def injection[X, Y] (f: X => Y): Injection[X, Y] =
    new Injection[X, Y]:
      def apply(x: X) = f(x)

/**
   * Injection of values of a certain type T1 to themselves, as a super type T.
   * @tparam T main type
   * @tparam T1 subtype
   */
  class Inclusion[T1, T >: T1] extends Injection[T1, T]:
    def apply(t: T1): T = t

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
  abstract class Bijection[X, Y] extends Injection[X, Y]:
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
    def compose[T](g: Bijection[T, X]): Bijection[T, Y] =
      bijection((t:T) => apply(g(t)), (y: Y) => g.unapply(unapply(y)))

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

  /**
    * Tentatively builds a bijection out of two assumingly inverse functions
    * @param f a function
    * @param g another function, which is an inverse of `f`
    * @tparam X argument type
    * @tparam Y result type
    * @return a Bijection
    */
  def bijection[X, Y](f: X => Y, g: Y => X): Bijection[X, Y] =
    new Bijection[X, Y] {
      def apply(x: X) = f(x)
      def unapply(y: Y) = g(y)
    }

  /**
   * Identity isomporphism on type T.
   * @tparam T domain type
   */
  class Id[T] extends Bijection[T, T]:
    def unapply(t: T): T = t
    def apply(t: T): T = t

  /**
   * Builds an identity function
   * @tparam T domain type
   *
   * @param domain domain
   * @return the identity function
   */
  def id[T] (domain: Set[T]): Id[T] = new Id[T]

  /**
   * Given a function f, builds another function that for each x
   * builds Map.Entry(x, f(x))
   *
   * @tparam X domain type
   * @tparam Y codomain type
   * @param f function to apply
   * @return a function that builds pairs.
   */
  def schwartzianTransform[X,Y] (f: X => Y): Bijection[X, (X, Y)] =
    bijection(
      (x: X) => (x, f(x)),
      _._1
    )

  /**
    * Restricts a function to a subtype
    * @param fun a function X -> Y
    * @tparam X argument type
    * @tparam X1 the subtype
    * @tparam Y value type
    * @return a function X1 -> Y
    */
  def restrict[X, X1 <: X, Y](fun: X => Y): X1 => Y = (x1: X1) => fun(x1)
