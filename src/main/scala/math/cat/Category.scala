package math.cat

import java.io.Reader

import math.cat.Sets._

/**
  * Category class, and the accompanying object.
  */
abstract class Category[O, A](val g: Graph[O, A]) extends Graph[O, A](g) {
  def objects: Set[O] = nodes

  val unit: O => A
  val m: (A, A) => Option[A]

  def composablePairs: Iterable[(A, A)] = Category.composablePairs(this)

  validate()

  def validate() {
    validateGraph()
    if (Sets.isFinite(objects)) {
    for (x <- objects) {
      val ux = unit(x)
      require(d0(ux) == x, s"Domain of unit $ux should be $x")
      require(d1(ux) == x, s"Codomain of unit $ux should be $x")
    }

    }
      
    if (Sets.isFinite(arrows)) {
    for (f <- arrows) {
      val u_f = m(unit(d0(f)), f)
      require(u_f contains f, s"Left unit law broken for ${unit(d0(f))} and $f: got $u_f")
    }

    for (f <- arrows) {
      val f_u = m(f, unit(d1(f)))
      require(f_u contains f, s"Right unit law broken for ${unit(d1(f))} and $f: got $f_u")
    }

    for (f <- arrows; g <- arrows if follows(g, f)) {
      val h = m(f, g)
      require(h.isDefined, s"composition must be defined for $f and $g")
      h.foreach(gf => {
        require(sameDomain(gf, f), s"Wrong composition $gf of $f and $g : its d0 is ${d0(gf)}, must be ${d0(f)}")
        require(sameCodomain(gf, g), s"Wrong composition $gf of %f and %g: its d1 is %{d1(gf)}, must be ${d1(g)}")
      }
      )
    }

    for (f <- arrows; g <- arrows if follows(g, f)) {
      val gfOpt = m(f, g)
      require (gfOpt.isDefined, s"Composition of $f and $g must be defined")
      gfOpt.foreach { gf =>
        for (h <- arrows if follows(h, g)) {
          val hgOpt = m(g, h)
          require(hgOpt.isDefined, s"Composition of $g and $h must be defined")
          hgOpt foreach {
            hg => 
              require(m(gf, h) == m(f, hg), s"Associativity broken for $f, $g and $h")
          }
        }
      }
    }
  }
  }

  //@deprecated("category theory is not equational")
  // cannot elimitate this: too many tests rely on comparing categories...
  override def equals(x: Any): Boolean = // error ("category theory is not equational")
  {
    x match {
      case other: Category[_, _] => other.asInstanceOf[Category[O, A]].equal(this)
      case _ => false
    }
  }

  // @deprecated("is category theory equational? does not seem like it is...")
  private def equal(that: Category[O, A]): Boolean = {
    val objectsEqual = this.objects == that.objects && this.arrows == that.arrows
    val unitsEqual = objectsEqual && objects.forall(x => this.unit(x) == that.unit(x))

    val isEqual = unitsEqual &&
      arrows.forall(f => arrows.forall(g => !follows(f, g) || this.m(f, g) == that.m(f, g)))

    isEqual
  }
  
  override def hashCode: Int = {
    val c1 = getClass.hashCode
    val c2 = objects.hashCode
    val c3 = arrows.hashCode
    c1 + c2 * 2 + c3 * 5
  }

  override def toString: String = "({" +
    objects.mkString(", ") + "}, {" +
    (arrows map (a => s"$a: ${d0(a)}->${d1(a)}")).mkString(", ") + "}, {" +
    (composablePairs collect {case (first, second) =>
      s"$first o $second = ${m(first, second).get}"}).mkString(", ") + "})"

  def areInverse(f: A, g: A): Boolean = (m(f, g) contains unit(d0(f))) && (m(g, f) contains unit(d0(g)))

  /**
    * Returnes an inverse arrow.
    *
    * @param f an arrow for which we are looking an inverse
    * @return inverse arrow
    */
  def inverse(f: A): Option[A] = hom(d1(f), d0(f)) find (areInverse(f, _))

  /**
    * Checks whether an arrow is an isomorphism.
    *
    * @param f an arrow to check
    * @return true iff f is an isomorphism
    */
  def isIsomorphism(f: A): Boolean = inverse(f).isDefined

  def isEndomorphism(f: A): Boolean = d0(f) == d1(f)

  /**
    * Checks whether an arrow is a monomorphism.
    *
    * @param f an arrow to check
    * @return true iff f is a monomorphism
    */
  def isMonomorphism(f: A): Boolean = {
    val iterable = for (g <- arrows if follows(f, g);
                        h <- arrows if follows(f, h) &&
      equalizes(g, h)(f)) yield {
      g == h
    }

    iterable forall (x => x)
  }

  /**
    * Checks whether an arrow is an epimorphism.
    *
    * @param f an arrow to check
    * @return true iff f is an epimorphism
    */
  def isEpimorphism(f: A): Boolean = {
    val iterable = for (g <- arrows if follows(g, f);
                        h <- arrows if follows(h, f) &&
      coequalizes(g, h)(f)) yield {
      g == h
    }

    iterable forall (x => x)
  }

  def isUnique[T](seq: Iterable[T]): Boolean = isSingleton(seq)

  /**
    * Builds a predicate that checks whether an arrow h: B -> A is such that
    * px o h = qx and py o h = qy
    * where qx: B -> X, qy: B -> Y, px: A -> X, py: A -> Y.
    *
    * @param q factoring pair of arrows
    * @param p factored pair of arrows
    * @return the specified predicate.
    */
  def factorsOnLeft(p: (A, A), q: (A, A)): A => Boolean = (h: A) => {
    val (px, py) = p
    val (qx, qy) = q
    sameDomain(h, qx) && sameDomain(h, qy) &&
      follows(px, h) && follows(py, h) &&
      sameCodomain(px, qx) && sameCodomain(py, qy) &&
      (m(h, px) contains qx) && (m(h, py) contains qy)
  }

  /**
    * Builds a predicate that checks whether an arrow h: A -> B is such that
    * h o px = qx and h o py = qy for q = (qx, qy), and p = (px, py)
    * where qx: X -> B, qy: Y -> B, px: X -> A, py: Y -> A.
    *
    * @param q factoring pair of arrows
    * @param p factored pair of arrows
    * @return the predicate described above.
    */
  def factorsOnRight(p: (A, A), q: (A, A)): A => Boolean = (h: A) => {
    val (px, py) = p
    val (qx, qy) = q
    sameDomain(px, qx) && sameDomain(py, qy) &&
      follows(h, px) && follows(h, py) &&
      sameCodomain(h, qx) && sameCodomain(h, qy) &&
      (m(px, h) contains qx) && (m(py, h) contains qy)
  }

  /**
    * Builds a predicate that checks if arrow g: y -> z
    * uniquely factors on the left the arrow f: x -> z - that is,
    * there is just one h: x -> y such that f = g o h.
    *
    * @param f arrow being factored
    * @return the specified predicate
    */
  def factorsUniquelyOnLeft(f: A): A => Boolean =
    (g: A) => {
      sameCodomain(g, f) &&
        isUnique(hom(d0(f), d0(g)).filter(m(_, g) contains f))
    }

  /**
    * Builds a predicate that checks if arrow g: x -> y
    * uniquely factors on the right the arrow f: x -> z - that is,
    * there is just one h: y -> z such that f = h o g.
    *
    * @param f factored arrow
    * @return the specified predicate
    */
  def factorsUniquelyOnRight(f: A): A => Boolean =
    (g: A) => {
      sameDomain(g, f) &&
        isUnique(hom(d1(g), d1(f)).filter(m(g, _) contains f))
    }

  /**
    * Builds a predicate that checks whether an arrow equalizes two other arrows,
    * that is, whether f o h = g o h  for a given arrow h.
    *
    * @param f first arrow
    * @param g second arrow
    * @return a predicate defined on arrows.
    */
  def equalizes(f: A, g: A): A => Boolean = {
    h: A => areParallel(f, g) && follows(f, h) && (m(h, f) == m(h, g))
  }

  /**
    * Checks if arrow h coequalizes arrows f and g (that is, whether h o f == h o g).
    *
    * @param f first arrow
    * @param g second arrow
    * @return true iff h o f == h o g
    */
  def coequalizes(f: A, g: A): A => Boolean = {
    h: A => areParallel(f, g) && follows(h, f) && (m(f, h) == m(g, h))
  }

  /**
    * Builds a set of all arrows that equalize f: A -> B and g: A -> B, that is,
    * such arrows h: X -> A that f o h = g o h.
    *
    * @param f first arrow
    * @param g second arrow
    * @return an Iterable of arrows that equalize f and g
    */
  def allEqualizingArrows(f: A, g: A): Iterable[A] = arrows filter equalizes(f, g)

  /**
    * Builds a predicate that checks
    * if an arrow is an equalizer of the other two arrows.
    *
    * @param f first arrow
    * @param g second arrow
    * @return a predicate that checks if an arrow is an equalizer of f and g
    */
  def isEqualizer(f: A, g: A): A => Boolean = {
    h: A =>
      areParallel(f, g) &&
        equalizes(f, g)(h) &&
        allEqualizingArrows(f, g).forall(factorsUniquelyOnLeft(h))
  }

  /**
    * Builds an equalizer arrow for a parallel pair of arrows.
    *
    * @param f first arrow
    * @param g second arrow
    * @return an equalizer arrow, wrapped in Option
    */
  def equalizer(f: A, g: A): Option[A] = arrows find isEqualizer(f, g)

  /**
    * Builds a set of all arrows that coequalize f: A -> B and g: A -> B, that is,
    * such arrows h: B -> X that h o f = h o g.
    *
    * @param f first arrow
    * @param g second arrow
    * @return an Iterable of arrows that coequalize f and g
    */
  def allCoequalizingArrows(f: A, g: A): Iterable[A] = arrows filter coequalizes(f, g)

  /**
    * Builds a predicate that checks if an arrow is a coequalizer of the other two arrows.
    *
    * @param f first arrow
    * @param g second arrow
    * @return true if it is a coequalizer
    */
  def isCoequalizer(f: A, g: A): A => Boolean = {
    require(areParallel(f, g))
    h: A =>
      coequalizes(f, g)(h) &&
        (allCoequalizingArrows(f, g) forall factorsUniquelyOnRight(h))
  }

  /**
    * Builds a coequalizer arrow for a parallel pair of arrows.
    *
    * @param f first arrow
    * @param g second arrow
    * @return a coequalizer arrow, if one exists, null othrewise
    */
  def coequalizer(f: A, g: A): Option[A] = arrows find isCoequalizer(f, g)

  /**
    * Calculates a coequalizer of a collection of parallel arrows.
    * Since the collection may be empty, should provide the codomain.
    *
    * @param arrows the arrows, all of which shold be coequalized
    * @return a coequalizer arrow
    */
  def coequalizer(arrows: Iterable[A]): Option[A] = {
    throw new UnsupportedOperationException("to be implemented later, maybe")
  }

  /**
    * Builds a predicate that checks if a pair of arrows p = (px, py) : A -> X x Y
    * factors uniquely a pair q = (qx, qy): B -> X x Y on the right,
    * that is, if there exists a unique arrow h: B -> A such that qx = px o h and qy = py o h.
    *
    * @return true if p factors q uniquely on the right
    */
  def factorsUniquelyOnRight(px: A, py: A): Tuple2[A, A] => Boolean =
    (q: (A, A)) => {
      val (qx, qy) = q
      d1(px) == d1(qx) &&
      d1(py) == d1(qy) &&
      isUnique(hom(d0(qx), d0(px)).filter((h: A) => (m(h, px) contains qx) && (m(h, py) contains qy)))
    }

  /**
    * Builds a predicate that checks if a pair of arrows p = (px, py), where
    * px: X -> A, py: Y -> A, factors uniquely a pair q = (qx, qy)
    * (where qx: X -> B, qy: Y -> B) on the left,
    * that is, if there exists a unique arrow h: A -> B
    * such that qx = h o px and qy = h o py.
    *
    * @return true if q factors p uniquely on the left
    */
  def factorsUniquelyOnLeft(f: A, g: A): Tuple2[A, A] => Boolean =
    (q: (A, A)) => {
      val (qx, qy) = q
      isUnique(hom(d1(f), d1(qx)).filter(factorsOnRight((f, g), q)))
    }

  /**
    * Builds a set of all pairs (px, py) of arrows that start at the same domain and end
    * at d0(f) and d0(g), equalizing them: f o px = g o py, that is, making the square
    * <pre>
    * py
    * U -----> Y
    * |        |
    * px|        | g
    * |        |
    * v        v
    * X -----> Z
    * f
    * </pre>
    * commutative.
    *
    * @param f first arrow
    * @param g second arrow
    * @return the set of all such pairs of arrows
    */
  def pairsEqualizing(f: A, g: A): Set[(A, A)] = {
    setOf(
      product2[A, A](arrows, arrows).
        filter(p => {
          val (px, py) = p
          sameDomain(px, py) &&
            follows(f, px) &&
            follows(g, py) &&
            m(px, f) == m(py, g)
        }
        )
    )
  }

  /**
    * Builds a set of all pairs (qx, qy) of arrows that end at the same codomain and start
    * at d1(f) and d1(g), coequalizing them: m(f, qx) = m(g, qy), making the square
    * <pre>
    * g
    * Z -----> Y
    * |        |
    * f|        | qy
    * |        |
    * v        v
    * X -----> U
    * qx
    * </pre>
    * commutative.
    *
    * @param f first arrow
    * @param g second arrow
    * @return an iterable of all such pairs of arrows
    */
  def pairsCoequalizing(f: A, g: A): Set[(A, A)] = setOf(
    product2(arrows, arrows).
      filter(q => {
        val (qx, qy) = q
        sameCodomain(qx, qy) &&
          follows(qx, f) &&
          follows(qy, g) &&
          m(f, qx) == m(g, qy)
      }
      )
  )

  /**
    * Builds a set of all arrows to x and y (respectively) that start at the same object.
    *
    * @param x first object
    * @param y second object
    * @return a set of pairs of arrows with the same domain, ending at x and y.
    */
  def pairsWithTheSameDomain(x: O, y: O): Set[(A, A)] = setOf(
    Sets.product2(arrows, arrows).
      filter(p => {
        val (px, py) = p
        sameDomain(px, py) &&
          d1(px) == x &&
          d1(py) == y
      }
      )
  )

  /**
    * Builds a set of all arrows that start at x and y, respectively, and end at the same object.
    *
    * @param x first object
    * @param y second object
    * @return a set of pairs of arrows with the same codomain, starting at x and y.
    */
  def pairsWithTheSameCodomain(x: O, y: O): Set[(A, A)] = setOf(
    Sets.product2(arrows, arrows).
      filter(p => {
        val (px, py) = p
        sameCodomain(px, py) &&
          d0(px) == x &&
          d0(py) == y
      }
      )
  )

  /**
    * Checks if p = (px, py) is a Cartesian product of objects x and y.
    *
    * @param x first object
    * @param y second object
    * @return true if this is a cartesian product
    */
  def isProduct(x: O, y: O): Tuple2[A, A] => Boolean = (p: (A, A)) => {
    val (px, py) = p
    val prod = d0(px)
    d0(py) == prod &&
      d1(px) == x &&
      d1(py) == y &&
      pairsWithTheSameDomain(x, y).forall(factorsUniquelyOnRight(px, py))
  }

  /**
    * Builds a Cartesian product of two objects, if it exists. Returns null otherwise.
    * The product is represented as a pair of projections from the product object to the
    * two which are being multiplied.
    *
    * @param x first object
    * @param y second object
    * @return a pair of arrows from product object to x and y, or null if none exists.
    */
  def product(x: O, y: O): Option[(A, A)] = Sets.product2(arrows, arrows).find(isProduct(x, y))

  /**
    * Checks if i = (ix, iy) is a union of objects x and y.
    *
    * @param x first object
    * @param y second object
    * @return true if this is a union
    */
  def isUnion(x: O, y: O): Tuple2[A, A] => Boolean = (i: (A, A)) => {
    val (ix, iy) = i
    d0(ix) == x && d0(iy) == y &&
      pairsWithTheSameCodomain(x, y).forall(factorsUniquelyOnLeft(ix, iy))
  }

  /**
    * Builds a union of two objects, if it exists. Returns null otherwise.
    * The union is represented as a pair of insertions of the two objects into their union
    *
    * @param x first object
    * @param y second object
    * @return a pair of arrows from a and b to their union, or null if none exists.
    */
  def union(x: O, y: O): Option[(A, A)] = Sets.product2(arrows, arrows).find(isUnion(x, y))

  /**
    * Checks if p = (pa, pb) is a pullback of arrows f and g.
    *
    * @param f first arrow
    * @param g second arrow
    * @return true if this is a pullback
    */
  def isPullback(f: A, g: A): Tuple2[A, A] => Boolean = (p: (A, A)) => {
    val (px, py) = p
    follows(f, px) && follows(g, py) &&
      m(px, f) == m(py, g) &&
      pairsEqualizing(f, g).forall(factorsUniquelyOnRight(px, py))
  }

  /**
    * Builds a pullback of two arrows, if it exists. Returns null otherwise.
    * The pullback is represented as a pair of projections from the pullback object to the
    * domains of the two arrows.
    *
    * @param f first arrows
    * @param g second arrow
    * @return a pair of arrows from pullback object to d0(f) and d0(g), or null if none exists.
    */
  def pullback(f: A, g: A): Option[(A, A)] = {
    require(sameCodomain(f, g), s"Codomains of $f and $g should be the same")
    Sets.product2(arrows, arrows).find(isPullback(f, g))
  }

  /**
    * Checks if p = (pa, pb) is a pushout of arrows f and g.
    *
    * @param f first arrow
    * @param g second arrow
    * @return true if this is a pushout
    */
  def isPushout(f: A, g: A): Tuple2[A, A] => Boolean = (p: (A, A)) => {
    val (px, py) = p
    val pushoutObject = d1(px)
    d1(py) == pushoutObject &&
      follows(px, f) &&
      follows(py, g) &&
      m(f, px) == m(g, py) &&
      pairsCoequalizing(f, g).forall(factorsUniquelyOnLeft(px, py))
  }

  /**
    * Builds a pushout of two arrows, if it exists. Returns null otherwise.
    * The pushout is represented as a pair of coprojections from the codomains of the two arrows
    * to the pushout object.
    *
    * @param f first arrows
    * @param g second arrow
    * @return a pair of arrows from d1(f) and d1(g) to the pushout object, or null if none exists.
    */
  def pushout(f: A, g: A): Option[(A, A)] = {
    require(sameDomain(f, g), "Domains should be the same")
    Sets.product2(arrows, arrows).find(isPushout(f, g))
  }

  /**
    * Checks if a given object (candidate) is a terminal object (aka unit).
    * Terminal object is the one which has just one arrow from every other object.
    */
  def isTerminal(t: O): Boolean = objects.forall((x: O) => isUnique(hom(x, t)))

  lazy val terminal: Option[O] = objects.find(isTerminal)

  /**
    * Checks if a given object (candidate) is an initial object (aka zero).
    * Initial object is the one which has just one arrow to every other object.
    */
  def isInitial(i: O): Boolean = objects.forall((x: O) => isUnique(hom(i, x)))

  lazy val initial: Option[O] = objects.find(isInitial)

  /**
    * an iterable of initial objects as defined
    */
  lazy val allRootObjects_byDefinition: Set[O] = objects filter (
    x => arrows filter (d1(_) == x) forall (d0(_) == x)
    )

  /**
    * a cheap alternative for the iterable (actually, a set) of initial objects
    */
  lazy val allRootObjects_programmersShortcut: Set[O] = {
    val wrongStuff = arrows filter (f => !isEndomorphism(f)) map d1
    Set() ++ objects -- wrongStuff // need this trick because objects is strictly immutable
  }

  /**
    * An iterable of all objects that do not have any non-endomorphic arrows pointing at them.
    * Constructively, these are all such objects that if an arrow ends at such an object, it is an endomophism.
    * Since producing a lazy set is too heavy, I just build it in an old-fashion way.
    */
  lazy val allRootObjects: Set[O] = allRootObjects_programmersShortcut

  /**
    * A set of all arrows that originate at initial objects (see allRootObjects)
    */
  lazy val arrowsFromRootObjects: Set[A] = arrows filter (allRootObjects contains d0(_))

  /**
    * Given a set of object and a set of arrows, build a map that maps each object to
    * a set of arrows starting at it.
    *
    * @param objects objects for which to build the bundles.
    * @param arrows  arrows that participate in the bundles.
    * @return a map.
    */
  def buildBundles(objects: Set[O], arrows: Set[A]): Map[O, Set[A]] = {
    val mor = SetMorphism(arrows, objects, d0).revert.function
    objects.map(o => o -> mor(o)).toMap.withDefaultValue(Set.empty[A])
  }

  /**
    * Builds a degree object (X*X... n times) for a given object.
    *
    *
    * @param x the source object
    * @param n degree to which to raise object x
    * @return x^n^ and its projections to x
    *         TODO(vpatryshev): write good unitests
    */
  def degree(x: O, n: Int): Option[(O, List[A])] = {
    if (n < 0)
      None
    else if (n == 0) {
      terminal map (x => (x, List()))
    }
    else {
      degree(x, n - 1) flatMap (
        value => {
          val (x_n_1, previous_projections) = value
          product(x, x_n_1) flatMap (
            xn => {
              val (p1, p_n_1) = xn
              val projections = p1 :: previous_projections map (m(p_n_1, _))
              Some((d0(p1), projections collect { case Some(f) => f}))
            }
            )
        }
        )
    }
  }

  /**
    * Creates an opposite category from this one.
    * That is, all arrows are inverted.
    *
    * @return this<sup>op</sup>
    */
  def op: Category[O, A] = {
    Category[O, A](super.unary_~, unit, (f: A, g: A) => m(g, f))
  }
}

trait CategoryFactory {
  /**
    * Builds a category out of a segment of integers between 0 and n (not included).
    *
    * @param n number of elements
    * @return a new category
    */
  def segment(n: Int): Category[Int, (Int, Int)] = apply(PoSet.range(0, n, 1))

  /**
    * Builds a discrete category on a given set of objects.
    *
    * @tparam T object type
    * @param objects set of this category's objects
    * @return the category
    */
  def apply[T](objects: Set[T]): Category[T, T] = Category(Graph(objects))

  /**
    * Builds a category given a graph, composition table, and a list of unit arrows.
    *
    * @tparam O type of objects
    * @tparam A type of arrows
    * @param g           the graph on which we are to create a category
    * @param units       maps objects to unit arrows
    * @param composition defines composition
    * @return a category built based on the data above
    */
  def apply[O, A](
                   g: Graph[O, A],
                   units: O => A,
                   composition: (A, A) => Option[A]): Category[O, A] =
    new Category[O, A](g) {
      lazy val unit: O => A = units
      lazy val m: (A, A) => Option[A] = composition
    }

  /**
    * Creates an instance of Category given a graph and arrow composition table
    *
    * @tparam T graph element and arrow type (must be the same)
    * @param graph       the underlying graph
    * @param composition arrows composition table
    * @return new category
    */

  def apply[T](
                graph: Graph[T, T],
                composition: (T, T) => Option[T]): Category[T, T] = {
    val isUnit = (f: T) => graph.nodes contains f
    val m = (f: T, g: T) =>
      if (isUnit(f)) Some(g) else if (isUnit(g)) Some(f) else composition(f, g)
    val g = addUnitsToGraph(graph)
    val unit = (x: T) => x

    Category[T, T](g, unit, m)
  }

  def composablePairs[O, A](graph: Graph[O, A]): Iterable[(A, A)] = {
    for (f <- graph.arrows; g <- graph.arrows if graph.follows(g, f)) yield (f, g)
  }

  private def addUnitsToGraph[T](graph: Graph[T, T]) = {
    val nodes = graph.nodes.asInstanceOf[Set[T]] // this and the next casting is to cover up a weird bug somewhere in scala
    val allArrows: Set[T] = nodes ++ graph.arrows
    val isUnit = (f: T) => graph.nodes contains f
    val d0 = (f: T) => if (isUnit(f)) f else graph.d0(f)
    val d1 = (f: T) => if (isUnit(f)) f else graph.d1(f)
    Graph(graph.nodes, allArrows, d0, d1)
  }

  /**
    * Creates an instance of Category given a graph, when no composition is required
    * TODO: do something about this fake composition!!!
    * 
    * @tparam T graph element and arrow type (must be the same)
    * @param g the underlying graph, with no units
    * @return new category
    */
  def apply[T](g: Graph[T, T]): Category[T, T] = apply(g, (f: T, g: T) => Some(f)) // map is meaningless here

  /**
    * Creates a new instance of of category, given objects, arrows, units, and composition table.
    *
    * @tparam O object type
    * @tparam A arrow type
    * @param objects     category's objects
    * @param d0          maps arrows to domains
    * @param d1          maps arrows to codomains
    * @param units       maps objects to unit arrows
    * @param composition composition table
    * @return a new category
    */
  def apply[O, A](
                   objects: Set[O],
                   arrows: Set[A],
                   d0: A => O,
                   d1: A => O,
                   units: O => A,
                   composition: (A, A) => Option[A]): Category[O, A] = {
    val graph = Graph(objects, arrows, d0, d1)
    Category(graph, units, composition)
  }

  /**
    * This method helps fill in obvious choices for arrows composition.
    * Case 1. There's an arrow f:a->b, and an arrow g:b->c; and there's just one arrow h:a->c.
    * What would be the composition of f and g? h is the only choice.
    * <p/>
    * Case 2. h o (g o f) = k; what is (h o g) o f? It is k. and vice versa.
    *
    * @param graph             - the graph of this category
    * @param compositionSource partially filled composition table
    */
  private def fillCompositionTable[O, A](graph: Graph[A, A], compositionSource: Map[(A, A), A]): Map[(A, A), A] = {
    // First, add units
    val addedUnits = (compositionSource /: graph.arrows) ((m, f) => m + ((graph.d0(f), f) -> f) + ((f, graph.d1(f)) -> f))

    // Second, add unique solutions
    def candidates(f: A, g: A) = graph.hom(graph.d0(f), graph.d1(g))

    def hasUniqueCandidate(f: A, g: A) = {
      val iterator = candidates(f, g).iterator
      iterator.hasNext && ! {
        iterator.next; iterator.hasNext
      }
    }

    def candidate(f: A, g: A) = candidates(f, g).iterator.next

    val pairsToScan = composablePairs(graph) filter (p => {
      val (f, g) = p
      hasUniqueCandidate(f, g)
    })

    val addedUniqueSolutions: Map[(A, A), A] = (addedUnits /: pairsToScan) {
      (m, p) => {
        val (f, g) = p
        m + ((f, g) -> candidate(f, g))
      }
    }

    val triplesToScan = for {
      f <- graph.arrows
      g <- graph.arrows if compositionSource.contains(f, g)
      h <- graph.arrows if compositionSource.contains(g, h)
    } yield (f, g, h)


    val addedDeducedCompositions: Map[(A, A), A] = (addedUniqueSolutions /: triplesToScan) {
      (m, t) => {
        val (f, g, h) = t
        val gf = m((f, g))
        val hg = m((g, h))
        if ((m contains(gf, h)) && !(m contains(f, hg))) {
          m + ((f, hg) -> m((gf, h)))
        } else if ((m contains(f, hg)) && !(m contains(gf, h))) {
          m + ((gf, h) -> m((f, hg)))
        } else {
          m
        }
      }
    }

    addedDeducedCompositions
  }

  /**
    * Builds a category given a limited (but sufficient) amount of data.
    * Objects have the same name as their units.
    *
    * @tparam T arrow and node type
    * @param graph             the graph
    * @param compositionSource source table of arrows composition (may be incomplete)
    * @return a newly-built category
    */
  def apply[T](
                graph: Graph[T, T],
                compositionSource: Map[(T, T), T]): Category[T, T] = {
    val graphWithUnits = addUnitsToGraph(graph)

    val composition = fillCompositionTable(graphWithUnits, compositionSource)

    def compositionFunction(f: T, g: T): Option[T] = composition.get((f, g))
   
    apply(graphWithUnits, idMap(graph.nodes), (f: T, g: T) => compositionFunction(f, g))
  }

  /**
    * Builds a category given a limited (but sufficient) amount of data.
    * Objects have the same name as their units.
    *
    * @tparam T arrow type
    * @param units             set of units (and objects)
    * @param domain            maps arrows to domains
    * @param codomain          maps arrows to codomain
    * @param compositionSource source table of arrows composition (may be incomplete)
    * @return a newly-built category
    */
  def apply[T](
                units: Set[T],
                domain: Map[T, T],
                codomain: Map[T, T],
                compositionSource: Map[(T, T), T]): Category[T, T] = {
    val g = Graph(units, domain.keySet, domain, codomain)
    apply(g, compositionSource)
  }

  /**
    * Builds a category out of a poset. Arrows are pairs (x,y) where x <= y.
    *
    * @tparam T poset element type
    * @param poset original poset
    * @return category based on he poset
    */
  def apply[T](poset: PoSet[T]): Category[T, (T, T)] = apply(Graph(poset), (x: T) => (x, x), (f: (T, T), g: (T, T)) => Option((f, g)) collect {
    case (first, second) if first._2 == second._1 => (first._1, second._2)
  })

  class Parser extends Graph.Parser {
    def category: Parser[Category[String, String]] = "(" ~ graph ~ "," ~ multTable ~ ")" ^^ { case "(" ~ g ~ "," ~ m ~ ")" => Category(g, m) }

    def multTable: Parser[Map[(String, String), String]] = "{" ~ repsep(multiplication, ",") ~ "}" ^^ { case "{" ~ m ~ "}" => Map() ++ m }

    def multiplication: Parser[((String, String), String)] = member ~ "o" ~ member ~ "=" ~ member ^^ { case f ~ "o" ~ g ~ "=" ~ h => ((f, g), h) }

    override def all: Parser[Graph[String, String]] = "(" ~ graph ~ ")" ^^ { case "(" ~ g ~ ")" => g }

    override def read(input: CharSequence): Category[String, String] =
      parseAll(category, input).get

    override def read(input: Reader): Category[String, String] =
      parseAll(category, input).get
  }

  /**
    * Factory method. Parses a string and builds a category from it.
    *
    * @param input input to parse
    * @return the category
    */
  def apply(input: Reader): Category[String, String] = (new Parser).read(input)

  /**
    * Factory method. Parses a string and builds a category from it.
    *
    * @param input the string to parse
    * @return the category
    */
  def apply(input: CharSequence): Category[String, String] = (new Parser).read(input)
}

object Category extends CategoryFactory {

  /**
    * Empty category
    */
  lazy val _0_ : Category[Int, (Int, Int)] = segment(0)

  /**
    * Singleton category
    */
  lazy val _1_ : Category[Int, (Int, Int)] = segment(1)

  /**
    * Discrete 2-object category
    */
  lazy val _1plus1_ = Category(Set("a", "b"))

  /**
    * Category <b>2</b>: 2 objects linearly ordered
    */
  lazy val _2_ : Category[Int, (Int, Int)] = segment(2)

  /**
    * Category <b>3</b>: 3 objects linearly ordered
    */
  lazy val _3_ : Category[Int, (Int, Int)] = segment(3)

  /**
    * Category <b>4</b>: 4 objects linearly ordered
    */
  lazy val _4_ : Category[Int, (Int, Int)] = segment(4)

  /**
    * Category with 2 objects and 2 parallel arrows from one to another
    */
  lazy val ParallelPair = Category("({0, 1}, {a:0->1, b:0->1}, {})")

  /**
    * Category <b>Z2</2> - a two-element monoid
    */
  lazy val Z2 = Category("({1}, {1: 1 -> 1, a: 1 -> 1}, {1 o 1 = 1, 1 o a = a, a o 1 = a, a o a = 1})")

  /**
    * "Split Monomorphism" category (see http://en.wikipedia.org/wiki/Morphism)
    * Two objects, and a split monomorphism from a to b
    */
  lazy val SplitMono =
    Category("({a,b}, {ab: a -> b, ba: b -> a, bb: b -> b}, {ba o ab = bb, ab o ba = a, ab o bb = ab, bb o ba = ba, bb o bb = bb})")
  
  /**
    * Commutative square category
    */
  lazy val Square = Category("({a,b,c,d}, {ab: a -> b, ac: a -> c, bd: b -> d, cd: c -> d, ad: a -> d}, {bd o ab = ad, cd o ac = ad})")

  /**
    * Pullback category: a -> c <- b
    */
  lazy val Pullback = Category("({a,b,c}, {ac: a -> c, bc: b -> c}, {})")

  /**
    * Pushout category: b <- a -> c
    */
  lazy val Pushout = Category("({a,b,c}, {ab: a -> b, ac: a -> c}, {})")

  /**
    * Sample W-shaped category: a -> b <- c -> d <- e
    */
  lazy val W = Category("({a,b,c,d,e}, {ab: a -> b, cb: c -> b, cd: c -> d, ed: e -> d}, {})")

  /**
    * Sample M-shaped category: a <- b -> c <- d -> e
    */
  lazy val M = Category("({a,b,c,d,e}, {ba: b -> a, bc: b -> c, dc: d -> c, de: d -> e}, {})")

  lazy val NaturalNumbers: Category[BigInt, (BigInt, BigInt)] =
    Category(PoSet.ofNaturalNumbers)
  
  lazy val KnownCategories = Set(_0_, _1_, _2_, _3_, _4_, _1plus1_,
    M, ParallelPair, Pullback, Pushout, SplitMono, Square, W, Z2, NaturalNumbers)
}
