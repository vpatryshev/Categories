package math.cat

import java.io.Reader

import math.cat.Category.Cat
import math.sets.Sets._
import math.sets.{PoSet, Sets}
import scalakittens.Result._
import scalakittens.{Good, Result}

/**
  * Category class, and the accompanying object.
  */
abstract class Category[Obj, A](override val graph: Graph)
  extends CategoryData[Obj, A](graph) {

  implicit def obj(x: Any): O = x match {
    case _ if objects contains x.asInstanceOf[O] => x.asInstanceOf[O]
  }

  implicitly[Obj => O](obj)

  lazy val terminal: Option[O] = objects.find(isTerminal)
  lazy val initial: Option[O] = objects.find(isInitial)
  /**
    * an iterable of initial objects as defined
    */
  lazy val allRootObjects_byDefinition: Objects = objects filter {
    arrowsEndingAt(_) forall isEndomorphism
  }
  /**
    * a cheap alternative for the iterable (actually, a set) of initial objects
    */
  lazy val allRootObjects_programmersShortcut: Objects = {
    val wrongStuff = arrows filter (f => !isEndomorphism(f)) map d1
    objects -- wrongStuff
  }
  /**
    * An iterable of all objects that do not have any non-endomorphic arrows pointing at them.
    * Constructively, these are all such objects that if an arrow ends at such an object, it is an endomophism.
    * Since producing a lazy set is too heavy, I just build it in an old-fashion way.
    */
  lazy val allRootObjects: Objects = allRootObjects_programmersShortcut
  /**
    * A set of all arrows that originate at initial objects (see allRootObjects)
    */
  lazy val arrowsFromRootObjects: Set[Arrow] = arrows filter (allRootObjects contains d0(_))
  /**
    * Creates an opposite category from this one.
    * That is, all arrows are inverted.
    *
    * @return this<sup>op</sup>
    */
  lazy val op: Category[graph.Node, graph.Arrow] = {
    val src = this
    new Category[graph.Node, graph.Arrow](~graph) {
      override def id(o: O): Arrow = src.id(o.asInstanceOf[src.O]).asInstanceOf[Arrow]

      override def m(f: Arrow, g: Arrow): Option[Arrow] =
        src.m(f.asInstanceOf[src.Arrow], g.asInstanceOf[src.Arrow]).asInstanceOf[Option[Arrow]]
    }
  }
  private[cat] lazy val listOfRootObjects = allRootObjects.toList.sortBy(_.toString)

  def isFinite: Boolean = Sets.isFinite(objects) && Sets.isFinite(arrows)

  def compositions: Iterable[(Arrow, Arrow, Arrow)] =
    for {f <- arrows
         g <- arrows
         h <- m(f, g)} yield (f, g, h)

  def isIdentity(a: Arrow): Boolean = a == id(d0(a))

  //@deprecated("category theory is not equational")
  // cannot elimitate this: too many tests rely on comparing categories...
  override def equals(x: Any): Boolean = // error ("category theory is not equational")
  {
    x match {
      case other: Category[_, _] => other.asInstanceOf[Category[O, Arrow]].equal(this)
      case _ => false
    }
  }

  // @deprecated("is category theory equational? does not seem like it is...")
  private def equal(that: Category[_, _]): Boolean = {
    val objectsEqual = this.objects == that.objects && this.arrows == that.arrows
    val idsEqual = objectsEqual && (objects forall { x => id(x) == that.id(x.asInstanceOf[that.O]) })

    val isEqual = idsEqual &&
      (arrows forall { f =>
        arrows forall { g =>
          !follows(f, g) ||
            this.m(f, g) == that.m(f.asInstanceOf[that.Arrow], g.asInstanceOf[that.Arrow])
        }
      })

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
    (composablePairs collect { case (first, second) =>
      s"$second o $first = ${m(first, second).get}"
    }).mkString(", ") + "})"

  def composablePairs: Iterable[(Arrow, Arrow)] = Category.composablePairs(this)

  /**
    * Produces a collection of arrows from x to y.
    *
    * @param from first object
    * @param to   second object
    * @return the set of all arrows from x to y
    */
  def hom(from: O, to: O): Arrows = setOf(arrows filter ((f: Arrow) => (d0(f) == from) && (d1(f) == to)))


  /**
    * Checks whether an arrow is an isomorphism.
    *
    * @param f an arrow to check
    * @return true iff f is an isomorphism
    */
  def isIsomorphism(f: Arrow): Boolean = inverse(anArrow(f)).isDefined

  /**
    * Returnes an inverse arrow.
    *
    * @param f an arrow for which we are looking an inverse
    * @return inverse arrow
    */
  def inverse(f: Arrow): Option[Arrow] = arrowsBetween(d1(anArrow(f)), d0(f)) find (areInverse(f, _))

  def areInverse(f: Arrow, g: Arrow): Boolean =
    (m(anArrow(f), anArrow(g)) contains id(d0(f))) && (m(g, f) contains id(d0(g)))

  def isEndomorphism(f: Arrow): Boolean = d0(anArrow(f)) == d1(f)

  /**
    * Checks whether an arrow is a monomorphism.
    *
    * @param f an arrow to check
    * @return true iff f is a monomorphism
    */
  def isMonomorphism(f: Arrow): Boolean = {
    val iterable = for (g <- arrows if follows(f, g);
                        h <- arrows if follows(f, h) &&
      equalizes(g, h)(f)) yield {
      g == h
    }

    iterable forall (x => x)
  }

  /**
    * Builds a predicate that checks whether an arrow equalizes two other arrows,
    * that is, whether f o h = g o h  for a given arrow h.
    *
    * @param f first arrow
    * @param g second arrow
    * @return a predicate defined on arrows.
    */
  def equalizes(f: Arrow, g: Arrow): Arrow => Boolean = {
    h: Arrow => areParallel(f, g) && follows(f, h) && (m(h, f) == m(h, g))
  }

  /**
    * Checks whether an arrow is an epimorphism.
    *
    * @param f an arrow to check
    * @return true iff f is an epimorphism
    */
  def isEpimorphism(f: Arrow): Boolean = {
    val iterable = for (g <- arrows if follows(g, f);
                        h <- arrows if follows(h, f) &&
      coequalizes(g, h)(f)) yield {
      g == h
    }

    iterable forall (x => x)
  }

  /**
    * Builds a predicate that checks whether an arrow h: B -> A is such that
    * px o h = qx and py o h = qy
    * where qx: B -> X, qy: B -> Y, px: A -> X, py: A -> Y.
    *
    * @param q factoring pair of arrows
    * @param p factored pair of arrows
    * @return the specified predicate.
    */
  def factorsOnLeft(p: (Arrow, Arrow), q: (Arrow, Arrow)): Arrow => Boolean = (h: Arrow) => {
    val (px, py) = p
    val (qx, qy) = q
    sameDomain(h, qx) && sameDomain(h, qy) &&
      follows(px, h) && follows(py, h) &&
      sameCodomain(px, qx) && sameCodomain(py, qy) &&
      (m(h, px) contains qx) && (m(h, py) contains qy)
  }

  /**
    * Builds an equalizer arrow for a parallel pair of arrows.
    *
    * @param f first arrow
    * @param g second arrow
    * @return an equalizer arrow, wrapped in Option
    */
  def equalizer(f: Arrow, g: Arrow): Option[Arrow] = arrows find isEqualizer(f, g)

  /**
    * Builds a predicate that checks
    * if an arrow is an equalizer of the other two arrows.
    *
    * @param f first arrow
    * @param g second arrow
    * @return a predicate that checks if an arrow is an equalizer of f and g
    */
  def isEqualizer(f: Arrow, g: Arrow)(h: Arrow): Boolean =
    areParallel(f, g) &&
      equalizes(f, g)(h) &&
      allEqualizingArrows(f, g).forall(factorsUniquelyOnLeft(h))

  /**
    * Builds a predicate that checks if arrow g: y -> z
    * uniquely factors on the left the arrow f: x -> z - that is,
    * there is just one h: x -> y such that f = g o h.
    *
    * @param f arrow being factored
    * @return the specified predicate
    */
  def factorsUniquelyOnLeft(f: Arrow)(g: Arrow): Boolean =
    sameCodomain(g, f) &&
      existsUnique(arrowsBetween(d0(f), d0(g)), (h: Arrow) => m(h, g) contains f)

  /**
    * Builds a set of all arrows that equalize f: A -> B and g: A -> B, that is,
    * such arrows h: X -> A that f o h = g o h.
    *
    * @param f first arrow
    * @param g second arrow
    * @return an Iterable of arrows that equalize f and g
    */
  def allEqualizingArrows(f: Arrow, g: Arrow): Iterable[Arrow] = arrows filter equalizes(f, g)

  /**
    * Builds a coequalizer arrow for a parallel pair of arrows.
    *
    * @param f first arrow
    * @param g second arrow
    * @return a coequalizer arrow, if one exists, null othrewise
    */
  def coequalizer(f: Arrow, g: Arrow): Option[Arrow] = arrows find isCoequalizer(f, g)

  /**
    * Builds a predicate that checks if an arrow is a coequalizer of the other two arrows.
    *
    * @param f first arrow
    * @param g second arrow
    * @return true if it is a coequalizer
    */
  def isCoequalizer(f: Arrow, g: Arrow): Arrow => Boolean = {
    require(areParallel(f, g))
    h: Arrow =>
      coequalizes(f, g)(h) &&
        (allCoequalizingArrows(f, g) forall factorsUniquelyOnRight(h))
  }

  /**
    * Builds a predicate that checks if arrow g: x -> y
    * uniquely factors on the right the arrow f: x -> z - that is,
    * there is just one h: y -> z such that f = h o g.
    *
    * @param f factored arrow
    * @return the specified predicate
    */
  def factorsUniquelyOnRight(f: Arrow): Arrow => Boolean =
    (g: Arrow) => {
      sameDomain(g, f) &&
        isUnique(arrowsBetween(d1(g), d1(f)).filter(m(g, _) contains f))
    }

  /**
    * Builds a set of all arrows that coequalize f: A -> B and g: A -> B, that is,
    * such arrows h: B -> X that h o f = h o g.
    *
    * @param f first arrow
    * @param g second arrow
    * @return an Iterable of arrows that coequalize f and g
    */
  def allCoequalizingArrows(f: Arrow, g: Arrow): Iterable[Arrow] = arrows filter coequalizes(f, g)

  /**
    * Checks if arrow h coequalizes arrows f and g (that is, whether h o f == h o g).
    *
    * @param f first arrow
    * @param g second arrow
    * @return true iff h o f == h o g
    */
  def coequalizes(f: Arrow, g: Arrow): Arrow => Boolean = {
    h: Arrow => areParallel(f, g) && follows(h, f) && (m(f, h) == m(g, h))
  }

  /**
    * Calculates a coequalizer of a collection of parallel arrows.
    * Since the collection may be empty, should provide the codomain.
    *
    * @param arrows the arrows, all of which shold be coequalized
    * @return a coequalizer arrow
    */
  def coequalizer(arrows: Iterable[Arrow]): Option[Arrow] = {
    throw new UnsupportedOperationException("to be implemented later, maybe")
  }

  /**
    * Builds a set of all arrows to x and y (respectively) that start at the same object.
    *
    * @param x first object
    * @param y second object
    * @return a set of pairs of arrows with the same domain, ending at x and y.
    */
  def pairsWithTheSameDomain(x: O, y: O): Set[(Arrow, Arrow)] = setOf(
    product2(arrows, arrows).
      filter(p => {
        val (px, py) = p
        sameDomain(px, py) &&
          d1(px) == x &&
          d1(py) == y
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
  def isProduct(x: O, y: O): ((Arrow, Arrow)) => Boolean = {
    case (px, py) =>
      d0(anArrow(px)) == d0(anArrow(py)) &&
        d1(px) == x &&
        d1(py) == y &&
        pairsWithTheSameDomain(x, y).forall(factorUniquelyOnRight(px, py))
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
  def product(x: O, y: O): Option[(Arrow, Arrow)] =
    product2(arrows, arrows) find isProduct(x, y)

  /**
    * Builds a union of two objects, if it exists. Returns null otherwise.
    * The union is represented as a pair of insertions of the two objects into their union
    *
    * @param x first object
    * @param y second object
    * @return a pair of arrows from a and b to their union, or null if none exists.
    */
  def union(x: O, y: O): Option[(Arrow, Arrow)] =
    product2(arrows, arrows) find isUnion(x, y)

  /**
    * Checks if i = (ix, iy) is a union of objects x and y.
    *
    * @param x first object
    * @param y second object
    * @return true if this is a union
    */
  def isUnion(x: O, y: O): ((Arrow, Arrow)) => Boolean = (i: (Arrow, Arrow)) => {
    val (ix, iy) = i
    d0(anArrow(ix)) == x && d0(anArrow(iy)) == y &&
      pairsWithTheSameCodomain(x, y).forall(factorUniquelyOnLeft(ix, iy))
  }

  /**
    * Builds a set of all arrows that start at x and y, respectively, and end at the same object.
    *
    * @param x first object
    * @param y second object
    * @return a set of pairs of arrows with the same codomain, starting at x and y.
    */
  def pairsWithTheSameCodomain(x: O, y: O): Set[(Arrow, Arrow)] = setOf(
    product2(arrows, arrows) filter {
      case (px, py) =>
        sameCodomain(px, py) &&
          d0(px) == x &&
          d0(py) == y
    }
  )

  /**
    * Builds a pullback of two arrows, if it exists. Returns null otherwise.
    * The pullback is represented as a pair of projections from the pullback object to the
    * domains of the two arrows.
    *
    * @param f first arrows
    * @param g second arrow
    * @return a pair of arrows from pullback object to d0(f) and d0(g), or null if none exists.
    */
  def pullback(f: Arrow, g: Arrow): Option[(Arrow, Arrow)] = {
    require(sameCodomain(f, g), s"Codomains of $f and $g should be the same")
    product2(arrows, arrows).find(isPullback(f, g))
  }

  /**
    * Checks if p = (pa, pb) is a pullback of arrows f and g.
    *
    * @param f first arrow
    * @param g second arrow
    * @return true if this is a pullback
    */
  def isPullback(f: Arrow, g: Arrow): ((Arrow, Arrow)) => Boolean = (p: (Arrow, Arrow)) => {
    val (px, py) = p
    follows(f, px) && follows(g, py) &&
      m(px, f) == m(py, g) &&
      pairsEqualizing(f, g).forall(factorUniquelyOnRight(px, py))
  }

  /**
    * Builds a predicate that checks if a pair of arrows p = (px, py) : A -> X x Y
    * factors uniquely a pair q = (qx, qy): B -> X x Y on the right,
    * that is, if there exists a unique arrow h: B -> A such that qx = px o h and qy = py o h.
    *
    * @return true if p factors q uniquely on the right
    */
  def factorUniquelyOnRight(px: Arrow, py: Arrow): ((Arrow, Arrow)) => Boolean = {
    case (qx, qy) =>
      sameCodomain(px, qx) &&
        sameCodomain(py, qy) &&
        isUnique(arrowsBetween(d0(qx), d0(px)).filter((h: Arrow) => (m(h, px) contains qx) && (m(h, py) contains qy)))
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
  def pairsEqualizing(f: Arrow, g: Arrow): Set[(Arrow, Arrow)] = {
    setOf(
      product2[Arrow, Arrow](arrows, arrows).
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
    * Builds a pushout of two arrows, if it exists. Returns null otherwise.
    * The pushout is represented as a pair of coprojections from the codomains of the two arrows
    * to the pushout object.
    *
    * @param f first arrows
    * @param g second arrow
    * @return a pair of arrows from d1(f) and d1(g) to the pushout object, or null if none exists.
    */
  def pushout(f: Arrow, g: Arrow): Option[(Arrow, Arrow)] = {
    require(sameDomain(f, g), "Domains should be the same")
    product2(arrows, arrows).find(isPushout(f, g))
  }

  /**
    * Checks if p = (pa, pb) is a pushout of arrows f and g.
    *
    * @param f first arrow
    * @param g second arrow
    * @return true if this is a pushout
    */
  def isPushout(f: Arrow, g: Arrow): ((Arrow, Arrow)) => Boolean = (p: (Arrow, Arrow)) => {
    val (px, py) = p
    val pushoutObject = d1(px)
    d1(py) == pushoutObject &&
      follows(px, f) &&
      follows(py, g) &&
      m(f, px) == m(g, py) &&
      pairsCoequalizing(f, g).forall(factorUniquelyOnLeft(px, py))
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
  def factorUniquelyOnLeft(f: Arrow, g: Arrow): ((Arrow, Arrow)) => Boolean =
    (q: (Arrow, Arrow)) => {
      val (qx, qy) = q
      isUnique(arrowsBetween(d1(f), d1(qx)).filter(factorsOnRight((f, g), q)))
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
  def factorsOnRight(p: (Arrow, Arrow), q: (Arrow, Arrow)): Arrow => Boolean = (h: Arrow) => {
    val (px, py) = p
    val (qx, qy) = q
    sameDomain(px, qx) && sameDomain(py, qy) &&
      follows(h, px) && follows(h, py) &&
      sameCodomain(h, qx) && sameCodomain(h, qy) &&
      (m(px, h) contains qx) && (m(py, h) contains qy)
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
  def pairsCoequalizing(f: Arrow, g: Arrow): Set[(Arrow, Arrow)] = setOf(
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
    * Checks if a given object (candidate) is a terminal object (aka unit).
    * Terminal object is the one which has just one arrow from every other object.
    */
  def isTerminal(t: O): Boolean =
    objects.forall((x: O) => isUnique(arrowsBetween(x, t)))

  /**
    * Checks if a given object (candidate) is an initial object (aka zero).
    * Initial object is the one which has just one arrow to every other object.
    */
  def isInitial(i: O): Boolean = objects.forall((x: O) => isUnique(arrowsBetween(i, x)))

  /**
    * Given a set of objects and a set of arrows, build a map that maps each object to
    * a set of arrows starting at it.
    *
    * @param setOfObjects objects for which to build the bundles.
    * @param arrows  arrows that participate in the bundles.
    * @return a map.
    */
  def buildBundles(setOfObjects: Objects, arrows: Arrows): Map[O, Arrows] = {
    val badArrows: Arrows = arrows.filterNot(a => setOfObjects(d0(a)))

    require(badArrows.isEmpty, s"These arrows don't belong: ${badArrows.mkString(",")}")

    val mor = SetMorphism(arrows, setOfObjects, d0).revert.function
    setOfObjects.map(o => o -> mor(o)).toMap.withDefaultValue(Set.empty[Arrow])
  }

  /**
    * Builds a degree object (X*X... n times) for a given object.
    * The trick is, find an object that "is" the x we provided
    *
    * @param x the source object
    * @param n degree to which to raise object x
    * @return x^n^ and its projections to x
    */
  def degree(x: O, n: Int): Option[(O, List[Arrow])] = {
    n match {
      case neg if neg < 0 => None
      case 0 => terminal map (x => (x, List()))
      case 1 => Option((x, id(x) :: Nil))
      case _ =>
        degree(x, n - 1) flatMap (
          value => {
            val (x_n_1, previous_projections) = value
            val tentativeProduct = product(x, x_n_1)
            tentativeProduct flatMap { xn => {
              val (p1, p_n_1) = xn
              val projections = p1 :: previous_projections map (m(p_n_1, _))
              val res = Some((d0(p1), projections collect { case Some(f) => f }))
              res
            }
            }
          }
          )
    }
  }

  protected def deg(n: Int)(x: O): Option[(O, List[Arrow])] = {
    n match {
      case neg if neg < 0 => None
      case 0 => terminal map (x => (x, List()))
      case 1 => Option((x, id(x) :: Nil))
      case _ =>
        degree(x, n - 1) flatMap (
          value => {
            val (x_n_1, previous_projections) = value
            val tentativeProduct = product(x, x_n_1)
            tentativeProduct flatMap { xn => {
              val (p1, p_n_1) = xn
              val projections = p1 :: previous_projections map (m(p_n_1, _))
              val res = Some((d0(p1), projections collect { case Some(f) => f }))
              res
            }
            }
          }
          )
    }
  }

  private def arrowsEndingAt(x: O): Arrows =
    arrows filter { x == d1(_) }
}

private[cat] abstract class CategoryData[Obj, A](val graph: Graph) extends Graph {
  type O = Node
  type Objects = Set[O]

  def id(o: O): Arrow

  def m(f: Arrow, g: Arrow): Option[Arrow]

  override def validate: Result[CategoryData[Obj, A]] = {
    val objectsHaveIds = OKif(!finiteNodes) orElse {
      Result.traverse(objects map {
        x =>
          val ux = id(x)
          OKif(d0(ux) == x, s"Domain of id $ux should be $x") andAlso
            OKif(d1(ux) == x, s"Codomain of id $ux should be $x")
      })
    }

    val idsAreNeutral = OKif(!finiteArrows) orElse {
      Result.traverse(arrows map { f =>
        val u_f = m(id(d0(f)), f)
        val f_u = m(f, id(d1(f)))
        OKif(u_f contains f, s"Left unit law broken for ${id(d0(f))} and $f: got $u_f") andAlso
          OKif(f_u contains f, s"Right unit law broken for ${id(d1(f))} and $f: got $f_u")
      })
    }

    val compositionsAreDefined = idsAreNeutral andThen OKif(!finiteArrows) orElse {
      Result.traverse {
        for {
          f <- arrows
          g <- arrows
          h = m(f, g)
        } yield {
          if (follows(g, f)) {
            Result(h) orCommentTheError s"composition must be defined for $f and $g" flatMap { gf =>
              OKif(sameDomain(gf, f),
                s"Wrong composition $gf of $f and $g : its d0 is ${d0(gf)}, must be ${d0(f)}") andAlso
                OKif(sameCodomain(gf, g),
                  s"Wrong composition $gf of $f and $g: its d1 is ${d1(gf)}, must be ${d1(g)}")
            } returning()
          }
          else {
            OKif(h.isEmpty, s"Wrongly defined composition of $f and $g")
          }
        }
      }
    }

    val compositionIsAssociative = compositionsAreDefined andThen (OKif(!finiteArrows) orElse {
      Result.traverse {
        for {
          f <- arrows
          g <- arrows
          h <- arrows
          gf <- m(f, g)
          hg <- m(g, h)
        } yield {
          val h_gf = m(gf, h)
          val hg_f = m(f, hg)
          // the following is for debugging
          val f0 = "" + f + ""
          if (h_gf != hg_f) {
            println(s"$f, $g, $h, $gf, $hg, $h_gf, $hg_f")
          }
          OKif(h_gf == hg_f, s"Associativity broken for $f, $g and $h, got $h_gf vs $hg_f")
        }
      }
    })

    objectsHaveIds andAlso compositionIsAssociative returning this
  }

  def objects: Objects = nodes

  def nodes: Objects = graph.nodes.asInstanceOf[Objects]

  def arrows: Arrows = graph.arrows.asInstanceOf[Arrows]

  def d0(a: Arrow): O = graph.d0(a.asInstanceOf[graph.Arrow]).asInstanceOf[O]

  def d1(a: Arrow): O = graph.d1(a.asInstanceOf[graph.Arrow]).asInstanceOf[O]

}

private[cat] trait CategoryFactory {
  /**
    * Builds a category out of a segment of integers between 0 and n (not included).
    *
    * @param n number of elements
    * @return a new category
    */
  def segment(n: Int): Cat = {
    val numbers = fromPoset(PoSet.range(0, n, 1))
    val maybeSegment = convert2Cat(numbers)(
      _.toString,
      { case (a, b) => s"$a.$b" })
    maybeSegment.fold(identity, err => throw new InstantiationException(err.toString))
  }

  def convert2Cat[O, A](
    source: Category[O, A])(
    object2string: source.O => String = (_: source.O).toString,
    arrow2string: source.Arrow => String = (_: source.Arrow).toString): Result[Cat] = {
    val objectStrings = source.objects map (o => o -> object2string(o))
    val osMap = objectStrings toMap
    val soMap = objectStrings map (_.swap) toMap
    val arrowStrings = source.arrows map (a => a -> arrow2string(a))
    val asMap = arrowStrings toMap
    val saMap = arrowStrings map (_.swap) toMap
    val objects = soMap.keySet
    val arrows = saMap.keySet
    val d0 = (f: String) => osMap(source.d0(saMap(f)))
    val d1 = (f: String) => osMap(source.d1(saMap(f)))
    val ids = (o: String) => asMap(source.id(soMap(o)))
    val composition = (f: String, g: String) => source.m(saMap(f), saMap(g)) map asMap


    for {
      _ <- OKif(source.isFinite, "Need a finite category")
      _ <- OKif(osMap.size == objectStrings.size, "some objects have the same string repr")
      _ <- OKif(asMap.size == arrowStrings.size, "some arrows have the same string repr")
      c <- Category.build(objects, arrows, d0, d1, ids, composition)
    } yield c.asInstanceOf[Cat]
  }

  /**
    * Creates a new instance of of category, given objects, arrows, ids, and composition table.
    *
    * @tparam O object type
    * @tparam A arrow type
    * @param objects     category's objects
    * @param d0          maps arrows to domains
    * @param d1          maps arrows to codomains
    * @param ids         maps objects to identity arrows
    * @param composition composition table
    * @return a new category
    */
  def build[O, A](
    objects: Set[O],
    arrows: Set[A],
    d0: A => O,
    d1: A => O,
    ids: O => A,
    composition: (A, A) => Option[A]): Result[Category[O, A]] = {
    val graph: Result[Graph] = Graph.build(objects, arrows, d0, d1)
    graph flatMap { buildFromGraphWithIdentity(_, ids, composition) }
  }

  /**
    * Builds a category out of a poset. Arrows are pairs (x,y) where x <= y.
    *
    * @tparam T poset element type
    * @param poset original poset
    * @return category based on he poset
    */
  def fromPoset[T](poset: PoSet[T]): Category[T, (T, T)] = {
    new Category[T, (T, T)](Graph.ofPoset(poset)) {
      type Node = T
      type Arrow = (T, T)

      override def id(o: O): Arrow = (o, o).asInstanceOf[Arrow]

      override def m(f: Arrow, g: Arrow): Option[Arrow] = (f, g) match {
        case (f: (T, T), g: (T, T)) =>
          Option(f._1, g._2).filter(_ => f._2 == g._1).asInstanceOf[Option[Arrow]]
      }
    }
  }

  /**
    * Builds a discrete category on a given set of objects.
    *
    * @tparam T object type
    * @param objects set of this category's objects
    * @return the category
    */
  def discrete[T](objects: Set[T]): Category[T, T] = new Category[T, T](Graph.discrete[T](objects)) {
    override def id(obj: O): Arrow = obj.asInstanceOf[Arrow]

    override def m(f: Arrow, g: Arrow): Option[Arrow] = Option(f) filter (g ==) // everything is an identity
  }

  /**
    * Creates an instance of Category given a graph, when no composition is required
    * The method returns Bad if composition is required
    *
    * @tparam T graph element and arrow type (must be the same)
    * @param g the underlying graph, with no id arrows
    * @return new category
    */
  def fromGraph[T](g: Graph): Result[Category[T, T]] =
    build[T](g, (f: T, g: T) => None)

  /**
    * Creates an instance of Category given a graph and arrow composition table
    *
    * @tparam T graph element and arrow type (must be the same)
    * @param graph       the underlying graph
    * @param composition arrows composition table
    * @return new category
    */
  def build[T](graph: Graph,
    composition: (T, T) => Option[T]): Result[Category[T, T]] = {
    val isUnit = (f: T) => graph.nodes(f.asInstanceOf[graph.Node])
    val m = (f: T, g: T) =>
      if (isUnit(f)) Some(g) else if (isUnit(g)) Some(f) else composition(f, g)
    val g: Graph = addUnitsToGraph(graph)
    val id = (x: T) => x
    buildFromGraphWithIdentity(g, id, m)
  }

  /**
    * Builds a category given a limited (but sufficient) amount of data.
    * Objects have the same name as their identity arrows.
    *
    * @tparam T arrow type
    * @param objects           set of objects (same as identity arrows)
    * @param domain            maps arrows to domains
    * @param codomain          maps arrows to codomain
    * @param compositionSource source table of arrows composition (may be incomplete)
    * @return a newly-built category
    */
  def build[T](
    objects: Set[T],
    domain: Map[T, T],
    codomain: Map[T, T],
    compositionSource: Map[(T, T), T]): Result[Category[T, T]] = {
    Graph.build(objects, domain.keySet, domain, codomain) flatMap (buildFromPartialData(_, compositionSource))
  }

  /**
    * Builds a category given a limited (but sufficient) amount of data.
    * Objects have the same name as their identities.
    *
    * @tparam T arrow and node type
    * @param graph             he graph
    * @param compositionSource source table of arrows composition (may be incomplete)
    * @return a newly-built category
    */
  def buildFromPartialData[T](
    graph: Graph,
    compositionSource: Map[(T, T), T]): Result[Category[T, T]] = {
    val graphWithUnits = addUnitsToGraph(graph)
    val composition = fillCompositionTable(graphWithUnits, compositionSource)

    def compositionFunction(f: T, g: T): Option[T] = composition.get((f, g))

    val result = buildFromGraphWithIdentity[T, T](
      graphWithUnits,
      idMap(graph.nodes).asInstanceOf[T => T],
      compositionFunction)
    result
  }

  /**
    * Builds a category given a graph, composition table, and a mapping for identity arrows.
    *
    * @tparam O type of objects
    * @tparam A type of arrows
    * @param g           the graph on which we are to create a category
    * @param ids         maps objects to identity arrows
    * @param composition defines composition
    * @return a category built based on the data above
    *
    *         TODO: eliminate code duplication
    */
  def buildFromGraphWithIdentity[Obj, A](
    g: Graph,
    ids: Obj => A,
    composition: (A, A) => Option[A]): Result[Category[Obj, A]] = {
    val data = new CategoryData[Obj, A](g) {
      override def id(o: O): Arrow = ids(o.asInstanceOf[Obj]).asInstanceOf[Arrow]

      override def m(f: Arrow, g: Arrow): Option[Arrow] =
        composition(f.asInstanceOf[A], g.asInstanceOf[A]).asInstanceOf[Option[Arrow]]
    }

    data.validate returning
      new Category[Obj, A](g) {
        def id(o: O): Arrow = ids(o.asInstanceOf[Obj]).asInstanceOf[Arrow]

        def m(f: Arrow, g: Arrow): Option[Arrow] =
          composition(f.asInstanceOf[A], g.asInstanceOf[A]).asInstanceOf[Option[Arrow]]

        override def d0(f: Arrow): O = graph.d0(f.asInstanceOf[graph.Arrow]).asInstanceOf[O]

        override def d1(f: Arrow): O = graph.d1(f.asInstanceOf[graph.Arrow]).asInstanceOf[O]
      }
  }

  private[cat] def addUnitsToGraph[T](graph: Graph): Graph = {
    val nodes = graph.nodes.asInstanceOf[Set[T]] // this and the next casting is to cover up a weird bug somewhere in
    // scala
    val allArrows: Set[T] = nodes ++ graph.arrows.asInstanceOf[Set[T]]

    def isIdentity(f: T): Boolean = graph.nodes contains f.asInstanceOf[graph.Node]

    new Graph {
      def nodes: Nodes = graph.nodes.asInstanceOf[Nodes]
      def arrows: Arrows = allArrows.asInstanceOf[Arrows]

      def d0(f: Arrow): Node =
        if (isIdentity(f.asInstanceOf[T])) f.asInstanceOf[Node]
        else graph.d0(f.asInstanceOf[graph.Arrow]).asInstanceOf[Node]

      def d1(f: Arrow): Node =
        if (isIdentity(f.asInstanceOf[T])) f.asInstanceOf[Node]
        else graph.d1(f.asInstanceOf[graph.Arrow]).asInstanceOf[Node]
    }
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
  protected def fillCompositionTable[A](graph: Graph, compositionSource: Map[(A, A), A]): Map[(A, A), A] = {
    // First, add identities
    val addedIds = defineCompositionWithIdentities(graph, compositionSource)

    // Second, add unique solutions
    val addedUniqueSolutions: Map[(A, A), A] = addUniqueCompositions(graph, addedIds)

    // Third, deduce compositions from associativity law
    val addedDeducedCompositions: Map[(A, A), A] = deduceCompositions(graph, addedUniqueSolutions)

    addedDeducedCompositions
  }

  // adding composition with identities to a composition table
  protected def defineCompositionWithIdentities[A](graph: Graph, compositionSource: Map[(A, A), A]): Map[(A, A)
    , A] = {
    (compositionSource /: graph.arrows) ((m, f) => {
      val fA = f.asInstanceOf[A]
      val id_d0 = graph.d0(f).asInstanceOf[A]
      val id_d1 = graph.d0(f).asInstanceOf[A]
      m + ((id_d0, fA) -> fA) + ((fA, id_d1) -> fA)
    })
  }

  // adding unique available compositions
  protected def addUniqueCompositions[A](graph: Graph, compositionSource: Map[(A, A), A]): Map[(A, A), A] = {
    // Second, add unique solutions
    def candidates(f: A, g: A) =
      graph.arrowsBetween(
        graph.d0(f.asInstanceOf[graph.Arrow]), graph.d1(g.asInstanceOf[graph.Arrow]))

    def hasUniqueCandidate(f: A, g: A) = {
      val iterator = candidates(f, g).iterator
      iterator.hasNext && ! {
        iterator.next
        iterator.hasNext
      }
    }

    def candidate(f: A, g: A) = candidates(f, g).iterator.next

    val pairsToScan = composablePairs(graph) filter (p => {
      val (f, g) = p
      hasUniqueCandidate(f.asInstanceOf[A], g.asInstanceOf[A])
    })

    val solutions: Map[(A, A), A] = (compositionSource /: pairsToScan) {
      (m, p) => {
        val (f, g) = p
        val fA = f.asInstanceOf[A]
        val gA = g.asInstanceOf[A]
        m + ((fA, gA) -> candidate(fA, gA).asInstanceOf[A])
      }
    }
    solutions
  }

  def composablePairs(graph: Graph): Iterable[(graph.Arrow, graph.Arrow)] = {
    for (f <- graph.arrows; g <- graph.arrows if graph.follows(g, f)) yield (f, g)
  }

  // adding composition that are deduced from associativity law
  protected def deduceCompositions[A](graph: Graph, compositionSource: Map[(A, A), A]): Map[(A, A), A] = {
    val triplesToScan = composableTriples(graph, compositionSource)

    val compositions: Map[(A, A), A] = (compositionSource /: triplesToScan) {
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
    compositions
  }

  // this is a technical method to list all possible triples that have compositions defined pairwise
  protected def composableTriples[A](graph: Graph, compositionSource: Map[(A, A), A]): Set[(A, A, A)] = {
    val triples: Set[(graph.Arrow, graph.Arrow, graph.Arrow)] = for {
      f <- graph.arrows
      g <- graph.arrows if compositionSource.contains((f, g).asInstanceOf[(A, A)])
      h <- graph.arrows if compositionSource.contains((g, h).asInstanceOf[(A, A)])
    } yield (f, g, h)

    triples.asInstanceOf[Set[(A, A, A)]]
  }

  /**
    * Factory method. Parses a string and builds a category from it.
    *
    * @param input input to parse
    * @return the category
    */
  def read(input: Reader): Result[Cat] = (new Parser).readCategory(input)

  /**
    * Factory method. Parses a string and builds a category from it.
    *
    * @param input the string to parse
    * @return the category
    */
  def read(input: CharSequence): Result[Cat] = (new Parser).readCategory(input)

  class Parser extends Graph.Parser {

    def readCategory(input: CharSequence): Result[Cat] = {
      val parseResult = parseAll(category, input)
      explain(parseResult)
    }

    def category: Parser[Result[Cat]] =
      "(" ~ graph ~ (("," ~ multTable) ?) ~ ")" ^^ { case "(" ~ g ~ mOpt ~ ")" => mOpt match {
        case None =>
          g.flatMap(buildFromPartialData(_, Map.empty[(String, String), String])).asInstanceOf[Result[Cat]]
        case Some("," ~ m) =>
          g.flatMap(buildFromPartialData(_, m)).asInstanceOf[Result[Cat]]
        case Some(garbage) => Result.error(s"bad data: $garbage")
      }
      }

    def multTable: Parser[Map[(String, String), String]] = "{" ~ repsep(multiplication, ",") ~ "}" ^^ { case "{" ~ m
      ~ "}" => Map() ++ m
    }

    def multiplication: Parser[((String, String), String)] = member ~ "o" ~ member ~ "=" ~ member ^^ { case g ~ "o" ~
      f ~ "=" ~ h => ((f, g), h)
    }

    def readCategory(input: Reader): Result[Cat] = {
      val parseResult = parseAll(category, input)
      explain(parseResult)
    }
  }

}

object Category extends CategoryFactory {

  type Cat = Category[String, String] {
    type Node = String
    type Arrow = String
  }

  /**
    * Empty category
    */
  lazy val _0_ : Cat = segment(0)

  /**
    * Singleton category
    */
  lazy val _1_ : Cat = segment(1)

  /**
    * Discrete 2-object category
    */
  lazy val _1plus1_ : Cat = Category.discrete(Set("a", "b")).asInstanceOf[Cat]

  /**
    * Category <b>2</b>: 2 objects linearly ordered
    */
  lazy val _2_ : Cat = segment(2)

  /**
    * Category <b>3</b>: 3 objects linearly ordered
    */
  lazy val _3_ : Cat = segment(3)

  /**
    * Category <b>4</b>: 4 objects linearly ordered
    */
  lazy val _4_ : Cat = segment(4)

  /**
    * Category <b>5</b>: 5 objects linearly ordered
    */
  lazy val _5_ : Cat = segment(5)

  /**
    * Category with 2 objects and 2 parallel arrows from one to another
    */
  lazy val ParallelPair = category"({0, 1}, {a:0->1, b:0->1})"

  /**
    * Category <b>Z2</2> - a two-element monoid
    */
  lazy val Z2 = category"({1}, {1: 1 -> 1, a: 1 -> 1}, {1 o 1 = 1, 1 o a = a, a o 1 = a, a o a = 1})"

  lazy val Z3 = category"({0}, {0: 0 -> 0, 1: 0 -> 0, 2: 0 -> 0}, {1 o 1 = 2, 1 o 2 = 0, 2 o 1 = 0, 2 o 2 = 1})"

  /**
    * "Split Monomorphism" category (see http://en.wikipedia.org/wiki/Morphism)
    * Two objects, and a split monomorphism from a to b
    */
  lazy val SplitMono =
    category"({a,b}, {ab: a -> b, ba: b -> a, bb: b -> b}, {ba o ab = a, ab o ba = bb, bb o ab = ab, ba o bb = ba, bb o bb = bb})"

  /**
    * Commutative square category
    */
  lazy val Square = category"({a,b,c,d}, {ab: a -> b, ac: a -> c, bd: b -> d, cd: c -> d, ad: a -> d}, {bd o ab = ad, cd o ac = ad})"

  /**
    * Pullback category: a -> c <- b
    */
  lazy val Pullback = category"({a,b,c}, {ac: a -> c, bc: b -> c})"

  /**
    * Pushout category: b <- a -> c
    */
  lazy val Pushout = category"({a,b,c}, {ab: a -> b, ac: a -> c})"

  /**
    * Sample W-shaped category: a -> b <- c -> d <- e
    */
  lazy val W = category"({a,b,c,d,e}, {ab: a -> b, cb: c -> b, cd: c -> d, ed: e -> d})"

  /**
    * Sample M-shaped category: a <- b -> c <- d -> e
    */
  lazy val M = category"({a,b,c,d,e}, {ba: b -> a, bc: b -> c, dc: d -> c, de: d -> e})"


  /**
    * A segment of simplicial category.
    * Represents three sets (empty, singleton and two-point) and
    * all their possible functions.
    */
  lazy val HalfSimplicial: Cat =
    Category.build(Set("0", "1", "2"),
      Map("0_1" -> "0", "0_2" -> "0", "2_1" -> "2", "2_a" -> "2", "2_b" -> "2", "a" -> "1", "b" -> "1", "2_swap" ->
        "2"), // d0
      Map("0_1" -> "1", "0_2" -> "2", "2_1" -> "1", "2_a" -> "2", "2_b" -> "2", "a" -> "2", "b" -> "2", "2_swap" ->
        "2"), // d1
      Map(("0_1", "a") -> "0_2",
        ("0_1", "b") -> "0_2",
        ("2_1", "a") -> "2_a",
        ("2_1", "b") -> "2_b",
        ("a", "2_swap") -> "b",
        ("a", "2_a") -> "a",
        ("b", "2_swap") -> "a",
        ("b", "2_a") -> "a",
        ("b", "2_b") -> "b",
        ("2_swap", "2_swap") -> "2",
        ("2_swap", "2_a") -> "2_a",
        ("2_swap", "2_b") -> "2_b",
        ("2_a", "2_a") -> "2_a",
        ("2_b", "2_b") -> "2_b",
        ("2_a", "2_swap") -> "2_b",
        ("2_b", "2_swap") -> "2_a"
      )
    ).
      getOrElse(throw new InstantiationException("Bad semisimplicial?")).
      asInstanceOf[Cat]

  lazy val NaturalNumbers: Category[BigInt, (BigInt, BigInt)] =
    Category.fromPoset(PoSet.ofNaturalNumbers)

  lazy val KnownCategories = Set(
    _0_, _1_, _2_, _3_, _4_, _5_, _1plus1_,
    ParallelPair, Pullback, Pushout, SplitMono, Square,
    M, W,
    Z2, Z3,
    HalfSimplicial, NaturalNumbers)


  implicit class CategoryString(val sc: StringContext) extends AnyVal {
    def category(args: Any*): Cat = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      var buf = new StringBuffer(strings.next)
      while (strings.hasNext) {
        buf append expressions.next
        buf append strings.next
      }
      Category.read(buf) match {
        case Good(c) => c
        case bad => throw new InstantiationException(bad.errorDetails.mkString)
      }
    }
  }


}
