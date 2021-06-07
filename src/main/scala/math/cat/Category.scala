package math.cat

import scala.language.implicitConversions
import scala.language.postfixOps

import math.Base._
import math.sets.Sets._
import math.sets.{BinaryRelation, FactorSet, Sets}
import scalakittens.Result._
import scalakittens.{Good, Result}

import scala.collection.{GenTraversableOnce, TraversableOnce}

/**
  * Category class, and the accompanying object.
  */
abstract class Category extends CategoryData {
  lazy val terminal: Result[Obj] = objects.find(isTerminal)
  lazy val initial: Result[Obj] = objects.find(isInitial)
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
    val wrongStuff = arrows filter (f ⇒ !isEndomorphism(f)) map d1
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
  lazy val op: Category = {
    val src = this
    new Category {
      override val graph: Graph = ~src

      override def id(o: Obj): Arrow = arrow(src.id(src.obj(o)))

      override def m(f: Arrow, g: Arrow): Option[Arrow] =
        src.m(src.arrow(g), src.arrow(f)) map arrow
    }
  }

  private[cat] lazy val listOfRootObjects = allRootObjects.toList.sortBy(_.toString)

  def foreach[U](f: Obj ⇒ U): Unit = objects foreach f

  def map[B](f: Obj ⇒ B): TraversableOnce[B] = objects.map(f)

  def flatMap[B](f: Obj ⇒ GenTraversableOnce[B]): TraversableOnce[B] = objects.flatMap(f)

  def compositions: Iterable[(Arrow, Arrow, Arrow)] =
    for {f ← arrows
         g ← arrows
         h ← m(f, g)} yield (f, g, h)

  private var source: Option[String] = None
  
  def withSource(s: String): this.type = {
    source = Option(s)
    this
  }
  
  override def toString: String =
    source getOrElse
    s"${if (name.isEmpty) "" else {name + ": " }}({" +
    objects.toList.sortBy(_.toString).mkString(", ") + "}, {" +
    (arrows.toList.filterNot(isIdentity).sortBy(_.toString) map (a ⇒ s"$a: ${d0(a)} →${d1(a)}")).mkString(", ") + "}, {" +
    (composablePairs collect {
      case (first, second) if !isIdentity(first) && !isIdentity(second) ⇒
        concat(second, "∘", first) + s" = ${m(first, second).get}"
    }).mkString(", ") + "})"

  /**
    * Checks whether an arrow is an isomorphism.
    *
    * @param f an arrow to check
    * @return true iff f is an isomorphism
    */
  def isIsomorphism(f: Arrow): Boolean = inverse(arrow(f)).isDefined

  def isomorphic(a: Obj, b: Obj): Boolean = hom(a, b) exists isIsomorphism

  lazy val clusters: SetMorphism[Obj, Objects] =
    Sets.factorset(objects, BinaryRelation(isomorphic _))

  /**
    * Returnes an inverse arrow.
    *
    * @param f an arrow for which we are looking an inverse
    * @return inverse arrow
    */
  def inverse(f: Arrow): Result[Arrow] = arrowsBetween(d1(arrow(f)), d0(f)) find (areInverse(f, _))

  def areInverse(f: Arrow, g: Arrow): Boolean =
    (m(arrow(f), arrow(g)) contains id(d0(f))) && (m(g, f) contains id(d0(g)))

  def isEndomorphism(f: Arrow): Boolean = d0(arrow(f)) == d1(f)

  /**
    * Checks whether an arrow is a monomorphism.
    *
    * @param f an arrow to check
    * @return true iff f is a monomorphism
    */
  def isMonomorphism(f: Arrow): Boolean = {

    val iterable = for {g ← arrows if follows(f, g)
                        h ← arrows if follows(f, h) && equalizes(g, h)(f)
    } yield g == h

    iterable forall (x ⇒ x)
  }

  /**
    * Builds a predicate that checks whether an arrow equalizes two other arrows,
    * that is, whether f ∘ h = g ∘ h  for a given arrow h.
    *
    * @param f first arrow
    * @param g second arrow
    * @return a predicate defined on arrows.
    */
  def equalizes(f: Arrow, g: Arrow): Arrow ⇒ Boolean = {
    h: Arrow ⇒ areParallel(f, g) && follows(f, h) && (m(h, f) == m(h, g))
  }

  /**
    * Checks whether an arrow is an epimorphism.
    *
    * @param f an arrow to check
    * @return true iff f is an epimorphism
    */
  def isEpimorphism(f: Arrow): Boolean = {
    val iterable = for (g ← arrows if follows(g, f);
                        h ← arrows if follows(h, f) &&
      coequalizes(g, h)(f)) yield {
      g == h
    }

    iterable forall (x ⇒ x)
  }

  /**
    * Checks if arrow h coequalizes arrows f and g (that is, whether h ∘ f == h ∘ g).
    *
    * @param f first arrow
    * @param g second arrow
    * @return true iff h ∘ f == h ∘ g
    */
  def coequalizes(f: Arrow, g: Arrow): Arrow ⇒ Boolean = {
    h: Arrow ⇒ areParallel(f, g) && follows(h, f) && (m(f, h) == m(g, h))
  }

  /**
    * Builds a predicate that checks whether an arrow h: B → A is such that
    * px ∘ h = qx and py ∘ h = qy
    * where qx: B → X, qy: B → Y, px: A → X, py: A → Y.
    *
    * @param q factoring pair of arrows
    * @param p factored pair of arrows
    * @return the specified predicate.
    */
  def factorsOnLeft(p: (Arrow, Arrow), q: (Arrow, Arrow)): Arrow ⇒ Boolean = (h: Arrow) ⇒ {
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
  def equalizer(f: Arrow, g: Arrow): Result[Arrow] = arrows find isEqualizer(f, g)

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
    * Builds a predicate that checks if arrow g: y → z
    * uniquely factors on the left the arrow f: x → z - that is,
    * there is just one h: x → y such that f = g ∘ h.
    *
    * @param f arrow being factored
    * @return the specified predicate
    */
  def factorsUniquelyOnLeft(f: Arrow)(g: Arrow): Boolean =
    sameCodomain(g, f) &&
      existsUnique(arrowsBetween(d0(f), d0(g)), (h: Arrow) ⇒ m(h, g) contains f)

  /**
    * Builds a set of all arrows that equalize f: A → B and g: A → B, that is,
    * such arrows h: X → A that f ∘ h = g ∘ h.
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
  def coequalizer(f: Arrow, g: Arrow): Result[Arrow] = Result(arrows find isCoequalizer(f, g))

  /**
    * Builds a predicate that checks if an arrow is a coequalizer of the other two arrows.
    *
    * @param f first arrow
    * @param g second arrow
    * @return true if it is a coequalizer
    */
  def isCoequalizer(f: Arrow, g: Arrow): Arrow ⇒ Boolean = {
    require(areParallel(f, g))
    h: Arrow ⇒
      coequalizes(f, g)(h) &&
        (allCoequalizingArrows(f, g) forall factorsUniquelyOnRight(h))
  }

  /**
    * Builds a predicate that checks if arrow g: x → y
    * uniquely factors on the right the arrow f: x → z - that is,
    * there is just one h: y → z such that f = h ∘ g.
    *
    * @param f factored arrow
    * @return the specified predicate
    */
  def factorsUniquelyOnRight(f: Arrow): Arrow ⇒ Boolean =
    (g: Arrow) ⇒ {
      sameDomain(g, f) &&
        isUnique(arrowsBetween(d1(g), d1(f)).filter(m(g, _) contains f))
    }

  /**
    * Builds a set of all arrows that coequalize f: A → B and g: A → B, that is,
    * such arrows h: B → X that h ∘ f = h ∘ g.
    *
    * @param f first arrow
    * @param g second arrow
    * @return an Iterable of arrows that coequalize f and g
    */
  def allCoequalizingArrows(f: Arrow, g: Arrow): Iterable[Arrow] = arrows filter coequalizes(f, g)

  /**
    * Calculates a coequalizer of a collection of parallel arrows.
    * Since the collection may be empty, should provide the codomain.
    *
    * @param arrows the arrows, all of which shold be coequalized
    * @return a coequalizer arrow
    */
  def coequalizer(arrows: Iterable[Arrow]): Result[Arrow] = NotImplemented

  /**
    * Builds a set of all arrows to x and y (respectively) that start at the same object.
    *
    * @param x first object
    * @param y second object
    * @return a set of pairs of arrows with the same domain, ending at x and y.
    */
  def pairsWithTheSameDomain(x: Obj, y: Obj): Set[(Arrow, Arrow)] = setOf(
    product2(arrows, arrows).
      filter(p ⇒ {
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
  def isProduct(x: Obj, y: Obj): ((Arrow, Arrow)) ⇒ Boolean = {
    case (px, py) ⇒
      d0(arrow(px)) == d0(arrow(py)) &&
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
  def product(x: Obj, y: Obj): Result[(Arrow, Arrow)] =
    Result(product2(arrows, arrows) find isProduct(x, y))

  /**
    * Builds a union of two objects, if it exists. Returns null otherwise.
    * The union is represented as a pair of insertions of the two objects into their union
    *
    * @param x first object
    * @param y second object
    * @return a pair of arrows from a and b to their union, or null if none exists.
    */
  def union(x: Obj, y: Obj): Result[(Arrow, Arrow)] =
    Result(product2(arrows, arrows) find isUnion(x, y))

  /**
    * Checks if i = (ix, iy) is a union of objects x and y.
    *
    * @param x first object
    * @param y second object
    * @return true if this is a union
    */
  def isUnion(x: Obj, y: Obj): ((Arrow, Arrow)) ⇒ Boolean = (i: (Arrow, Arrow)) ⇒ {
    val (ix, iy) = i
    d0(arrow(ix)) == x && d0(arrow(iy)) == y &&
      pairsWithTheSameCodomain(x, y).forall(factorUniquelyOnLeft(ix, iy))
  }

  /**
    * Builds a set of all arrows that start at x and y, respectively, and end at the same object.
    *
    * @param x first object
    * @param y second object
    * @return a set of pairs of arrows with the same codomain, starting at x and y.
    */
  def pairsWithTheSameCodomain(x: Obj, y: Obj): Set[(Arrow, Arrow)] = setOf(
    product2(arrows, arrows) filter {
      case (px, py) ⇒
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
  def pullback(f: Arrow, g: Arrow): Result[(Arrow, Arrow)] = {
    OKif(sameCodomain(f, g), s"Codomains of $f and $g should be the same in $name") andThen
      product2(arrows, arrows).find(isPullback(f, g))
  }

  /**
    * Checks if p = (pa, pb) is a pullback of arrows f and g.
    *
    * @param f first arrow
    * @param g second arrow
    * @return true if this is a pullback
    */
  def isPullback(f: Arrow, g: Arrow): ((Arrow, Arrow)) ⇒ Boolean = (p: (Arrow, Arrow)) ⇒ {
    val (px, py) = p
    follows(f, px) && follows(g, py) &&
      m(px, f) == m(py, g) &&
      pairsEqualizing(f, g).forall(factorUniquelyOnRight(px, py))
  }

  /**
    * Builds a predicate that checks if a pair of arrows p = (px, py) : A → X x Y
    * factors uniquely a pair q = (qx, qy): B → X x Y on the right,
    * that is, if there exists a unique arrow h: B → A such that qx = px ∘ h and qy = py ∘ h.
    *
    * @return true if p factors q uniquely on the right
    */
  def factorUniquelyOnRight(px: Arrow, py: Arrow): ((Arrow, Arrow)) ⇒ Boolean = {
    case (qx, qy) ⇒
      sameCodomain(px, qx) &&
        sameCodomain(py, qy) &&
        isUnique(arrowsBetween(d0(qx), d0(px)).filter((h: Arrow) ⇒ (m(h, px) contains qx) && (m(h, py) contains qy)))
  }

  /**
    * Builds a set of all pairs (px, py) of arrows that start at the same domain and end
    * at d0(f) and d0(g), equalizing them: f ∘ px = g ∘ py, that is, making the square
    * <pre>
    * py
    * U —————→ Y
    * |        |
    * px|        | g
    * |        |
    * ↓        ↓
    * X —————→ Z
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
        filter(p ⇒ {
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
  def pushout(f: Arrow, g: Arrow): Result[(Arrow, Arrow)] = {
    OKif(sameDomain(f, g), "Domains should be the same in $name") andThen
      product2(arrows, arrows).find(isPushout(f, g))
  }

  /**
    * Checks if p = (pa, pb) is a pushout of arrows f and g.
    *
    * @param f first arrow
    * @param g second arrow
    * @return true if this is a pushout
    */
  def isPushout(f: Arrow, g: Arrow): ((Arrow, Arrow)) ⇒ Boolean = (p: (Arrow, Arrow)) ⇒ {
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
    * px: X → A, py: Y → A, factors uniquely a pair q = (qx, qy)
    * (where qx: X → B, qy: Y → B) on the left,
    * that is, if there exists a unique arrow h: A → B
    * such that qx = h ∘ px and qy = h ∘ py.
    *
    * @return true if q factors p uniquely on the left
    */
  def factorUniquelyOnLeft(f: Arrow, g: Arrow): ((Arrow, Arrow)) ⇒ Boolean =
    (q: (Arrow, Arrow)) ⇒ {
      val (qx, qy) = q
      isUnique(arrowsBetween(d1(f), d1(qx)).filter(factorsOnRight((f, g), q)))
    }

  /**
    * Builds a predicate that checks whether an arrow h: A → B is such that
    * h ∘ px = qx and h ∘ py = qy for q = (qx, qy), and p = (px, py)
    * where qx: X → B, qy: Y → B, px: X → A, py: Y → A.
    *
    * @param q factoring pair of arrows
    * @param p factored pair of arrows
    * @return the predicate described above.
    */
  def factorsOnRight(p: (Arrow, Arrow), q: (Arrow, Arrow)): Arrow ⇒ Boolean = (h: Arrow) ⇒ {
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
    * Z —————→ Y
    * |        |
    * f |        | qy
    * |        |
    * ↓        ↓
    * X —————→ U
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
      filter(q ⇒ {
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
  def isTerminal(t: Obj): Boolean =
    objects.forall((x: Obj) ⇒ isUnique(arrowsBetween(x, t)))

  /**
    * Checks if a given object (candidate) is an initial object (aka zero).
    * Initial object is the one which has just one arrow to every other object.
    */
  def isInitial(i: Obj): Boolean = objects.forall((x: Obj) ⇒ isUnique(arrowsBetween(i, x)))

  /**
    * Given a set of objects and a set of arrows, build a map that maps each object to
    * a set of arrows starting at it.
    *
    * @param setOfObjects objects for which to build the bundles.
    * @param arrows       arrows that participate in the bundles.
    * @return a map.
    */
  def buildBundles(setOfObjects: Objects, arrows: Arrows): Map[Obj, Arrows] = {
    val badArrows: Arrows = arrows.filterNot(a ⇒ setOfObjects(d0(a)))

    require(badArrows.isEmpty, s"These arrows don't belong: ${badArrows.mkString(",")} in $name")

    val mor = SetMorphism.build(arrows, setOfObjects, d0).iHope.revert.function
    setOfObjects.map(o ⇒ o → mor(o)).toMap.withDefaultValue(Set.empty[Arrow])
  }

  /**
    * Builds a degree object (X*X... n times) for a given object.
    * The trick is, find an object that "is" the x we provided
    *
    * @param x the source object
    * @param n degree to which to raise object x
    * @return x^n^ and its projections to x
    */
  def degree(x: Obj, n: Int): Result[(Obj, List[Arrow])] = {
    OKif(n >= 0) andThen {
      n match {
        case 0 ⇒ terminal map (x ⇒ (x, List()))
        case 1 ⇒ Good((x, id(x) :: Nil))
        case _ ⇒ degree(x, n - 1) flatMap {
          case (x_n_1, previous_projections) ⇒
            product(x, x_n_1) map {
              case (p1, p_n_1) ⇒
                val projections = p1 :: previous_projections map (m(p_n_1, _))
                (d0(p1), projections.flatten)
            }
        }
      }
    }
  }

  def arrowsEndingAt(x: Obj): Arrows = arrows filter { x == d1(_) }

  /**
    * Remove the arrows that are not required for drawing:
    * identities and uniquely-determined compositions.
    *
    * @return a graph with the same nodes, but with less arrows
    */
  def baseGraph: Graph = {
    // first, remove identities
    val nontrivialArrows = arrows filterNot isIdentity toList
    // then, remove compound arrows - those that were deduced during creation
    val listOfArrows = nontrivialArrows sortBy(_.toString) filterNot (_.toString.contains("∘")) reverse
    // then, remove all those that are still deducible
    val essentialArrows = selectBaseArrows(listOfArrows)

    val essentialArrowsMap: Map[Arrow, (Node, Node)] = essentialArrows map {
      a ⇒ a -> (d0(a), d1(a))
    } toMap

    Graph.fromArrowMap(name, nodes, essentialArrowsMap) iHope
  }

  private def selectBaseArrows(arrows: List[Arrow]): List[Arrow] = {
    arrows.find(canDeduce(arrows)) match {
      case None ⇒ arrows
      case Some(f) ⇒ selectBaseArrows(arrows.filterNot(f ==))
    }
  }

  private[cat] def canDeduce(arrows: Iterable[Arrow])(a: Arrow): Boolean = {
    val from = d0(a)
    val to = d1(a)
    hom(from, to).size == 1 && arrows.exists {
      f ⇒ d1(f) != from && d1(f) != to && arrows.exists {
        g ⇒ m(f, g) contains a
      }
    }
  }

  def isIdentity(a: Arrow): Boolean = a == id(d0(a))

  def connectedComponents: Set[Category] = {
    val connected: BinaryRelation[Obj, Obj] =
      BinaryRelation((x, y) ⇒ arrows.exists(a ⇒
        (x == d0(a) && y == d1(a)) || (x == d1(a) && y == d0(a))))

    val sets = new FactorSet(objects, connected)

    sets.zipWithIndex map {
      case (s, i) ⇒ completeSubcategory(s"$name.${i + 1}", s)
    }
  }

  def completeSubcategory(name: String, setOfObjects: Objects): Category = {
    val src = this
    val sub = subgraph(name, setOfObjects)

    new Category {
      override val graph: Graph = sub

      override def id(o: Obj): Arrow = arrow(src.id(src.obj(o)))

      override def m(f: Arrow, g: Arrow): Option[Arrow] =
        src.m(src.arrow(f), src.arrow(g)) map arrow
    }
  }
}

object Category extends CategoryFactory
