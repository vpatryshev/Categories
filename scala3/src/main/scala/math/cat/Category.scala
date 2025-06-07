package math.cat

import math.Base.*
import math.cat.Graph.build
import math.cat.construction.{CategoryData, CategoryFactory}
import math.sets.Functions.*
import math.sets.Sets.*
import math.sets.{BinaryRelation, FactorSet, Sets}
import scalakittens.Result.*
import scalakittens.{Params, Good, Result}

import scala.annotation.tailrec
import scala.collection.IterableOnce
import scala.collection.mutable.ListBuffer
import scala.language.{implicitConversions, postfixOps}
import scalakittens.Containers.*

/**
  * Category class, and the accompanying object.
  */
abstract class Category(name: String) extends CategoryData(name):

  /**
    * Terminal object of this category (if exists)
    */
  lazy val terminal: Result[Obj] = objects.find(isTerminal)

  /**
    * Initial object of this category (if exists)
    */
  lazy val initial: Result[Obj] = objects.find(isInitial)
  /**
    * an iterable of initial objects as defined
    */
  lazy val allRootObjects_byDefinition: Objects = objects filter {
    arrowsEndingAt(_) forall isEndomorphism
  }
  /**
    * A fast alternative for the iterable (actually, a set) of initial objects
    */
  lazy val allRootObjects_programmersShortcut: Objects =
    val wrongStuff = arrows filterNot isEndomorphism map d1
    objects -- wrongStuff

  /**
    * An iterable of all objects that do not have any non-endomorphic arrows pointing at them.
    * Constructively, these are all such objects that if an arrow ends at such an object, it is an endomorphism.
    * Since producing a lazy set is too heavy, I just build it in an old-fashion way.
    */
  lazy val allRootObjects: Objects = allRootObjects_programmersShortcut
  
  /**
    * A set of all arrows that originate at initial objects (see allRootObjects)
    */
  lazy val arrowsFromRootObjects: Set[Arrow] = arrows.filter(f => d0(f) ∈ allRootObjects)
  
  private[cat] lazy val listOfRootObjects = listSorted(allRootObjects)

  infix def foreach[U](f: Obj => U): Unit = objects foreach f

  infix def map[B](f: Obj => B): IterableOnce[B] = objects map f

  infix def flatMap[B](f: Obj => IterableOnce[B]): IterableOnce[B] = objects flatMap f

  private var source: Option[String] = None

  infix def withSource(s: String): this.type =
    source = Option(s)
    this
  
  override def toString: String =
    source getOrElse
      val prefix = if name.isEmpty then "" else name + ": "
      if (isFinite)
        val objectsAsString = asString(objects)
        s"$prefix({$objectsAsString}, {" +
          (nonIdentities map (a => s"$a: ${d0(a)} ->${d1(a)}")).mkString(", ") + "}, {" +
          (composablePairs collect {
            case (first, second) if !isIdentity(first) && !isIdentity(second) =>
              concat(second, "∘", first) + s" = ${m(first, second).get}"
          }).mkString(", ") + "})"
      else
        s"$prefix{(infinite category)}"

  /**
    * Checks whether an arrow is an isomorphism.
    *
    * @param f an arrow to check
    * @return true iff f is an isomorphism
    */
  def isIsomorphism(f: Arrow): Boolean = inverse(f).isDefined

  /**
    * Checks whether two objects are isomorphic
    * @param a first object 
    * @param b second object
    * @return true iff `a` and `b` are isomorphic
    */
  def isomorphic(a: Obj, b: Obj): Boolean = hom(a, b) exists isIsomorphism

  /**
    * This method is being used for rendering the category
    */
  lazy val clusters: SetMorphism[Obj, Objects] =
    SetCategory.factorset[Obj](objects, BinaryRelation(isomorphic))

  /**
    * Returns an inverse arrow.
    *
    * @param f an arrow for which we are looking an inverse
    * @return inverse arrow
    */
  def inverse(f: Arrow): Result[Arrow] =
    arrowsBetween(d1(f), d0(f)) find (areInverse(f, _))

  /**
    * Checks whether two arrows are inverse
    * @param f first arrow
    * @param g second arrow
    * @return true iff f∘g=id and g∘f=id
    */
  def areInverse(f: Arrow, g: Arrow): Boolean =
    (id(d0(f)) ∈ m(f,g)) && (id(d0(g)) ∈ m(g,f))

  /**
    * Checks whether an arrow is an endomorphism
    * @param f an arrow
    * @return true iff d0(f) = d1(f)
    */
  def isEndomorphism(f: Arrow): Boolean = d0(f) == d1(f)

  /**
    * Checks whether an arrow is a monomorphism.
    *
    * @param f an arrow to check
    * @return true iff arrow f is a monomorphism
    */
  def isMonomorphism(f: Arrow): Boolean =
    val comparisons =
      for
        g <- arrows if follows(f, g)
        fg = m(f,g)
        h <- arrows if fg == m(f, h)
      yield g == h

    comparisons forall (x => x)

  /**
    * Checks whether an arrow is an epimorphism.
    *
    * @param f an arrow to check
    * @return true iff f is an epimorphism
    */
  def isEpimorphism(f: Arrow): Boolean =
    val comparisons =
      for 
        g <- arrows if follows(g, f)
        gf = m(g,f)
        h <- arrows if gf == m(h,f)
      yield g == h

    comparisons forall (x => x)

  /**
    * Checks if arrow h coequalizes arrows f and g (that is, whether h∘f == h∘g).
    *
    * @param f first arrow
    * @param g second arrow
    * @return true iff h ∘ f == h ∘ g
    */
  def coequalizes(f: Arrow, g: Arrow): Arrow => Boolean =
    (h: Arrow) => areParallel(f, g) && follows(h, f) && (m(f, h) == m(g, h))

  /**
    * Builds a predicate that checks whether an arrow h: B -> A is such that
    * px ∘ h = qx and py ∘ h = qy
    * where qx: B -> X, qy: B -> Y, px: A -> X, py: A -> Y.
    *
    * @param q a factoring pair of arrows
    * @param p a factored pair of arrows
    * @return the specified predicate.
    */
  def factorsOnLeft(p: (Arrow, Arrow), q: (Arrow, Arrow)): Arrow => Boolean = (h: Arrow) =>
    val (px, py) = p
    val (qx, qy) = q
    sameDomain(h, qx) && sameDomain(h, qy) &&
      follows(px, h) && follows(py, h) &&
      sameCodomain(px, qx) && sameCodomain(py, qy) &&
      qx ∈ m(h, px) && qy ∈ m(h, py)

  /**
    * Builds an equalizer arrow for a parallel pair of arrows.
    *
    * @param f first arrow
    * @param g second arrow
    * @return an equalizer arrow, wrapped in Option
    */
  def equalizer(f: Arrow, g: Arrow): Result[Arrow] =
    arrows find isEqualizer(f, g)

  /**
    * Builds a predicate that checks whether an arrow equalizes two other arrows,
    * that is, whether f ∘ h = g ∘ h  for a given arrow h.
    *
    * @param f first arrow
    * @param g second arrow
    * @return a predicate defined on arrows.
    */
  def equalizes(f: Arrow, g: Arrow): Arrow => Boolean =
    (h: Arrow) => follows(f, h) && (m(h, f) == m(h, g))

  /**
    * Builds a predicate that checks
    * if an arrow is an equalizer of the other two arrows.
    *
    * @param f first arrow
    * @param g second arrow
    * @return a predicate that checks if an arrow is an equalizer of f and g
    */
  def isEqualizer(f: Arrow, g: Arrow)(h: Arrow): Boolean =
      equalizes(f, g)(h) &&
        (allEqualizingArrows(f, g) forall factorsUniquelyOnLeft(h))

  /**
    * Builds a predicate that checks if arrow g: y -> z
    * uniquely factors on the left the arrow f: x -> z - that is,
    * there is just one h: x -> y such that f = g ∘ h.
    *
    * @param f arrow being factored
    * @return the specified predicate
    */
  def factorsUniquelyOnLeft(f: Arrow)(g: Arrow): Boolean =
    sameCodomain(g, f) &&
      existsUnique(arrowsBetween(d0(f), d0(g)), (h: Arrow) => f ∈ m(h, g))

  /**
    * Builds a set of all arrows that equalize f: A -> B and g: A -> B, that is,
    * such arrows h: X -> A that f ∘ h = g ∘ h.
    *
    * @param f first arrow
    * @param g second arrow
    * @return an Iterable of arrows that equalize f and g
    */
  def allEqualizingArrows(f: Arrow, g: Arrow): Iterable[Arrow] =
    arrows filter equalizes(f, g)

  /**
    * Builds a coequalizer arrow for a parallel pair of arrows.
    *
    * @param f first arrow
    * @param g second arrow
    * @return Good coequalizer arrow, if one exists, None otherwise
    */
  def coequalizer(f: Arrow, g: Arrow): Result[Arrow] =
    Result(arrows find isCoequalizer(f, g))

  /**
    * Builds a predicate that checks if an arrow is a coequalizer of the other two arrows.
    *
    * @param f first arrow
    * @param g second arrow
    * @return true if it is a coequalizer
    */
  def isCoequalizer(f: Arrow, g: Arrow): Arrow => Boolean =
    (h: Arrow) =>
      coequalizes(f, g)(h) &&
        (allCoequalizingArrows(f, g) forall factorsUniquelyOnRight(h))

  /**
    * Builds a predicate that checks if arrow g: x -> y
    * uniquely factors on the right the arrow f: x -> z - that is,
    * there is just one h: y -> z such that f = h ∘ g.
    *
    * @param f factored arrow
    * @return the specified predicate
    */
  def factorsUniquelyOnRight(f: Arrow): Arrow => Boolean =
    (g: Arrow) =>
      sameDomain(g, f) &&
        isSingleton(arrowsBetween(d1(g), d1(f)) filter (f ∈ m(g, _)))

  /**
    * Builds a set of all arrows that coequalize f: A -> B and g: A -> B, that is,
    * such arrows h: B -> X that h ∘ f = h ∘ g.
    *
    * @param f first arrow
    * @param g second arrow
    * @return an Iterable of arrows that coequalize f and g
    */
  def allCoequalizingArrows(f: Arrow, g: Arrow): Iterable[Arrow] =
    arrows filter coequalizes(f, g)

  /**
    * Given a collection of parallel arrows, calculate their coequalizer.
    * Since the collection may be empty, we should provide the codomain.
    *
    * @param arrows the arrows, all of which should be coequalized
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
      filter(p =>
        val (px, py) = p
        sameDomain(px, py) &&
          d1(px) == x &&
          d1(py) == y
      )
  )

  /**
    * Checks if p = (px, py) is a Cartesian product of objects x and y.
    *
    * @param x first object
    * @param y second object
    * @return true if this is a cartesian product
    */
  def isProduct(x: Obj, y: Obj): ((Arrow, Arrow)) => Boolean =
    case (px, py) =>
      d0(px) == d0(py) &&
      d1(px) == x &&
      d1(py) == y &&
      (pairsWithTheSameDomain(x, y) forall factorUniquelyOnRight(px, py))

  /**
    * Builds a Cartesian product of two objects if it exists.
    * Returns None otherwise.
    * The product is represented as a pair of projections from the product object to the
    * two which are being multiplied.
    *
    * @param x first object
    * @param y second object
    * @return Good pair of arrows from product object to x and y, or None.
    */
  def product(x: Obj, y: Obj): Result[(Arrow, Arrow)] =
    Result(product2(arrows, arrows) find isProduct(x, y))

  /**
    * Builds a union of two objects if it exists.
    * Returns None otherwise.
    * Given two objects, the union is represented as a pair of their insertions into the union.
    *
    * @param x first object
    * @param y second object
    * @return Good pair of arrows from a and b to their union, or None.
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
  def isUnion(x: Obj, y: Obj): ((Arrow, Arrow)) => Boolean =
    (i: (Arrow, Arrow)) =>
      val (ix, iy) = i
      d0(ix) == x && d0(iy) == y &&
      pairsWithTheSameCodomain(x, y).forall(factorUniquelyOnLeft(ix, iy))

  /**
    * Builds a set of all arrows that start at x and y, respectively, and end at the same object.
    *
    * @param x first object
    * @param y second object
    * @return a set of pairs of arrows with the same codomain, starting at x and y.
    */
  def pairsWithTheSameCodomain(x: Obj, y: Obj): Set[(Arrow, Arrow)] = setOf(
    product2(arrows, arrows) filter {
      case (px, py) =>
        sameCodomain(px, py) &&
        d0(px) == x &&
        d0(py) == y
    }
  )

  /**
    * Builds a pullback of two arrows if it exists.
    * Returns None otherwise.
    * The pullback is represented as a pair of projections from the pullback object to the
    * domains of the two arrows.
    *
    * @param f first arrows
    * @param g second arrow
    * @return Good pair of arrows from pullback object to d0(f) and d0(g), or None.
    */
  def pullback(f: Arrow, g: Arrow): Result[(Arrow, Arrow)] =
    OKif(sameCodomain(f, g), s"Codomains of $f and $g should be the same in $name") andThen
      product2(arrows, arrows).find(isPullback(f, g))

  /**
    * Checks if p = (pa, pb) is a pullback of arrows f and g.
    *
    * @param f first arrow
    * @param g second arrow
    * @return true if this is a pullback
    */
  def isPullback(f: Arrow, g: Arrow): ((Arrow, Arrow)) => Boolean = (p: (Arrow, Arrow)) =>
    val (px, py) = p
    follows(f, px) && follows(g, py) &&
      m(px, f) == m(py, g) &&
      pairsEqualizing(f, g).forall(factorUniquelyOnRight(px, py))

  /**
    * Builds a predicate that checks if a pair of arrows p = (px, py) : A -> X x Y
    * factors uniquely a pair q = (qx, qy): B -> X x Y on the right,
    * that is, if there exists a unique arrow h: B -> A such that qx = px ∘ h and qy = py ∘ h.
    *
    * @return true if p factors q uniquely on the right
    */
  def factorUniquelyOnRight(px: Arrow, py: Arrow): ((Arrow, Arrow)) => Boolean =
    case (qx, qy) =>
      sameCodomain(px, qx) &&
      sameCodomain(py, qy) &&
      isSingleton(
        arrowsBetween(d0(qx), d0(px)) filter(
          (h: Arrow) => qx ∈ m(h, px) && qy ∈ m(h, py)))

  /**
    * Builds a set of all pairs (px, py) of arrows that start at the same domain and end
    * at d0(f) and d0(g), equalizing them: f ∘ px = g ∘ py, that is, making the square
    * <pre>
    * py
    *   U —————→ Y
    *   |        |
    * px|        | g
    *   |        |
    *   ↓        ↓
    *   X —————→ Z
    * f
    * </pre>
    * commutative.
    *
    * @param f first arrow
    * @param g second arrow
    * @return the set of all such pairs of arrows
    */
  def pairsEqualizing(f: Arrow, g: Arrow): Set[(Arrow, Arrow)] =
    setOf(
      product2[Arrow, Arrow](arrows, arrows).
        filter(p =>
          val (px, py) = p
            follows(f, px) &&
            m(px, f) == m(py, g)
        )
    )

  /**
    * Builds a pushout of two arrows if it exists.
    * Returns None otherwise.
    * The pushout is represented as a pair of coprojections from the codomains of the two arrows
    * to the pushout object.
    *
    * @param f first arrows
    * @param g second arrow
    * @return Good pair of arrows from d1(f) and d1(g) to the pushout object, or None.
    */
  def pushout(f: Arrow, g: Arrow): Result[(Arrow, Arrow)] =
    OKif(sameDomain(f, g), "Domains should be the same in $name") andThen
      product2(arrows, arrows).find(isPushout(f, g))

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
      m(f, px) == m(g, py) &&
      pairsCoequalizing(f, g).forall(factorUniquelyOnLeft(px, py))
  }

  /**
    * Builds a predicate that checks if a pair of arrows p = (px, py), where
    * px: X -> A, py: Y -> A, factors uniquely a pair q = (qx, qy)
    * (where qx: X -> B, qy: Y -> B) on the left,
    * that is, if there exists a unique arrow h: A -> B
    * such that qx = h ∘ px and qy = h ∘ py.
    *
    * @return true if q factors p uniquely on the left
    */
  def factorUniquelyOnLeft(f: Arrow, g: Arrow): ((Arrow, Arrow)) => Boolean =
    (q: (Arrow, Arrow)) => {
      val (qx, qy) = q
      isSingleton(hom(d1(f), d1(qx)).filter(factorsOnRight((f, g), q)))
    }

  /**
    * Builds a predicate that checks whether an arrow h: A -> B is such that
    * h ∘ px = qx and h ∘ py = qy for q = (qx, qy), and p = (px, py)
    * where qx: X -> B, qy: Y -> B, px: X -> A, py: Y -> A.
    *
    * @param q a factoring pair of arrows
    * @param p a factored pair of arrows
    * @return the predicate described above.
    */
  def factorsOnRight(p: (Arrow, Arrow), q: (Arrow, Arrow)): Arrow => Boolean = (h: Arrow) => {
    val (px, py) = p
    val (qx, qy) = q
    sameDomain(px, qx) && sameDomain(py, qy) &&
      follows(h, px) && follows(h, py) &&
      sameCodomain(h, qx) && sameCodomain(h, qy) &&
      qx ∈ m(px, h) && qy ∈ m(py, h)
  }

  /**
    * Builds a set of all pairs (qx, qy) of arrows that end at the same codomain and start
    * at d1(f) and d1(g), coequalizing them: m(f, qx) = m(g, qy), making the square
    * <pre>
    *       g
    *   Z —————-> Y
    *   |        |
    * f |        | qy
    *   |        |
    *   ↓        ↓
    *   X —————-> U
    *      qx
    * </pre>
    * commutative.
    *
    * @param f first arrow
    * @param g second arrow
    * @return All such coequalizing pairs of arrows
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
    * Checks if a given object is a terminal object (aka unit).
    * Terminal object is the one that has just one arrow from every other object.
    */
  def isTerminal(t: Obj): Boolean =
    objects.forall((x: Obj) => isSingleton(hom(x, t)))

  /**
    * Checks if a given object is an initial object (aka zero).
    * An initial object is the one that has just one arrow to every other object.
    */
  def isInitial(i: Obj): Boolean = objects.forall(x => isSingleton(hom(i, x)))

  /**
    * Given a set of objects and a set of arrows, build a map that maps each object to
    * a set of arrows starting at it.
    *
    * @param setOfObjects objects for which to build the bundles.
    * @param arrows       arrows that participate in the bundles.
    * @return a map.
    */
  def buildBundles(setOfObjects: Objects, arrows: Arrows): Map[Obj, Arrows] =
    if (Params.fullCheck)
      val badArrows: Arrows = arrows filterNot (d0(_) ∈ setOfObjects)

      // TODO: return a Result
      require(badArrows.isEmpty, s"These arrows don't belong: ${asString(badArrows)} in $name")

    val grouped = arrows.groupBy(d0)
    val mor = SetMorphism.build(arrows, setOfObjects, d0).iHope.revert.function
    setOfObjects.map(o => o -> mor(o)).toMap withDefaultValue Set.empty[Arrow]

  /**
    * Builds a degree object (X×X... n times) for a given object.
    * The trick is, find an object that "is" the x we provided
    *
    * @param x the source object
    * @param n degree to which to raise object x
    * @return x^n^ and its projections to x
    */
  def degree(x: Obj, n: Int): Result[(Obj, List[Arrow])] =
    OKif(n >= 0) >>= {
      n match
        case 0 => terminal map (x => (x, List()))
        case 1 => Good((x, id(x) :: Nil))
        case _ => degree(x, n - 1).flatMap{
          case (x_n_1, previous_projections) =>
            product(x, x_n_1) map {
              case (p1, p_n_1) =>
                val projections = p1 :: previous_projections map (m(p_n_1, _))
                (d0(p1), projections.flatten)
            }
        }
    }

  /**
    * Collection of arrows that end at x
    * @param x an object
    * @return the collection
    */
  private def arrowsEndingAt(x: Obj): Arrows = arrows filter { x == d1(_) }

  private lazy val nonIdentities: List[Arrow] = listSorted(arrows filterNot isIdentity)

  private[cat] lazy val nontrivialArrows: List[Arrow] =
      // Remove compound arrows - those that were deduced during creation
      nonIdentities sortBy(_.toString) filterNot (_.toString.contains("∘")) reverse


  /**
    * Removes the arrows that are not required for drawing:
    * identities and uniquely determined compositions.
    *
    * @return a graph with the same nodes, but with fewer arrows
    */
  def baseGraph: Graph =
    // remove all those that are still deducible
    val essentialArrows = selectBaseArrows(nontrivialArrows)

// TODO: figure out which one is faster  
    Graph.build(name, nodes, essentialArrows.toSet, d0,  d1) iHope

  @tailrec
  private def selectBaseArrows(arrows: List[Arrow]): List[Arrow] =
    val isDeductible = canDeduce(arrows)  
    arrows.find(isDeductible) match
      case None => arrows
      case Some(f) => selectBaseArrows(arrows filterNot (f ==))


  private[cat] def canDeduce(arrows: Iterable[Arrow])(a: Arrow): Boolean =
    val from = d0(a)
    val to = d1(a)
    hom(from, to).size == 1 && arrows.exists {
      f => 
        val d1f = d1(f)
        d1f != from &&
        d1f != to &&
        arrows.exists { g => a ∈ m(f, g) }
    }

  /**
    * Checks whether an arrow is an identity
    * @param a an arrow
    * @return true iff it is an identity
    */
  def isIdentity(a: Arrow): Boolean = a == id(d0(a))

  /**
    * Split a category into connected components.
    * You may need this for drawing the category, otherwise it's probably useless.
    * 
    * TODO: figure out if it's better to move all such methods to a separate object
    * 
    * @return a set of components, each one being a category.
    */
  def connectedComponents: Set[Category] =
    val connected: BinaryRelation[Obj, Obj] =
      BinaryRelation((x, y) => arrows.exists(a =>
        (x == d0(a) && y == d1(a)) || (x == d1(a) && y == d0(a))))

    val sets = new FactorSet(objects, connected)

    for
      (s, i) <- sets.zipWithIndex
      cat <- completeSubcategory(s"$name.${i + 1}", s).asOption
    yield cat

  /**
    * Build a complete subcategory of this category, given its set of names.
    * 
    * @param newName the name we give to the subcategory
    * @param setOfObjects objects of the subcategory
    * @return the subcategory
    */
  def completeSubcategory(newName: String, setOfObjects: Objects): Result[Category] =
    val src = this
    subgraph(newName, setOfObjects) map {
      sub =>
        new Category(newName):
          override type Nodes = Set[Node]
          override type Arrows = Set[Arrow]
          def d0(f: Arrow) = sub.d0(f)
          def d1(f: Arrow) = sub.d1(f)
          override def nodes = sub.nodes.asInstanceOf[Nodes]
          override def arrows = sub.arrows.asInstanceOf[Arrows]

          override def id(o: Obj): Arrow = src.id(o)

          override def m(f: Arrow, g: Arrow): Option[Arrow] =
            src.m(f, g) map asArrow
    }


  /**
    * Creates an opposite category from this one.
    * That is, all arrows are inverted.
    *
    * @return this<sup>op</sup>
    */
  lazy val op: Category = Categories.op(this)


/**
  * Serves as a factory
  */
object Category extends CategoryFactory
