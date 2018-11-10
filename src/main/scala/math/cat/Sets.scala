package math.cat

import language.postfixOps
import java.io.Reader

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.parsing.combinator.RegexParsers
import Functions._

/**
  * Lazy sets functionality
  *
  * @author vpatryshev
  */
object Sets {

  /**
    * A big set of all finite sets in Java. This set is infinite, of course.
    */
  val FiniteSets: BigSet[Set[Any]] = BigSet((o: Set[_]) => o.size < Int.MaxValue)
  
  def requireImmutability = throw new UnsupportedOperationException("Immutable class")

  def setOf[X](
    sourceIterator: => Iterator[X],
    sizeEvaluator: => Int,
    predicate: X => Boolean): Set[X] =
    setForIterator(sourceIterator, sizeEvaluator, predicate)

  def setOf[X](
    source: Iterable[X],
    sizeEvaluator: => Int,
    predicate: X => Boolean): Set[X] =
    setForIterator(source.iterator, sizeEvaluator, predicate)

  def setOf[X](content: Iterable[X], predicate: X => Boolean): Set[X] =
    setOf(content, (0 /: content) ((n, x) => n + 1), predicate)

  def setOf[X](content: Iterable[X]): Set[X] =
    setOf(content, x => content exists (_ == x))

  def range(n: Int): Set[Int] = range(0, n, 1)
  
  def range(first: Int, last1: Int, step: Int): Set[Int] =
    Range(first, last1, step).toSet

  /**
    * Builds a union of two non-intersecting sets
    */
  def union[X : ClassTag, X1 <: X : ClassTag, X2 <: X : ClassTag](set1: Set[X1], set2: Set[X2]): Set[X] = {
    lazy val parIterable: Iterable[X] = new ParallelIterable(set1, set2)
    lazy val size = if (set1.size == Int.MaxValue ||
      set2.size == Int.MaxValue) {
      Int.MaxValue
    } else {
      set1.size + set2.size
    }

    def inX1(x: X) = x match {
      case x1: X1 => set1(x1)
      case _ => false
    }

    def inX2(x: X) = x match {
      case x2: X2 => set2(x2)
      case _ => false
    }

    setOf[X](parIterable,
      size,
      (x: X) => inX1(x) || inX2(x)
    )
  }

  /**
    * Builds a union of non-intersecting sets
    */
  def union[X](sets: Iterable[Set[X]]): Set[X] = {
    lazy val content = sets.flatten
    val result = setOf(content, (x: X) => sets exists (_ contains x))
    result
  }

  /**
    * Encapsulates disjoint union of a list of sets. Disjoint means that even if the 
    * sets contain common elements, the union will make them distinct by tagging all elements.
    * The result consists of pairs, the first being list index, the second an element of a set.
    * E.g. disjointUnion(LIst(singleton("a"), singleton("b")) returns
    * Set(Pair(0, "a"), Pair(1, "b")).
    *
    * @tparam T the type of elements in the sets being joined. The same for all sets (it's Java...)
    * @param sets the sets being joined
    */
  case class DisjointUnion[T](sets: List[Set[T]]) {

    /**
      * @return the (virtual) set that is the disjoint union of given sets
      */
    def unionSet: Set[(Int, T)] = {
      val tagged: Iterable[Set[(Int, T)]] = sets.zipWithIndex map {
        case (set, i) => set map (x => (i, x))
      }

      union(tagged)
    }

    /**
      * Maps an i-th set into the union
      *
      * @param i index of the set to inject in the list of sets
      * @return the injection (which is an Injection which is a Function))
      */
    def injection(i: Int): Injection[T, (Int, T)] = Functions.injection(t => (i, t))

    /**
      * @return the list of injections of original sets to their union
      */
    def injections: List[Injection[T, (Int, T)]] = sets.indices map injection toList
  }

  def cantorIterator[X, Y](xs: Iterable[X], ys: Iterable[Y]): Iterator[(X, Y)] =
    new Iterator[(X, Y)] {
      private var iterators: mutable.Queue[Iterator[Y]] = mutable.Queue()
      private var xi = xs.iterator
      private var yi: Iterator[Iterator[Y]] = Iterator.empty
      private var shift = 0

      def next: (X, Y) = {
        if (!yi.hasNext) {
          if (xi.hasNext) {
            iterators enqueue ys.iterator
          }
          yi = iterators.iterator
          xi = xs.iterator.drop(shift)
        }

        val yii = yi.next
        val y = yii.next
        val res = (xi.next, y)

        if (iterators.nonEmpty && yii.isEmpty) {
          iterators.dequeue
          shift += 1
        }

        res
      }

      def hasNext: Boolean = xs.nonEmpty && ys.nonEmpty &&
        (xi.hasNext || (iterators.nonEmpty && iterators.head.hasNext))
    }

  /**
    * Cartesian product of two sets
    */
  def product2[X, Y](xs: Set[X], ys: Set[Y]): Set[(X, Y)] = {
    val predicate = (p: (X, Y)) => xs.contains(p._1) && ys.contains(p._2)
    setOf(
      cantorIterator(xs, ys),
      xs.size * ys.size,
      predicate
    )
  }

  def cat[T](p: (T, List[T])): List[T] = p._1 :: p._2

  def product[X](xss: Iterable[Set[X]]): Set[List[X]] =
    xss match {
      case List() => Set(List())
      case head :: tail => product2(head, product(tail)) map cat
    }

  def pow(a: Int, b: Int): Int = if (b == 0) 1 else a * pow(a, b - 1)

  def powerset[X](xs: Set[X]): Set[Set[X]] =
    setOf((List(Set.empty[X]) /: xs) ((xss, x) => xss ::: xss.map(_ + x)),
      pow(2, xs.size),
      (sub: Set[X]) => sub subsetOf xs)

  def split[X](xs: Iterable[X]): (X, Iterable[X]) = (xs.iterator.next, xs drop 1)

  /**
    * Set of all possible maps from set xs to set ys
    * @param xs domain of maps
    * @param ys codomain of maps
    * @tparam X type of domain elements
    * @tparam Y type of codomain elements
    * @return the set of all possible maps
    */
  def exponent[X, Y](xs: Set[X], ys: Set[Y]): Set[Map[X, Y]] =
    setOf(exponentElements(ys, xs),
      pow(ys size, xs size),
      (m: Map[X, Y]) => xs == m.keySet
    )

  def groupBy[X, Y](xs: Set[X], ys: Set[Y], f: X => Y): Y => Set[X] = {
    y => Set.empty[X] ++ xs.filter(f(_) == y)
  }

  def pullback[X, Y, Z](xs: Set[X], ys: Set[Y], fx: X => Z, fy: Y => Z): Set[(X, Y)] =
    product2(xs, ys) filter (p => fx(p._1) == fy(p._2))

  /**
    * Builds a factorset epimorphism that projects a set to its factorset,
    * given a set and binary relationship.
    * Factoring is done on the relationship's transitive closure.
    *
    * @tparam T set element type
    * @param set the main set
    * @param r   binary relationship (not necessarily equivalence relationship) that determines factoring
    * @return factorset epimorphism
    */
  def factorset[T](set: Set[T], r: BinaryRelation[T, T]): SetMorphism[T, Set[T]] = {
    val factory = new FactorSet[T](set, r)
    SetMorphism(set, factory.content, factory.asFunction)
  }

  def idMap[X](xs: Set[X]): MapForFunction[X, X] = buildMap(xs, identity)

  def buildMap[K, V](keys: Set[K], f: K => V) = new MapForFunction(keys, f)

  def toString(s: Set[_]): String = "{" + s.mkString(", ") + "}"

  def parse(input: Reader): Set[String] = (new Parser).read(input)

  def parse(input: CharSequence): Set[String] = (new Parser).read(input)

  def isSingleton[T](ts: Iterable[T]): Boolean = {
    val i = ts.iterator
    i.hasNext && { i.next; !i.hasNext }
  }
  
  def main(args: Array[String]) {
    val a = Set("a", "b", "c")
    val b = Set("x", "y")
    println("Some examples of set operations")

    println("Building a union of two sets, " + a + " and " + b)
    val u = union(a, b)
    println("Union size=" + u.size)
    println("Union itself is " + u)
    println("Trying to compare sets")
    println(a == b)
    println(u == b)
    println("Making a lazy copy")
    val newb = setOf(b, 2, (x: String) => b contains x)
    println("same size? " + (newb.size == b.size))
    println("Equal to source? " + (b == newb))

    println("Let's build an exponent")
    val a2b = exponent(b, a)
    println("We have an exponent of size " + a2b.size + ", but its still under construction")
    val anElement = Map("x" -> "b", "y" -> "a")
    println("Check membership for " + anElement + ": " + (a2b contains anElement))
    println("We use exponent, but its elements are still not listed... but wait: ")
    println(a2b)
    val q = parse("{a, b, c}")
    println(q)
    println(Set("c", "b", "a") == q)
    println(q.contains("a"))
    val tuples = for (arg <- Set(1, 2, 3)) yield {
      ("key" + arg, "v" + arg)
    }

    println(Map() ++ tuples)

    println("and now the product!")
    val source = List(Set("a", "b", "c"), Set("1", "2"), Set("Ebony", "Ivory"), Set("Hi", "Lo"))
    println(product(source))
  }

  private def filteredSet[X](i: => Iterator[X], p: X => Boolean): Set[X] = {
    def j = i filter p

    def n = (0 /: j) ((k, x) => k + 1)

    setOf(j, n, p)
  }

  private[Sets] def sizePlus(size: => Int, delta: Int): Int =
    if (size == Int.MaxValue) size
    else if (delta < 0) Math.max(0, size + delta)
    else delta + Math.min(size, Int.MaxValue - delta)

  private def setForIterator[X](
    sourceIterator: => Iterator[X],
    sizeEvaluator: => Int,
    predicate: X => Boolean): Set[X] = {
    val s: Set[X] = new Set[X] {
      override def contains(x: X) = predicate(x)

      override def size: Int = sizeEvaluator

      override def iterator: Iterator[X] = sourceIterator filter predicate

      override def isEmpty: Boolean = !iterator.hasNext

      override def -(x: X): Set[X] = setForIterator(
        sourceIterator,
        sizePlus(sizeEvaluator, -1),
        y => y != x && predicate(y))

      override def +(x: X): Set[X] = setForIterator(
        List(x).iterator ++ sourceIterator,
        sizePlus(sizeEvaluator, +1),
        y => y == x || predicate(y))

      def map[Y](f: Injection[X, Y]): Set[Y] = f.applyTo(this)

      override def filter(p: X => Boolean): Set[X] =
        filteredSet(sourceIterator, (x: X) => predicate(x) && p(x))
    }
    s
  }

  /**
    * set of numbers from 0 to n-1
    * @param n size of the set
    * @return the set of numbers
    */
  def numbers(n: Int): Set[Int] = numbers(0, n)

  /**
    * set of numbers from `from` to `to`
    * @param from first element of numbers range
    * @param to upper limit of the numbers in the range (exclusive)
    * @return the set of numbers
    */
  def numbers(from: Int, to: Int): Set[Int] = numbers(from, to, 1)

  /**
    * set of numbers from `from` to `to`, step `step`
    * @param from first element of numbers range
    * @param to upper limit of the numbers in the range (exclusive)
    * @param step range step
    * @return the set of numbers
    */
  def numbers(from: Int, to: Int, step: Int): Set[Int] = setOf(range(from, to, step))
  
  private def exponentElements[X, Y](ys: Iterable[Y], xs: Iterable[X]): Iterable[Map[X, Y]] = {
    if (xs.iterator hasNext) {
      val (x, tail) = split(xs)
      for (y <- ys;
           z <- exponentElements(ys, tail))
        yield {
          z + (x -> y)
        }
    } else List(Map[X, Y]())
  }

  class InterleavingIterator[X, X1 <: X, X2 <: X](
      iterator1: Iterator[X1],
      iterator2: Iterator[X2]) extends Iterator[X] {
    private var i2: (Iterator[X], Iterator[X]) = (iterator1, iterator2)

    def hasNext: Boolean = iterator1.hasNext || iterator2.hasNext

    def next: X = {
      i2 = i2.swap
      if (i2._1.hasNext) i2._1.next else i2._2.next
    }
  }

  class ParallelIterable[X, X1 <: X, X2 <: X](iterable1: Iterable[X1], iterable2: Iterable[X2])
    extends Iterable[X] {
    def iterator = new InterleavingIterator(iterable1.iterator, iterable2.iterator)
  }

  /**
    * Implements factorset functionality. A factorset may be built given a BinaryRelation,
    * or incrementally, when the client provides pairs of equivalent elements.
    * The factorset is not lazy; equivalence classes are stored in a map, and the resulting
    * factorset is returned as a new HashSet.
    *
    * @tparam X element type
    */
  class FactorSet[X](set: Set[X]) {

    /**
      * @return the latest version of factorset built here.
      */
    lazy val content: Set[Set[X]] = equivalenceClasses.values.toSet
    /**
      * Maps elements of the main set to their equivalence classes (they constitute the factorset).
      */
    private var equivalenceClasses: Map[X, Set[X]] = (Map[X, Set[X]]() /: set) ((m, x) => m + (x -> Set(x)))

    /**
      * Builds a factorset of a given set, by the transitive closure of a given relationship.
      *
      * @param set base set
      * @param r   binary relationship
      */
    def this(set: Set[X], r: BinaryRelation[X, X]) {
      this(set)
      factorByRelationship(r)
    }

    /**
      * Given a <code>BinaryRelation r</code>, merges equivalent classes if they
      * contain elements that are in <code>r</code>.
      *
      * @param r the binary relationship. Does not have to be symmetrical or transitive.
      */
    def factorByRelationship(r: BinaryRelation[X, X]): Unit =
      for {
        x1 <- set
        x2 <- set
        if r(x1, x2) || r(x2, x1)} merge(x1, x2)

    /**
      * Merges equivalence classes for two elements
      *
      * @param x1 first element
      * @param x2 second element
      */
    def merge(x1: X, x2: X) {
      for (
        class1 <- equivalenceClasses.get(x1);
        class2 <- equivalenceClasses.get(x2)) {
        val merged: Set[X] = class1 ++ class2
        for (x3 <- merged) {
          equivalenceClasses = equivalenceClasses + (x3 -> merged)
        }
      }
    }

    /**
      * @return the domain set.
      */
    def domain: Set[X] = set

    /**
      * @return the function from the domain set to the factorset.
      */
    def asFunction: Function[X, Set[X]] = equivalenceClasses
  }

  class MapForFunction[K, +V](xs: Set[K], f: K => V) extends Map[K, V] {
    override def keys: Set[K] = xs

    override def apply(x: K): V =
      if (xs contains x) f(x) else throw new RuntimeException("oops, no value")

    override def +[V1 >: V](kv: (K, V1)): Map[K, V1] = requireImmutability

    override def -(x: K): Map[K, V] = requireImmutability

    override def updated[V1 >: V](x: K, y: V1): Map[K, V1] = requireImmutability

    override def get(x: K): Option[V] = if (xs contains x) Some(f(x)) else None

    override def size: Int = xs size

    override def iterator: Iterator[(K, V)] = (xs map { x: K => (x, f(x)) }) iterator

    //    override def seq = (xs map { x: K => (x, f(x)) })
  }

  class Parser extends RegexParsers {
    def read(input: CharSequence): Set[String] = parseAll(set, input).get

    def set: Parser[Set[String]] = "{" ~ repsep(member, ",") ~ "}" ^^ { case "{" ~ ms ~ "}" => Set() ++ ms }

    def member: Parser[String] = regex("""[\w\\.]+""".r)

    def read(input: Reader): Set[String] = parseAll(set, input).get
  }
}