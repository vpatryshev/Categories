package math.sets

import math.Base.itsImmutable
import math.cat.SetMorphism
import math.sets.Functions.Injection
import math.sets.SetOps._
import scalakittens.{Good, Result}

import java.io.Reader
import scala.collection.immutable.AbstractSeq
import scala.language.postfixOps
import scala.reflect.ClassTag
import scala.util.parsing.combinator.RegexParsers

/**
  * Lazy sets functionality
  */
object Sets:
  /**
    * In set theory, set elements have no type
    */
  type set = Set[Any]

  type factorset = FactorSet[Any]

  /**
    * Untyper for sets
    */
  extension[T](s: Set[T])
    def untyped: set = s.asInstanceOf[set]

  /**
    * Traditional representation of the fact that a set has an unknown or an infinite size
    */
  val InfiniteSize: Int = Int.MaxValue

  /**
    * Standard empty set
    */
  val Empty: set = Set.empty[Any]

  /**
    * A singleton set. There are plenty of singletons, they are all isomorphic.
    */
  val Unit: set = Set(Empty)

  /**
    * A big set of all finite sets in Scala. This set is infinite, of course.
    */
  val FiniteSets: Set[Set[Any]] = BigSet.comprehension(isFinite)

  /**
    * Checks whether a set is infinite.
    * @param s
    * @return
    */
  def isInfinite(s: Set[_]): Boolean = s.size == InfiniteSize

  /**
    * Builds a union of two non-intersecting sets
    */
  def union[X: ClassTag, X1 <: X : ClassTag, X2 <: X : ClassTag](set1: Set[X1], set2: Set[X2]): Set[X] =
    lazy val parIterable: Iterable[X] = new ParallelIterable(set1, set2)
    lazy val size = if isFinite(set1) && isFinite(set2) then set1.size + set2.size else InfiniteSize

    def inX1(x: X) = x match
      case x1: X1 => try { set1(x1) } catch { case x: Exception => false }
      case _ => false

    def inX2(x: X) = x match
      case x2: X2 => try { set2(x2) } catch { case x: Exception => false }
      case _ => false

    setOf[X](parIterable,
      size,
      (x: X) => inX1(x) || inX2(x)
    )

  /**
    * Builds a union of non-intersecting sets
    */
  def union[X: ClassTag](sets: Iterator[Set[X]]): Set[X] =
    if !sets.hasNext then Set.empty[X]
    else
      val first = sets.next()
      val rest = union[X](sets)
      union[X,X,X](first, rest)

  /**
    * Builds a union of non-intersecting sets
    */
  def union[X: ClassTag](sets: Iterable[Set[X]]): Set[X] = union[X](sets.iterator)

  private def cat[T](p: (T, List[T])): List[T] = p._1 :: p._2

  /**
    * Builds a Cartesian product of a list of sets
    * @param xss list of sets
    * @tparam X type of sets elements
    * @return a Cartesian product of the sets: a set of lists
    */
  def product[X](xss: List[Set[X]]): Set[List[X]] =
    xss match
      case List() => Set(List())
      case head :: tail => product2(head, product(tail)) map cat

  private def pow(a: Int, b: Int): Int = if (b == 0) 1 else a * pow(a, b - 1)

  /**
    * Powerset of a set (set of all subsets)
    * @param xs the set
    * @tparam X set element type
    * @return pow(xs)
    */
  def pow[X](xs: Set[X]): Set[Set[X]] =
    val elements =
      xs.foldLeft(List(Set.empty[X]))((xss, x) => xss ::: xss.map(_ + x))
        
    setOf(
      elements,
      pow(2, xs.size),
      (sub: Set[X]) => sub subsetOf xs)

  /**
    * Set of all possible maps from set xs to set ys
    *
    * @param xs domain of maps
    * @param ys codomain of maps
    * @tparam X type of domain elements
    * @tparam Y type of codomain elements
    * @return the set of all possible maps
    */
  def exponent[X, Y](xs: Set[X], ys: Set[Y]): Set[Map[X, Y]] =
    setOf(allMaps(xs.toList, ys.toList),
      pow(ys size, xs size),
      (m: Map[X, Y]) => xs == m.keySet
    )

  private def allMaps[X, Y](xs: List[X], ys: List[Y]): List[Map[X, Y]] =
    xs.foldLeft(List(Map.empty[X, Y])) ((maps, x) =>
      maps flatMap (m => ys map (y => m + (x -> y)))
    )

  /**
    * Groups a set by a function, returning a function
    * given a set `xs` and a function `f: X -> Y``
    * returns a function `g: Y -> Set[X]` such that `g(y) = {x | f(x) = y}``
    * 
    * @param xs domain set for `f`
    * @param f a function `X -> Y`
    * @tparam X domain type for `f`
    * @tparam Y range type for `f`
    * @return a function
    */
  def groupBy[X, Y](xs: Set[X], f: X => Y): Y => Set[X] =
    y => Set.empty[X] ++ xs.filter(f(_) == y)

  /**
    * Groups a set by a function, returning a map
    * given a set `xs` and a function `f: X -> Y``
    * returns a function `g: Y -> Set[X]` such that `g(y) = {x | f(x) = y}``
    *
    * @param xs domain set for `f`
    * @param f a function `X -> Y`
    * @tparam X domain type for `f`
    * @tparam Y range type for `f`
    * @return a map
    */
  def groupedBy[X, Y](xs: Set[X], f: X => Y) = // groupBy[X,Y](xs, f)
    xs groupBy f withDefaultValue Set.empty[X]

  def pullback[X, Y, Z](xs: Set[X], ys: Set[Y], fx: X => Z, fy: Y => Z): Set[(X, Y)] =
    product2(xs, ys) filter (p => fx(p._1) == fy(p._2))

  /**
    * Cartesian product of two sets
    */
  def product2[X, Y](xs: Set[X], ys: Set[Y]): Set[(X, Y)] =
    val predicate = (p: (X, Y)) => xs.contains(p._1) && ys.contains(p._2)
    setOf(
      cantorIterable(xs, ys),
      if (isFinite(xs) && isFinite(ys)) xs.size * ys.size else InfiniteSize,
      predicate
    )

  def isFinite(s: Set[_]): Boolean = s.size != InfiniteSize

  def idMap[X](xs: Set[X]): Map[X, X] = buildMap(xs, identity)

  def buildMap[K, V](keys: Iterable[K], f: K => V) = keys map {k => k -> f(k)} toMap

  def toString(s: Set[_]): String = "{" + s.mkString(", ") + "}"

  def parse(input: Reader): Result[Set[String]] = (new SetParser).read(input)

  def parse(input: CharSequence): Result[Set[String]] = (new SetParser).read(input)

  def singleton[T](x: T): Set[T] = Set(x)

  def isSingleton[T](ts: Iterable[T]): Boolean =
    val i = ts.iterator
    i.hasNext && { i.next(); !i.hasNext }

  def existsUnique[T](seq: Iterable[T], p: T => Boolean): Boolean = isSingleton(seq filter p take 2)

  def filter[X](sourceSet: Set[X], p: X => Boolean): Set[X] =
    if (isFinite(sourceSet)) filteredSet(sourceSet, p)
    else setOf(sourceSet, InfiniteSize, p)

  private def filteredSet[X](i: => Iterable[X], p: X => Boolean): Set[X] =
    def j = i filter p
    val n = j.foldLeft(0)((k, x) => k + 1)
    setOf(j, n, p)

  /**
    * set of numbers from 0 to n-1
    *
    * @param n size of the set
    * @return the set of numbers
    */
  def numbers(n: Int): Set[Int] = numbers(0, n)

  /**
    * set of numbers from `from` to `to`
    *
    * @param from first element of numbers range
    * @param to   upper limit of the numbers in the range (exclusive)
    * @return the set of numbers
    */
  def numbers(from: Int, to: Int): Set[Int] = numbers(from, to, 1)

  /**
    * set of numbers from `from` to `to`, step `step`
    *
    * @param from first element of numbers range
    * @param to   upper limit of the numbers in the range (exclusive)
    * @param step range step
    * @return the set of numbers
    */
  def numbers(from: Int, to: Int, step: Int): Set[Int] = setOf(range(from, to, step))

  def range(first: Int, last1: Int, step: Int): Set[Int] =
    Range(first, last1, step).toSet

  private[Sets] def sizePlus(size: => Int, delta: Int): Int =
    if (size == InfiniteSize) size
    else if (delta < 0) Math.max(0, size + delta)
    else delta + Math.min(size, InfiniteSize - delta)

  class setOf[X] protected(
    source: => Iterable[X],
    sizeEvaluator: => Int,
    predicate: X => Boolean) extends
    setForIterable[X](source, sizeEvaluator, predicate):
      override def incl(elem: X): Set[X] = setOf[X](
        List(elem) ++ source,
        if (contains(elem)) sizeEvaluator else sizePlus(sizeEvaluator, 1),
        (x: X) => x == elem || predicate(x)
      )

      override def excl(elem: X): Set[X] = setOf[X](
        source filterNot(elem==),
        if (contains(elem)) sizePlus(sizeEvaluator, - 1) else sizeEvaluator,
        (x: X) => x != elem || predicate(x)
      )

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
  case class DisjointUnion[T](sets: List[Set[T]]):

    /**
      * @return the (virtual) set that is the disjoint union of given sets
      */
    def unionSet: Set[(Int, T)] =
      val tagged: Iterable[Set[(Int, T)]] = sets.zipWithIndex map {
        case (s, i) => s map (x => (i, x))
      }

      union(tagged)

    /**
      * @return the list of injections of original sets to their union
      */
    def injections: List[Injection[T, (Int, T)]] = sets.indices map injection toList

    /**
      * Maps an i-th set into the union
      *
      * @param i index of the set to inject in the list of sets
      * @return the injection (which is an Injection which is a Function))
      */
    def injection(i: Int): Injection[T, (Int, T)] = Functions.injection(t => (i, t))

  class InterleavingIterator[X, X1 <: X, X2 <: X](
    iterator1: Iterator[X1],
    iterator2: Iterator[X2]) extends Iterator[X]:
    private var i1i2: (Iterator[X], Iterator[X]) = (iterator1, iterator2)

    def hasNext: Boolean = iterator1.hasNext || iterator2.hasNext

    def next(): X =
      i1i2 = i1i2.swap
      (if (i1i2._1.hasNext) i1i2._1 else i1i2._2).next()

  class ParallelIterable[X, X1 <: X, X2 <: X](iterable1: Iterable[X1], iterable2: Iterable[X2])
    extends Iterable[X]:
    def iterator = new InterleavingIterator(iterable1.iterator, iterable2.iterator)

  class MapForFunction[K, +V](xs: Set[K], f: K => V) extends Map[K, V]:
    override def keys: Set[K] = xs

    override def apply(x: K): V =
      if (xs contains x) f(x) else throw new RuntimeException(s"oops, $x is not in domain")

    def updated[V1 >: V](kv: (K, V1)): Map[K, V1] = itsImmutable

    def removed(key: K): Map[K, V] = itsImmutable

    override def updated[V1 >: V](x: K, y: V1): Map[K, V1] = itsImmutable

    override def get(x: K): Option[V] = if (xs contains x) Some(f(x)) else None

    override def size: Int = xs size

    override def iterator: Iterator[(K, V)] = (xs map { (x:K) => (x, f(x)) }) iterator

  class SetParser extends RegexParsers:
    def read(input: CharSequence): Result[Set[String]] =
      parseAll(parserOfSet, input).get

    def read(input: Reader): Result[Set[String]] = parseAll(parserOfSet, input).get

    def parserOfSet: Parser[Result[Set[String]]] =
      "{" ~ repsep(word, ",") ~ "}" ^^ {
        case "{" ~ ms ~ "}" => Good(Set() ++ ms)
        case nonsense => Result.error(s"Failed to parse $nonsense")
      }

    def word: Parser[String] = regex("""[\w\\.]+""".r)

  private[math] class setForIterable[X](
    source: => Iterable[X],
    sizeEvaluator: => Int,
    predicate: X => Boolean) extends Set[X]:
    override def contains(x: X): Boolean = predicate(x)

    override def isEmpty: Boolean = !iterator.hasNext

    override def excl(x: X): Set[X] = new setForIterable(
      source filterNot(x ==),
      sizePlus(sizeEvaluator, -1),
      (y: X) => y != x && predicate(y))

    override def incl(x: X): Set[X] = new setForIterable(
      List(x) ++ source,
      sizePlus(sizeEvaluator, +1),
      (y:X) => y == x || predicate(y))

    def map[Y](f: Injection[X, Y]): Set[Y] =
      val source: Iterable[X] = this
      val target: Iterable[Y] = source.map(f)
      val predicate: Y => Boolean = (y: Y) => iterator exists { f(_) == y }
      setOf(target, size, predicate)

    override def size: Int = sizeEvaluator

    override def iterator: Iterator[X] = source.iterator filter predicate

    override def filter(p: X => Boolean): Set[X] =
      filteredSet(source, (x: X) => predicate(x) && p(x))

    override def hashCode: Int = if (isInfinite(this)) sample.hashCode else super.hashCode

    override def equals(other: Any): Boolean = other match
      case s: Set[_] => if (isInfinite(this)) this.eq(s) else super.equals(s)
      case somethingelse => false

    override def toString: String =
      if (isInfinite(this)) then
        sample.mkString("infinite Set(", ",", ",...)")
      else
        super.toString

    def sample: Set[X] = if (isInfinite(this)) take(3) else this

  object setOf:
    def elements[X](content: X*): setOf[X] = apply(content, x => content contains x)

    def apply[X](content: Iterable[X], predicate: X => Boolean): setOf[X] =
      apply(content, content.size, predicate)

    def apply[X](
      source: Iterable[X],
      sizeEvaluator: => Int,
      predicate: X => Boolean): setOf[X] =
      new setOf(source, sizeEvaluator, predicate)

    def apply[X](content: Iterable[X]): Set[X] = setOf(content, x => true)

  
  def main(args: Array[String]): Unit =
    val a = Set("a", "b", "c")
    val b = Set("x", "y")
    println("Some examples of set operations")

    println("Building a union of two sets, " + a + " and " + b)
    val u = union(a, b)
    println("Union size=" + u.size)
    println("Union itself is " + u)
    println("Trying to compare sets")
    println(s"Is $a equal to $b? ${a == b}")
    println(s"Is $u equal to $b? ${u == b}")
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
    val r = Set("c", "b", "a")
    println(s"Is $r equal to $q? ${Good(r) == q}")
    println(s"Does $q contain $a? ${q.contains("a")}")
    val tuples = for (arg <- Set(1, 2, 3)) yield ("key" + arg, "v" + arg)

    println(Map() ++ tuples)

    println("and now the product!")
    val source = List(Set("a", "b", "c"), Set("1", "2"), Set("Ebony", "Ivory"), Set("Hi", "Lo"))
    println(product(source))
