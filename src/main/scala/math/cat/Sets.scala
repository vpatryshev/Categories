package math.cat

  import Functions._
  
object Sets {
  // TODO: ask people about what's the best name for my own kind of sets Ens?
  // TODO: introduce that kind; define implicit transformation from a set to this class
  // TODO: define union and product and powerset and exponent in this new category
  
  
  def parseSet(s: String): Set[String] = {
    val content: Array[String] = s.substring(1, s.length - 1).split(",\\s*").map(_.trim).filter(!_.isEmpty)
    (Set.empty[String] /: content) (_+_)
  }

  def groupBy[X, Y](xs: Set[X], ys: Set[Y], f: X => Y): Y => Set[X] = {
    y => Set.empty[X] ++ xs.filter(f(_) == y)
  }   

  def setOf[X](iterable: () => Iterator[X], size: => Int, contains: X => Boolean) = new Set[X] {
      override def contains(x: X) = contains(x)
      override def size = size
      override def -(x: X): Set[X] = throw new UnsupportedOperationException
      override def +(x: X): Set[X] = throw new UnsupportedOperationException
//      override def empty[Y]: Set[Y] = throw new UnsupportedOperationException
//      override def iterator = iterable()
  }

  def setOf[X](elements: Iterable[X], size: => Int, contains: X => Boolean) = new Set[X] {
      override def contains(x: X) = contains(x)
      override def size = size
      override def -(x: X): Set[X] = throw new UnsupportedOperationException
      override def +(x: X): Set[X] = throw new UnsupportedOperationException
   //   override def empty[Y]: Set[Y] = throw new UnsupportedOperationException
      override def iterator = elements.iterator
  }
  
  def union[X] (set1: scala.collection.Set[X], set2: scala.collection.Set[X]): scala.collection.Set[X] = setOf(
    set1 ++ set2, 
    set1.size + set2.size, 
    (x: X) => (set1 contains x) || (set2 contains x)
  )
    
  def product[X, Y](xs: scala.collection.Set[X], ys: scala.collection.Set[Y]): scala.collection.Set[(X, Y)] = setOf(
    () => (for (x <- xs; y <- ys) yield (x, y)),
    xs.size * ys.size,
    (p: (X, Y)) => xs.contains(p._1) && ys.contains(p._2)
  )
  
  def powerset[X](xs: scala.collection.Set[X]) = setOf((List(Set.empty[X]) /: xs) ((xss, x) => xss ::: xss.map(_ + x)), 2 ^ xs.size, (sub: Set[X]) => (sub subsetOf xs))

  def exponent[X, Y](ys: scala.collection.Set[Y], xs: scala.collection.Set[X]) = {
    (Set(Map.empty[X,Y])/: xs) { // initially the set is empty; will append mappings for each x
      (set, x) => { // given a set of maps and an x, build a new set, where maps are defined on x
        val pairs = { for (y <- ys; map <- set) yield (map, y) } // will append x->y to each map for each y for given x
        (Set(Map.empty[X,Y]) /: pairs) {
          (newSetOfMaps, p) => {
            val map: Map[X, Y] = p._1
            val y: Y = p._2
            newSetOfMaps + (map + (x->y)) 
          }
        }
      }  
    }
  }
}

/**
 * All kinds of additional Set functionality.
 * 
 * @author Vlad Patryshev
 * All source code is stored on <a href="http://code.google.com/p/categories/">http://code.google.com/p/categories/</a>
 * 
 */
object SetsAlt {

  /**
   * This class serves as a namespace for Cartesian product of iterables.
   * <p/>
   * Given a sequence of sequences, Cartesian.product builds a product
   * of these sequences.
   * <p/>
   * Type parameters:
   * @tparam X the type of elements of each sequence (must be the same)
   * @tparam XS the type of sequences, must extend Set[X].
   */
  class Cartesian[X, XS <: Set[X]] {
    /**
     * Public method that builds the Cartesian product. The product is lazy.
     * @tparam X element type
     * @tparam XS set type
     *
     * @param xss sequence of sequences that are being multiplied.
     * @return Cartesian product of the sequences.
     */
    def product[X, XS <: Set[X]] (xss: Iterable[XS]): Set[Iterable[X]] =
      new Cartesian[X, XS].calculateProduct(xss)
    
    /**
     * Public method that builds the Cartesian product. The product is lazy.
     * @tparam X element type
     * @tparam XS set type
     *
     * @param xss list of sets that are being multiplied.
     * @return Cartesian product of the sets.
     */
    def product[X, XS <: Set[X]] (xss: List[XS]): Set[List[X]] =
      new Cartesian[X, XS].calculateProduct(xss)

    /**
     * Builds a function that builds projections from a Cartesian product of a list of sets
     * to its components
     * @tparam X element type
     *
     * @return a function that for each i returns a function just takes an i-th element of a list.
     */
    def projection[X]: Function[Integer, Function[List[X], X]] = 
      (i: Integer) => ((xs: List[X]) => xs(i))

    /**
     * Public method that takes a vararg of sequences and builds their Cartesian product.
     * @tparam X element type
     * @tparam XS set type
     *
     * @param xss (a plurality of) sequences that are being multiplied.
     * @return Cartesian product of the sequences.
     */
    def product[X, XS <: Set[X]](xss: XS*): Iterable[List[X]] = product(xss)

    /**
     * Actually builds the product of the sequence of sequences.
     *
     * @param xss sequences to multiply.
     * @return their Cartesian product.
     */
    private def calculateProduct[X, XS](xss: Iterable[XS]): Set[Iterable[X]]  = {
      xss.foldRight(Set(List[X]())){case (xs:XS, current:Set[Iterable[X]]) => appendComponentToProductOfIterable(xs, current map (_.asInstanceOf[Iterable[X]]))}

    }

    /**
     * Actually builds the product of the list of sets.
     *
     * @param xss list of sets to multiply.
     * @return their Cartesian product.
     */
    private def calculateProduct(xss: List[XS]): Set[List[X]] = 
      if (xss.isEmpty) {
        singleton(List[X]())
      } else {
        appendComponentToProductOfIterable(xss.head, calculateProduct(xss.tail))
      }  

    /**
     * Given a Cartesian product of zero or more sequences (components), appends
     * yet another component. So that if we have A0 and A1 x A2 x A3,
     * we get A0 x A1 x A2 x A3.
     *
     * @param component      a component to append to the head of Cartesian product.
     * @param partialProduct the product of zero or more components of the same kind.
     * @return the new product.
     */
    private def appendComponentToProductOfIterable(component: XS, partialProduct: Set[Iterable[X]]): Set[Iterable[X]] =
     {
      // E.g. [1, 2, 3], [[4, 6], [5, 6]] ->
      // [[[1, 4, 6], [1, 5, 6]], [[2, 4, 6], [2, 5, 6]], [[3, 4, 6], [3, 5, 6]]] ->
      // [[1, 4, 6], [1, 5, 6], [2, 4, 6], [2, 5, 6], [3, 4, 6], [3, 5, 6]]
      val concatenatedComponents: Set[Set[Iterable[X]]] = consWithIterableProduct(partialProduct).map(component)
      union(concatenatedComponents)
    }

    /**
     * Given a Cartesian product of zero or more sequences (components), appends
     * yet another component. So that if we have A0 and A1 x A2 x A3,
     * we get A0 x A1 x A2 x A3.
     *
     * @param component      a component to append to the head of Cartesian product.
     * @param partialProduct the product of zero or more components of the same kind.
     * @return the new product.
     */
    private def
      appendComponentToProductOfList(component: XS, partialProduct: Set[List[X]]): Set[List[X]] = {
      // E.g. [1, 2, 3], [[4, 6], [5, 6]] ->
      // [[[1, 4, 6], [1, 5, 6]], [[2, 4, 6], [2, 5, 6]], [[3, 4, 6], [3, 5, 6]]] ->
      // [[1, 4, 6], [1, 5, 6], [2, 4, 6], [2, 5, 6], [3, 4, 6], [3, 5, 6]]
      val concatenatedComponents: Set[Set[List[X]]] = consWithListProduct(partialProduct).map(component)
      union(concatenatedComponents)
    }
    
    /**
     * Takes [[x11,x12,...],[x21,x22,...]], that is, the product of n-1 components,
     * and produces a function that, given an x, returns a sequence of
     * [[x,x11,x12,...],[x,x21,x22,...]].
     * @param <ElementOfProduct> type of product element
     *
     * @param pxs the product of n-1 iterables (components)
     * @return the product of singleton {x} and the given product
     */

    /**
     * Takes [[x11,x12,...],[x21,x22,...]], that is, the product of n-1 components,
     * and produces a function that, given an x, returns a sequence of
     * [[x,x11,x12,...],[x,x21,x22,...]].
     * @param <ElementOfProduct> the type of product element (probably a list of X)
     *
     * @param pxs the product of n-1 lists (components)
     * @return the product of singleton {x} and the given product
     */
    def consWithListProduct[ElementOfProduct <: List[X]] (pxs: Set[ElementOfProduct]): Injection[X, Set[List[X]]] = 
      (x: X) => pxs.map(x :: _)
  }

  def f : Integer = 1

  /**
   * Given an iterable of elements, builds a new (lazy) set with elements from that iterable.
   * The size of the set is unknown; we could calculate it, of course, but we should not.
   * Since its size is unknown, you should not use size() or isEmpty().
   * That's a phisosophical issue. Usually programmers check the size
   * of a set out of fear that they would be caught scanning an empty list. It's okay actually.
   *
   * @param elements an iterable listing the elements of the new set.
   * @return a set that contains all these elements.
   */
   def SetOfUnknownSize[X](theElements: Iterable[X]): Set[X] = {
    new AbstractSet[X]() {
      override def elements(): Iterator[X] = return theElements.elements
      override def size: Integer = Integer.MAX_VALUE
  }

  /**
   * Builds a (virtual) set of numbers 0..n-1
   * @param n the number of numbers
   * @return the set
   */
  def numbers(n: Integer): Set[Integer] = numbers(0, n)

  /**
   * Builds a (virtual) set of numbers a..b-1
   * @param a the first number
   * @param b the last but one number
   * @return the set
   */
  def numbers(a: Integer, b: Integer): Set[Integer] = numbers(a, b, 1)

  /**
   * Builds a (virtual) set of numbers from a to b-c with step c
   * @param a the first number
   * @param b the last but one number
   * @param c step; can be negative
   * @return the set
   */
  def numbers(a: Integer, b: Integer, c: Integer): Set[Integer] = {
    val delta = b - a
    val dir = if (c < 0) 1 else -1

    val setSize = if (c == 0 || delta == 0 || c * delta < 0) 0 else
        Math.max(0, (delta + c + dir) / c)

    new scala.collection.AbstractSet[Integer]() {

      override def elements: Iterator[Integer] = {
        new Iterator[Integer]() {
          private var i = 0;
          
          def hasNext: Boolean = i < setSize

          def next: Integer = {
            if (hasNext) then a + (i++) * c
            else throw new NoSuchElementException()
          }
          
          def remove {
            throw new UnsupportedOperationException();
          }
        }
      }

      override def size = setSize;
    }
  }

  /**
   * Take an iterable, trust it to be non-repeating,
   * return a virtual set based on the iterable's elements.
   * 
   * @param [T] element type
   * @param iterable source of elements
   * @return a set consisting of the iterable's elements.
   */
  def Set[T] (iterable: Iterable[T]): Set[T] = {
    new scala.collection.AbstractSet[T]() {
      override def elements: Iterator[T] = iterable.elements
      override def isEmpty: boolean = elements.isEmpty
      override def size = countEntries(this);
    }
  }

  /**
   * Extracts a set from a string as produced by Set.toString().
   * Entries in the set can not be empty strings and cannot contain commas.
   *
   * @param s the string
   * @return restored set
   */
  def parseSet(s: String): Set[String] = {
    val content = s.substring(1, s.length() - 1).split(",\\s*").map(_.trim).filter(_.nonEmpty)

//    val empty: Set[String] = Set[String]()
    Set[String](content) // TODO: fix it, should not work: repetitions
    //(empty /: content)(+)
  }

  /**
   * Builds a (virtual) map of unknown size, based on a given iterable of pairs.
   * You cannot know the size of an iterable without scanning it; but still
   * maps based on unlimited collections (iterables) are pretty useful; the
   * only problem is halting problem: if the iterable is infinite, and the key
   * cannot be found, it will run forever looking.
   * 
   * @tparam X key type
   * @param [Y] value type
   * @param [P] the type of key-value pairs
   * @param pairs key-balue pairs
   * @return the map
   */
  def Map[X, Y, P <: Pair[X, Y]](pairs: Iterable[P]): Map[X, Y] = {
    Injection[P, Entry[X, Y]] cast = new Inclusion[Entry[X, Y], P]();

    new AbstractMap[X, Y]() {
      override def entrySet: Set[Entry[X, Y]] = cast.map(SetOfUnknownSize(pairs));
    }
  }
/* probably it is just a toMap*/
  /**
   * Creates a (virtual) map based on a given set of key-value pairs
   * @tparam X key type
   * @param [Y] value type
   * @param [P] the type of key-value pairs
   * @param pairs the pairs
   * @return the (virtual) map
   *
  def [X, Y, P extends Pair[X, Y]] Map[X, Y] Map(final Set[P> pairs) {
    Injection[P, Entry[X, Y]] cast = new Inclusion<Entry[X, Y], P>();

    return new AbstractMap[X, Y]() {
      @Override
      public Set[Entry[X, Y]] entrySet() {
        return cast.map(pairs);
      }
    };
  }
*/
  /**
   * Builds a (virtual) set of all maps from one set (<code>setX</code>) to another (<code>setY</code>).
   * 
   * @tparam X the type of elements of the first set (<code>setX</code>)
   * @tparam Y the type of elements of the second set (<code>setY</code>)
   * @param setX the first set (domain)
   * @param setY the second set (codomain)
   * @return the set of all maps
   */
  def allMaps[X,Y] (setX: Set[X], setY: Set[Y]): Set[Map[X, Y]] = {
    type SetOfPairs = Set[Pair[X, Y]]
    val pairBuilder: Injection[X, SetOfPairs] =  new Injection[X, SetOfPairs] {
      // this function builds pairs (x, y) for all y for a given x
      override def apply(x:X): Set[Pair[X, Y]] = Pair.withLeft(x).map(setY)
    }

    val sections: Set[Set[Pair[X, Y]]] = pairBuilder.map(setX)

    val product: Set[Iterable[Pair[X, Y]]] = Cartesian.product(sections)
    val setOfSetsOfPairs: Set[Set[Pair[X, Y]]] =
        Set(new IterableToSet[Pair[X, Y]].map(product))
    new PairsToMap[X, Y].map(setOfSetsOfPairs)
  }

  /**
   * Factory method. Builds a function that makes one set a factorset of another.
   * 
   * @tparam X the main set element type
   * @param set the set
   * @param factorset the subset
   * @return projection from the set to the factorset.
   */
  def factorset[X](set: Set[X], factorset: Set[Set[X]]): Function[X, Set[X]] = {
    require(set == union(factorset), "factorset's components should contain all elements of main set")
    var size = 0;
    for (component <- factorset) {
      require(component.size > 0, "factorset component should be non-empty")
      size += component.size
    }
    require(size <= set.size, "factorset should be disjoint")

    (x: X) => {
      factorset find (_ contains x) match {
        case Option(component) => component
        case _ => throw new IllegalArgumentException("Factorset component not found for " + x);
      }
    }
  }

  /**
   * Implements factorset functionality. A factorset may be built given a BinaryRelationship,
   * or incrementally, when the client provides pairs of equivalent elements.
   * The factorset is not lazy; equivalence classes are stored in a map, and the resulting
   * factorset is returned as a new HashSet.
   * 
   * @tparam X element type
   */
  class FactorSet[X](baseSet: Set[X]) {
    /**
     * The set being factored.
     */
    val set = baseSet
    
    /**
     * Maps elements of the main set to their equivalence classes (they constitute the factorset).
     */
    var equivalenceClasses: Map[X, Set[X]] = (Map[X, Set[X]]() /: set)((map, x) => map + (x -> Set(x)))

    /**
     * Builds a factorset of a given set, by the transitive closure of a given relationship.
     *
     * @param set base set
     * @param r   binary relationship
     */
    def this(baseSet: Set[X], r: BinaryRelationship[X, X]) {
      this(set)
      factorByRelationship(r)
    }

    /**
     * Given a <code>BinaryRelationship r</code>, merges equivalent classes if they
     * contain elements that are in <code>r</code>.
     *
     * @param r the binary relationship. Does not have to be symmetrical or transitive.
     */
    def factorByRelationship(BinaryRelationship[X, X] r): Unit {
      for (x1 <- set) {
        for (x2 <- set) {
          if (x1 == x2) {
            // skip same value
          } else if (equivalenceClasses(x1) == equivalenceClasses(x2)) {
            // skip same class
          } else if (r(x1, x2) || r(x2, x1)) {
            merge(x1, x2);
          }
        }
      }
    }

    /**
     * Merges equivalence classes for two elements
     *
     * @param x1 first element
     * @param x2 second element
     */
    def merge(x1: X, x2: X): Unit {
      val class1 = equivalenceClasses(x1)
      val class2 = equivalenceClasses(x2)
      val merged = class1 + class2
      for (x3 <- merged) {
        equivalenceClasses += (x3 -> merged);
      }
    }

    /**
     * @return the latest version of factorset built here.
     */
    def factorset: Set[Set[X]] = Set[Set[X]](equivalenceClasses.values());
    }

    /**
     * @return the function from the domain set to the factorset.
     */
    def asFunction: = equivalenceClasses(_)

    /**
     * @return the domain set.
     */
    def domain = set
  }

  /**
   * Builds a union of disjoint sets.
   *
   * @param [T] element type
   * @param components sets to be used in building the union.
   * @return a set containing the elements of all the given sets.
   */
  def union[T](components: Set[T]*): Set[T] =  union(Arrays.asList(components))

  /**
   * Builds a union of an Iterable of disjoint sets.
   *
   * @param [T] element type
   * @param iterableOfSets provides sets to be used in building the union.
   * @return a set containing the elements of all the sets of the Iterable.
   */
  def union[T](Iterable[Set[T]] iterableOfSets): Set[T] = 
    new AbstractSet[T]() {
      override def elements = {
        Iterator[Iterable[T]] top = iterableOfSets.elements
        require(top != null, "Top iterable's iterator() should not be null")
        return new Iterator[T]() {
          Iterator[T] current = Base.emptyIterator();

          def hasNext: Boolean = {
            while (!current.hasNext && top.hasNext) {
              Iterable[T] nextTop = top.next
              require(nextTop != null, "Top level iterator should not return nulls")
              current = nextTop.elements
            }
            return current.hasNext
          }

          def next: T = {
            if (hasNext) {
              current.next
            } else {
              throw new NoSuchElementException
            }
          }

          def remove: Unit {
            current.remove
          }
        }
      }

      override def size: Integer = (0 /: iterableOfSets)( _ + _.size)
  }

  
  /**
   * Encapsulates disjoint union of a list of sets. Disjoint means that even if the 
   * sets contain common elements, the union will make them distinct by tagging all elements.
   * The result consists of pairs, the first being list index, the second an element of a set.
   * E.g. disjointUnion(LIst(singleton("a"), singleton("b")) returns
   * Set(Pair(0, "a"), Pair(1, "b")).
   * 
   * @param [T] the type of elements in the sets being joined. The same for all sets (it's Java...)
   */
  def class DisjointUnion[T](sets: List[Set[T]]) {
    
    /**
     * @return the (virtual) set that is the disjoint union of given sets
     */
    def unionSet: Set[Pair[Integer, T]] {
      val indexed = zip(range(sets.size()), sets)
      val tojoin = (pair: Pair[Integer, Set[T]]) => injection(pair._1).map(pair._2)}.map(indexed);
      return union(tojoin)
    }
    
    /**
     * Maps an i-th set into the union
     * @param i index of the set to inject in the list of sets
     * @return the injection (which is an Injection which is a Function))
     */
    def injection(i: Integer): Injection[T, Pair[Integer, T]] = BasePair.[Integer, T]withLeft(i)

    /**
     * @return the list of injections of original sets to their union
     */
    def injections: List[Injection[T, Pair[Integer, T]]] = range(sets.size) map ((index: Integer) => injection(index)).map())
  }
  
  /**
   * Builds a Cartesian product of two sets.
   * 
   * @tparam X element type for the first set
   * @param [Y] element type for the second set
   * @param x first product component.
   * @param y second product component.
   * @return a product set, contains all possible pairs of elements from x and y.
   */
  def product[X, Y](x: Set[X], y: Set[Y]) : Set[Pair[X, Y]] = {

import math.cat.Functions.Injection

import math.cat.Functions.Injection

import scala.collection.AbstractSet

import scala.collection.AbstractSet

import scala.collection.AbstractSet

import scala.collection.AbstractSet

import scala.collection.AbstractSet

new AbstractSet[Pair[X, Y]]() {
      override def Iterator[Pair[X, Y]] elements = buildProductIterator(x, y)
      override def size = x.size * y.size
    }
  }

  /**
   * Given two sets, xs and ys, and a function f: xs->ys,
   * builds a map ys->xss, for each y returning all those x in xs that f(x)=y.
   * 
   * @tparam X domain element type
   * @param [Y] codomain element type
   * @param xs domain
   * @param ys codomain
   * @param f function
   * @return the map
   */
   def groupBy[X, Y](xs: Set[X], ys: Set[Y], f: X => Y): Map[Y, Set[X]] = {
    Map[Y, Set[X]] result = (new Map[Y, Set[X]]() /: ys)(_ + (_ -> Set[X]()))
//    for (Y y <- ys) {
//      result += (y -> new Set[X]());
//    }

    for (X x <- xs) {
      result(f(x)) += x
    }

    result
  }

  /**
   * Powerset of a set. Cheap solution with bitmask.
   * 
   * @tparam X set element type
   * @tparam XS powerset element type
   * @param xs the set which powerset we are calculating.
   * @return a powerset, lazy.
   */
  def powerset[X, XS <: Set[X]](xs: XS): Set[Set[X]] = {
    xs match {
      case Set() => Set(xs)
      case nontempty => {
        val allElements = xs.elements
        val minusFirst = allElements.tail
        minusFirst ::: (minusFirst map (_ + allElements.head)
      }  
    }
/*
    return new AbstractSet[Set[X]]() {

      override def elements: Iterator[Set[X]] = {
        new Iterator[Set[X]]() {
          var i = 0;

          def hasNext = i < size

          def next: Set[X] = {
            i++;
            new AbstractSet[X]() {
              int bits = i - 1;

              override def elements = {
                new Predicate[X]() {
                  val mask = bits * 2

                  override def eval(X x) = (mask /= 2) % 2 == 1
                }.filter(xs.elements) // cheating!!!
              }

              override def size = Integer.bitCount(bits)
            };
          }

          public void remove() {
            throw new UnsupportedOperationException();
          }
        };
      }

      @Override
      public int size() {
        return 1 << xs.size();
      }
    };
*/    
  }

}
