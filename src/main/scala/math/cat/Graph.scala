package math.cat

import java.io.Reader

//import util.parsing.combinator.JavaTokenParsers

import scala.collection.immutable.Set
import Sets._

class Graph[N, A] (
        val nodes: Set[N],
        val arrows: Set[A],
        val d0: A => N,
        val d1: A => N
      ) extends Set[N] {

  def this(source: Graph[N, A]) = this(source.nodes, source.arrows, source.d0, source.d1)

  validate

  type Nodes = N
  type Arrows = A
  type MY_TYPE = this.type

  override def elements = nodes.iterator
  override def iterator = nodes.iterator
  override def contains(node: N): Boolean = nodes contains node
  override def size = nodes.size
  override def hashCode = nodes.hashCode * 61 + arrows.hashCode
  def -(x:N) = requireImmutability
  def +(x:N) = requireImmutability

  override def equals(x: Any): Boolean = {
    x match {
      case other: Graph[_, A] => other.equal(this)
      case _ => false
    }
  }

  def validate {
    require (arrows != null, "arrowset cannot be null")
    require (nodes != null, "nodeset cannot be null")

    if (!arrows.isEmpty) {
      require (d0 != null, "d0 cannot be null")
      require (d1 != null, "d1 cannot be null")
    }
    for(a <- arrows) require(nodes contains d0(a), " d0 for " + a + " should be in set of nodes")
    for(a <- arrows) require(nodes contains d1(a), " d1 for " + a + " should be in set of nodes")
  }

  /**
   * Returned a collection of arrows from x to y.
   *
   * @param from first node
   * @param to   second node
   * @return the set of all arrows from x to y
   */
  def hom(from: N, to: N) = setOf(arrows filter ((f: A) => (d0(f) == from) && (d1(f) == to)))

  /**
   * Checks if one arrow follows another
   * @param f an arrow
   * @param g an arrow
   * @return true iff f follows g
   */
  def follows(f: A, g: A) = d0(f) == d1(g)

  /**
   * Checks if two arrows have the same domain
   * @param f an arrow
   * @param g an arrow
   * @return true iff g and f have the same domain
   */
  def sameDomain(f: A, g: A) = d0(f) == d0(g)

  /**
   * Checks if two arrows have the same codomain
   * @param f an arrow
   * @param g an arrow
   * @return true iff g and f have the same codomain
   */
  def sameCodomain(f: A, g: A) = d1(f) == d1(g)

  /**
   * Checks if two arrows are parallel
   * @param f an arrow
   * @param g an arrow
   * @return true iff g and f have the same domain and codomain
   */
  def areParallel(f: A, g: A) = sameDomain(f, g) && sameCodomain(f, g)

  /**
   *  Checks equality of this graph to that one.
   * They are equal if they have the same sets of nodes and arrows, and the arrows
   * originate and end at the same nodes.
   *
   * @param that another graph
   * @return true if they are equal.
   */
  private def equal(that: Graph[_, A]) = {
    val isEqual = this.nodes == that.nodes && this.arrows == that.arrows
    (isEqual /: arrows) ((bool: Boolean, a: A) => bool && (this.d0(a) == that.d0(a)) && (this.d1(a) == that.d1(a)))
  }

  override def toString = "({" +
    nodes.mkString(", ") + "}, {" + (arrows map (a => a.toString + ": " + d0(a) + "->" + d1(a))).mkString(", ") +
    "})"

  def unary_~ = new Graph[N, A](nodes, arrows, d1, d0)
}

object Graph {
  def apply[N, A] (nodes: Set[N], arrows: Set[A], d0: A => N, d1: A => N) = {
    require (arrows != null)
    new Graph(nodes, arrows, d0, d1)
  }

  def apply[N, A] (nodes: Set[N], arrows: Map[A, (N, N)]): Graph[N, A] = {
    require (arrows.keySet != null)
    apply(nodes, arrows.keySet, a => arrows(a)._1,  (a: A) => arrows(a)._2)
  }
  
  implicit def apply[N] (nodes: Set[N]): Graph[N, N] = apply(nodes, Set.empty[N], (a: N) => a, (a: N) => a)

  def first[N](p:(N,N)): N = p._1
  def second[N](p:(N,N)): N = p._2

  implicit def apply[N] (poset: PoSet[N]): Graph[N, (N, N)] = {
    val sequenceOfPairs = for(x <- poset; y <- poset if poset.le(x, y)) yield (x, y)
    lazy val size = (0 /: sequenceOfPairs) ((n, x) => n+1)
    val setOfPairs: Set[(N, N)] = Sets.setOf(sequenceOfPairs, size, (p: (N, N)) => poset.le(p._1, p._2))
    apply(poset.underlyingSet, setOfPairs, first, second)
  }        

  class Parser extends Sets.Parser {
    def all: Parser[Graph[String, String]] = "("~graph~")" ^^ {case "("~g~")" => g}
    def graph: Parser[Graph[String, String]] = set~","~arrows ^^ {case s~","~a => Graph(s, a)}
    def arrows: Parser[Map[String, (String, String)]] = "{"~repsep(arrow, ",")~"}" ^^ { case "{"~m~"}" => Map()++m}
    def arrow: Parser[(String, (String, String))] = member~":"~member~"->"~member ^^ {case f~":"~x~"->"~y => (f, (x, y))}
    override def read(input: CharSequence) = parseAll(all, input).get
    override def read(input: Reader) = parseAll(all, input).get
  }

  def apply(input: Reader) = (new Parser).read(input)

  def apply(input: CharSequence) = (new Parser).read(input)
}