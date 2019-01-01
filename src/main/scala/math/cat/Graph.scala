package math.cat

import java.io.Reader

import math.sets.{PoSet, Sets}
import math.sets.Sets._

abstract class Graph[N, A] { graph =>
  def nodes: Set[N]
  def arrows: Set[A]
  def d0: A => N
  def d1: A => N

  protected lazy val finiteNodes: Boolean = isFinite(nodes)
  protected lazy val finiteArrows: Boolean = isFinite(arrows)

  validateGraph()

  def contains(node: N): Boolean = nodes contains node
  def size: Int = nodes.size
  
  override def hashCode: Int = getClass.hashCode + 41 + nodes.hashCode * 61 + arrows.hashCode
  def -(x:N): Set[N] = itsImmutable
  def +(x:N): Set[N] = itsImmutable

  override def equals(x: Any): Boolean = {
    x match {
      case other: Graph[_, A] => other.equal(this)
      case _ => false
    }
  }

  def validateGraph() {
    if (finiteArrows) {
    for(a <- arrows) require(nodes contains d0(a), " d0 for " + a + " should be in set of nodes")
    for(a <- arrows) require(nodes contains d1(a), " d1 for " + a + " should be in set of nodes")
  }
  }

  /**
   * Returned a collection of arrows from x to y.
   *
   * @param from first node
   * @param to   second node
   * @return the set of all arrows from x to y
   */
  def hom(from: N, to: N): Set[A] = setOf(arrows filter ((f: A) => (d0(f) == from) && (d1(f) == to)))

  def anArrow(f: A): A = {
    require(arrows(f), s"Unknown arrow $f")
    f
  }
  
  /**
   * Checks if one arrow follows another
   * @param f an arrow
   * @param g an arrow
   * @return true iff f follows g
   */
  def follows(f: A, g: A): Boolean = {
    d0(anArrow(f)) == d1(anArrow(g))
  }

  /**
   * Checks if two arrows have the same domain
   * @param f an arrow
   * @param g an arrow
   * @return true iff g and f have the same domain
   */
  def sameDomain(f: A, g: A): Boolean = {
    d0(anArrow(f)) == d0(anArrow(g))
  }

  /**
   * Checks if two arrows have the same codomain
   * @param f an arrow
   * @param g an arrow
   * @return true iff g and f have the same codomain
   */
  def sameCodomain(f: A, g: A): Boolean = {
    d1(anArrow(f)) == d1(anArrow(g))
  }

  /**
   * Checks if two arrows are parallel
   * @param f an arrow
   * @param g an arrow
   * @return true iff g and f have the same domain and codomain
   */
  def areParallel(f: A, g: A): Boolean = sameDomain(f, g) && sameCodomain(f, g)

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

  override def toString: String = {
    val nodess = nodes.mkString(", ")
    val arrowss =
      arrows map ((a: A) => s"$a: ${d0(a)}->${d1(a)}") mkString ", "
    s"({$nodess}, {$arrowss})"
  }

  def unary_~ : Graph[N, A] = new Graph[N, A] {
    def nodes: Set[N] = graph.nodes
    def arrows: Set[A] = graph.arrows
    def d0: A => N = graph.d1
    def d1: A => N = graph.d0
  }
}

object Graph {
  
  def apply[N, A](
    nodes0: Set[N],
    arrows0: Set[A], d00: A => N, d10: A => N): Graph[N, A] = {
    new Graph[N, A] {
      def nodes: Set[N] = nodes0
      def arrows: Set[A] = arrows0
      def d0: A => N = d00
      def d1: A => N = d10
    }
  }

  def apply[N, A] (nodes: Set[N], arrows: Map[A, (N, N)]): Graph[N, A] = {
    apply(nodes, arrows.keySet, a => arrows(a)._1,  (a: A) => arrows(a)._2)
  }

  def apply[N](nodes: Set[N]): Graph[N, N] = apply(nodes, Set.empty[N], (a: N) => a, (a: N) => a)

  def apply[N](poset: PoSet[N]): Graph[N, (N, N)] = {
    val nodes = poset.underlyingSet

    val posetSquare = Sets.product2(nodes, nodes)
    val goodPairs:Set[(N,N)] = Sets.filter(posetSquare, poset.le)

    apply(nodes, goodPairs, _._1, _._2)
  }        

  class Parser extends Sets.Parser {
    def all: Parser[Graph[String, String]] = "("~graph~")" ^^ {case "("~g~")" => g}

    def graph: Parser[Graph[String, String]] = parserOfSet~","~arrows ^^ {case s~","~a => Graph(s, a)}

    def arrows: Parser[Map[String, (String, String)]] = "{"~repsep(arrow, ",")~"}" ^^ { case "{"~m~"}" => Map()++m}

    def arrow: Parser[(String, (String, String))] = member~":"~member~"->"~member ^^ {case f~":"~x~"->"~y => (f, (x, y))}

    private def explain(pr: ParseResult[Graph[String, String]]): Graph[String, String] = {
      pr match {
        case Success(graph:Graph[String, String], _) => graph
        case e: NoSuccess => throw new IllegalArgumentException(s"Failed to parse graph: $e")
      }
    }

    def readGraph(input: CharSequence): Graph[String, String] = {
      val pr: ParseResult[Graph[String, String]] = parseAll(all, input)
      explain(pr)
    }

    def readGraph(input: Reader): Graph[String, String] = explain(parseAll(all, input))
  }

  def apply(input: Reader): Graph[String, String] = (new Parser).readGraph(input)

  def apply(input: CharSequence): Graph[String, String] = (new Parser).readGraph(input)
}
