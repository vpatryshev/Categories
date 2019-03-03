package math.cat

import java.io.Reader

import j.math.cat.N
import math.sets.Sets._
import math.sets.{PoSet, Sets}
import scalakittens.{Good, Result}
import scalakittens.Result._

abstract class Graph[N, A] extends GraphCandidate[N, A] { graph =>
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

  /**
    * Produces a collection of arrows from x to y.
    *
    * @param from first node
    * @param to   second node
    * @return the set of all arrows from x to y
    */
  def arrowsBetween(from: Node, to: Node): Set[Arrow] = setOf(arrows filter ((f: Arrow) => (d0(f) == from) && (d1(f) == to)))

  def anArrow(f: Arrow): Arrow = {
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

  def unary_~ : Graph[N, A] = new Graph[N, A] {
    def nodes: Set[N] = graph.nodes
    def arrows: Set[A] = graph.arrows
    def d0(f: A): N = graph.d1(f)
    def d1(f: A): N = graph.d0(f)
  }
}

private[cat] abstract class GraphCandidate[N, A] {
  final type Node = N
  type Nodes = Set[Node]
  final type Arrow = A
  type Arrows = Set[Arrow]
  
  def nodes: Nodes
  def arrows: Arrows
  def d0(f: Arrow): Node
  def d1(f: Arrow): Node

  protected lazy val finiteNodes: Boolean = isFinite(nodes)
  protected lazy val finiteArrows: Boolean = isFinite(arrows)

  def validateGraph(): Result[GraphCandidate[N, A]] =
    OKif(!finiteArrows) orElse {
      Result.fold(arrows map {
        a =>
          OKif(nodes contains d0(a), " d0 for " + a + " should be in set of nodes") andAlso
          OKif(nodes contains d1(a), " d1 for " + a + " should be in set of nodes") returning ()
      })
    } returning this
}

object Graph {
  def build[N, A](
    nodes0: Set[N],
    arrows0: Set[A],
    d00: A => N,
    d10: A => N): Result[Graph[N, A]] = {
    val candidate = new GraphCandidate[N, A] {
      def nodes: Set[N] = nodes0
      def arrows: Set[A] = arrows0
      def d0(f: A): N = d00(f)
      def d1(f: A): N = d10(f)
    }

    candidate.validateGraph returning
      new Graph[N, A] {
        def nodes: Set[N] = candidate.nodes
        def arrows: Set[A] = candidate.arrows
        def d0(f: A): N = candidate.d0(f)
        def d1(f: A): N = candidate.d1(f)
      }
  }

  def build[N, A] (nodes: Set[N], arrows: Map[A, (N, N)]): Result[Graph[N, A]] = {
    build(nodes, arrows.keySet, a => arrows(a)._1,  (a: A) => arrows(a)._2)
  }

  def discrete[N](points: Set[N]): Graph[N, N] =
    new Graph[N, N] {
      def nodes: Set[N] = points
      def arrows: Set[N] = Set.empty
      def d0(f: N): N = f
      def d1(f: N): N = f
    }

  def ofPoset[N](poset: PoSet[N]): Graph[N, (N, N)] = {
    val points = poset.underlyingSet
    val posetSquare = Sets.product2(points, points)
    val goodPairs: Set[(N,N)] = Sets.filter(posetSquare, poset.le)

    new Graph[N, (N, N)] {
      def nodes: Set[N] = points
      def arrows: Arrows = goodPairs
      def d0(f: (N, N)): N = f._1
      def d1(f: (N, N)): N = f._2
    }
  }        

  class Parser extends Sets.Parser {
    def all: Parser[Result[Graph[String, String]]] = "("~graph~")" ^^ {case "("~g~")" => g}

    def graph: Parser[Result[Graph[String, String]]] = parserOfSet~","~arrows ^^ {case s~","~a => Graph.build(s, a)}

    def arrows: Parser[Map[String, (String, String)]] = "{"~repsep(arrow, ",")~"}" ^^ { case "{"~m~"}" => Map()++m}

    def arrow: Parser[(String, (String, String))] = member~":"~member~"->"~member ^^ {case f~":"~x~"->"~y => (f, (x, y))}

    def explain[T](pr: ParseResult[Result[T]]): Result[T] = {
      pr match {
        case Success(res, _) => res
        case e: NoSuccess => Result.error(s"Failed to parse: $e")
      }
    }

    def readGraph(input: CharSequence): Result[Graph[String, String]] = {
      val pr: ParseResult[Result[Graph[String, String]]] = parseAll(all, input)
      explain(pr)
    }

    def readGraph(input: Reader): Result[Graph[String, String]] = explain(parseAll(all, input))
  }

  def read(input: Reader): Result[Graph[String, String]] = (new Parser).readGraph(input)

  def read(input: CharSequence): Result[Graph[String, String]] = (new Parser).readGraph(input)

  implicit class GraphString(val sc: StringContext) extends AnyVal {
    def graph(args: Any*): Graph[String, String] = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      var buf = new StringBuffer(strings.next)
      while (strings.hasNext) {
        buf append expressions.next
        buf append strings.next
      }
      Graph.read(buf) match {
        case Good(g) => g
        case bad => 
          throw new InstantiationException(bad.errorDetails.mkString)
      }
    }
  }
}
