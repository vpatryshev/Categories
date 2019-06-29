package math.cat

import java.io.Reader

import math.cat.Categories.{Cat, read}
import math.sets._
import math.sets.Sets._
import scalakittens.{Good, Result}
import scalakittens.Result._

trait Graph extends GraphData { graph ⇒
  
  def contains(any: Any): Boolean = try { nodes contains any } catch { case _:Exception ⇒ false }

  def size: Int = nodes.size
  
  implicit def pairOfNodes(p: (_, _)): (Node, Node) = (node(p._1), node(p._2))

  override def hashCode: Int = getClass.hashCode + 41 + nodes.hashCode * 61 + arrows.hashCode

  override def equals(x: Any): Boolean = {
    x match {
      case other: Graph ⇒ other.equal(this)
      case _ ⇒ false
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
  private def equal(that: Graph) = {
    val isEqual = this.nodes == that.nodes && this.arrows == that.arrows
    (isEqual /: arrows) (
      (bool: Boolean, aHere: this.Arrow) ⇒ try {
        val aThere = that.arrow(aHere)
        bool && (d0(aHere) == that.d0(aThere)) && (d1(aHere) == that.d1(aThere))
      } catch { case _: Exception ⇒ false }
    )
  }

  override def toString: String = {
    val nodess = nodes.mkString(", ")
    val arrowss =
      arrows map ((a: Arrow) ⇒ s"$a: ${d0(a)}→${d1(a)}") mkString ", "
    s"({$nodess}, {$arrowss})"
  }

  /**
    * Produces a collection of arrows from x to y.
    *
    * @param from first node
    * @param to   second node
    * @return the set of all arrows from x to y
    */
  def arrowsBetween(from: Node, to: Node): Arrows = asSet(arrows filter ((f: Arrow) ⇒ (d0(f) == from) && (d1(f) == to)))
  
  /**
    * Checks if one arrow follows another
    * @param f an arrow
    * @param g an arrow
    * @return true iff f follows g
    */
  def follows(f: Arrow, g: Arrow): Boolean = {
    d0(f) == d1(g)
  }

  /**
    * Checks if two arrows have the same domain
    * @param f an arrow
    * @param g an arrow
    * @return true iff g and f have the same domain
    */
  def sameDomain(f: Arrow, g: Arrow): Boolean = {
    d0(arrow(f)) == d0(arrow(g))
  }

  /**
    * Checks if two arrows have the same codomain
    * @param f an arrow
    * @param g an arrow
    * @return true iff g and f have the same codomain
    */
  def sameCodomain(f: Arrow, g: Arrow): Boolean = {
    d1(arrow(f)) == d1(arrow(g))
  }

  /**
    * Checks if two arrows are parallel
    * @param f an arrow
    * @param g an arrow
    * @return true iff g and f have the same domain and codomain
    */
  def areParallel(f: Arrow, g: Arrow): Boolean = sameDomain(f, g) && sameCodomain(f, g)

  def unary_~ : Graph = new Graph {
    type Node = graph.Node
    type Arrow = graph.Arrow
    def nodes: Nodes = graph.nodes
    def arrows: Arrows = graph.arrows
    def d0(f: Arrow): Node = graph.d1(f)
    def d1(f: Arrow): Node = graph.d0(f)
  }
  
  def subgraph(aName: String, setOfNodes: Nodes): Graph = {
    require(setOfNodes.subsetOf(nodes), s"Unknown nodes: ${setOfNodes.diff(nodes)}")
    new Graph {
      override val name = aName
      type Node = graph.Node
      type Arrow = graph.Arrow

      def nodes: Nodes = setOfNodes

      def arrows: Arrows = graph.arrows filter (a => setOfNodes(d0(a)) && setOfNodes(d1(a)))

      def d0(f: Arrow): Node = graph.d0(f)

      def d1(f: Arrow): Node = graph.d1(f)
    }
  }
 
  def connectedComponents: Set[Graph] = {
    val connected: BinaryRelation[Node, Node] =
      BinaryRelation((x, y) => arrows.exists(a =>
        (x == d0(a) && y == d1(a)) || (x == d1(a) && y == d0(a))))

    val sets = new FactorSet(nodes, connected)

    sets.zipWithIndex map {
      case (s, i) => subgraph(s"$name.${i+1}", s)
    }
  }

}

private[cat] trait GraphData {
  val name: String = "a graph"
  type Node
  type Arrow
  type Nodes = Set[Node]
  type Arrows = Set[Arrow]
  
  def nodes: Nodes
  def arrows: Arrows
  def d0(f: Arrow): Node
  def d1(f: Arrow): Node

  implicit def node(x: Any): Node = x match {
    case _ if nodes contains x.asInstanceOf[Node] ⇒ x.asInstanceOf[Node]
    case other ⇒
      throw new IllegalArgumentException(s"<<$other>> is not a node in $name")
  }

  implicit def arrow(a: Any): Arrow = a match {
    case _ if arrows contains a.asInstanceOf[Arrow] ⇒ a.asInstanceOf[Arrow]
    case other ⇒
      throw new IllegalArgumentException(s"<<$other>> is not an arrow of $name")
  }

  protected lazy val finiteNodes: Boolean = Sets.isFinite(nodes)
  protected lazy val finiteArrows: Boolean = Sets.isFinite(arrows)
  
  def isFinite: Boolean = finiteNodes && finiteArrows

  def validate: Result[GraphData] =
    OKif(!finiteArrows) orElse {
      Result.fold(arrows map {
        a ⇒
          OKif(nodes contains d0(a), " d0 for " + a + " should be in set of nodes") andAlso
          OKif(nodes contains d1(a), " d1 for " + a + " should be in set of nodes") returning ()
      })
    } returning this
}

object Graph {
  def build[N, A](
    nodes0: Set[N],
    arrows0: Set[A],
    d00: A ⇒ N,
    d10: A ⇒ N): Result[Graph] = {
    val data: GraphData = new GraphData {
      override type Node = N
      override type Arrow = A
      def nodes: Nodes = nodes0
      def arrows: Arrows = arrows0
      def d0(f: Arrow): Node = d00(f)
      def d1(f: Arrow): Node = d10(f)
    }

    data.validate returning
      new Graph {
        def nodes: Nodes = data.nodes.asInstanceOf[Nodes] // TODO: get rid of cast
        def arrows: Arrows = data.arrows.asInstanceOf[Arrows] // TODO: get rid of cast
        override type Node = N
        override type Arrow = A
        override def d0(f: Arrow): Node = d00(f).asInstanceOf[Node]
        override def d1(f: Arrow): Node = d10(f).asInstanceOf[Node]
      }
  }

  def fromArrowMap[N, A] (nodes: Set[N], arrows: Map[A, (N, N)]): Result[Graph] = {
    build(nodes, arrows.keySet, (a:A) ⇒ arrows(a)._1,  (a: A) ⇒ arrows(a)._2)
  }

  def discrete[N](points: Set[N]): Graph =
    new Graph {
      type Node = N
      type Arrow = N
      def nodes: Nodes = points
      def arrows: Arrows = Set.empty
      def d0(f: Arrow): Node = Map.empty(f) // there's nothing there, but we need a signature
      def d1(f: Arrow): Node = Map.empty(f)
    }

  def ofPoset[N](poset: PoSet[N]): Graph = {
    val points = poset.underlyingSet
    val posetSquare = Sets.product2(points, points)
    val goodPairs: Set[(N,N)] = Sets.filter(posetSquare, poset.le)

    new Graph {
      type Node = N
      type Arrow = (N, N)
      def nodes: Nodes = points
      def arrows: Arrows = goodPairs
      def d0(f: Arrow): Node = f._1
      def d1(f: Arrow): Node = f._2
    }
  }        

  class Parser extends Sets.Parser {
    def all: Parser[Result[Graph]] = "("~graph~")" ^^ {case "("~g~")" ⇒ g}

    def graph: Parser[Result[Graph]] = parserOfSet~","~arrows ^^ {case s~","~a ⇒ Graph.fromArrowMap(s, a)}

    def arrows: Parser[Map[String, (String, String)]] = "{"~repsep(arrow, ",")~"}" ^^ { case "{"~m~"}" ⇒ Map()++m}

    def arrow: Parser[(String, (String, String))] = word~":"~word~"→"~word ^^ {case f~":"~x~"→"~y ⇒ (f, (x, y))}

    def explain[T](pr: ParseResult[Result[T]]): Result[T] = {
      pr match {
        case Success(res, _) ⇒ res
        case e: NoSuccess ⇒ Result.error(s"Failed to parse: $e")
      }
    }

    def readGraph(input: CharSequence): Result[Graph] = {
      val pr: ParseResult[Result[Graph]] = parseAll(all, input)
      explain(pr)
    }

    def readGraph(input: Reader): Result[Graph] = explain(parseAll(all, input))
  }

  def read(input: Reader): Result[Graph] = (new Parser).readGraph(input)

  def read(input: CharSequence): Result[Graph] = (new Parser).readGraph(input)

  implicit class GraphString(val sc: StringContext) extends AnyVal {
    def graph(args: Any*): Graph = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      var buf = new StringBuffer(strings.next)
      while (strings.hasNext) {
        buf append expressions.next
        buf append strings.next
      }
      Graph.read(buf) match {
        case Good(g) ⇒ g
        case bad ⇒ 
          throw new InstantiationException(bad.errorDetails.mkString)
      }
    }
  }
}
