package math.cat

import scala.language.implicitConversions
import scala.language.postfixOps
import java.io.Reader
import math.Base._
import math.sets._
import math.sets.Sets._
import scalakittens.{Good, Result}
import scalakittens.Result._

trait Graph(val name: String) extends GraphData:
  graph =>
  
  def contains(any: Any): Boolean = nodeOpt(any).isGood

  def size: Int = nodes.size
  
  implicit def pairOfNodes(p: (_, _)): (Node, Node) = (node(p._1), node(p._2))

  override def hashCode: Int = getClass.hashCode + 41 + nodes.hashCode * 61 + arrows.hashCode

  override def equals(x: Any): Boolean = {
    x match {
      case other: Graph => other.equal(this)
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
  private def equal(that: Graph) = checkThat {
    this.nodes == that.nodes && this.arrows == that.arrows &&
    arrows.forall(arrowHere =>
      val arrowThere = that.arrow(arrowHere)
      (d0(arrowHere) == that.d0(arrowThere)) && (d1(arrowHere) == that.d1(arrowThere))
    )
  }

  override def toString: String =
    val nodeStrings = nodes.toList.map(_.toString).sorted mkString ", " 
    val arrowStrings =
      arrows.toList.map(a => s"$a: ${d0(a)}->${d1(a)}").sorted mkString ", "

    s"({$nodeStrings}, {$arrowStrings})"

  
  /**
    * Produces a collection of arrows from x to y.
    *
    * @param from first node
    * @param to   second node
    * @return the set of all arrows from x to y
    */
  def arrowsBetween(from: Node, to: Node): Arrows =
    setOf(arrows filter ((f: Arrow) => (d0(f) == from) && (d1(f) == to)))
  
  /**
    * Checks if one arrow follows another
    * @param f an arrow
    * @param g an arrow
    * @return true iff f follows g
    */
  def follows(f: Arrow, g: Arrow): Boolean =
    d0(f) == d1(g)

  /**
    * Checks if two arrows have the same domain
    * @param f an arrow
    * @param g an arrow
    * @return true iff g and f have the same domain
    */
  def sameDomain(f: Arrow, g: Arrow): Boolean =
    d0(arrow(f)) == d0(arrow(g))

  /**
    * Checks if two arrows have the same codomain
    * @param f an arrow
    * @param g an arrow
    * @return true iff g and f have the same codomain
    */
  def sameCodomain(f: Arrow, g: Arrow): Boolean =
    d1(arrow(f)) == d1(arrow(g))

  /**
    * Checks if two arrows are parallel
    * @param f an arrow
    * @param g an arrow
    * @return true iff g and f have the same domain and codomain
    */
  def areParallel(f: Arrow, g: Arrow): Boolean =
    sameDomain(f, g) && sameCodomain(f, g)

  def unary_~ : Graph =
    new Graph(if (graph.name.startsWith("~")) graph.name.tail else "~" + graph.name):
      type Node = graph.Node
      type Arrow = graph.Arrow
      def nodes: Nodes = graph.nodes
      def arrows: Arrows = graph.arrows
      def d0(f: Arrow): Node = graph.d1(f)
      def d1(f: Arrow): Node = graph.d0(f)
  
  def subgraph(name: String, setOfNodes: Nodes): Result[Graph] =
    OKif(setOfNodes.subsetOf(nodes), s"Unknown nodes: ${asString(setOfNodes.diff(nodes))}").
      returning {
        new Graph(name):
          type Node = graph.Node
          type Arrow = graph.Arrow

          def nodes: Nodes = setOfNodes
          def arrows: Arrows = graph.arrows filter (a => setOfNodes(d0(a)) && setOfNodes(d1(a)))
          def d0(f: Arrow): Node = graph.d0(f)
          def d1(f: Arrow): Node = graph.d1(f)
      }

  end subgraph
  
  def addArrows(newArrows: Map[Arrow, (Node, Node)]): Result[Graph] = 
    val result = new Graph(name):

      def nodes: Nodes = graph.nodes.asInstanceOf[Nodes]

      lazy val arrows: Arrows = (newArrows.keySet ++ graph.arrows).asInstanceOf[Arrows]

      def d0(f: Arrow): Node = {
        val fA = f.asInstanceOf[graph.Arrow]
        val newOne: Option[Node] = newArrows.get(fA).map(_._1.asInstanceOf[Node])
        newOne.getOrElse(node(graph.d0(fA)))
      }

      def d1(f: Arrow): Node = {
        val fA = f.asInstanceOf[graph.Arrow]
        val newOne: Option[Node] = newArrows.get(fA).map(_._2.asInstanceOf[Node])
        newOne.getOrElse(node(graph.d1(fA)))
      }
    
    result.validate orCommentTheError s"Failed in Graph $this"
  
  override def validate: Result[Graph] = super.validate returning this

end Graph

private[cat] trait GraphData:
  data =>
  type Node
  type Arrow
  type Nodes = Set[Node]
  type Arrows = Set[Arrow]

  def nodes: Nodes
  def arrows: Arrows
  def d0(f: Arrow): Node
  def d1(f: Arrow): Node

  def nodeOpt(x: Any): Result[Node] = Result.forValue(node(x))

  def node(x: Any): Node = x match {
    case _ if nodes contains x.asInstanceOf[Node] => x.asInstanceOf[Node]
    case other =>
      throw new IllegalArgumentException(s"<<$other>> is not a node")
  }

  def arrow(a: Any): Arrow = {
    val arrow = a.asInstanceOf[Arrow]
    if arrows contains arrow then arrow
    else
      throw new IllegalArgumentException(s"<<$a>> is not an arrow")
  }

  protected lazy val finiteNodes: Boolean = Sets.isFinite(nodes)
  protected lazy val finiteArrows: Boolean = Sets.isFinite(arrows)

  def isFinite: Boolean = finiteNodes && finiteArrows

  def validate: Result[GraphData] =
    OKif(!finiteArrows) orElse {
      Result.fold(arrows map {
        a =>
          OKif(nodes contains d0(a), " d0 for " + a + " should be in set of nodes") andAlso
          OKif(nodes contains d1(a), " d1 for " + a + " should be in set of nodes")
      })
    } returning this

  def build(name: String): Graph = new Graph(name):

    def nodes: Nodes = data.nodes //.asInstanceOf[Nodes] // TODO: get rid of cast
    def arrows: Arrows = data.arrows //.asInstanceOf[Arrows] // TODO: get rid of cast

    override type Node = data.Node
    override type Arrow = data.Arrow

    def d0(f: Arrow): Node = data.d0(f)
    def d1(f: Arrow): Node = data.d1(f)

end GraphData

object Graph:
  
  private[cat] def data[N, A](
    nodes: Set[N], arrows: Map[A, (N, N)]): Result[GraphData] =
    data(nodes, arrows.keySet, (a:A) => arrows(a)._1,  (a: A) => arrows(a)._2)
  
  private[cat] def data[N, A](
    setOfNodes: Set[N],
    setOfArrows: Set[A],
    source: A => N,
    target: A => N): Result[GraphData] =
    new GraphData {
      override type Node = N
      override type Arrow = A
      def nodes: Nodes = setOfNodes
      def arrows: Arrows = setOfArrows
      
      def d0(f: Arrow): Node = source(f)
      def d1(f: Arrow): Node = target(f)
    } validate
  
  def build[N, A](
    name: String,
    nodes0: Set[N],
    arrows0: Set[A],
    d00: A => N,
    d10: A => N): Result[Graph] =
    val parsed: Result[GraphData] = data[N, A](nodes0, arrows0, d00, d10)

    parsed flatMap {
      d =>
        new Graph(name) {

          override type Node = N
          override type Arrow = A
          override type Nodes = Set[N]
          def nodes: Nodes = d.nodes.asInstanceOf[Nodes] // TODO: get rid of cast
          def arrows: Arrows = d.arrows.asInstanceOf[Arrows] // TODO: get rid of cast

          override def d0(f: Arrow): Node = d00(f)
          override def d1(f: Arrow): Node = d10(f)
        } validate
    }

  def fromArrowMap[N, A] (name: String, nodes: Set[N], arrows: Map[A, (N, N)]): Result[Graph] =
    build(name, nodes, arrows.keySet, (a:A) => arrows(a)._1,  (a: A) => arrows(a)._2)

  def discrete[N](points: Set[N], nameit: String = ""): Graph =
    val name: String =
      if (nameit == "") s"DiscreteGraph_${points.size}" else nameit
    
    new Graph(name):
      type Node = N
      type Arrow = N
      def nodes: Nodes = points
      def arrows: Arrows = Set.empty
      def d0(f: Arrow): Node = Map.empty(f) // there's nothing there, but we need a signature
      def d1(f: Arrow): Node = Map.empty(f)

  
  def ofPoset[N](name: String, poset: PoSet[N]): Graph =
    val points = poset.elements
    val posetSquare = Sets.product2(points, points)
    val goodPairs: Set[(N,N)] = Sets.filter(posetSquare, poset.le)

    new Graph(name):
      type Node = N
      type Arrow = (N, N)
      def nodes: Nodes = points
      def arrows: Arrows = goodPairs
      def d0(f: Arrow): Node = f._1
      def d1(f: Arrow): Node = f._2

  def read(input: Reader): Result[Graph] = (new GraphParser).readGraph(input)

  def read(input: CharSequence): Result[Graph] = (new GraphParser).readGraph(input)

  implicit class GraphString(val sc: StringContext) extends AnyVal:
    def graph(args: Any*): Graph =
      Graph.read(bufferFromContext(sc, args:_*)) iHope
  
  class GraphParser extends Sets.SetParser:
    def all: Parser[Result[Graph]] = (name ?) ~ "("~graphData~")" ^^ {
      case nameOpt~"("~gOpt~")" =>
        val name = nameOpt getOrElse Good("graph")
        (gOpt andAlso name) map { case (g, n) => g build n }

      case nonsense => Result.error(s"Failed to parse $nonsense")
    }

    def name: Parser[Result[String]] = word ~ ":" ^^ {
      case n ~ ":" => Good(n)
      case nonsense => Result.error(s"Failed to parse $nonsense")
    }

    def graphData: Parser[Result[GraphData]] = parserOfSet~","~arrows ^^ {
      case s~","~arrows => (arrows andAlso s) flatMap {
        case (arr, s0) => Graph.data(s0, arr)
      }
      case nonsense => Result.error(s"Failed to parse $nonsense")
    }

    def arrows: Parser[Result[Map[String, (String, String)]]] = "{"~repsep(arrow, ",")~"}" ^^ {
      case "{"~m~"}" => Result.traverse(m).map(Map()++)
      case nonsense => Result.error(s"Failed to parse $nonsense")
    }

    def arrow: Parser[Result[(String, (String, String))]] = word~":"~word~"->"~word ^^ {
      case f~":"~x~"->"~y => Good((f, (x, y)))
      case nonsense => Result.error(s"Failed to parse $nonsense")
    }

    def explain[T](pr: ParseResult[Result[T]]): Result[T] =
      pr match
        case Success(res, _) => res
        case e: NoSuccess => Result.error(s"Failed to parse: $e")

    def readGraph(input: CharSequence): Result[Graph] = explain(parseAll(all, input))

    def readGraph(input: Reader): Result[Graph] = explain(parseAll(all, input))

  end GraphParser

end Graph
