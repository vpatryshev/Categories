package math.cat

import math.Base.*
import math.sets.Sets.*
import math.sets.*
import scalakittens.Result.*
import scalakittens.{Good, Params, Result}

import java.io.Reader
import scala.language.{implicitConversions, postfixOps}
import scalakittens.Containers.*

import scala.annotation.targetName

trait Graph(val name: String) extends GraphData:
  graph =>

  def size: Int = nodes.size

  def composablePairs: Iterable[(Arrow, Arrow)] =
    for f <- arrows; g <-arrows if follows(g, f) yield (f, g)

  override lazy val hashCode: Int = getClass.hashCode + 41 + nodes.hashCode * 61 + arrows.hashCode

  override def equals(x: Any): Boolean =
    x match
      case other: Graph => other equal this
      case _ => false

  /**
    *  Checks equality of this graph to that one.
    * They are equal if they have the same sets of nodes and arrows, and the arrows
    * originate and end at the same nodes.
    *
    * @param that another graph
    * @return true if they are equal.
    */
  private infix def equal(that: Graph) = checkThat {
    this.nodes == that.nodes && this.arrows == that.arrows &&
    arrows.forall(arrowHere =>
      val arrowThere: that.Arrow = that.asArrow(arrowHere)
      (d0(arrowHere) == that.d0(arrowThere)) && (d1(arrowHere) == that.d1(arrowThere))
    )
  }

  override lazy val toString: String =
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
    * @return true iff arrows have the same domain
    */
  def sameDomain(f: Arrow, g: Arrow): Boolean =
    d0(f) == d0(g)

  /**
    * Checks if two arrows have the same codomain
    * @param f an arrow
    * @param g an arrow
    * @return true iff g and f have the same codomain
    */
  def sameCodomain(f: Arrow, g: Arrow): Boolean =
    d1(f) == d1(g)

  /**
    * Checks if two arrows are parallel
    * @param f an arrow
    * @param g an arrow
    * @return true iff g and f have the same domain and codomain
    */
  def areParallel(f: Arrow, g: Arrow): Boolean =
    sameDomain(f, g) && sameCodomain(f, g)

  @targetName("opposite")
  def unary_~ : Graph =
    new Graph(if graph.name.startsWith("~") then graph.name.tail else "~" + graph.name):
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
    if newArrows.isEmpty then
      Good(this)
    else
      val result = new Graph(name):
        lazy val nodes: Nodes = graph.nodes.asInstanceOf[Nodes]
        lazy val arrows: Arrows = (newArrows.keySet ++ graph.arrows).asInstanceOf[Arrows]

        private def d0d1(f: Arrow): Option[(Graph.this.Node, Graph.this.Node)] =
          newArrows.get(f.asInstanceOf[graph.Arrow])  // shortcut: no check required
      
        def d0(f: Arrow): Node = d0d1(f).map(_._1).getOrElse(graph.d0(f))
        def d1(f: Arrow): Node = d0d1(f).map(_._2).getOrElse(graph.d1(f))
    
      result.validate orCommentTheError s"Failed in Graph $this"
  
  override def validate: Result[Graph] = super.validate returning this

end Graph

private[cat] trait GraphData:
  data =>
  type Node <: Matchable
  type Arrow <: Matchable
  type Nodes = Set[Node]
  type Arrows = Set[Arrow]

  def nodes: Nodes
  def arrows: Arrows
  def d0(f: Arrow): Node
  def d1(f: Arrow): Node

  infix def contains(x: Any): Boolean = x match
    case node: Node @unchecked => nodes(node)
    case _ => false

  implicit def asNode(x: Any): Node = x match
    case node: Node @unchecked if nodes(node) => node
    case badNode: Node => 
      throw new IllegalArgumentException(s"<<$badNode>> is not listed as a node")
    case notaNode => 
      throw new IllegalArgumentException(s"<<$notaNode>> is not a node")

  implicit def asArrow(a: Any): Arrow =
    a match
      case arrow: Arrow @unchecked if arrows(arrow) => arrow
      case badArrow: Arrow =>
        throw new IllegalArgumentException(s"<<$badArrow>> is not listed as an arrow")
      case notAnArrow =>
        throw new IllegalArgumentException(s"<<$notAnArrow>> is not an arrow")

  /*
    TODO: figure out how come this does not work
    given Conversion[Any, Node] = _ match
    case node: Node @unchecked if contains(node) => node
    case other =>
      throw new IllegalArgumentException(s"<<$other>> is not a node")
   */

  protected lazy val finiteNodes: Boolean = nodes.isFinite
  protected lazy val finiteArrows: Boolean = arrows.isFinite

  lazy val isFinite: Boolean = finiteNodes && finiteArrows
  lazy val isInfinite: Boolean = !isFinite

  private def arrowOk(a: Arrow) =
    OKif(d0(a) ∈ nodes, " d0 for " + a + " should be in set of nodes") andAlso
    OKif(d1(a) ∈ nodes, " d1 for " + a + " should be in set of nodes")

  def validate: Result[GraphData] =
    (OKif(!finiteArrows) orElse Result.fold(arrows map arrowOk)) returning this

  infix def build(name: String): Graph = new Graph(name):

    def nodes: Nodes = data.nodes
    def arrows: Arrows = data.arrows

    override type Node = data.Node
    override type Arrow = data.Arrow

    def d0(f: Arrow): Node = data.d0(f)
    def d1(f: Arrow): Node = data.d1(f)

  lazy val arrowsAsString: String = asString(arrows)
  
end GraphData

object Graph:
  
  private[cat] def data[N <: Matchable, A <: Matchable](
    nodes: Set[N], arrows: Map[A, (N, N)]): Result[GraphData] =
    data(nodes, arrows.keySet, (a:A) => arrows(a)._1,  (a: A) => arrows(a)._2)
  
  private[cat] def data[N <: Matchable, A <: Matchable](
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
  
  def build[N <: Matchable, A <: Matchable](
    name: String,
    nodes0: Set[N],
    arrows0: Set[A],
    d00: A => N,
    d10: A => N): Result[Graph] =
    val parsed: Result[GraphData] = data[N, A](nodes0, arrows0, d00, d10)

    parsed.flatMap :
      d =>
        new Graph(name) {

          override type Node = N
          override type Arrow = A
          val nodes: Nodes = d.nodes.asInstanceOf[Nodes] // TODO: get rid of cast
          val arrows: Arrows = d.arrows.asInstanceOf[Arrows] // TODO: get rid of cast

          override def d0(f: Arrow): Node = d00(f)
          override def d1(f: Arrow): Node = d10(f)
        } validate

  def fromArrowMap[N <: Matchable, A <: Matchable] (name: String, nodes: Set[N], arrows: Map[A, (N, N)]): Result[Graph] =
    build(name, nodes, arrows.keySet, (a:A) => arrows(a)._1,  (a: A) => arrows(a)._2)

  def discrete[N <: Matchable](points: Set[N], nameIt: String = ""): Graph =
    val name: String =
      if nameIt == "" then s"DiscreteGraph_${points.size}" else nameIt
    
    new Graph(name):
      type Node = N
      type Arrow = N
      def nodes: Nodes = points
      def arrows: Arrows = Set.empty
      def d0(f: Arrow): Node = Map.empty(f) // there's nothing there, but we need a signature
      def d1(f: Arrow): Node = Map.empty(f)

  
  def ofPoset[N <: Matchable](name: String, poset: PoSet[N]): Graph =
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

  extension (sc: StringContext)
    def graph(args: Any*): Graph =
      Graph.read(bufferFromContext(sc, args*)) iHope
  
  class GraphParser extends Sets.SetParser:
    private def all: Parser[Result[Graph]] = (name ?) ~ "("~graphData~")" ^^ {
      case nameOpt~"("~gOpt~")" =>
        val name = nameOpt getOrElse Good("graph")
        gOpt.andAlso(name) map { case (g, n) => g build n }

      case nonsense => Result.error(s"Failed to parse $nonsense")
    }

    def name: Parser[Result[String]] = word ~ ":" ^^ {
      case n ~ ":" => Good(n)
      case nonsense => Result.error(s"Failed to parse $nonsense")
    }

    def graphData: Parser[Result[GraphData]] = parserOfSet~","~arrows ^^ {
      case s~","~arrows => (arrows andAlso s).flatMap :
        case (arr, s0) => Graph.data(s0, arr)

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
