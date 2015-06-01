package math.cat

class Graph[N, A] (theElements: scala.collection.Set[N], theArrows: scala.collection.Set[A], domain: A => N, codomain: A => N) extends scala.collection.Set[N] {
  validate

  val setOfNodes = theElements
  val setOfArrows = theArrows
  val d0 = domain
  val d1 = codomain
  
  def elements = setOfNodes.elements
  override def contains(node: N): Boolean = setOfNodes contains node
  override def size = setOfNodes.size
  override def hashCode = setOfNodes.hashCode * 61 + setOfArrows.hashCode

  override def equals(x: Any): Boolean = {
    x match {
      case other: Graph[_, _] => other.equals(this)
      case _ => false
    }
  }

  private def validate {
    for(a <- setOfArrows) require(setOfNodes contains d0(a), " d0 for " + a + " is not in set of nodes")
    for(a <- setOfArrows) require(setOfNodes contains d1(a), " d1 for " + a + " is not in set of nodes")
  }

  /**
   * Checks equality of this graph to that one.
   * They are equal if they have the same sets of nodes and arrows, and the arrows
   * originate and end at the same nodes.
   *
   * @param that another graph
   * @return true if they are equal.
   */
  private def equals(that: Graph[N, A]) = {
    val isEqual = this.setOfNodes == that.setOfNodes && this.setOfArrows == that.setOfArrows
    (isEqual /: setOfArrows) ((bool: Boolean, a: A) => bool && (this.d0(a) == that.d0(a)) && (this.d1(a) == that.d1(a)))
  }

  override def toString = "(" + 
    setOfNodes.toString + ", {" + (setOfArrows map (a => a.toString + ": " + d0(a) + "->" + d1(a))) + 
    "})"

  def unary_op = new Graph[N, A](setOfNodes, setOfArrows, d1, d0)
}

object Graph {
  def apply[N, A] (nodes: scala.collection.Set[N], arrows: scala.collection.Set[A], d0: A => N, d1: A => N) = new Graph(nodes, arrows, d0, d1)

  def apply[N, A] (nodes: scala.collection.Set[N], arrows: Map[A, (N, N)]): Graph[N, A] = apply(nodes, arrows.keySet, (a: A) => arrows(a)._1,  (a: A) => arrows(a)._2)
  
  implicit def discrete[N] (nodes: Set[N]): Graph[N, N] = apply(nodes, Set.empty[N], (a: N) => a, (a: N) => a)

  implicit def fromPoset[N] (poset: PoSet[N]): Graph[N, (N, N)] = {
    val sequenceOfPairs = for(x <- poset; y <- poset if poset.le(x, y)) yield (x, y)
    val setOfPairs: scala.collection.Set[(N, N)] = Sets.setOf(sequenceOfPairs, Integer.MAX_VALUE, (p: (N, N)) => poset.le(p._1, p._2))
    apply(poset.underlyingSet, setOfPairs, p => p._1, p => p._2)
  }        

  /**
   * Parses a graph from a kind of string that is produced by toString()
   *
   * @param source source string
   * @return a parsed graph
   */
  def apply(source: String): Graph[String, String] = {
    val string = source.substring(1, source.length - 1)
    val nodesAndArrows = string.split("\\],\\s*\\{")
    require(nodesAndArrows.length > 0, "Could not find nodes or arrows in " + string)
    val underlyingSet = Sets.parseSet(nodesAndArrows(0) + "]")
    if (nodesAndArrows.length > 1) {
      val arrowsAsString = nodesAndArrows(1)
      val arrayOfMappings = arrowsAsString substring(0, arrowsAsString.length - 1) split(",\\s*")
      def appendNewArrow(map: Map[String, (String, String)], arrow: String): Map[String, (String, String)] = {
        val arrowAndNodes = arrow split ":\\s*"
        if (arrowAndNodes.length == 2) {
          val domainCodomain = arrowAndNodes(1) split "\\s*->\\s*"
          require(domainCodomain.length == 2, "wrong arrow definition: " + arrow)
          val newMapping = (arrowAndNodes(0) -> (domainCodomain(0), domainCodomain(1))) 
          val newMap: Map[String, (String, String)] = map + newMapping
          newMap
        } else {
          map
        }
      }
      
      val arrows = (Map.empty[String, (String, String)] /: arrayOfMappings) ((map: Map[String, (String, String)], arrow: String) => appendNewArrow(map, arrow))
      apply(underlyingSet, arrows)
    } else {
      discrete(underlyingSet)
    }
  }  
}
