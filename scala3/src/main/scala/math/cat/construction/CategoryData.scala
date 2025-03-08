package math.cat.construction

import math.Base._
import math.cat.construction._
import math.cat.construction.CategoryData._
import math.cat.{Category, Graph}
import math.sets.Sets._
import scalakittens.Result
import scalakittens.Result._

import java.util.Objects
import scala.collection.mutable
import scala.language.{implicitConversions, postfixOps}
import scalakittens.Containers.*

/**
  * The data used in building an instance of Category
  */
private[cat] abstract class CategoryData(name: String) extends Graph(name):
  type Obj = Node
  type Objects = Nodes

  val graph: Graph = this

  /// TODO: figure out why do we need it
  def d0(f: Arrow): Node = graph.d0(f)
  def d1(f: Arrow): Node = graph.d1(f)
  
  lazy val listOfObjects: List[Obj] =
    if isFinite then listSorted(objects)
    else throw new IllegalStateException("Cannot sort infinite set")

  implicit def obj(x: Any): Obj = asNode(x)

  def id(o: Obj): Arrow

  def m(f: Arrow, g: Arrow): Option[Arrow]
  
  def validateGraph: Result[CategoryData] =
    super.validate returning this

  def factory: Result[CategoryBuilder] =
    val graphIsOk = validateGraph
    val objectsHaveIds = OKif(!finiteObjects) orElse
      Result.check { 
        objects.map { x =>
          val ux = id(x)
          OKif(d0(ux) == x, s"Domain of id $ux should be $x in $name") andAlso
          OKif(d1(ux) == x, s"Codomain of id $ux should be $x in $name")
        }
      }

    val idsAreNeutral = OKif(!finiteArrows) orElse
      Result.check { arrows.map { f =>
        val u_f = m(id(d0(f)), f)
        val f_u = m(f, id(d1(f)))
        OKif(u_f contains f, s"Left unit law broken for ${id(d0(f))} and $f: got $u_f in $name") andAlso
        OKif(f_u contains f, s"Right unit law broken for ${id(d1(f))} and $f: got $f_u in $name")
      }}

    val compositionsAreOk =
      idsAreNeutral andThen OKif(!finiteArrows) orElse checkCompositions

    def listAssociativityProblems =
      Result.check {
        for
          f <- arrows
          g <- arrows
          h <- arrows
          gf <- m(f, g)
          hg <- m(g, h)
        yield
          val h_gf = m(gf, h)
          val hg_f = m(f, hg)
          OKif(h_gf == hg_f, s"Associativity broken for $f, $g and $h, got $h_gf vs $hg_f in $name")
      }

    val compositionIsAssociative = OKif(!finiteArrows) orElse 
          (compositionsAreOk andThen listAssociativityProblems)

    val validated: Result[((CategoryData, Any), Any)] =
      graphIsOk andAlso objectsHaveIds andAlso compositionIsAssociative
    
    validated map (_._1._1) map (new CategoryBuilder(_))
  
  end factory

  def objects: Objects = nodes
  
  def finiteObjects: Boolean = objects.isFinite
  
  def objectByAlphabet: List[Obj] = listSorted(objects)

//  def nodes: Objects = graph.nodes

  def arrows: Arrows = graph.arrows match
    case arrows: Arrows => arrows
    case _ => throw new IllegalStateException(s"arrows of $name coming from graph are not Arrows")

  def composablePairs: Iterable[(Arrow, Arrow)] = Category.composablePairs(this)

  private[cat] def checkCompositions: Outcome =
    val check1 = Result.check(missingCompositions.map {
      case (f, g) => Oops(s"composition must be defined for $f and $g in $name")
    })

    val check2 = Result.check {
      for
        f <- arrows
        g <- arrows
        h = m(f, g)
      yield
        if follows(g, f) then
          Result(h).flatMap{ gf =>
            OKif(sameDomain(gf, f),
              s"Wrong composition $gf of $f and $g : its d0 is ${d0(gf)}, must be ${d0(f)} in $name") andAlso
              OKif(sameCodomain(gf, g),
                s"Wrong composition $gf of $f and $g: its d1 is ${d1(gf)}, must be ${d1(g)} in $name")
          }
        else
          OKif(h.isEmpty, s"Wrongly defined composition of $f and $g in $name")
      }

    check1 andAlso check2
  
  end checkCompositions
  
  private[cat] def nonexistentCompositions: Iterable[(Arrow, Arrow)] =
    for
      (f, g) <- composablePairs
      if hom(d0(f), d1(g)).isEmpty
    yield (f, g)

  private[cat] def missingCompositions: Iterable[(Arrow, Arrow)] =
    for
      (f, g) <- composablePairs
      if m(f, g).isEmpty
    yield (f, g)

  /**
    * Builds a category given a data.
    *
    * @return a category built based on the data above
    *
    * TODO: eliminate code duplication
    */
  private[cat] def build: Result[Category] =
    factory.map { _.newCategory }

  // TODO: try to start using
  private val homCache: mutable.Map[(Obj, Obj), Arrows] = mutable.Map[(Obj, Obj), Arrows]()

  /**
    * Produces a collection of arrows from x to y.
    *
    * @param from first object
    * @param to   second object
    * @return the set of all arrows from x to y
    */
  def hom(from: Obj, to: Obj): Arrows =
    if isFinite then
      homCache.getOrElseUpdate((from, to), calculateHom(from, to))
    else calculateHom(from, to)

  private def calculateHom(from: Obj, to: Obj): Arrows =
    setOf(arrows filter ((f: Arrow) => (d0(f) == from) && (d1(f) == to)))

end CategoryData



/**
  * Partial data for a category
  * Objects have the same names as their identities.
  *
  * @param graph the underlying graph
  * @return category data
  */
private[construction] class PartialData(override val graph: Graph)
  extends CategoryData(graph.name):

  def nodes: Nodes = graph.nodes.asInstanceOf[Nodes] // TODO: remove this cast
  
  type CompositionTable = Composition[graph.Arrow]
  lazy val composition: CompositionTable = fillCompositionTable
  val compositionSource: CompositionTable = CategoryData.Empty[graph.Arrow]
  
  override def id(o: Obj): Arrow = o

  override def m(f: Arrow, g: Arrow): Option[Arrow] =
    composition.get((f, g)) map asArrow

  /**
    * This method helps fill in obvious choices for arrows composition.
    * Case 1. There's an arrow f:a->b, and an arrow g:b->c; and there's just one arrow h:a->c.
    * What would be the composition of f and g? h is the only choice.
    * <p/>
    * Case 2. h ∘ (g ∘ f) = k; what is (h ∘ g) ∘ f? It is k. and vice versa.
    */
  private def fillCompositionTable: CompositionTable =
    // First, add identities
    val addedIds = defineCompositionWithIds

    // Second, add unique solutions
    val addedUniqueSolutions: CompositionTable = addUniqueCompositions(addedIds)

    // Third, deduce compositions from associativity law
    val addedDeducedCompositions: CompositionTable =
      deduceCompositions(addedUniqueSolutions)

    addedDeducedCompositions
  
  end fillCompositionTable

  // adding composition that are deduced from associativity law
  private[cat] def deduceCompositions(compositionSource: CompositionTable): CompositionTable =
    val triplesToScan = composableTriples(compositionSource)

    val compositions: CompositionTable =
      triplesToScan.foldLeft(compositionSource) {
      (m, t) =>
        val (f, g, h) = t
        val gf = m((f, g))
        val hg = m((g, h))
        if m.contains(gf, h) && !m.contains(f, hg) then
          m + ((f, hg) -> m((gf, h)))
        else if m.contains(f, hg) && !m.contains(gf, h) then
          m + ((gf, h) -> m((f, hg)))
        else
          m
    }
    compositions
  
  end deduceCompositions  

  // this is a technical method to list all possible triples that have compositions defined pairwise
  private[cat] def composableTriples(compositionSource: CompositionTable):
    Set[(graph.Arrow, graph.Arrow, graph.Arrow)] =
    for
      a <- graph.arrows
      b <- graph.arrows if compositionSource.contains((a, b))
      c <- graph.arrows if compositionSource.contains((b, c))
      /* The following two lines don't work yet.
         They also break other tests, totally unrelated.
            b <- graph.arrows if compositionSource contains ((b, a))
            c <- graph.arrows if compositionSource contains ((b, c))
       */
      
    yield (a, b, c)

  
  // adding composition with identities to a composition table
  private def defineCompositionWithIds: CompositionTable =
    graph.arrows.foldLeft(compositionSource) { (m, f) =>
      val fA: graph.Arrow = f
      val id_d0: graph.Arrow = graph.d0(f)
      val id_d1: graph.Arrow = graph.d1(f)
      m + ((id_d0, fA) -> fA) + ((fA, id_d1) -> fA)
    }

  // adding unique available compositions
  private[cat] def addUniqueCompositions(compositionSource: CompositionTable):
    Map[(graph.Arrow, graph.Arrow), graph.Arrow] =
    
    def candidates(a: graph.Arrow, b: graph.Arrow) =
      graph.arrowsBetween(graph.d0(a), graph.d1(b)).take(2).toList

    composablePairs.foldLeft(compositionSource) {
      case (m, (a, b)) =>
        val aA = a.asInstanceOf[graph.Arrow] // no check
        val bA = b.asInstanceOf[graph.Arrow] // no check
        candidates(a, b) match
          case c::Nil => m + ((aA, bA) -> c)
          case _       => m
    }

  end addUniqueCompositions

  def newComposition(f: Any, g: Any): Option[Arrow] = None

  /**
    * Builds a category given a data.
    *
    * @return a category built based on the data above
    *
    *         TODO: eliminate code duplication
    */
  private[cat] override def build: Result[Category] = Result.forValue {
    CategoryData.transitiveClosure(this).factory.map { validData => validData.newCategory }
  }.flatten

end PartialData

object CategoryData:

  type Composition[Arr] = Map[(Arr, Arr), Arr]
  val nothing: Any => None.type = (t: Any) => None

  def Empty[Arr] = Map.empty[(Arr, Arr), Arr]

  /**
    * Builds a category given a limited (but sufficient) amount of data.
    * Objects have the same name as their identities.
    *
    * @param g    the underlying graph
    * @param comp source table of arrows composition (can be incomplete)
    * @return a newly-built category
    */
  def partial[Arr](g: Graph)(
    comp: Composition[Arr] = Empty[Arr],
    compositionFactory: ((Arr, Arr)) => Option[Arr] = nothing):
  PartialData =
    new PartialData(addIdentitiesToGraph(g)):

      override def newComposition(f: Any, g: Any): Option[Arrow] =
        compositionFactory(f.asInstanceOf[Arr], g.asInstanceOf[Arr]).map(_.asInstanceOf[Arrow])

      override val compositionSource: CompositionTable = comp.asInstanceOf[CompositionTable] // same type

  private def addIdentitiesToGraph(graph: Graph): Graph =

    val nodesOpt: Option[Set[Any]] = if graph.isFinite then Some(graph.nodes.toSet) else None

    def isIdentity(f: Any): Boolean = nodesOpt map (_ contains f) getOrElse (graph contains f)

    new Graph(graph.name):

      def nodes: Nodes = graph.nodes.asInstanceOf[Nodes]

      lazy val arrows: Arrows = (graph.nodes ++ graph.arrows).asInstanceOf[Arrows]

      def d0(f: Arrow): Node =
        if isIdentity(f) then f else graph.d0(f)

      def d1(f: Arrow): Node =
        if isIdentity(f) then f else graph.d1(f)

  end addIdentitiesToGraph

  def apply(gr: Graph)(
    ids: gr.Node => gr.Arrow,
    composition: (gr.Arrow, gr.Arrow) => Option[gr.Arrow]): CategoryData =
    new CategoryData(gr.name):
      override val graph: gr.type = gr

      def nodes: Nodes = graph.nodes.asInstanceOf[Nodes] // TODO: remove this cast

      override def id(o: Obj): Arrow = ids(o)

      override def m(f: Arrow, g: Arrow): Option[Arrow] =
        composition(f, g) map asArrow

  
  // TODO: don't throw exception, return a result
  private[cat] def transitiveClosure(data: PartialData): PartialData =
  
    val missing = data.missingCompositions
  
    if missing.isEmpty then data else

      val newArrows: Map[data.Arrow, (data.Obj, data.Obj)] =
        data.nonexistentCompositions.flatMap{
          case (f, g) =>
            data.newComposition(f, g).map {
              h => (h, (data.d0(f), data.d1(g)))
            }
        } toMap

      require(newArrows.nonEmpty,
        s"${data.name}: ${missing.size} arrows still missing: $missing")

      val newData: Result[PartialData] =
        data.addArrows(newArrows).map { graph =>
          new PartialData(graph):
            override def newComposition(f: Any, g: Any): Option[Arrow] =
              data.newComposition(f, g).map(_.asInstanceOf[Arrow])

            override val compositionSource: CompositionTable =
              data.composition.asInstanceOf[CompositionTable]
      }
      
      newData map transitiveClosure iHope

end CategoryData