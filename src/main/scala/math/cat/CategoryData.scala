package math.cat

import math.cat.CategoryData.Composition
import scalakittens.{Good, Result}
import scalakittens.Result._

/**
  * The data used in building an instance of Category
  */
private[cat] abstract class CategoryData extends Graph { data =>
  type Obj = Node
  type Objects = Set[Obj]
  lazy val listOfObjects: List[Obj] =
    if (isFinite) objects.toList.sortBy(_.toString)
    else throw new IllegalStateException("Cannot sort infinite set")
  val graph: Graph

  override def name: String = graph.name

  def obj(x: Any): Obj =
    Result.forValue(asObj(x)) filter (objects contains) getOrElse {
      throw new IllegalArgumentException(s"$x is not an object in $name")
    }

  def id(o: Obj): Arrow

  def m(f: Arrow, g: Arrow): Option[Arrow]

  override def validate: Result[ValidCategoryData] = {
    val objectsHaveIds = OKif(!finiteNodes) orElse {
      Result.traverse(objects map {
        x ⇒
          val ux = id(x)
          OKif(d0(ux) == x, s"Domain of id $ux should be $x in $name") andAlso
            OKif(d1(ux) == x, s"Codomain of id $ux should be $x in $name")
      })
    }

    val idsAreNeutral = OKif(!finiteArrows) orElse {
      Result.traverse(arrows map { f ⇒
        val u_f = m(id(d0(f)), f)
        val f_u = m(f, id(d1(f)))
        OKif(u_f contains f, s"Left unit law broken for ${id(d0(f))} and $f: got $u_f in $name") andAlso
          OKif(f_u contains f, s"Right unit law broken for ${id(d1(f))} and $f: got $f_u in $name")
      })
    }

    val compositionsAreOk = idsAreNeutral andThen OKif(!finiteArrows) orElse checkCompositions

    val compositionIsAssociative = compositionsAreOk andThen (OKif(!finiteArrows) orElse {
      Result.traverse {
        for {
          f ← arrows
          g ← arrows
          h ← arrows
          gf ← m(f, g)
          hg ← m(g, h)
        } yield {
          val h_gf = m(gf, h)
          val hg_f = m(f, hg)
          // the following is for debugging
          val f0 = "" + f + ""
          OKif(h_gf == hg_f, s"Associativity broken for $f, $g and $h, got $h_gf vs $hg_f in $name")
        }
      }
    })

    objectsHaveIds andAlso compositionIsAssociative returning new ValidCategoryData(this)
  }

  def objects: Objects = nodes

  def nodes: Objects = graph.nodes.asInstanceOf[Objects]

  def arrows: Arrows = graph.arrows.asInstanceOf[Arrows]

  def d0(a: Arrow): Obj = {
    val gd0 = graph.d0(graph.arrow(a))
    obj(gd0)
  }

  def d1(a: Arrow): Obj = obj(graph.d1(graph.arrow(a)))

  def composablePairs: Iterable[(Arrow, Arrow)] = Categories.composablePairs(this)

  private[cat] def asObj(x: Any): Obj = x.asInstanceOf[Obj]

  private[cat] def asArrow(a: Any): Arrow = a.asInstanceOf[Arrow]

  private[cat] def checkCompositions = {
    val check1 = Result.traverse(missingCompositions map {
      case (f, g) => Oops(s"composition must be defined for $f and $g in $name")
    })

    val check2 = Result.traverse {
      for {
        f ← arrows
        g ← arrows
        h = m(f, g)
      } yield {
        if (follows(g, f)) {
          Result(h) flatMap { gf ⇒
            OKif(sameDomain(gf, f),
              s"Wrong composition $gf of $f and $g : its d0 is ${d0(gf)}, must be ${d0(f)} in $name") andAlso
              OKif(sameCodomain(gf, g),
                s"Wrong composition $gf of $f and $g: its d1 is ${d1(gf)}, must be ${d1(g)} in $name")
          } returning()
        }
        else {
          OKif(h.isEmpty, s"Wrongly defined composition of $f and $g in $name")
        }
      }
    }

    check1 andAlso check2
  }

  private[cat] def missingCompositions: Iterable[(Arrow, Arrow)] =
    for {
      (f, g) ← composablePairs
      if m(f, g).isEmpty
    } yield (f, g)

  /**
    * Builds a category given a data.
    *
    * @return a category built based on the data above
    *
    *         TODO: eliminate code duplication
    */
  private[cat] def build: Result[Category] = {
    validate map { validData => validData.newCategory }
  }
}

class ValidCategoryData(source: CategoryData) extends CategoryData {
  data =>
  val graph = source.graph

  def newCategory: Category = {
    if (isFinite) newFiniteCategory
    else {
      new Category {
        val graph = data.graph

        override def d0(f: Arrow): Obj = data.d0(data.arrow(f))

        override def d1(f: Arrow): Obj = data.d1(data.arrow(f))

        def id(o: Obj): Arrow = data.id(data.node(o))

        def m(f: Arrow, g: Arrow): Option[Arrow] =
          data.m(data.arrow(f), data.arrow(g)) map arrow
      }
    }
  }

  def newFiniteCategory: Category = {

    val d0Map: Map[Any, Obj] = arrows.map(f ⇒ f -> asObj(d0(arrow(f)))).toMap
    val d1Map: Map[Any, Obj] = arrows.map(f ⇒ f -> asObj(d1(arrow(f)))).toMap

    val idMap: Map[Any, Arrow] = objects.map(o ⇒ o -> id(node(o))).toMap

    val mMap: Map[(Any, Any), Arrow] = {
      for {f <- arrows
           g <- arrows
           h <- m(arrow(f), arrow(g))
      } yield (f, g) -> h
    } toMap

    new Category {
      val graph = data.graph

      override def d0(f: Arrow): Obj = asObj(d0Map(f))

      override def d1(f: Arrow): Obj = asObj(d1Map(f))

      def id(o: Obj): Arrow = idMap(o)

      def m(f: Arrow, g: Arrow): Option[Arrow] = mMap.get((f, g)) map asArrow
    }
  }

  override def id(o: Obj): Arrow = source.id(source.node(o)).asInstanceOf[Arrow]

  override def m(f: Arrow, g: Arrow): Option[Arrow] =
    source.m(source.arrow(f), source.arrow(g)) map arrow
}


/**
  * Builds a category given a limited (but sufficient) amount of data.
  * Objects have the same name as their identities.
  *
  * @tparam T arrow and node type
  * @param g                 the underlying graph
  * @param compositionSource source table of arrows composition (may be incomplete)
  * @return a newly-built category
  */
private[cat] class PartialData(val graph: Graph) extends CategoryData { self =>
  type CompositionTable = Composition[graph.Arrow]
  val compositionSource: CompositionTable = CategoryData.Empty[graph.Arrow]
  lazy val composition: CompositionTable = fillCompositionTable

  override def id(o: Obj): Arrow = o

  override def m(f: Arrow, g: Arrow): Option[Arrow] = {
    composition.find {
      case ((first, second), value) ⇒ first == f && second == g
    }
  } map { m ⇒ arrow(m._2) }

  /**
    * This method helps fill in obvious choices for arrows composition.
    * Case 1. There's an arrow f:a→b, and an arrow g:b→c; and there's just one arrow h:a→c.
    * What would be the composition of f and g? h is the only choice.
    * <p/>
    * Case 2. h ∘ (g ∘ f) = k; what is (h ∘ g) ∘ f? It is k. and vice versa.
    */
  private def fillCompositionTable: CompositionTable = {
    // First, add identities
    val addedIds = defineCompositionWithIds

    // Second, add unique solutions
    val addedUniqueSolutions: CompositionTable = addUniqueCompositions(addedIds)

    // Third, deduce compositions from associativity law
    val addedDeducedCompositions: CompositionTable =
      deduceCompositions(addedUniqueSolutions)

    addedDeducedCompositions
  }

  // adding composition that are deduced from associativity law
  private[cat] def deduceCompositions(compositionSource: CompositionTable): CompositionTable = {
    val triplesToScan = composableTriples(compositionSource)

    val compositions: CompositionTable = (compositionSource /: triplesToScan) {
      (m, t) ⇒ {
        val (f, g, h) = t
        val gf = m((f, g))
        val hg = m((g, h))
        if ((m contains(gf, h)) && !(m contains(f, hg))) {
          m + ((f, hg) → m((gf, h)))
        } else if ((m contains(f, hg)) && !(m contains(gf, h))) {
          m + ((gf, h) → m((f, hg)))
        } else {
          m
        }
      }
    }
    compositions
  }

  // this is a technical method to list all possible triples that have compositions defined pairwise
  private[cat] def composableTriples(compositionSource: CompositionTable):
  Set[(graph.Arrow, graph.Arrow, graph.Arrow)] = {
    val triples: Set[(graph.Arrow, graph.Arrow, graph.Arrow)] = for {
      a ← graph.arrows
      b ← graph.arrows if compositionSource.contains((a, b).asInstanceOf[(graph.Arrow, graph.Arrow)])
      c ← graph.arrows if compositionSource.contains((b, c).asInstanceOf[(graph.Arrow, graph.Arrow)])
    } yield (a, b, c)

    triples
  }

  // adding composition with identities to a composition table
  private def defineCompositionWithIds: CompositionTable = {
    (compositionSource /: graph.arrows) ((m, f) ⇒ {
      val fA = f.asInstanceOf[graph.Arrow]
      val id_d0 = graph.arrow(graph.d0(f))
      val id_d1 = graph.arrow(graph.d1(f))
      try {
        m + ((id_d0, fA) → fA) + ((fA, id_d1) → fA)
      } catch {
        case npe: NullPointerException =>
          throw npe
      }
    })
  }


  // adding unique available compositions
  private[cat] def addUniqueCompositions(compositionSource: CompositionTable): Map[(graph
  .Arrow, graph.Arrow), graph.Arrow] = {
    def candidates(a: graph.Arrow, b: graph.Arrow) =
      graph.arrowsBetween(graph.d0(a), graph.d1(b)).take(2).toList

    def hasUniqueCandidate(a: graph.Arrow, b: graph.Arrow) = {
      candidates(a, b).size == 1
    }

//  def candidateMaybe(a: graph.Arrow, b: graph.Arrow) = candidates(a, b) match {
//    case f::g::_ => None
//    case f::Nil => Option(f)
//    case Nil => 
//  }


  def candidate(a: graph.Arrow, b: graph.Arrow) = candidates(a, b).head

    val pairsToScan = composablePairs filter { case (a, b) ⇒
      hasUniqueCandidate(a.asInstanceOf[graph.Arrow], b.asInstanceOf[graph.Arrow])
    }

    val solutions: CompositionTable = (compositionSource /: pairsToScan) {
      case (m, (a, b)) ⇒ {
        val aA = a.asInstanceOf[graph.Arrow]
        val bA = b.asInstanceOf[graph.Arrow]
        m + ((aA, bA) → candidate(aA, bA).asInstanceOf[graph.Arrow])
      }
    }
    solutions
  }

  /**
    * Builds a category given a data.
    *
    * @return a category built based on the data above
    *
    *         TODO: eliminate code duplication
    */
  private[cat] override def build: Result[Category] = Result.forValue{
    CategoryData.transitiveClosure(this).validate map { validData => validData.newCategory }
  }.flatten
}

object CategoryData {

  type Composition[Arr] = Map[(Arr, Arr), Arr]
  def Empty[Arr] = Map.empty[(Arr, Arr), Arr]
  
  private def addUnitsToGraph(graph: Graph): Graph = {

    val nodesOpt: Option[Set[Any]] = if (graph.isFinite) Some(graph.nodes.toSet) else None

    def isIdentity(f: Any): Boolean = nodesOpt map (_ contains f) getOrElse (graph contains f)

    new Graph {
      override val name = graph.name

      def nodes: Nodes = graph.nodes.asInstanceOf[Nodes]

      lazy val arrows: Arrows = (graph.nodes ++ graph.arrows).asInstanceOf[Arrows]

      def d0(f: Arrow): Node =
        if (isIdentity(f)) node(f) else node(graph.d0(graph.arrow(f)))

      def d1(f: Arrow): Node =
        if (isIdentity(f)) node(f) else node(graph.d1(graph.arrow(f)))
    }
  }

  /**
    * Builds a category given a limited (but sufficient) amount of data.
    * Objects have the same name as their identities.
    *
    * @param g                 the underlying graph
    * @param comp source table of arrows composition (may be incomplete)
    * @return a newly-built category
    */
  def partial[Arr](g: Graph)(comp: Composition[Arr] = Empty[Arr]):
  PartialData = {
    new PartialData(addUnitsToGraph(g)) {
      override val compositionSource: CompositionTable = comp.asInstanceOf[CompositionTable] // same type
    }
  }

  def apply(gr: Graph)(
    ids: gr.Node ⇒ gr.Arrow,
    composition: (gr.Arrow, gr.Arrow) ⇒ Option[gr.Arrow]): CategoryData = {
    new CategoryData {
      val graph = gr

      override def id(o: Obj): Arrow = ids(gr.node(o))

      override def m(f: Arrow, g: Arrow): Option[Arrow] =
        composition(gr.arrow(f), gr.arrow(g)) map arrow
    }
  }

  private[cat] def transitiveClosure(data: PartialData, previouslyMissing: Int = Int.MaxValue): PartialData = {
    val missing = data.missingCompositions
    if (missing.isEmpty) data else {
      if (missing.size >= previouslyMissing) {
        throw new IllegalArgumentException(s"${data.name}: ${missing.size} arrows still missing, can't fix")
      }
      transitiveClosure(new PartialData(data) {
        override val compositionSource: CompositionTable = data.composition.asInstanceOf[CompositionTable]
      }, missing.size)
    }
  }



}