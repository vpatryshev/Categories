package math.cat

import scalakittens.Result
import scalakittens.Result.OKif

/**
  * The data used in building an instance of Category
  */
private[cat] abstract class CategoryData(val graph: Graph) extends Graph {
  override val name: String = graph.name
  
  type Obj = Node
  type Objects = Set[Obj]
  
  lazy val listOfObjects: List[Obj] =
    if (isFinite) objects.toList.sortBy(_.toString)
    else throw new IllegalStateException("Cannot sort infinite set")

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

    val compositionsAreDefined = idsAreNeutral andThen OKif(!finiteArrows) orElse checkCompositions

    val compositionIsAssociative = compositionsAreDefined andThen (OKif(!finiteArrows) orElse {
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

  private[cat] def asObj(x: Any): Obj = x.asInstanceOf[Obj]

  private[cat] def asArrow(a: Any): Arrow = a.asInstanceOf[Arrow]

  def composablePairs: Iterable[(Arrow, Arrow)] = Categories.composablePairs(this)

  private[cat] def checkCompositions = {
    Result.traverse {
      for {
        f ← arrows
        g ← arrows
        h = m(f, g)
      } yield {
        if (follows(g, f)) {
          Result(h) orCommentTheError s"composition must be defined for $f and $g in $name" flatMap { gf ⇒
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
  }

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

object CategoryData {
  def apply(gr: Graph)(
    ids: gr.Node ⇒ gr.Arrow,
    composition: (gr.Arrow, gr.Arrow) ⇒ Option[gr.Arrow]): CategoryData = {
    new CategoryData(gr) {
      override def id(o: Obj): Arrow = ids(gr.node(o))

      override def m(f: Arrow, g: Arrow): Option[Arrow] =
        composition(gr.arrow(f), gr.arrow(g)) map arrow
    }
  }
}

class ValidCategoryData(source: CategoryData) extends CategoryData(source.graph) {
  data =>
  def newCategory: Category = {
    if (isFinite) newFiniteCategory
    else {
      new Category(data) {
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

    new Category(this) {
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
  * @param g             the underlying graph
  * @param compositionSource source table of arrows composition (may be incomplete)
  * @return a newly-built category
  */
private[cat] class PartialData[T](
  val g: Graph,
  compositionSource: Map[(T, T), T] = Map.empty[(T, T), T])
  extends CategoryData(Categories.addUnitsToGraph(g)) {
//  val graph1: Graph = Categories.addUnitsToGraph(graph)
  val composition: Map[(T, T), T] = Categories.fillCompositionTable(graph, compositionSource)

  override def id(o: Obj): Arrow = o

  override def m(f: Arrow, g: Arrow): Option[Arrow] = {
      composition.find {
        case ((first, second), value) ⇒ first == f && second == g
      }
    } map { m ⇒ arrow(m._2) }
}
