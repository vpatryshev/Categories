package math.cat.construction

import math.cat._
import math.sets.Sets._

import scala.language.{implicitConversions, postfixOps}
import math.Base.*

/**
  * Given `CategoryData` as a source,
  * creates a new category, with validation.
  * The logic depends on whether the category is finite or not.
  * For finite categories, mappings are cached in maps,
  * which makes the code several times faster.
  * 
  * @param source data for building a category
  */
class CategoryBuilder(val source: CategoryData):

  val graph: Graph = source

  private def sd0[A](a: A): source.Obj = source.d0(a)
  private def sd1[A](a: A): source.Obj = source.d1(a)
  private def sid[O](o: O): source.Arrow = source.id(o)
  
  def newCategory: Category =
    if source.isFinite then
      newFiniteCategory
    else
      new Category(source.name):
        override val graph = source
        override def nodes = source.nodes.asInstanceOf[Nodes] // TODO: remove this cast

        override def d0(f: Arrow): Obj = sd0(f)
        override def d1(f: Arrow): Obj = sd1(f)
        def id(o: Obj): Arrow = sid(o)

        def m(f: Arrow, g: Arrow): Option[Arrow] =
          source.m(f, g) map asArrow

  end newCategory

  def newFiniteCategory: Category =

    new Category(source.name):
      override val graph: Graph = source
      override def nodes = graph.nodes.asInstanceOf[Nodes] // TODO: remove this cast
      private val d0Map: Map[Any, Obj]   = buildMap(graph.arrows,  f => sd0(f))
      private val d1Map: Map[Any, Obj]   = buildMap(graph.arrows,  f => sd1(f))
      private val idMap: Map[Any, Arrow] = buildMap(source.objects, o => sid(o))

      override inline def d0(f: Arrow): Obj = d0Map(f)
      override inline def d1(f: Arrow): Obj = d1Map(f)

      inline def id(o: Obj): Arrow = idMap(o)
  
      private val mMap = {
        for
          f <- source.arrows
          g <- source.arrows
          h <- source.m(f, g)
        yield (f, g) -> asArrow(h) // TODO: find a way to get rid of this `asArrow`
      } toMap
      
      def m(f: Arrow, g: Arrow): Option[Arrow] = mMap.get((f, g))

  end newFiniteCategory
