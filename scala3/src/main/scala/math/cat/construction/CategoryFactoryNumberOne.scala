package math.cat.construction

import scala.language.{implicitConversions, postfixOps}
import math.cat._
import math.sets.Sets._

class CategoryFactoryNumberOne(source: CategoryData):

  val graph = source.graph

  def newCategory: Category =
    if source.isFinite then newFiniteCategory
    else
      new Category(source.name) :
        override val graph = source.graph

        override def d0(f: Arrow): Obj = node(source.d0(source.arrow(f)))
        override def d1(f: Arrow): Obj = node(source.d1(source.arrow(f)))
        def id(o: Obj): Arrow = source.id(source.node(o)).asInstanceOf[Arrow]

        def m(f: Arrow, g: Arrow): Option[Arrow] =
          source.m(source.arrow(f), source.arrow(g)) map arrow

  end newCategory

  /**
    * A special kind of map builder (somehow it's much faster than the one in `Sets`
    * @param coll collection of keys
    * @param f mapping of keys to values
    * @tparam T value type
    * @return a map
    */
  def buildMap[T](coll: Iterable[_], f: Any => T): Map[Any, T] = coll.map(x => x -> f(x)).toMap

  def newFiniteCategory: Category =


    val mMap: Map[(Any, Any), source.Arrow] = {
      for
        f <- source.arrows
        g <- source.arrows
        h <- source.m(f, g)
      yield (f, g) -> h
    } toMap

    new Category(source.name) :
      override val graph = source.graph
      private val d0Map: Map[Any, Obj]   = buildMap(source.arrows,  f => asObj(source.d0(source.arrow(f))))
      private val d1Map: Map[Any, Obj]   = buildMap(source.arrows,  f => asObj(source.d1(source.arrow(f))))
      private val idMap: Map[Any, Arrow] = buildMap(source.objects, o => asArrow(source.id(source.node(o))))

      override inline def d0(f: Arrow): Obj = d0Map(f)
      override inline def d1(f: Arrow): Obj = d1Map(f)

      inline def id(o: Obj): Arrow = idMap(o) 

      def m(f: Arrow, g: Arrow): Option[Arrow] = mMap.get((f, g)) map asArrow

  end newFiniteCategory
