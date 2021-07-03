package math.cat.construction

import scala.language.{implicitConversions, postfixOps}
import math.cat._

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

  def bm[T](coll: Set[_], f: Any => T): Map[Any, T] = coll.map(x => x -> f(x)).toMap

  def newFiniteCategory: Category =

    val d0Map: Map[Any, source.Obj] = bm(source.arrows, f => source.asObj(source.d0(source.arrow(f))))
    val d1Map: Map[Any, source.Obj] = bm(source.arrows, f => source.asObj(source.d1(source.arrow(f))))
    val idMap: Map[Any, source.Arrow] = bm(source.objects, o => source.arrow(source.id(source.node(o))))

    /*
  def d0(a: Arrow): Obj =
    val gd0 = graph.d0(graph.arrow(a))
    obj(gd0)

  def d1(a: Arrow): Obj = obj(graph.d1(graph.arrow(a)))


 */

    //    override def d0(f: Arrow): Obj = node(data.d0(data.arrow(f)))
    //    override def d1(f: Arrow): Obj = node(data.d1(data.arrow(f)))


    val mMap: Map[(Any, Any), source.Arrow] = {
      for
        f <- source.arrows
        g <- source.arrows
        h <- source.m(source.arrow(f), source.arrow(g))
      yield (f, g) -> h
    } toMap

    new Category(source.name) :
      override val graph = source.graph

      override def d0(f: Arrow): Obj = asObj(d0Map(f))

      override def d1(f: Arrow): Obj = asObj(d1Map(f))

      def id(o: Obj): Arrow = idMap(o).asInstanceOf[Arrow]

      def m(f: Arrow, g: Arrow): Option[Arrow] = mMap.get((f, g)) map asArrow

  end newFiniteCategory
