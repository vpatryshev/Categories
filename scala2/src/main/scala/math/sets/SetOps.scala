package math.sets

import scala.collection.mutable

object SetOps {

  def cantorIterable[X, Y](xs: Iterable[X], ys: Iterable[Y]): Iterable[(X, Y)] =
    new Iterable[(X, Y)] {
      override def iterator: Iterator[(X, Y)] = cantorIterator(xs, ys)
    }

  def cantorIterator[X, Y](xs: Iterable[X], ys: Iterable[Y]): Iterator[(X, Y)] =
    new Iterator[(X, Y)] {
      private[this] var iterators: mutable.Queue[Iterator[Y]] = new mutable.Queue()
      private[this] var xi = xs.iterator
      private[this] var yi: Iterator[Iterator[Y]] = Iterator.empty
      private[this] var shift = 0
      private[this] var i = 0
      
      def next(): (X, Y) = {
        if (!yi.hasNext) {
          if (xi.hasNext) {
            iterators enqueue ys.iterator
          }
          yi = iterators.iterator
          xi = xs.iterator.drop(shift)
        }
        val yii = yi.next()
        val y = yii.next()
        val res = (xi.next(), y)

        if (iterators.nonEmpty && yii.isEmpty) {
          iterators.dequeue()
          yi = iterators.iterator
          shift += 1
        }
        
        i += 1
        res
      }

      def hasNext: Boolean = xs.nonEmpty && ys.nonEmpty &&
        (xi.hasNext || (iterators.nonEmpty && iterators.head.hasNext))
    }

}
