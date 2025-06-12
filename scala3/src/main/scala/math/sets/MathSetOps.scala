package math.sets

import scala.collection.{Iterator, mutable}

object MathSetOps:

  /**
    * Having two iterables, possibly infinite, produce an iterable that iterates over pairs 
    * using Cantor's algorithm, slightly modified.
    * 
    * @param xs first iterable
    * @param ys second iterable
    * @tparam X type of values of first iterable
    * @tparam Y type ov values of second iterable
    * @return an iterable that produces pairs.
    */
  def cantorIterable[X, Y](xs: Iterable[X], ys: Iterable[Y]): Iterable[(X, Y)] =
    new Iterable[(X, Y)]:
      override def iterator: Iterator[(X, Y)] = cantorIterator(xs, ys)

  /**
    * Having two iterables, possibly infinitely, iterate over pairs 
    * using Cantor's algorithm, slightly modified.
    *
    * @param xs first iterable
    * @param ys second iterable
    * @tparam X type of values of first iterable
    * @tparam Y type ov values of second iterable
    * @return an iterable that produces pairs.
    */
  def cantorIterator[X, Y](xs: Iterable[X], ys: Iterable[Y]): Iterator[(X, Y)] =
    if xs.isEmpty || ys.isEmpty then Iterator.empty
    else
      new Iterator[(X, Y)]:
        private val iterators: mutable.Queue[Iterator[Y]] = new mutable.Queue()
        private var xi = xs.iterator
        private var yi: Iterator[Iterator[Y]] = Iterator.empty
        private var shift = 0

        def hasNext: Boolean = xi.hasNext || !iterators.isEmpty

        def next(): (X, Y) =
          if !yi.hasNext then
            if xi.hasNext then iterators enqueue ys.iterator
            yi = iterators.iterator
            xi = xs.iterator.drop(shift)
          val yii = yi.next()
          val y = yii.next()

          if !iterators.isEmpty && yii.isEmpty then
            iterators.dequeue()
            yi = iterators.iterator
            shift += 1

          (xi.next(), y)
      

