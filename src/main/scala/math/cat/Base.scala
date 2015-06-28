package math.cat

object Base {
  /**
   * Do we even need it?
   * @param xs
   * @param ys
   * @tparam X
   * @tparam Y
   * @return
   */
  def product[X, Y] (xs: Iterable[X], ys: Iterable[Y]) = 
    for (x <- xs; y <- ys) yield (x, y)
}

