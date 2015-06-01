package math.cat

object Base {
  def product[X, Y] (xs: Iterable[X], ys: Iterable[Y]) = 
    for (x <- xs; y <- ys) yield (x, y)
}

