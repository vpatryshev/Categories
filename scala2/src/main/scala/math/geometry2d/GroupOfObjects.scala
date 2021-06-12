package math.geometry2d

case class GroupOfObjects[T](objects: Iterable[T]) {

  def arrangeInCircle(c: Pt, r: Rational) = {
    objects.size match {
      case 1 => Set((objects.head, c))
      case n =>
        val da = 2 * Math.PI / n

        def p(i: Int): Pt = {
          val a = da * i + Math.PI/6
          val x = -r * Math.cos(a)
          val y = r * Math.sin(a)
          val relative = Pt(x, y)
          val abs = c + relative
          abs
        }

        (for {(name, i) <- objects.zipWithIndex} yield (name, p(i))).toSet
    }
  }

}
