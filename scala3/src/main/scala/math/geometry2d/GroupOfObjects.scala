package math.geometry2d

import scala.language.implicitConversions
import Math._

case class GroupOfObjects[T](objects: Iterable[T]):

  def arrangeInCircle(c: Pt, r: Rational): Set[(T, Pt)] =
    objects.size match
      case 0 => Set.empty[(T, Pt)]
      case 1 => Set((objects.head, c))
      case n =>
        val da = 2 * PI / n

        def p(i: Int): Pt =
          val a = da * i + PI/6
          c + Pt(-cos(a), sin(a)) * r

        (for (name, i) <- objects.zipWithIndex yield (name, p(i))).toSet
