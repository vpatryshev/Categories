package math.cat

abstract class Morphism[X, Y] (d0: X, d1: Y) {
  val domain = d0
  val codomain = d1
}
