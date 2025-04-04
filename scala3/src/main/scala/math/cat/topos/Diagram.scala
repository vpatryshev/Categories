package math.cat.topos

import math.Base.*
import math.cat.*
import math.sets.Sets
import math.sets.Sets.*

import scala.language.{implicitConversions, postfixOps}

/**
  * This class is deprecated. Use Diagramme instead.
  */
abstract class Diagram(
  val t: GrothendieckTopos)(val source: t.Diagramme)
  extends Functor(source.tag):
  override val d1: Category = source.d1

  def asFunction(a: d1.Arrow): SetFunction = source.asFunction(a)

  given Conversion[d1.Arrow, SetFunction] = asFunction

  infix def apply(x: Any): set = source(x)
