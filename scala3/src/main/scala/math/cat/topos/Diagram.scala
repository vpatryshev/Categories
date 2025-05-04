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
  extends Functor(source.tag, source.d0, source.d1)

//  given Conversion[d1.Arrow, SetFunction] = source.asFunction
