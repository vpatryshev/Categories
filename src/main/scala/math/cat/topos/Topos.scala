package math.cat.topos

import math.cat.Category

/**
  * Topos functionality
  */
trait Topos { self: Category =>
  type Obj
  type Arrow
  // finite limits should exist
  val Ω: Obj
  
//  def pow: Obj => Obj
}
