package math.cat.topos

import math.cat.Category

/**
  * Topos functionality
  */
trait Topos:
  type Obj
  type Arrow
  // finite limits should exist
  def product2(x: Obj, y: Obj): Obj
  
  val Ω: Obj

  def ΩxΩ: Obj

// TODO: implement
//  def pow: Obj => Obj
