package math.cat.topos

import math.cat.Category

/**
  * Topos functionality
  */
trait Topos[O, A]:
  type Obj = O
  type Arrow = A
  // finite limits should exist
  def product2(x: O, y: O): O
  
  val Ω: O

  def ΩxΩ: O

// TODO: implement
//  def pow: Obj => Obj
