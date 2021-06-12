package math

import org.specs2.mutable._

/**
  * Prototype for all tests
  */
class _Test extends Specification {
  class T[A](val a:A) {
    final type A0 = A
    val a0: A0 = a
    def a1: A0 = a
    final type SA = Set[A0]
    val sample: SA = Set(a)
  }
  
  type TA = T[Int]
  type TB = T[Double]

//  def ct1[C <: T[_], D <: T[_]](x: C, y: D)(implicit ev: C =:= T[_]): Unit = {
//    ct11(x, y)
//    val xa0: x.A0 = x.a0
//    val xa1: C#A0 = xa0
//    val xa2: C#A0 = x.a1
//    val xa: C#A0 = x.a0
//    val ya: D#A0 = y.a0
//  }
//
//  def ct11[X](x: T[X], y: T[X]): Unit = {
//    val xa0: X = x.a0
//    val xa1: X = xa0
//    val xa2: X = x.a1
//    val xa: X = x.a0
//    val ya: X = y.a0
//  }
//
//  def comp[X](a: X, b: X): Boolean = a == b
//  
//  def ct2[C <: T[_]](x: C, y: C): Unit = {
//    val xa: C#A0 = x.a0
//    val xa1: C#A0 = x.a0
//    val ya: C#A0 = y.a0
//    val equal = comp(xa, ya)
//    val xs: C#SA = x.sample
//    val ys: C#SA = y.sample
//    val equals = comp(xs, ys)
//  }

  
  
  "_" should {
    "work" in {
      ok
    }
  }
}
