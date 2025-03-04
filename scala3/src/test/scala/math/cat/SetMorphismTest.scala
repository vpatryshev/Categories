package math.cat

import math.Test
import math.cat.SetMorphism._
import math.sets.Sets._
import math.sets.{N, Sets}
import org.specs2.mutable._
import scalakittens._

class SetMorphismTest extends Test:
  val ints: Set[BigInt] = Set(1, 2, 3, 5, 8, 13)
  val strings = Set("even", "odd", "totally crazy")

  val testSetX = Set(1, 2, 3, 4, 5)
  val testSetZ = Set("#1", "#2", "#3", "#4", "#5")

  val m: SetMorphism[BigInt, String] =
    new SetMorphism[BigInt, String]("testFun", ints, strings,
      i => List("even", "odd")((i % 2).toInt)
  )

  "Constructor1()" >> {
    m(8) === "even"
    m(13) === "odd"

    try {
      m(4)
      m(4)
      failure("4 does not belong to domain; should have thrown an error");
    } catch  {
      case e: NoSuchElementException => // as expected
    }
    ok
  }

  "Composition with id" >>  {
    m.andThen(id(strings)) === Some(m)
    id(ints).andThen(m) === Some(m)
  }

  "Constructor2" >> {
    val sut = SetMorphism.build(testSetX, testSetZ, (n: Int) => "#" + n).iHope
    sut(3) === "#3"
    sut.d0 === testSetX
    sut.d1 === testSetZ
  }

  "Constructor_negative" >> {
    val y = Set("#1", "#2", "#3", "#5")
    SetMorphism.build(testSetX, y, (n: Int) => "#" + n).isBad should beTrue
  }

  "Equals" >> {
    val sut1 = SetMorphism.build(testSetX, testSetZ, (n: Int) => "#" + n).iHope
    val sut2 = SetMorphism.build(Set(5, 4, 3, 2, 1), Set("#5", "#4", "#3", "#2", "#1"),
                           Map(1 -> "#1", 2 -> "#2", 3 -> "#3", 4 -> "#4", 5 -> "#5")).iHope
    sut1 === sut2
    val sut3 = SetMorphism.build(Set(5, 4, 3, 2, 1), Set("#5", "#4", "#3", "#2", "#1"),
                           Map(1 -> "#1", 2 -> "#2", 3 -> "#3", 4 -> "#4", 5 -> "#3")).iHope
    (sut1 != sut3) must beTrue
  }

  "Compose" >> {
    val x = Set(11, 12, 13, 14, 15)
    val f = SetMorphism.build(x, testSetX, (n: Int) => n - 10).iHope
    val g = SetMorphism.build(testSetX, testSetZ, (n: Int) => "#" + n).iHope
    val h = f andThen g
    
    h === Some(SetMorphism.build(x, testSetZ, (n: Int) => "#" + (n - 10)).iHope)
  }

  "Revert" >> {
    val sut1 = SetMorphism.build(Set("abc", "x", "def"), testSetX, (s:String) => s.length).iHope
    val sut2 = SetMorphism.build(
      Set(5, 4, 3, 2, 1),
      Set(Set("abc", "def"), Set("x"), Set()),
      Map(1 -> Set("x"), 2 -> Set(), 3 -> Set("abc", "def"), 4 -> Set(), 5 -> Set())).iHope
    
    sut2 === sut1.revert
  }

  "id" >> {
    val s = Set(1, "haha", 2.71828)
    val sut = SetMorphism.id(s)
    Set(2.71828, 1, "haha") === sut.d0
    Set(2.71828, 1, "haha") === sut.d1
    sut("haha") === "haha"
  }

  "Const" >> {
    val strings = Set("a", "b", "c")
    val ints = Set(1, 2, 3)
    val two:Int = 2
    val sut = SetMorphism.const(strings, ints, two)
    sut.d0 === strings
    sut.d1 === ints
    sut("a") === 2
  }

  "Hom" >> {
    val d0 = Set("1", "2")
    val d1 = Set("x", "y", "z")
    (d0 != d1) must beTrue
    val actual = SetMorphism.hom(d0, d1)
    (actual.size == 9) must beTrue
    (actual contains SetMorphism.build(d0, d1, Map("1" -> "y", "2" -> "x")).iHope) must beTrue
  }

  "Variance_byX" >> {
    val x = setOf(testSetX, testSetX.size, testSetX.contains)
    val sut = SetMorphism.build(x, testSetZ, (n: Int) => "#" + n).iHope
    sut(3) === "#3"
  }

  "Variance_byY" >> {
    val y = setOf(testSetZ, testSetZ.size, testSetZ.contains)
    val sut = SetMorphism.build(testSetX, y, (n: Int) => "#" + n).iHope
    sut(3) === "#3"
  }

  "Product" >> {
    val x = Set(0, 1, 2, 3, 4, 5)
    val y = Set(0, 1, 2)
    val sut = SetMorphism.build(x, y, (x: Int) => x % 3).iHope.product

    def m(x: Int, y: Int, z: Int) = Map(0 -> x, 1 -> y, 2 -> z)

    sut === Set(
      m(0, 1, 2), m(0, 1, 5),
      m(0, 4, 2), m(0, 4, 5),
      m(3, 1, 2), m(3, 1, 5),
      m(3, 4, 2), m(3, 4, 5))
  }

  "Pullback" >> {
    val xs = Set[BigInt](1,2,3,4,5)
    val ys = Set[BigInt](2,4,6,8,10)
    val f = SetMorphism.build("f", xs, N, (x:BigInt) => x/2).iHope
    val g = SetMorphism.build("g", ys, N, (y:BigInt) => y/5).iHope
    val (left, right) = SetMorphism.pullback(f, g)
    val pullbackSet = Set[(BigInt,BigInt)]((1,2),(1,4),(2,6),(2,8),(3,6),(3,8),(4,10),(5,10))
    for p <- pullbackSet do
      left(p) === p._1
      right(p) === p._2

    ok
  }
  
  "Good inclusion" >> {
    val xs = Set[BigInt](1,2,3,4,5)
    val ys = Set[BigInt](0,1,2,3,4,5,6)

    inclusion[BigInt](xs, ys) match
      case Good(sm) =>
        sm.d0 === xs
        sm.d1 === ys
        for
          i <- xs
        do sm(i) === i

      case nogood => failure(nogood.toString)
    ok
  }

  "Bad inclusion" >> {
    val xs = Set[BigInt](1,2,3,4,5)
    val ys = Set[BigInt](0,1,2,3,4,5,6)
    inclusion[BigInt](ys, xs) match
      case Good(sm) => failure("Could not detect an error in bad inclusion")
      case nogood => ok

    ok
  }

  "product of two morphisms" >> {
    val fd0 = Set("a", "b", "c")
    val gd0 = Set("A", "B")
    val expected_d0 = Set(("a", "A"), ("a", "B"), ("b", "A"), ("b", "B"), ("c", "A"), ("c", "B"))

    val fd1 = Set("A", "B", "C")
    val gd1 = Set("a", "b")
    val expected_d1 = Set(("A", "a"), ("A", "b"), ("B", "a"), ("B", "b"), ("C", "a"), ("C", "b"))
    
    val f = new SetMorphism("f", fd0, fd1, (_: String).toUpperCase)
    val g = new SetMorphism("g", gd0, gd1, (_: String).toLowerCase)
    val product = SetMorphism.product2(f, g)
    product.tag === "f×g"
    product.d0 === expected_d0
    product.d1 === expected_d1

    for
      (x, y) <- expected_d0
    do
      product((x, y)) === (x.toUpperCase, y.toLowerCase)

    ok
  }
