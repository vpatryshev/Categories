package math.cat

import org.specs2.mutable._
import SetMorphism._

class SetMorphismTest extends Specification {
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
    true
  }

  "UnitComposition()" >>  {
    m.compose(id(strings)) === Some(m)
    id(ints).compose(m) === Some(m)
  }

 "Constructor2" >> {
    val sut = SetMorphism(testSetX, testSetZ, (n: Int) => "#" + n)
    sut(3) === "#3"
    sut.d0 === testSetX
    sut.d1 === testSetZ
  }

  "Constructor_negative" >> {
    val y = Set("#1", "#2", "#3", "#5")
    SetMorphism(testSetX, y, (n: Int) => "#" + n) should throwA[IllegalArgumentException]
  }

  "Equals" >> {
    val sut1 = SetMorphism(testSetX, testSetZ, (n: Int) => "#" + n)
    val sut2 = SetMorphism(Set(5, 4, 3, 2, 1), Set("#5", "#4", "#3", "#2", "#1"),
                           Map(1 -> "#1", 2 -> "#2", 3 -> "#3", 4 -> "#4", 5 -> "#5"))
    (sut1 == sut2) must beTrue
    val sut3 = SetMorphism(Set(5, 4, 3, 2, 1), Set("#5", "#4", "#3", "#2", "#1"),
                           Map(1 -> "#1", 2 -> "#2", 3 -> "#3", 4 -> "#4", 5 -> "#3"))
    (sut1 != sut3) must beTrue
  }

  "Compose" >> {
    val x = Set(11, 12, 13, 14, 15)
    val f = SetMorphism(x, testSetX, (n: Int) => n - 10)
    val g = SetMorphism(testSetX, testSetZ, (n: Int) => "#" + n)
    val h = f compose g
    
    h === Some(SetMorphism(x, testSetZ, (n: Int) => "#" + (n - 10)))
  }

  "Revert" >> {
    val sut1 = SetMorphism(Set("abc", "x", "def"), testSetX, (s:String) => s.length)
    val sut2 = SetMorphism(Set(5, 4, 3, 2, 1), Set(Set("abc", "def"), Set("x"), Set()),
                           Map(1 -> Set("x"), 2 -> Set(), 3 -> Set("abc", "def"), 4 -> Set(), 5 -> Set()))
    (sut2 == sut1.revert) must beTrue
  }

  "Unit" >> {
    val set = Set(1, "haha", 2.71828)
    val sut = SetMorphism.id(set)
    (Set(2.71828, 1, "haha") == sut.d0) must beTrue
    (Set(2.71828, 1, "haha") == sut.d1) must beTrue
    ("haha" == sut("haha")) must beTrue
  }

  "Const" >> {
    val strings = Set("a", "b", "c")
    val ints = Set(1, 2, 3)
    val two:Int = 2
    val sut = SetMorphism.const(strings, ints, two)
    (strings == sut.d0) must beTrue
    (ints == sut.d1) must beTrue
    (2 == sut("a")) must beTrue
  }

  "Hom" >> {
    val d0 = Set("1", "2")
    val d1 = Set("x", "y", "z")
    (d0 != d1) must beTrue
    val actual = SetMorphism.hom(d0, d1)
    (actual.size == 9) must beTrue
    (actual contains SetMorphism(d0, d1, Map("1" -> "y", "2" -> "x"))) must beTrue
  }

  "Variance_byX" >> {
    val x = Sets.setOf(testSetX, testSetX.size, testSetX.contains _)
    val sut = SetMorphism(x, testSetZ, (n: Int) => "#" + n)
    ("#3" == sut(3)) must beTrue
  }

  "Variance_byY" >> {
    val y = Sets.setOf(testSetZ, testSetZ.size, testSetZ.contains _)
    val sut = SetMorphism(testSetX, y, (n: Int) => "#" + n)
    ("#3" == sut(3)) must beTrue
  }

  "Product" >> {
    val x = Set(0, 1, 2, 3, 4, 5)
    val y = Set(0, 1, 2)
    val sut = SetMorphism(x, y, (x: Int) => x % 3).product

    def m(x: Int, y: Int, z: Int) = Map(0 -> x, 1 -> y, 2 -> z)

    sut === Set(m(0, 1, 2), m(0, 1, 5),
      m(0, 4, 2), m(0, 4, 5),
      m(3, 1, 2), m(3, 1, 5),
      m(3, 4, 2), m(3, 4, 5))
  }

  "Pullback" >> {
    val xs = Set[BigInt](1,2,3,4,5)
    val ys = Set[BigInt](2,4,6,8,10)
    val f = SetMorphism("f", xs, N, (x:BigInt) => x/2)
    val g = SetMorphism("g", ys, N, (y:BigInt) => y/5)
    val (left, right) = SetMorphism.pullback(f, g)
    val pullbackSet = Set[(BigInt,BigInt)]((1,2),(1,4),(2,6),(2,8),(3,6),(3,8),(4,10),(5,10))
    for (p <- pullbackSet) {
      (p._1 == left(p)) must beTrue
      (p._2 == right(p)) must beTrue
    }
    true
  }
}