package testing

class DepTypesTest extends TestBase:
  case class Something(what: String):
    override def toString = s"this is $what"
  
  class A:
    type TA = Something
    val a: TA = Something("an a")
    type TAS = Set[TA]
    val as = Set[TA](Something("a1"), Something("a2"))
    def print(x: TA) = println(x)
    
  class B extends A:
    type TB1 = TA
    type TB2 = Something
    type TAS0 = TAS
    type TAS1 = Set[TA]
    type TB1S = Set[TB1]
    type TB2S = Set[TB2]
    val b1: TB1 = a
    val b2: TB2 = a
    print(b1)
    print(b2)
    val as0: TAS0 = as
    val as1: TAS1 = as
    val bs1: TB1S = as
    val bs2: TB2S = as
        
  
  "types" should :
    "be compatible" in :
      val aa: A = new A
      val bb = new B
      bb.print(aa.a)
      ok

