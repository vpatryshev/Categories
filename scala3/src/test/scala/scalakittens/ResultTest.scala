package scalakittens

import testing.TestBase
import Result.*
import org.specs2.matcher.MatchResult

import scala.collection.immutable.Nil as bad


/**
 * Prototype for all tests
 */
class ResultTest extends TestBase:

  implicit class checker(r: Result[?]):
    infix def mustBeBad(beef: String*): MatchResult[Any] =
      (r.isBad aka r.toString) must_== true
      r.errorDetails must beSome(beef mkString "; ")

    infix def mustBeGood[T](value: T): MatchResult[Any] =
      (r.isGood aka r.toString) must_== true
      r must_== Good(value)

  private val oops = error("oops")

  "Good" should:
    "be good" in :
      Good(42).isGood === true

    "be not bad" in :
      Good(43).isBad === false

    "list no errors" in :
      Good(scala.math.Pi).listErrors.isEmpty === true

    "do nothing on error" in :
      var beenThere = false
      Good("I'm good").onError((errors: Any) => {
        beenThere = true
      })

      beenThere === false

    "Map as designed" in :
      Good("hello") map ((s: String) => s.toUpperCase) mustBeGood "HELLO"

    "FlatMap as designed" in :
      Good("hello") flatMap ((s: String) => Good(s.toUpperCase)) mustBeGood "HELLO"
      Good("hello") flatMap ((s: String) => oops) mustBeBad "oops"
      Good("hello") flatMap ((s: String) => Empty) must_== Empty

    "collect as designed" in :
      val err = ":("
      Good("hello") collect( {
        case "hello" => 1
      }, _ => ":(") mustBeGood 1
      Good("hello") collect( {
        case "Hello" => 1
      }, _ => ":(") mustBeBad err

    "convert to Some" in :
      Good(":)").asOption must beSome(":)")

    "stay put in orElse" in :
      Good(":)").orElse(Empty) mustBeGood ":)"
    "stay put in getOrElse" in :
      Good(":)").getOrElse(":(") === ":)"

    "blend properly via <*>" in :
      Good(1) <*> Good(2) mustBeGood ((1, 2))
      Good(1) <*> oops mustBeBad "oops"
      Good(1) <*> Empty === Empty

    "call function in foreach" in :
      var v: String = ":("
      Good(":)") foreach (v = _)
      v === ":)"

    "filter as designed" in :
      Good("hello") filter((s: String) => s.startsWith("he"), "oi vei") mustBeGood "hello"
      Good("huilo") filter((s: String) => s.startsWith("he"), "oi vei") mustBeBad "oi vei"
      Good("huilo") filter ((s: String) => s.startsWith("he")) must_== Empty

    "Show nothing in errorDetails" in :
      Good("sh").errorDetails === None

    "combine with other goods in sugared loop" in :
      val actual =
        for
          x <- Good("x")
          y <- Good("y")
        yield x + y

      actual mustBeGood "xy"

    "have equality" in :
      Good("x") mustBeGood "x"
      Good("x") must_!= Good("y")
      Good("x") must_!= null

  "Bad" should:
    "be bad" in :
      oops.isGood === false

    "list errors" in :
      val errors: Iterable[Throwable] =
        new Exception("Whose life is it?") ::
        new Exception("Hey Euler!") :: Nil

      Result.bad[BigInt](errors).listErrors must_== errors

    "behave on error" in :
      val errors = new Exception("Say hi") ::
        new Exception("Say bye") :: Nil
      var beenThere = false
      var ed: Errors = Nil
      Result.bad(errors).onError((es: Errors) => {
        beenThere = true
        ed = es
      })
      (beenThere aka "visited what you were not supposed to visit") must_== true
      ed === errors

  "Map as designed" in :
    var wasThere = false
    error[Int]("oops") map ((x: Int) => {
      wasThere = true
      1 + x
    }) must_== oops
    wasThere aka "visited what you were not supposed to visit" must beFalse


  "flatMap as designed" in :
    var wasThere = false
    val r1 = error[String]("oops") flatMap ((s: String) => {
      wasThere = true
      Good(s.toUpperCase)
    })
    r1 must_== oops
    wasThere aka "visited what you were not supposed to visit" must beFalse
    val r20: Result[String] = oops
    val r2 = r20 flatMap (s => {
      wasThere = true
      oops
    })
    r2 mustBeBad "oops"
    wasThere aka "visited what you were not supposed to visit" must beFalse
    val r3: Result[String] = oops
    r3 flatMap (s => {
      wasThere = true
      Empty
    }) mustBeBad "oops"
    wasThere aka "visited what you were not supposed to visit" must beFalse

  "collect nothing" in :
    var wasThere = false
    val bad: Result[String] = oops
    bad collect( {
      case "hello" => wasThere = true; 1
    }, _ => ":(") must_== oops
    wasThere aka "visited what you were not supposed to visit" must beFalse
    val bad1: Result[String] = oops
    bad1 collect( {
      case "Hello" => wasThere = true; 1
    }, _ => ":(") must_== oops
    wasThere aka "visited what you were not supposed to visit" must beFalse

  "convert to None" in :
    oops.asOption must beNone

  "get ignored in orElse" in :
    oops.orElse(error(":(")) must_== error(":(")
    oops.orElse(Good(":)")) must_== Good(":)")
    oops.orElse(Empty) must_== Empty

  "get ignored in getOrElse" in :
    oops.getOrElse(":)") must_== ":)"

  "blend properly via <*>" in :
    oops <*> Good(2) must_== oops
    oops <*> error(":(") mustBeBad("oops", ":(")
    oops <*> Empty must_== oops

  "ignore call function in foreach" in :
    var wasThere = false
    val bad: Result[String] = oops
    bad foreach { s => wasThere = true }
    wasThere aka "visited what you were not supposed to visit" must beFalse

  "filter as designed" in :
    oops filter((s: String) => s.startsWith("he"), "oi vei") must_== oops
    oops filter((s: String) => s.startsWith("lo"), "oi vei") must_== oops

  "Merge errorDetails" in :
    val detailsOpt: Option[String] = 
      Result.bad(new ResultException("beer too expensive") :: new ResultException("are we there?") :: Nil).errorDetails
    detailsOpt.isEmpty must beFalse
    val desc = detailsOpt.get
    val expectedDesc = "beer too expensive; are we there?"
    desc must_== expectedDesc
    detailsOpt must beSome(expectedDesc)

  "combine with goods in sugared loop" in :
    val actual1 = for (x <- error[String]("x yourself");
                       y <- Good("y")) yield x + y
    actual1 mustBeBad "x yourself"

    val actual2 = for (x <- Good("x");
                       y <- error[String]("y yourself")) yield x + y
    actual2 mustBeBad "y yourself"

  "combine with bads in sugared loop" in :
    val actual = for (x <- error[String]("x yourself");
                      y <- error[String]("y yourself")) yield x + y
    actual mustBeBad "x yourself"

  "work applicatively" in :
    val blended: Result[(Int, Int)] = error[Int]("x yourself") <*> error[Int]("y yourself")

    def sum(x: Int, y: Int): Int = x + y

    val actual: Result[Int] = blended map sum.tupled
    actual mustBeBad("x yourself", "y yourself")

  "Empty" should:
    "be bad" in :
      Empty.isGood must beFalse

    "list errors" in :
      Empty.listErrors.isEmpty must beTrue

    "ignore on error" in :
      var beenThere = false
      Empty.onError((es: Any) => {
        beenThere = true
      })
      beenThere must beFalse

    "Map as designed" in :
      var wasThere = false
      Empty map (x => {
        wasThere = true; null == x
      }) must_== Empty
      wasThere aka "visited what you were not supposed to visit" must beFalse

    "flatMap as designed" in :
      var wasThere = false
      Empty flatMap (s => {
        wasThere = true; Empty
      }) must_== Empty
      wasThere aka "visited what you were not supposed to visit" must beFalse

    "collect nothing" in :
      var wasThere = false
      val sut: Result[String] = Empty
      sut collect( {
        case "hello" => wasThere = true; 1
      }, _ => "whatever") must_== Empty
      wasThere aka "visited what you were not supposed to visit" must beFalse

    "convert to None" in :
      Empty.asOption must beNone

    "get ignored in orElse" in :
      Empty.orElse(Result.error(":(")) must_== Result.error(":(")
      Empty.orElse(Good(":)")) must_== Good(":)")
      Empty.orElse(Empty) must_== Empty

    "get ignored in getOrElse" in :
      Empty.getOrElse(":)") must_== ":)"

    "blend properly via <*>" in :
      Empty <*> Good(2) must_== Empty
      Empty <*> Result.error(":(") must_== Empty
      Empty <*> Empty must_== Empty

    "ignore call function in foreach" in :
      var wasThere = false
      Empty foreach { x => wasThere = true }
      wasThere aka "visited what you were not supposed to visit" must beFalse

    "Filter as designed" in :
      Empty filter((x: Any) => x != null, "oi vei") must_== Empty
      Empty filter((x: Any) => x == null, "oi vei") must_== Empty

    "Show nothing in errorDetails" in :
      Empty.errorDetails must beNone

    "combine with goods in sugared loop" in :
      val nr: Result[String] = Empty
      val actual1 = for (x <- nr;
                         y <- Good("y")) yield x + y
      actual1 must_== Empty

      val actual2 = for (x <- Good("x");
                         y <- nr) yield x + y
      actual2 must_== Empty

    "combine with bads in sugared loop" in :
      val nr: Result[String] = Empty
      val actual1 = for (x <- nr;
                         y <- Result.error[String]("y yourself")) yield x + y
      actual1 must_== nr
      val actual2 = for (x <- Result.error[String]("x yourself");
                         y <- nr) yield x + y

      actual2 mustBeBad "x yourself"

    "combine with Empty in sugared loop" in :
      val nr: Result[String] = Empty
      val actual1 = for (x <- nr;
                         y <- nr) yield x + y
      actual1 must_== nr
