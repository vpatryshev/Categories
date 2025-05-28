package scalakittens

import testing.TestBase
import Result.*
import org.specs2.execute.Result as MatchResult
import scala.language.implicitConversions

import scala.collection.immutable.Nil as bad


/**
 * Prototype for all tests
 */
class ResultTest extends TestBase:

  implicit class checker(r: Result[?]):
    infix def mustBeBad(beef: String*): MatchResult =
      (r.isBad aka r.toString) must beTrue
      r.errorDetails must beSome(beef mkString "; ")

    infix def mustBeGood[T](value: T): MatchResult =
      (r.isGood aka r.toString) must beTrue
      r must be_==(Good(value))

  private val oops = error("oops")

  "Good" should:
    "be good" in :
      Good(42).isGood must beTrue

    "be not bad" in :
      Good(43).isBad must beFalse

    "list no errors" in :
      Good(scala.math.Pi).listErrors.isEmpty must beTrue

    "do nothing on error" in :
      var beenThere = false
      Good("I'm good").onError((errors: Any) => {
        beenThere = true
      })

      beenThere must beFalse

    "Map as designed" in :
      Good("hello") map ((s: String) => s.toUpperCase) mustBeGood "HELLO"

    "FlatMap as designed" in :
      Good("hello") flatMap ((s: String) => Good(s.toUpperCase)) mustBeGood "HELLO"
      Good("hello") flatMap ((s: String) => oops) mustBeBad "oops"
      Good("hello") flatMap ((s: String) => Empty) must be_==(Empty)

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
      Good(1) <*> Empty must be_==(Empty)

    "call function in foreach" in :
      var v: String = ":("
      Good(":)") foreach (v = _)
      v === ":)"

    "filter as designed" in :
      Good("hello") filter((s: String) => s.startsWith("he"), "oi vei") mustBeGood "hello"
      Good("huilo") filter((s: String) => s.startsWith("he"), "oi vei") mustBeBad "oi vei"
      Good("huilo") filter ((s: String) => s.startsWith("he")) must be_==(Empty)

    "Show nothing in errorDetails" in :
      Good("sh").errorDetails must be_==(None)

    "combine with other goods in sugared loop" in :
      val actual =
        for
          x <- Good("x")
          y <- Good("y")
        yield x + y

      actual mustBeGood "xy"

    "have equality" in :
      Good("x") mustBeGood "x"
      Good("x") == Good("y") must beFalse
      Good("x") == null must beFalse

  "Bad" should:
    "be bad" in :
      oops.isGood must beFalse

    "list errors" in :
      val errors: Iterable[Throwable] =
        new Exception("Whose life is it?") ::
        new Exception("Hey Euler!") :: Nil

      Result.bad[BigInt](errors).listErrors must be_==(errors)

    "behave on error" in :
      val errors = new Exception("Say hi") ::
        new Exception("Say bye") :: Nil
      var beenThere = false
      var ed: Errors = Nil
      Result.bad(errors).onError((es: Errors) => {
        beenThere = true
        ed = es
      })
      (beenThere aka "visited what you were not supposed to visit") must beTrue
      ed must be_==(errors)

  "Map as designed" in :
    var wasThere = false
    error[Int]("oops") map ((x: Int) => {
      wasThere = true
      1 + x
    }) must be_==(oops)
    wasThere aka "visited what you were not supposed to visit" must beFalse


  "flatMap as designed" in :
    var wasThere = false
    val r1 = error[String]("oops") flatMap ((s: String) => {
      wasThere = true
      Good(s.toUpperCase)
    })
    r1 must be_==(oops)
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
    }, _ => ":(") must be_==(oops)
    wasThere aka "visited what you were not supposed to visit" must beFalse
    val bad1: Result[String] = oops
    bad1 collect( {
      case "Hello" => wasThere = true; 1
    }, _ => ":(") must be_==(oops)
    wasThere aka "visited what you were not supposed to visit" must beFalse

  "convert to None" in :
    oops.asOption must beNone

  "get ignored in orElse" in :
    oops.orElse(error(":(")) must be_==(error(":("))
    oops.orElse(Good(":)")) must be_==(Good(":)"))
    oops.orElse(Empty) must be_==(Empty)

  "get ignored in getOrElse" in :
    oops.getOrElse(":)") must be_==(":)")

  "blend properly via <*>" in :
    oops <*> Good(2) must be_==(oops)
    oops <*> error(":(") mustBeBad("oops", ":(")
    oops <*> Empty must be_==(oops)

  "ignore call function in foreach" in :
    var wasThere = false
    val bad: Result[String] = oops
    bad foreach { s => wasThere = true }
    wasThere aka "visited what you were not supposed to visit" must beFalse

  "filter as designed" in :
    oops filter((s: String) => s.startsWith("he"), "oi vei") must be_==(oops)
    oops filter((s: String) => s.startsWith("lo"), "oi vei") must be_==(oops)

  "Merge errorDetails" in :
    val detailsOpt: Option[String] = 
      Result.bad(new ResultException("beer too expensive") :: new ResultException("are we there?") :: Nil).errorDetails
    detailsOpt.isEmpty must beFalse
    val desc = detailsOpt.get
    val expectedDesc = "beer too expensive; are we there?"
    desc must be_==(expectedDesc)
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
      }) must be_==(Empty)
      wasThere aka "visited what you were not supposed to visit" must beFalse

    "flatMap as designed" in :
      var wasThere = false
      Empty flatMap (s => {
        wasThere = true; Empty
      }) must be_==(Empty)
      wasThere aka "visited what you were not supposed to visit" must beFalse

    "collect nothing" in :
      var wasThere = false
      val sut: Result[String] = Empty
      sut collect( {
        case "hello" => wasThere = true; 1
      }, _ => "whatever") must be_==(Empty)
      wasThere aka "visited what you were not supposed to visit" must beFalse

    "convert to None" in :
      Empty.asOption must beNone

    "get ignored in orElse" in :
      Empty.orElse(Result.error(":(")) must be_==(Result.error(":("))
      Empty.orElse(Good(":)")) must be_==(Good(":)"))
      Empty.orElse(Empty) must be_==(Empty)

    "get ignored in getOrElse" in :
      Empty.getOrElse(":)") must be_==(":)")

    "blend properly via <*>" in :
      Empty <*> Good(2) must be_==(Empty)
      Empty <*> Result.error(":(") must be_==(Empty)
      Empty <*> Empty must be_==(Empty)

    "ignore call function in foreach" in :
      var wasThere = false
      Empty foreach { x => wasThere = true }
      wasThere aka "visited what you were not supposed to visit" must beFalse

    "Filter as designed" in :
      Empty filter((x: Any) => x != null, "oi vei") must be_==(Empty)
      Empty filter((x: Any) => x == null, "oi vei") must be_==(Empty)

    "Show nothing in errorDetails" in :
      Empty.errorDetails must beNone

    "combine with goods in sugared loop" in :
      val nr: Result[String] = Empty
      val actual1 = for (x <- nr;
                         y <- Good("y")) yield x + y
      actual1 must be_==(Empty)

      val actual2 = for (x <- Good("x");
                         y <- nr) yield x + y
      actual2 must be_==(Empty)

    "combine with bads in sugared loop" in :
      val nr: Result[String] = Empty
      val actual1 = for (x <- nr;
                         y <- Result.error[String]("y yourself")) yield x + y
      actual1 must be_==(nr)
      val actual2 = for (x <- Result.error[String]("x yourself");
                         y <- nr) yield x + y

      actual2 mustBeBad "x yourself"

    "combine with Empty in sugared loop" in :
      val nr: Result[String] = Empty
      val actual1 = for (x <- nr;
                         y <- nr) yield x + y
      actual1 must be_==(nr)
