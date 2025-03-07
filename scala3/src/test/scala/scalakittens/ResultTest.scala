package scalakittens

import testing.TestBase
import Result._


/**
 * Prototype for all tests
 */
class ResultTest extends TestBase:

  implicit class checker(r: Result[?]):
    infix def mustBeBad(msgs: String*) =
      (r.isBad aka r.toString) must_== true
      r.errorDetails must_== Some(msgs mkString "; ")

    infix def mustBeGood[T](value: T) =
      (r.isGood aka r.toString) must_== true
      r must_== Good(value)

  "Good" should :
    "be good" in :
      Good(42).isGood === true

    "be not bad" in :
      Good(43).isBad === false

    "list no errors" in :
      Good(scala.math.Pi).listErrors.isEmpty === true

    "do nothing on error" in :
      var beenThere = false
      Good("I'm good").onError((errors: Any) => { beenThere = true })

      beenThere === false

    "Map as designed" in :
      Good("hello") map ((s: String) => s.toUpperCase) mustBeGood("HELLO")

    "FlatMap as designed" in :
      Good("hello") flatMap ((s: String) => Good(s.toUpperCase)) mustBeGood("HELLO")
      Good("hello") flatMap ((s: String) => Result.error("alas...")) mustBeBad "alas..."
      Good("hello") flatMap ((s: String) => Empty) must_== Empty

    "collect as designed" in :
      val err = ":("
      Good("hello") collect( {case "hello" => 1}, _ => ":(") must_== Good(1)
      Good("hello") collect( {case "Hello" => 1}, _ => ":(") mustBeBad(err)

    "convert to Some" in :
      Good(":)").asOption === Some(":)")

    "stay put in orElse" in :
      Good(":)").orElse(Empty) mustBeGood(":)")

    "stay put in getOrElse" in :
      Good(":)").getOrElse(":(") === ":)"

    "blend properly via <*>" in :
      Good(1) <*> Good(2) mustBeGood((1, 2))
      Good(1) <*> Result.error(":(") mustBeBad(":(")
      Good(1) <*> Empty === Empty

    "call function in foreach" in :
      var v: String = ":("
      Good(":)") foreach (v = _)
      v === ":)"

/*"filter as designed" in {
  Good("hello") filter((s: String) => s.startsWith("he"), "oi vei") === Good("hello")
  Good("huilo") filter((s: String) => s.startsWith("he"), "oi vei") === Result.error("oi vei")
  Good("huilo") filter ((s: String) => s.startsWith("he")) === Empty
}
"Show nothing in errorDetails" in {
  Good("sh").errorDetails === None
}

"combine with other goods in sugared loop" in {
  val actual = for (x ← Good("x");
                    y ← Good("y")) yield x + y
  actual === Good("xy")
}

"have equality" in {
  Good("x") === Good("x")
  Good("x") must_!= Good("y")
  Good("x") must_!= null
}
}

"Bad" should {
  "be bad" in {
    Result.error("what was the question?").isGood === false
  }
  "list errors" in {
    val errors = new Exception("Whose life is it?") ::
      new Exception("Hey Euler!") :: Nil
    bad(errors).listErrors === errors
  }
  "behave on error" in {
    val errors = new Exception("Say hi") ::
      new Exception("Say bye") :: Nil
    var beenThere = false
    var ed: Errors = Nil
    bad(errors).onError((es: Errors) => {
      beenThere = true;
      ed = es
    })
    beenThere aka "visited what you were not supposed to visit" === true
    ed === errors
  }
*/
