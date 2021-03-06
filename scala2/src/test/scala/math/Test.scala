package math

import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification
import scalakittens._

class Test extends Specification {
  val NumberRegex = "(\\d+)".r
  val PairRegex = "(\\d+)\\.(\\d+)".r
  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  type SUT

  def checkOpt[T](g: Result[T], op: T => Unit): MatchResult[Any] = {
    g match {
      case Good(sut) => op(sut)
      case bad => failure(bad.toString)
    }
    ok
  }

  def check[T](g: T, op: T => Unit): MatchResult[Any] = {
    op(g)
    ok
  }

  def expect(op: SUT => Unit)(sutOpt: Result[SUT]): MatchResult[Any] = checkOpt[SUT](sutOpt, op)
  
  def checkError[T](op: String => Boolean, sutOpt: Result[T]): MatchResult[_] = {
    sutOpt match {
      case Good(bad) => failure(s"Expected failure, got a $bad")
      case nogood =>
        val details = nogood.errorDetails
        (details exists op) aka details.getOrElse("???") must beTrue
    }
    ok
  }
}
