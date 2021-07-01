package math

import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification
import scalakittens._
import testing.TestBase

class Test extends TestBase {
  val NumberRegex = "(\\d+)".r
  val PairRegex = "(\\d+)\\.(\\d+)".r
  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  extension[T] (n: Int)
    def asArrow = n.asInstanceOf[T]

  extension[T] (s: String)
    def asArrow = s.asInstanceOf[T]

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
