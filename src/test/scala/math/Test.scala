package math

import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification
import scalakittens._

class Test extends Specification {
  type SUT
  
  def check[T](g: Result[T], op: T => Unit): MatchResult[Any] = {
    g match {
      case Good(sut) => op(sut)
      case bad => failure(bad.toString)
    }
    ok
  }

  def expect(op: SUT => Unit)(sutOpt: Result[SUT]): MatchResult[Any] = check[SUT](sutOpt, op)
  
  def checkError[T](op: String => Boolean, sutOpt: Result[T]): MatchResult[_] = {
    sutOpt match {
      case Good(bad) => failure(s"Expected failure, go $bad")
      case nogood => (nogood.errorDetails exists op) === true
    }
    ok
  }
  
}
