package math

import j.math.cat.N
import math.cat.Graph
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification
import scalakittens.{Good, Result}
import scalaz.Alpha.T

class Test extends Specification {
  type SUT
  
  def check[T](g: Result[T], op: T => Unit): MatchResult[Any] = {
    g match {
      case Good(sut) => op(sut)
      case bad => failure(bad.toString)
    }
    ok
  }

  def testWith(op: SUT => Unit)(sutOpt: Result[SUT]): MatchResult[Any] = check[SUT](sutOpt, op)
  
}
