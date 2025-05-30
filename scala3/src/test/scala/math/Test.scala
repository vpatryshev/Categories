package math

import org.specs2.mutable.Specification
import org.specs2.execute.Result as TestResult
import scalakittens.Result.{OKif, Outcome}
import scalakittens.*
import testing.TestBase

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.Duration

class Test extends TestBase:
  val NumberRegex = "(\\d+)".r
  val PairRegex = "(\\d+)\\.(\\d+)".r

  extension (sc: StringContext)
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x")*)

  type SUT

  def checkOption[T](g: Result[T], op: T => Unit): TestResult =
    g match
      case Good(sut) => op(sut)
      case bad => failure(bad.toString)

    ok

  def check[T](g: T, op: T => Unit): TestResult =
    op(g)
    ok

  def expect(op: SUT => Unit)(sutOpt: Result[SUT]): TestResult = checkOption[SUT](sutOpt, op)

  def expectOk(id: String, r: Result[?]): TestResult =
    r.isGood aka s"$id: $r" must beTrue
  
  def expectError[T](op: String => Boolean, r: Result[T]): TestResult =
    r match
      case Good(bad) => failure(s"Expected failure, got a $bad")
      case nogood =>
        val details = nogood.errorDetails
        (details exists op) aka details.getOrElse("???") must beTrue

    ok

  def expectError[T](msg: String, r: Result[T]): TestResult =
    r match
      case Good(bad) => failure(s"Expected failure, got a $bad")
      case nogood =>
        val details = nogood.errorDetails
        (details exists (_.contains(msg))) aka details.getOrElse("???") must beTrue

    def expectErrors[T](msgs: Set[String], r: Result[T]): TestResult =
      r match
        case Good(bad) => failure(s"Expected failure, got a $bad")
        case nogood =>
          val details = nogood.errorDetails.getOrElse("").split("; ").toSet
          msgs must be_==(details)

    ok

  def expectError(tag: String, r: Result[?], messages: String*): TestResult =
    r.isBad must beTrue
    r.errorDetails match
      case Some(things) =>
        val matches: Seq[Outcome] = messages.map{ message => OKif(things.contains(message)) }
        expectOk(tag, Result.traverse(matches))

      case None => failure(s"Expected errors in $r")


  trait Sentinel[T]:
    def on(op: => Result[T]): Result[T]


  def spendNotMoreThan[T](time: Duration, extraTimePercent:Int = 1): Sentinel[T] =
    new Sentinel[T]:
      def on(op: => Result[T]): Result[T] =
        import java.util.concurrent.locks.LockSupport._
        var res:Result[T] = Empty
        val millis = time.toMillis
        val finalDeadline = System.currentTimeMillis + millis * (100 + extraTimePercent) / 100 + 1
        val done = new AtomicBoolean(false)
        val worker = new Thread:
          override def run(): Unit =
            try
              res = op
              done.set(true)
            catch
              case ie: InterruptedException => // ok

        worker.setPriority(1)
        worker.start()

        try
          worker.join(time.toMillis)
        finally
          if worker.isAlive then
            worker.interrupt()
            parkUntil(finalDeadline)

          if worker.isAlive then worker.interrupt()

        if done.get then res else Result.error(s"Timeout after $time")


