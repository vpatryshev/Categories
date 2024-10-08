package math.sets

import math.cat.SetMorphism
import math.sets.SetOps.cantorIterator
import org.specs2.execute.Failure
import org.specs2.mutable._
import scalakittens.{Empty, Result}

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.Duration

/**
 * Test suite for Sets object
 */
class SetOpsTest extends Specification {

"SetOps" >> {
    "Cantor iterator should behave - 1" >> {
      val sut = cantorIterator(1::2::Nil, 'X'::Nil)
      sut.hasNext must beTrue
      sut.next() === (1, 'X')
      sut.hasNext must beTrue
      sut.next() === (2, 'X')
      sut.hasNext must beFalse
      ok
    }
  "Cantor iterator should behave - 2" >> {
    val sut = cantorIterator(1::Nil, 'a'::'b'::Nil)
    sut.hasNext must beTrue
    sut.next() === (1, 'a')
    sut.hasNext must beTrue
    sut.next() === (1, 'b')
    sut.hasNext must beFalse
    ok
  }
  "Cantor iterator should behave - 3" >> {
    val sut = cantorIterator(1::2::Nil, 'a'::'b'::Nil)
    sut.hasNext must beTrue
    sut.next() === (1, 'a')
    sut.hasNext must beTrue
    sut.next() === (1, 'b')
    sut.hasNext must beTrue
    sut.next() === (2, 'a')
    sut.hasNext must beTrue
    sut.next() === (2, 'b')
    sut.hasNext must beFalse
    ok
  }
  }
  
  def spendNotMoreThan[T](time: Duration, extraTimePercent:Int = 1): Object {
    def on(op: (() => Boolean) => Result[T]): Result[T]
  } = new {
    private val done = new AtomicBoolean(false)
    val signal: () => Boolean = done.get

    def on(op: (() => Boolean) => Result[T]): Result[T] = {
      import java.util.concurrent.locks.LockSupport._
      var res:Result[T] = Empty
      val millis = time.toMillis
      val finalDeadline = System.currentTimeMillis + millis * (100 + extraTimePercent) / 100 + 1
      val worker = new Thread {
        override def run(): Unit = {
          try {
            res = op(signal)
            done.set(true)
          } catch {case ie: InterruptedException => }
        }
      }
      worker.setPriority(1)
      worker.start()
      try {
        worker.join(time.toMillis)
      } finally {
        if (worker.isAlive) {
          worker.interrupt()
          parkUntil(finalDeadline)
        }
        if (worker.isAlive) done.set(true)
      }
      res.filter(done.get, s"Timeout after $time")
    }
  }
}


