package scalakittens

/**
  * A privitive idea of a container; can be empty.
  * Created by vpatryshev on 10/18/15.
  */
trait Container[+T] extends Goodness:
  def isEmpty: Boolean
  inline def isDefined: Boolean = nonEmpty
  inline def nonEmpty: Boolean = !isEmpty

trait NothingInside[+T] extends Container[T] with NegativeAttitude:
  inline def isEmpty = true

trait SomethingInside[+T] extends Container[T] with PositiveAttitude:
  inline def isEmpty = false
