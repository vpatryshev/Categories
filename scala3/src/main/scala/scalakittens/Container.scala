package scalakittens

/**
  * A primitive idea of a container; can be empty.
  * Created by vpatryshev on 10/18/15.
  */
trait Container[+T]:
  def isEmpty: Boolean
  inline def nonEmpty: Boolean = !isEmpty
  inline def isDefined: Boolean = nonEmpty
  infix def contains[X >: T](x: X): Boolean

trait NothingInside[T] extends Container[T]:
  inline def isEmpty = true

trait SomethingInside[T] extends Container[T]:
  inline def isEmpty = false

object ContainerOps:
  extension [T, C <: Container[T]](x: T)
    infix inline def isIn(c: C): Boolean = c contains x
    infix inline def isNotIn(c: C): Boolean = !(c contains x)