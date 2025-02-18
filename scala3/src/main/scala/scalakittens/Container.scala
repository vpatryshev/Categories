package scalakittens

import scala.annotation.{nowarn, targetName}
import scala.collection.immutable.Set

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
  infix def contains[T1 >: T](x: T1): Boolean = false

trait SomethingInside[T] extends Container[T]:
  inline def isEmpty = false

object  Containers:
  extension [T, C <: Container[T]](x: T)
    @targetName("in")
    infix inline def ∈(c: C): Boolean = c contains x
    @targetName("notIn")
    infix inline def ∉(c: C): Boolean = !(c contains x)

  extension [T](x: T)
    @targetName("in")
    infix inline def ∈(c: Set[T]): Boolean = c contains x
    @targetName("notIn")
    infix inline def ∉(c: Set[T]): Boolean = !(c contains x)

    @targetName("in")
    infix inline def ∈(c: Seq[T]): Boolean = c contains x
    @targetName("notIn")
    infix inline def ∉(c: Seq[T]): Boolean = !(c contains x)

    @targetName("in")
    infix inline def ∈(c: Container[T]): Boolean = c contains x
    @targetName("notIn")
    infix inline def ∉(c: Container[T]): Boolean = !(c contains x)

    @targetName("in")
    infix inline def ∈(c: Option[T]): Boolean = c contains x
    @targetName("notIn")
    infix inline def ∉(c: Option[T]): Boolean = !(c contains x)
