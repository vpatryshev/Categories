package math

import scalakittens.Result
import Base._
import scala.language.postfixOps

/**
  * Base tools used in this package.
  */
object Base:
  type IntMap[X] = Map[Int, X]

  /**
    * Builds an inverse map. Assumes that the map is inversible.
    *
    * @tparam A map key type
    * @tparam B map value type
    * @param m the map to invert
    * @return inverse for map m
    */
  def inverse[A, B](m: Map[A, B]): Map[B, A] =
    
    val result: Map[B, A] = m map {
      case (k, v) => v -> k
    }
        
    require(result.size == m.size, "map not invertible")
    result

  /**
    * Builds a map that returns a list element by its index.
    *
    * @tparam X  element type
    * @param list the list
    * @return the
    */
  def toMap[X](list: List[X]): IntMap[X] =
    list.zipWithIndex map { case (x, i) => i -> x } toMap

  implicit class Optimist[T](opt: Option[T]):
    def iHope: T = opt.getOrElse(throw new InstantiationException("Oops, no value"))

  /**
    * Concatenates two strings using a connector
    * @param first first value (can be anything)
    * @param conn connector string
    * @param second second value (can be anything)
    * @return
    */
  def concat(first: Any, conn: String, second: Any): String =
    def stringOf(x: Any): String =
      val s0 = String valueOf x trim;
      if (s0 contains " ") || (s0 contains conn) then s"($s0)" else s0

    val s1 = stringOf(first)
    val s2 = stringOf(second)
    val insert = if (s1.length > 1 || s2.length > 1) s" $conn " else conn
    s1 + insert + s2

  /**
   * Use this method for building objects from strings
   * @param sc context
   * @param args args
   * @return a StringBuffer
   */
  def bufferFromContext(sc: StringContext, args: Any*) =
    val strings = sc.parts.iterator
    val expressions = args.iterator
    var buf = new StringBuffer(strings.next())
    while (strings.hasNext)
      buf append expressions.next()
      buf append strings.next()
    buf

  def itsImmutable: Nothing = cannotDo("Immutable class")

  def cannotDo(message: String): Nothing =
    throw new UnsupportedOperationException(message)
    
  def notNull[T](value: => T, explanation: String): T =
    Result.forValue(value).orCommentTheError(explanation) iHope

  def plural(n: Int, w: String) = if (n == 1) s"1 $w" else s"$n ${w}s"

  def checkThat(cond: => Boolean) =
    try cond catch case x => false

  def listSorted[T](things: Iterable[T]) = things.toList.sortBy(_.toString)

  def asString(things: Iterable[_]): String =
    things.map(_.toString).toList.sorted.mkString(",")
