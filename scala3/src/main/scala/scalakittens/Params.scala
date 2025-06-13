package scalakittens

import scala.collection.mutable

object Params:
  def config(name: String, alt: String): String =
    val p1 = Option(System.getenv(name))
    val p2 = Option(System.getProperty(name))
    val p: String = p1 orElse p2 getOrElse alt
    p

  def flag(name: String, alt: Boolean = false): Boolean =
    config(name, alt.toString).toBooleanOption getOrElse alt

  lazy val fullCheck: Boolean = flag("FullCheck")
  lazy val verbose:   Boolean = flag("Verbose")
  lazy val debug:     Boolean = flag("Debug")
  lazy val profile:   Boolean = flag("Profile")
  lazy val trace:     Boolean = flag("Trace")

  inline def debug(s: String): Unit =
    if debug then println(s"DEBUG:$s")

  private val allKeys = mutable.Map[String, Array[StackTraceElement]]()

  def deepDebug(s: String): Unit =
    if debug then
      val stack: Array[StackTraceElement] = callStack
      val calledBy: StackTraceElement = stack(6)
      val msg = s"$s from ${calledBy.getFileName}:${calledBy.getLineNumber}"
      debug(msg)
      if allKeys contains msg then
        val previous = allKeys.get(msg)
        println("WTF! Duplicate msg")
      allKeys += (msg -> stack)

  private def callStack = Thread.currentThread.getStackTrace

  def limitStack(max: Int): Unit =
    val n = callStack.length
    if n > max then
      throw new StackOverflowError(s"Stack too big: $n > $max")

  inline def verbose(s: String): Unit =
    if verbose || debug then println(s"VERBOSE:$s")
