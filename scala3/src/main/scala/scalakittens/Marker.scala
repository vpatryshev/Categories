package scalakittens

trait Marker:
  val created: Array[StackTraceElement] = Thread.currentThread.getStackTrace
