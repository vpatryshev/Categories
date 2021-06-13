import scala.concurrent._
val ec = scala.concurrent.ExecutionContext.Implicits.global

(0 to 100) foreach { n =>
  println(s"@$n")
  Future {
    println("starting Future: " + n)
  //  blocking {
      Thread.sleep(30)
//    }
    println("ending Future: " + n)
  }(ec)
}

sealed trait EitherTree[T] {
  def map[U](f: T => U): EitherTree[U]
  def flatMap[U](f: T => EitherTree[U]): EitherTree[U]
}

case class Right[T](v: T) extends EitherTree[T]  {
  def map[U](f: T => U): Right[U] = Right(f(v))

  override def flatMap[U](f: T => EitherTree[U]): EitherTree[U] = f(v)
}

sealed trait Left[T] extends EitherTree[T] {
  def map[U](f: T => U): Left[U]
  def flatMap[U](f: T => EitherTree[U]): Left[U]
}

case class LeftLeaf[T](cause: Exception) extends Left[T] {
  def map[U](f: T => U): LeftLeaf[U] = LeftLeaf[U](cause)

  override def flatMap[U](f: T => EitherTree[U]): LeftLeaf[U] = LeftLeaf[U](cause)
}

case class LeftTree[T](first: Left[T], second: Left[T]) extends Left[T] {
  def map[U](f: T => U): LeftTree[U] = LeftTree[U](first.map(f), second.map(f))

  override def flatMap[U](f: T => EitherTree[U]): LeftTree[U] =
    LeftTree[U](first.flatMap(f), second.flatMap(f))
}