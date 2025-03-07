package scalakittens

import scalakittens.Result._

import scala.concurrent.Future
import scala.language.{implicitConversions, postfixOps}
import scala.util.{Failure, Success, Try}
import scala.annotation.targetName

sealed trait Result[+T] extends Container[T] with Goodness:

  def clock: TimeReader = DateAndTime
  def listErrors: Errors
  def onError[X >: Errors, Y](op: X => Y):Result[T]
  infix def map[U](f: T => U): Result[U]
  infix def flatMap[U](f: T => Result[U]): Result[U]
  infix def returning[U](u: => U): Result[U] = map[U](_ => u)
  infix def andThen[U](next: => Result[U]): Result[U] = flatMap(_ => next)
  @targetName("and_then")
  infix def >>=[U](next: => Result[U]): Result[U] = andThen[U](next)
  inline def flatten[U](implicit asResult: T => Result[U]): Result[U] = flatMap(asResult)
  def collect[U](pf: PartialFunction[T, U], onError: T => String): Result[U]
  def asOption: Option[T]
  infix def orElse[T1 >: T] (next: => Result[T1]): Result[T1]
  infix def getOrElse[T1 >: T](alt: => T1): T1
  @targetName("and_also")
  infix def <*>[U](other: Result[U]): Result[(T,U)]
  inline infix def andAlso[U](other: Result[U]): Result[(T,U)] = <*>(other)
  protected def foreach_(f: T => Unit): Unit
  infix def foreach(f: T => Unit): Result[T] = {foreach_(f); this}
  infix def filter(p: T => Boolean): Result[T]
  infix def filter(p: T => Boolean, onError: T=> String): Result[T]
  infix def filter(p: T => Boolean, errorMessage: => String): Result[T] = filter(p, x => errorMessage)
  infix def filter(flag:Boolean, errorMessage: => String):Result[T] = filter ((x:T) => flag, errorMessage)
  infix def withFilter(p: T => Boolean): Result[T] = filter(p)
  infix def filterNot(p: T => Boolean): Result[T] = filter((t:T) => !p(t))
  infix def filterNot(p: T => Boolean, onError: T=> String): Result[T] =  filter((t:T) => !p(t), onError)
  infix def filterNot(p: T => Boolean, errorMessage: => String): Result[T] = filterNot(p, x => errorMessage)
  infix def filterNot(flag:Boolean, errorMessage: => String): Result[T] = filterNot ((x:T) => flag, errorMessage)
  infix def exists(p: T => Boolean): Boolean
  infix def forall(p: T => Boolean): Boolean
  def errorDetails: Option[String]
  def fold[U](good: T => U, bad: Errors => U): U
  infix def orCommentTheError(message: => Any): Result[T]
  infix def tap(op: T => Unit): Result[T]
  inline def optionally[U](f: T => U => U): U => U = this.map(f).getOrElse(identity[U])
  def iHope: T

case class Good[T](protected val value: T) extends Result[T] with SomethingInside[T] with PositiveAttitude:
  val listErrors: Errors = Nil
  def onError[X >: Errors, Y](op: X => Y): Result[T] = this
  def asOption: Option[T] = Some(value)
  infix def map[U](f: T=>U): Result[U] = Result.forValue(f(value))
  infix def flatMap[U](f: T => Result[U]): Result[U] = f(value)
  infix def collect[U](pf: PartialFunction[T, U], onError: T => String): Result[U] = pf.lift(value) match
    case Some(t) => Good(t)
    case None    => Result.error(onError(value))
  def fold[U](good: T => U, bad: Errors => U): U = good(value)

  override def toString: String = value match
    case () => "Good."
    case x => s"Good($x)"

  infix def orElse[T1 >: T] (next: => Result[T1]): Result[T1] = this
  infix def getOrElse[T1 >: T](alt: => T1): T1 = value
  @targetName("and_also")
  infix def <*>[U](other: Result[U]): Result[(T, U)] = other.flatMap(u => Good((value, u)))
  protected def foreach_(f: T => Unit): Unit = f(value)
  infix def filter(p: T => Boolean): Result[T] =
    Result.forValue(if p(value) then this else empty[T]).flatten
  infix def filter(p: T => Boolean, onError: T => String): Result[T] =
    Result.forValue(if p(value) then this else Result.error(onError(value))).flatten
  infix def exists(p: T => Boolean): Boolean = p(value)
  infix def forall(p: T => Boolean): Boolean = p(value)
    
  def errorDetails: Option[String] = None
  infix def orCommentTheError(message: =>Any): Good[T] = this
  infix def tap(op: T => Unit): Result[T] = {op(value); this}
  infix def contains[T1 >: T](x: T1): Boolean = value == x
  def iHope: T = value

trait NoGood[T] extends NothingInside[T] with NegativeAttitude:
  self: Result[T] =>
  def filter(p: T => Boolean):Result[T] = self
  def filter(p: T => Boolean, onError: T => String):Result[T] = self
  def exists(p: T => Boolean): Boolean = false
  def forall(p: T => Boolean): Boolean = true
  protected def foreach_(f: T => Unit): Unit = ()
  def getOrElse[T1 >: T](alt: => T1): T1 = alt
  def orElse[T1 >: T] (next: => Result[T1]): Result[T1] = next
  def asOption: Option[T] = None
  def fold[U](good: T => U, bad: Errors => U): U = bad(listErrors)
  def errors: String = listErrors mkString "; "
  def tap(op: T => Unit): Result[T] = this

  private val timestamp: Long = clock.currentTime
  inline def tag: String = s"${ts2b32(timestamp)}"
  inline override def toString = s"Error: ~$tag($errors)"

  private def base32map = "abdeghjklmnopqrstvxyz.".zipWithIndex.map{case (c,i) => ('a'+i).toChar -> c}.toMap withDefault identity

  /**
    * Transforms a timestamp to a string
    * First, takes timestamp mod year length, in seconds
    * Then transforms it to base32, skipping probably offending letters (bad words all contain c,f,u letters)
    * e.g. 08/28/2015 @ 12:35am (UTC) -> 1440722109 -> "molly"
    *
    * @param n time (millis)
    * @return a string
    */
  private def ts2b32(n: Long): String =
    val s0 = java.lang.Long.toString(((n+500)/1000) % 31557600, 32)
    val s1 = s0 map base32map
    "0"*(5-s1.length) + s1

  def iHope: T = throw new InstantiationException(errors)

class Bad[T](val listErrors: Errors) extends Result[T] with NoGood[T]:

  import Result._

  def onError[X >: Errors, Y](op: X => Y):Result[T] = {op(listErrors); this}
  infix def map[U](f: T=>U): Result[U] = bad[U](listErrors)
  infix def flatMap[U](f: T => Result[U]): Result[U] = bad(listErrors)
  infix def collect[U](pf: PartialFunction[T, U], onError: T => String): Result[U] = bad(listErrors)
  @targetName("and_also")
  infix def <*>[U](other: Result[U]): Result[(T, U)] =
    bad(listErrors ++ (other.listErrors dropWhile (lastError contains)))
  inline def lastError: Option[Throwable] = listErrors.lastOption

  private def listMessages(t: Throwable):String =
    Option(t.getCause).fold(Result.messageOf(t))((cause:Throwable) => t.getMessage + ", " + listMessages(cause))

  inline def errorDetails: Option[String] = Some(listErrors map listMessages mkString "; ")

  private def stackTrace2(t:Throwable): String =
    val stack = t.getStackTrace
    val tail = stack.dropWhile(el => el.getClassName.contains("Bad") || el.getMethodName == "stackTrace")
    tail.take(35) mkString "\n"

  private def details(t:Throwable): String =
    val st1 = stackTrace2(t)
    Option(t.getCause).fold(st1)((cause:Throwable) => st1 + "\n\n" + stackTrace2(cause))

    val stack = t.getStackTrace
    val tail = stack.dropWhile(el => el.getClassName.contains("Bad") || el.getMethodName == "stackTrace")
    Result.messageOf(t) + "\n" + (tail.take(35) mkString "\n")

  def stackTrace:String = listErrors map details mkString "\n\n"

  infix def orCommentTheError(message: =>Any): Bad[T] =
    bad[T](List(recordEvent(message)) ++ listErrors)

  override def equals(other: Any): Boolean = other match
    case that: Bad[?] => that.listErrors == listErrors
    case basura       => false

  override def hashCode: Int = listErrors.hashCode + tag.hashCode*71

def empty[T]: Result[T] = Empty.asInstanceOf[Result[T]] // see the same in Scala Set

case object Empty extends NoResult with NoGood[Nothing]:
  val listErrors: Errors = Nil
  infix def onError[X >: Errors, Y](op: X => Y):NoResult = this
  infix def map[U](f: Nothing => U): Result[U] = empty[U]
  infix def flatMap[U](f: Nothing => Result[U]): Result[U] = empty[U]
  infix def collect[U](pf: PartialFunction[Nothing, U], onError: Nothing => String): Result[U] = empty[U]
  @targetName("and_also")
  infix def <*>[U](other: Result[U]): Result[(Nothing, U)] = empty[(Nothing, U)]
  inline def errorDetails: Option[String] = None
  infix def orCommentTheError(message: =>Any): Result[Nothing] = Result.error(message)

private object NoException extends Exception:
  def root(x: Throwable): Throwable = x match
    case NoException => null
    case other => other

class ResultException(message:String, source: Throwable = NoException)
  extends Exception(message, NoException.root(source)):
  private val check: String = message
  override def toString: String = message
  override def equals(other: Any): Boolean = other match
    case that: ResultException => that.toString == toString
    case basura                => false

// TODO(vlad): stringify better
case class BadResultException(errors: Errors) extends Exception:
  override def getMessage: String = "errors: " + (errors mkString "; ")

object Result:

  class UnacceptableResult:
    throw new UnsupportedOperationException("This is not a good result, and we can never produce it")

  type Errors = Iterable[Throwable]
  type NoResult = Result[Nothing]
  type Outcome = Result[Unit]

  protected[scalakittens] def forTesting[T](es: Errors, testClock: TimeReader): Bad[T] = 
    new Bad[T](es):
      override def clock: TimeReader = testClock

  inline def messageOf(t: Throwable): String = Result.forValue(t.getMessage).getOrElse(t.toString)

  def recordEvent(message:Any):Throwable =
    new ResultException(""+message)

  inline def check[T](results: IterableOnce[Result[T]]): Outcome =
    traverse(results)
  
  implicit inline def asOutcome(r:Result[?]): Outcome = r andThen OK
  
  def attempt[T](
    eval: => Result[T],
    onException: Exception => Result[T] = (e: Exception) => exception(e)): Result[T] =
    try { eval } catch { case e: Exception => onException(e) }

  def attempt[T](eval: =>Result[T], errMsg: => String): Result[T] =
    attempt(eval, exception(_, errMsg) )

  private def legalize[T](optT: => Option[T]): Result[T] =
    optT map Good.apply getOrElse empty[T]
      
  implicit def apply[T](optT: Option[T]):                         Result[T] = legalize(optT)
  implicit def apply[T](optT: Option[T], onError: => String):     Result[T] = legalize(optT) orCommentTheError onError

  def apply[T](tryT: Try[T]): Result[T] = tryT match
    case Success(t) => Good(t)
    case Failure(x) => exception(x)

  def apply[T](maybeT: Either[String, T]): Result[T] = maybeT match
    case Left(bad) => error[T](bad)
    case Right(good) => Good(good)

  def app[X,Y](fOpt:Result[X=>Y])(xOpt:Result[X]):Result[Y] = (fOpt andAlso xOpt).map{case (f,x) => f(x)}

  extension[X,Y](fOpt: Result[X=>Y])
    def apply(xOpt:Result[X]): Result[Y] = app(fOpt)(xOpt)
    def apply(x:X): Result[Y] = app(fOpt)(Good(x))


  // TODO(vlad): a) don't accept Results; b) don't accept booleans
  // see:
  //  inline def error(inline msg: String): Nothing
  //  inline def fail(p1: ⇒ Any) = error(code"failed on: $p1")
  inline def forValue[T](value: =>T):              Result[T] = attempt(apply(Option(value)))
  inline def forValue[T](value: =>T, onError: => String):   Result[T] = attempt(apply(Option(value), onError), onError)

  inline def bad[T](errorList: Errors): Bad[T] = new Bad[T](errorList)

  inline def error[T](message: => Any): Bad[T] =
    bad[T](recordEvent(message)::Nil)

  inline def exception[T](x: Throwable): Bad[T] = bad(x::Nil)
  inline def exception[T](x: Throwable, comment: Any): Bad[T] = exception[T](x) orCommentTheError comment

  def partition[T](results: IterableOnce[Result[T]]): (List[T], List[Errors]) =
    val (goodOnes, badOnes) = 
      results.iterator.foldLeft((List.empty[T], List.empty[Errors]))(
        (collected, current) =>
      current match
        case Good(good) => (good::collected._1, collected._2)
        case noGood     => (collected._1, noGood.listErrors.toList::collected._2)
    )
    
    (goodOnes, badOnes)

  private def badOrEmpty[T](errors: Errors): Result[T] = if errors.isEmpty then empty[T] else bad(errors.toSet)

  private def bad[T](results: Result[?]*): Result[T] = bad(results flatMap (_.listErrors))

  extension [T] (source: LazyList[Result[T]])
    def map[U](f: T => U): LazyList[Result[U]] = source map (_ map f)
    def |>[U](op: T => Result[U]): LazyList[Result[U]] = source map (t => t flatMap op)
    def filter(p: T => Result[?]): LazyList[Result[T]] = |> (x => p(x) returning x)

  infix def traverse[T](results: IterableOnce[Result[T]]): Result[Iterable[T]] =
    val (goodOnes, badOnes) = partition(results)

    if badOnes.nonEmpty then badOrEmpty(badOnes.flatten)
    else Good(goodOnes.reverse)

  def fold(results:Iterable[Outcome]): Outcome =
    results.foldLeft(OK: Outcome)((x,y) => x andAlso y)

  def zip[X1,X2](r1: Result[X1], r2: Result[X2]): Result[(X1, X2)] = r1 andAlso r2

  def zip[X1,X2,X3](r1: Result[X1], r2: Result[X2], r3: Result[X3]): Result[(X1,X2,X3)] =
    (r1,r2,r3) match
      case (Good(x1),Good(x2),Good(x3)) => Good((x1,x2,x3))
      case (b1,b2,b3)                   => bad(b1, b2, b3)

  def zip[X1,X2,X3,X4](r1: Result[X1], r2: Result[X2], r3: Result[X3], r4: Result[X4]): Result[(X1,X2,X3, X4)] =
    (r1,r2,r3,r4) match
      case (Good(x1),Good(x2),Good(x3),Good(x4)) => Good((x1,x2,x3,x4))
      case (b1,b2,b3,b4)                         => bad(b1, b2, b3, b4)

  def zip[X1,X2,X3,X4,X5](r1: Result[X1], r2: Result[X2], r3: Result[X3], r4: Result[X4], r5: Result[X5]): Result[(X1,X2,X3, X4,X5)] =
    (r1,r2,r3,r4,r5) match
      case (Good(x1),Good(x2),Good(x3),Good(x4),Good(x5)) => Good((x1,x2,x3,x4,x5))
      case (b1,b2,b3,b4,b5)                               => bad(b1, b2, b3, b4, b5)

  def zip[X1,X2,X3,X4,X5,X6](r1: Result[X1], r2: Result[X2], r3: Result[X3], r4: Result[X4], r5: Result[X5], r6: Result[X6]): Result[(X1,X2,X3, X4,X5,X6)] =
    (r1,r2,r3,r4,r5,r6) match
      case (Good(x1),Good(x2),Good(x3),Good(x4),Good(x5),Good(x6)) => Good((x1,x2,x3,x4,x5,x6))
      case (b1,b2,b3,b4,b5,b6)                                     => bad(b1, b2, b3, b4, b5, b6)

  def zip[X1,X2,X3,X4,X5,X6,X7](r1: Result[X1], r2: Result[X2], r3: Result[X3], r4: Result[X4], r5: Result[X5], r6: Result[X6], r7: Result[X7]): Result[(X1,X2,X3, X4,X5,X6,X7)] =
    (r1,r2,r3,r4,r5,r6,r7) match
      case (Good(x1),Good(x2),Good(x3),Good(x4),Good(x5),Good(x6),Good(x7)) => Good((x1,x2,x3,x4,x5,x6,x7))
      case (b1,b2,b3,b4,b5,b6,b7)                                     => bad(b1, b2, b3, b4, b5, b6, b7)

  def zip[X1,X2,X3,X4,X5,X6,X7,X8](r1: Result[X1], r2: Result[X2], r3: Result[X3], r4: Result[X4], r5: Result[X5], r6: Result[X6], r7: Result[X7], r8: Result[X8]): Result[(X1,X2,X3, X4,X5,X6,X7,X8)] =
    (r1,r2,r3,r4,r5,r6,r7,r8) match
      case (Good(x1),Good(x2),Good(x3),Good(x4),Good(x5),Good(x6),Good(x7),Good(x8)) => Good((x1,x2,x3,x4,x5,x6,x7,x8))
      case (b1,b2,b3,b4,b5,b6,b7,b8)                                     => bad(b1, b2, b3, b4, b5, b6, b7, b8)

  def zip[X1,X2,X3,X4,X5,X6,X7,X8,X9](r1: Result[X1], r2: Result[X2], r3: Result[X3], r4: Result[X4], r5: Result[X5], r6: Result[X6], r7: Result[X7], r8: Result[X8], r9: Result[X9]): Result[(X1,X2,X3, X4,X5,X6,X7,X8,X9)] =
    (r1,r2,r3,r4,r5,r6,r7,r8,r9) match
      case (Good(x1),Good(x2),Good(x3),Good(x4),Good(x5),Good(x6),Good(x7),Good(x8),Good(x9)) => Good((x1,x2,x3,x4,x5,x6,x7,x8,x9))
      case (b1,b2,b3,b4,b5,b6,b7,b8,b9)                                                       => bad(b1, b2, b3, b4, b5, b6, b7, b8, b9)

  def zip[X1,X2,X3,X4,X5,X6,X7,X8,X9, X10](r1: Result[X1], r2: Result[X2], r3: Result[X3], r4: Result[X4], r5: Result[X5], r6: Result[X6], r7: Result[X7], r8: Result[X8], r9: Result[X9], r10: Result[X10]): Result[(X1,X2,X3, X4,X5,X6,X7,X8,X9,X10)] =
    (r1,r2,r3,r4,r5,r6,r7,r8,r9,r10) match
      case (Good(x1),Good(x2),Good(x3),Good(x4),Good(x5),Good(x6),Good(x7),Good(x8),Good(x9),Good(x10)) => Good((x1,x2,x3,x4,x5,x6,x7,x8,x9, x10))
      case (b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)                                                       => bad(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10)

  object OK extends Good(⊤) with Outcome:
    override def toString = "OK"
  
  def OKif(cond: => Boolean): Outcome = OK.filter(_ => cond)
  def OKif(cond: => Boolean, onError: => String): Outcome = OK.filter(_ => cond, _ => onError)
  def OKifNot(cond: => Boolean): Outcome = OK.filterNot(_ => cond)
  def Oops[T](complaint: Any): Bad[T] = error(complaint)
  def NotImplemented[T]: Bad[T] = Oops[T]("not implemented")
  val ⊤ : Unit = ()

  extension (x: Any)
    def typed[T]: Result[T] = Result.forValue(x.asInstanceOf[T])

