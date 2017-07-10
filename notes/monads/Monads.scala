// Notes on monad lecture
// https://www.coursera.org/learn/progfun2/lecture/98tNE/lecture-1-4-monads


/**
  * Monad M is a parametric type M[T] with 2 operations
  * - flatMap - also known as 'bind'
  * - unit
  */
trait  M[T] {
  def flatMap[U](f: T => M[U]): M[U]
}

def unit[T](x: T): M[T]


// Examples of Monads

// List
unit(x) = List(x)

// Set
unit(x) = Set(x)

// Option
unit(x) = Some(x)

// Generator
unit(x) = single(x)



/*
map can be defined for every monad as a combination of flatMap and unit
m map f == m flatMap (x => unit(f(x)))
        == m flatMap (f andThen unit)
*/

// -------------------------------------------------------------------------------
// Monad Laws

// Associativity - "inline" nested for expressions
m flatMap f flatMap g == m flatMap (x => f(x) flatMap g)
// Monoids are simpler forms of monads that don't bind anything, i.e. Integers
// associativity: (x + y) + z = x + (y + z)

// Left unit
unit(x) flatMap f == f(x)

// Right unit
m flatMap unit == m


// -------------------------------------------------------------------------------
// Checking Monad Laws

abstract class Option[+T] {
  def flatMap[U](f: T => Option[U]): Option[U] = this match {
    case Some(x) => f(x)
    case None => None
  }
}

// Show left unit
Some(x) flatMap f == f(x) // obvious

// Show right unit
// opt = Some(x) or None
opt flatMap Some == opt

opt flatMap Some
  = opt match {
    case Some(x) => Some(x)
    case None => None
  }
  = opt

// Show associativity
opt flatMap f flatMap g == opt flatMap (x => f(x) flatMap g)
  = opt match {
    case Some(x) => f(x) // : Option[U]
    case None => None
  } match {
    case Some(y) => g(y) // : Option[V]
    case None => None
  }
  = opt match {
    case Some(x) => f(x) match {
      case Some(y) => g(y)
      case None => None
    }
    case None => None match { // this reduces to just None
      case Some(y) => g(y)
      case None => None
    }
  }
  = opt match {
    case Some(x) => f(x) flatMap g
    case None => None
  }
  = opt flatMap (x => f(x) flatMap g)

// -------------------------------------------------------------------------------
// Try - similar to Option
// Success or Failure
//
// Try is not a Monad because it fails for left unit law
// However, it has the "bullet-proof" principle, which means that
// any expression composed from Try map and flatMap will never throw
// a non-fatal exception

// however, for for-expressions, left unit law is not really important,
// so even though Try is not technically a monad, it is still useful for for-expressions

abstract class Try[+T] {
  def flatMap[U](f: T => Try[U]): Try[U] = this match {
    case Success(x) => try f(x) catch { case NonFatal(ex) => Failure(ex) }
    case fail: Failure => fail
  }

  def map[U](f: T => U): Try[U] = this match {
    case Success(x) => Try(f(x))
    case fail: Failure => fail
  }
}
case class Success[T](x: T) extends Try[T]
case class Failure(ex: Exception) extends Try[Nothing]

object Try {
  def apply[T](expr: => T): Try[T] =
    try Success(expr)
    catch {
      case NonFatal(ex) => Failure(ex)
    }
}


// -------------------------------------------------------------------------------
// Many types defining flatMap are monads, but not all
// Monads that also define withFilter are called "monads with zero"


// -------------------------------------------------------------------------------
// Futures are monads
// Combinators on Futures

object Future {
  def apply(body: => T)(implicit context: ExecutionContext): Future[T]
}

// like "map" for the error case of the Future
def recover(f: PartialFunction[Throwable, T]): Future[T]

// like "flatMap" for the error case of the Future
def recoverWith(f: PartialFunction[Throwable, Future[T]]): Future[T]


def sendTo(url: URL, packet: Array[Byte]): Future[Array[Byte]] =
  Http(url, Request(packet))
    .filter(response => response.isOK)
    .map(response => response.toByteArray)

def sendToSafe(packet: Array[Byte]): Future[Array[Byte]] =
  sendTo(mailServer.europe, packet) recoverWith {
    case europeError => sendTo(mailServer.usa, packet) recover {
      case usaError => usaError.getMessage.toByteArray
    }
  }

def fallbackTo(that: => Future[T]): Future[T] = {
  // if this future failse, take the successful result of that future
  // if that future fails too, take the error of this future
  this recoverWith {
    case _ => that recoverWith { case _ => this }
  }
}

trait Awaitable[T] extends AnyRef {
  abstract def ready(atMost: Duration): Unit
  abstract def result(atMost: Duration): T
}

trait Future[T] extends Awaitable[T] {
  def filter(p: T=>Boolean): Future[T]
  def map[S](f: T=>S): Future[S]
  def recoverWith(f: PartialFunction[Throwable, Future[T]]): Future[T]
  def onComplete(callback: Try[T] => Unit) = ...
  def flatMap[S](f: T=>Future[S]): Future[S] =
    new Future[S] {
      def onComplete(callback: Try[S] => Unit): Unit =
        self.onComplete {
          case Success(x) => f(x).onComplete(callback)
          case Failure(e) => callback(Failure(e))
        }
    }
}



val socket = Socket()
val packet: Future[Array[Byte]] =
  socket.readFromMemory()
val confirmation: Future[Array[Byte]] =
  packet.flatMap(socket.sendToSafe(_))

// or
val confirmation: Future[Array[Byte]] = for {
  packet <- socket.readFromMemory()
  confirmation <- socket.sendToSafe(packet)
} yield confirmation

// recursive method
def retry(noTimes: Int)(block: => Future[T]): Future[T] = {
  if (noTimes == 0) {
    Future.failed(new Exception("Sorry"))
  } else {
    block fallbackTo {
      retry(noTimes-1){block}
    }
  }
}

// using foldLeft
def retry(noTimes: Int)(block: => Future[T]): Future[T] = {
  val ns = (1 to noTimes).toList
  val attempts = ns.map(_ => () => block)
  val failed = Future.failed(new Exception("boom"))
  val result = attempts.foldLeft(failed)((a, block) => a recoverWith { block() })
  result
}

// using foldRight
def retry(noTimes: Int)(block: => Future[T]): Future[T] = {
  val ns = (1 to noTimes).toList
  val attempts = ns.map(_ => () => block)
  val failed = Future.failed(new Exception)
  val results = attempts.foldRight(() => failed)((block, a) => () => { block() fallbackTo { a() }})
  result ()
}

// recursion is simplest to read
