package org.justinhj

object MyThrowable {

  // This is just here to keep Wartremover happy
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit final class AnyOps[A](self: A) {
    def ===(other: A): Boolean = self == other
  }

  // Define Try and make it a monad
  sealed trait Try[+T]
  final case class Success[+T](value: T) extends Try[T]
  final case class Failure[+T](throwable: Throwable) extends Try[Nothing]

  final implicit class TryOps[A](val `try`: Try[A]) extends AnyVal {
    def flatMap[B](f: A => Try[B]): Try[B] = {
      `try` match {
        case Success(a) =>
          f(a)
        case Failure(fail) =>
          Failure[B](fail)
      }
    }
    def map[B](f: A => B): Try[B] = {
      `try` match {
        case Success(a) =>
          unit(f(a))
        case Failure(fail) =>
          Failure[B](fail)
      }
    }
    def unit[C](c: C) = Success(c)
  }

  def tryOrOOM(m: Int): Try[Int] = {

    if(m === 1) {
      val s = Success(1)
      s
    }
    else {
      val f = Failure[Int](new IllegalArgumentException("I only like ones"))
      f
    }
  }

  val ok = for (
    a <- tryOrOOM(1);
    b <- tryOrOOM(a)
  ) yield b

}