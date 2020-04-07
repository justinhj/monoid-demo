package org.justinhj

// Fun with Const

import cats.Functor
import cats.Id
import cats.implicits._
import scala.util._

object Const {

  val f = Failure

  trait Lens[S, A] {
    def get(s: S): A

    def set(s: S, a: A): S

    def modify(s: S)(f: A => A): S =
      modifyF[Id](s)(f)

    def modifyF[F[_]: Functor](s: S)(f: A => F[A]): F[S]
    //=
      //f(get(s)).map(a => set(s, a))
  }

}
