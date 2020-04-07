package org.justinhj

import zio.blocking.Blocking
import zio.clock.Clock
import zio.console._
import zio.random.Random
import zio.system.System
import zio._
import zio.interop.monix._
import zio.internal.Platform

/*
I was wondering what the best way to convert from a ZIO[R,Throwable,A] to a Monix eval.Task is?
I cant seem to get the implicit conversions to work in zio-interop-monix? Do I have to remove
the environment from the effect somehow first?
Working through interop layers with ZIO and monix and cats io, for the most part
 has been pretty seamless, but this one has me stumped a bit
*/

object ZioAmmonite {

  type Env = Clock with Console with System with Random with Blocking

  import monix.execution.Scheduler.Implicits.global

  // A runtime instance
  implicit val rts: Runtime[Env] = Runtime.default

  def z1(n: Int) : ZIO[Env, Throwable, Int] = {
    for (
      _ <- putStrLn(s"Hello with number $n")
    ) yield (n + 1)
  }

  val mio = z1(10).provide(rts.environment).toTask
  val mioRan = rts.unsafeRun(mio)

  def main(args: Array[String]): Unit = {
    mioRan.runToFuture.foreach(r => println(s"IO to task result is $r"))
  }

}
