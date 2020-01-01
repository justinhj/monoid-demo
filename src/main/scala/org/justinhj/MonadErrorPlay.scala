package org.justinhj

import cats.{Monoid, Eq}
import cats.effect._
import scala.util.Try
import scala.io.StdIn
import cats.implicits._

object MonadErrorPlay {

    def stringToInt(str: String): Try[Double] = {
        Try(str.toDouble)
    }

    def getInput() : IO[String] = IO {
        StdIn.readLine()
    }

    def writeOutput(s: String) : IO[Unit] = IO {
        print(s)
    }

    def usdToCad(usd : Double) = usd * 1.32528730

    //def gallonsToLitres(gallons: Double) = gallons * 4.54609

    def litresToGallons(litres: Double) = litres / 4.54609

    def usaGasToCanadaGas(usaPricePerGallon: Double) = litresToGallons(usdToCad(usaPricePerGallon))

    val getUSAPrice : IO[Double] = (for (
            _ <- writeOutput("Enter US gas price per gallon: ");
            in <- getInput();
            usaPrice <- IO.fromTry(stringToInt(in))
        ) yield usaPrice).handleErrorWith(_ => getUSAPrice)

    val prog = for (
        usaPrice <- getUSAPrice;
        cadPrice = usaGasToCanadaGas(usaPrice);
        _ <- writeOutput(s"Canadian price per litre: $cadPrice\n")
    ) yield ()

    /// TEMP
    object Temp {
        case class LastOption[A](val opt: Option[A]) extends AnyVal

        object LastOption {
            implicit def lastOptionMonoid[A]: Monoid[LastOption[A]] = new Monoid[LastOption[A]] {
                def combine(a1: LastOption[A], a2: LastOption[A]): LastOption[A] =
                    LastOption(a2.opt.orElse(a1.opt))

                def empty: LastOption[A] = LastOption(None)
            }
            implicit def lastOptionEq[A]: Eq[LastOption[A]] = new Eq[LastOption[A]] {
                def eqv(a1: LastOption[A], a2: LastOption[A]): Boolean =
                  a1.opt == a2.opt
              }
        }

        object AccountState {
            implicit val accountStateMonoid = new Monoid[AccountState] {
                def combine(x: AccountState, y: AccountState): AccountState = {
                    AccountState(
                        x.address |+| y.address,
                        x.balance |+| y.balance
                    )
                }
                def empty = AccountState(LastOption(None), 0)
            }
        }

        case class AccountState(address: LastOption[String], balance: Int)
    }

    // implicit def arbLastOption[A](implicit ev: Arbitrary[LastOption[A]]): Arbitrary[LastOption[A]] =
    //      Arbitrary { ev.arbitrary map { LastOption[A](_) } }

    /// END TEMP

    def main(args: Array[String]): Unit = {
        prog.unsafeRunSync
    }

}