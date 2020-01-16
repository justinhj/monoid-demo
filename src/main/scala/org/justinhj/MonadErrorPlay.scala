package org.justinhj

import cats.effect._
import scala.util.Try
import scala.io.StdIn

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

    def main(args: Array[String]): Unit = {
        prog.unsafeRunSync
    }

}