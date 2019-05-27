package org.justinhj

import cats._
import cats.data._
import cats.implicits._

import production._

object MonoidDemo {

  def main(args: Array[String]) : Unit = {

    implicit val clock = SystemClock
    implicit val now = clock.currentTimeMillis

    // Handle a map of them
    val inventory1 = Map(
      1 -> ProducedItem(10, now - Clock.oneHourMillis, 10),
      2 -> ProducedItem(10, now - Clock.oneHourMillis, 5))

    val inventory2 = Map(
      1 -> ProducedItem(-5, 0, 0),
      2 -> ProducedItem(-5, 0, 0),
      3 -> ProducedItem(1, 0, 0))

    val inventory3 = Map(
      3 -> ProducedItem(1, 0, 0))

    val addInventories = inventory1 |+| inventory2 |+| inventory3

    println(show"Inventory $addInventories")
  }

}
