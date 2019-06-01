package org.justinhj

import cats._
import cats.data._
import cats.implicits._
import cats.kernel.instances.all
import alleycats.std.map._

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

    //type Inventory[A] = Map[Int,A]
    //val ass1 = Foldable[Map[Int,?]].fold(addInventories)

    val bloop = Monoid[Map[Int, Int]].combine(Map (1 -> 50), Map(1 -> -100, 2 -> -100)) 

    val ass2 = Foldable[List].fold(List(inventory1, inventory2, inventory3))

    val ass3 = Foldable[({type MapA[A] = Map[Int, A]})#MapA].fold(Map(1 -> 200, 3 -> 5, 5 -> 100))  

    println(show"Inventory $addInventories")
  }

}
