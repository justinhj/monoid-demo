package org.justinhj

import scalaz._, Scalaz._

import production._
import production.productionscalaz._

object ProducedItemMonoidScalaz {

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
    println(z"Inventory added with |+| $addInventories")

    val mapOfInts = Monoid[Map[Int, Int]].append(Map (1 -> 50), Map(1 -> -100, 2 -> -100)) 
    println(z"Map of ints $mapOfInts")

    val listOfInventories = Foldable[List].fold(List(inventory1, inventory2, inventory3))
    println(z"List of inventories $listOfInventories")

    // Folding a single inventory gives us the total amount of things we have right now
    // Note that because we must turn the keys of the map into some order and that order may not be defined,
    // this function is not pure. That's why the instance for folding a map is in alley cats, a library that 
    // allows unlawful operations.
    val foldMapofInts = Foldable[({type MapA[A] = Map[Int, A]})#MapA].fold(Map(1 -> 200, 3 -> 5, 5 -> 100))  
    println(z"Fold map of ints $foldMapofInts")
  }

}
