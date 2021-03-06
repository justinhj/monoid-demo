package org.justinhj.production

import org.scalatest.flatspec.AnyFlatSpec

import cats.syntax.monoid._
import cats.instances.map._
import org.scalacheck.ScalacheckShapeless._
import cats.tests.CatsSuite
import cats.kernel.laws.discipline.MonoidTests

import productioncats.ProducedItem
import productioncats.ProducedItem._

class ProducedItemLawTestsCats extends CatsSuite {
  implicit val clock = FixedClock(System.currentTimeMillis + Clock.oneHourMillis)
  checkAll("ProducedItem.MonoidLaws", MonoidTests[ProducedItem].monoid)
}

class ProducedItemTestCats extends AnyFlatSpec {

  "Map of ProducedItem" should "append correctly" in {

    // Sample with test clock
    implicit val clock = FixedClock(System.currentTimeMillis + Clock.oneHourMillis)
    //implicit val clock = SystemClock
    val now = clock.currentTimeMillis

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

    val expected = Map(
      1 -> ProducedItem(15,now,10.0), 
      2 -> ProducedItem(10,now,5.0), 
      3 -> ProducedItem(2,now,0.0))

    assert(expected === addInventories)
  }
}