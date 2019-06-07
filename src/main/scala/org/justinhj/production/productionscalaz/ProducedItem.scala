package org.justinhj.production.productionscalaz

import scalaz.{Monoid, Equal, Show, Cord}

import org.justinhj.production.Clock

// Represents an item that may be produced over time
// Note that this duplicates the implementation in production.cats for pedagogical reasons
case class ProducedItem(snapshotAmount: Long, snapshotTime: Long, amountPerHour: Double) {
    def currentAmount(implicit clock: Clock) = 
        snapshotAmount + (((clock.currentTimeMillis - snapshotTime) / Clock.oneHourMillis) * amountPerHour).toLong
}

object ProducedItem {
    def empty = ProducedItem(0, System.currentTimeMillis(), 0)

//    implicit val eqProducedItem : Eq[ProducedItem] = Eq.fromUniversalEquals

    implicit def eqProducedItem(implicit clock : Clock) = new Equal[ProducedItem] {
        def equal(x: ProducedItem, y: ProducedItem): Boolean = {
            x.currentAmount == y.currentAmount
        }
    }

    implicit val showProducedItem = new Show[ProducedItem] {
        def show(p: ProducedItem): Cord = 
          Cord(s"ProducedItem: Snapshot amount ${p.snapshotAmount} at ${p.snapshotTime}, increasing ${p.amountPerHour} per hour")
      }

    implicit def monoidProducedItemOps(implicit clock : Clock) = new Monoid[ProducedItem] {
        def zero = ProducedItem(0, 0, 0)
        def append(p1: ProducedItem, p2: => ProducedItem) : ProducedItem = {
            val p1A = p1.currentAmount
            val p2A = p2.currentAmount
    
            ProducedItem(p1A + p2A, clock.currentTimeMillis, Math.max(Math.abs(p1.amountPerHour), Math.abs(p2.amountPerHour)))
        }
    }
}



