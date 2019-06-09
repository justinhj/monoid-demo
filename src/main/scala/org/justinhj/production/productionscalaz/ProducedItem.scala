package org.justinhj.production.productionscalaz

import scalaz.{Monoid, Equal, Show, Cord}
import scalaz.syntax.show._
import scalaz.std.anyVal.{longInstance, doubleInstance}
import org.justinhj.production.Clock

// Represents an item that may be produced over time
// Note that this duplicates the implementation in production.cats for pedagogical reasons
case class ProducedItem(snapshotAmount: Long, snapshotTime: Long, amountPerHour: Double) {
    def currentAmount(implicit clock: Clock) = 
        snapshotAmount + (((clock.currentTimeMillis - snapshotTime) / Clock.oneHourMillis) * amountPerHour).toLong
}

object ProducedItem {
    def empty = ProducedItem(0, System.currentTimeMillis(), 0)

    // Naive implementation of equals that causes our Monoid to violate the left and right identity laws
    // implicit val ProducedItemEquals = new Equal[ProducedItem] {
    //     // we are using Scalaz === internally
    //     def equal(a1: ProducedItem, a2: ProducedItem): Boolean = {
    //         a1.amountPerHour == a2.amountPerHour &&
    //         a1.snapshotAmount == a2.snapshotAmount &&
    //         a1.snapshotTime == a2.snapshotTime
    //     }
    //   }

    // Implementation of Equals that reflects better our business requirements 
    // and allows us to pass the Monoid laws
    implicit def eqProducedItem(implicit clock : Clock) = new Equal[ProducedItem] {
        def equal(x: ProducedItem, y: ProducedItem): Boolean = {
            x.currentAmount == y.currentAmount
        }
    }

    implicit val showProducedItem = new Show[ProducedItem] {
        def show(p: ProducedItem): Cord = 
          Cord(z"ProducedItem: Snapshot amount ${p.snapshotAmount} at ${p.snapshotTime}, increasing ${p.amountPerHour} per hour")
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



