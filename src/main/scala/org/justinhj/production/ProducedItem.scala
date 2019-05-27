package org.justinhj.production

import cats._

// Represents an item that may be produced over time
case class ProducedItem(snapshotAmount: Long, snapshotTime: Long, amountPerHour: Double)

object ProducedItem {
    def empty = ProducedItem(0, System.currentTimeMillis(), 0)

    implicit val showProducedItem = new Show[ProducedItem] {
        def show(p: ProducedItem): String = 
          s"ProducedItem: Snapshot amount ${p.snapshotAmount} at ${p.snapshotTime}, increasing ${p.amountPerHour} per hour"
      }
      
    implicit def monoidProducedItemOps(implicit clock : Clock) = new Monoid[ProducedItem] {
        def empty = ProducedItem(0, 0, 0)
        def combine(p1: ProducedItem, p2: ProducedItem) : ProducedItem = {
            val p1A = p1.snapshotAmount + (((clock.currentTimeMillis - p1.snapshotTime) / Clock.oneHourMillis) * p1.amountPerHour).toLong
            val p2A = p2.snapshotAmount + (((clock.currentTimeMillis - p2.snapshotTime) / Clock.oneHourMillis) * p2.amountPerHour).toLong
    
            ProducedItem(p1A + p2A, clock.currentTimeMillis, Math.max(p1.amountPerHour, p2.amountPerHour))
        }
    }
}



