package org.justinhj.production

trait Clock {
    def currentTimeMillis : Long
  }

object Clock {
    val oneHourMillis = (1000 * 60 * 60)
}

// A clock fixed at a particular time
case class FixedClock(val currentTimeMillis : Long) extends Clock

// Clock that gets the system time
case object SystemClock extends Clock {
    def currentTimeMillis = System.currentTimeMillis
}
