package org.justinhj

import scalaz._
import Scalaz._
import org.scalatest.PropSpec
import org.justinhj.IncreaseCountMonoidExample._
import org.scalacheck.{Properties, Test}
import scalaz.scalacheck.ScalazProperties._
import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.util.ConsoleReporter
import org.scalatest.MustMatchers.convertToAnyMustWrapper
import org.scalatest.flatspec.AnyFlatSpec

class IncreasedCountLaws extends AnyFlatSpec {

  "IncreasedCount" should "obey the monoid laws" in {

    val ml: Properties = monoid.laws[IncreaseCount]

    val testParams = Test.Parameters.default.withTestCallback(ConsoleReporter(1))

    val res = Test.checkProperties(testParams, ml)
    val passedCount = res.count(_._2.passed)

    assert(passedCount == res.size)
  }

  it should "obey left identity" in {

    val sample = IncreaseCount(-953793985,0,1)
    val result = sample |+| Monoid[IncreaseCount].zero
    assert(Equal[IncreaseCount].equal(result, sample))
  }

}