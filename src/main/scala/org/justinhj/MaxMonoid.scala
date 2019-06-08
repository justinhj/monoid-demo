package org.justinhj

import scalaz.Foldable
import scalaz.Monoid
import scalaz.std.list._
import scalaz.syntax.semigroup._

// Note that we are selective about imports here, or Scalaz will define an Int monoid and will use that instead...
object MaxMonoid {
    def main(args: Array[String]): Unit = {
        implicit val maxIntMonoid : Monoid[Int] = Monoid.instance[Int]({case (a : Int,b :  Int) => Math.max(a,b)} , Int.MinValue)

        val testAppend =  10 |+| 20
        println(testAppend)

        val testLeftIdentity = maxIntMonoid.zero |+| 10
        val testRightIdentity = 10 |+| maxIntMonoid.zero
        println(testLeftIdentity == testRightIdentity)

        val ilist = List[Int](1,2,3,4,5,4,3,2,1,-10,1,2,3,4)
        val folded = Foldable[List].fold(ilist)
        println(folded)
      }
}