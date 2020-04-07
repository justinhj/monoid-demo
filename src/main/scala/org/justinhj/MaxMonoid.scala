package org.justinhj

import scalaz._
import Scalaz._

object MaxMonoid {

   @SuppressWarnings(Array("org.wartremover.warts.Equals"))
    implicit final class AnyOps[A](self: A) {
      def ===(other: A): Boolean = self == other
    }

    def main(args: Array[String]): Unit = {
      // Note we use the name intInstance deliberately to override the
      // implicit in Scalaz
      implicit val intInstance : Monoid[Int] = Monoid.instance[Int]({case (a : Int,b :  Int) => Math.max(a,b)} , Int.MinValue)

      // Append is now Max
      val testAppend =  10 |+| 20
      println(testAppend) // 20

      // Show that left identity and right identity hold
      val testLeftIdentity = intInstance.zero |+| 10
      val testRightIdentity = 10 |+| intInstance.zero
      println(testLeftIdentity === testRightIdentity)

      val ilist = IList[Int](1,2,3,4,5,4,3,2,1,-10,1,2,3,4)
      val folded = Foldable[IList].fold(ilist)
      println(folded) // Print the maximum value in the list (5)
    }
}