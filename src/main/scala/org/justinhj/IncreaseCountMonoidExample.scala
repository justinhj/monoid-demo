package org.justinhj

import scalaz._
import Scalaz._

import scala.io.Source

object IncreaseCountMonoidExample {

  def iListFromStream[A](as: Stream[A]): IList[A] =
    as.foldRight(IList.empty[A])(ICons(_, _))

  def main(args: Array[String]): Unit = {

    case class IncreaseCount(count: Int, left: Int, right: Int)

    // Note we use the name intInstance deliberately to override the
    // implicit in Scalaz
    implicit val increaseCountInstance : Monoid[IncreaseCount] = {
      Monoid.instance[IncreaseCount]({
        case (l : IncreaseCount,r :  IncreaseCount) =>
          if(l.right < r.left) {
            IncreaseCount(l.count + r.count + 1, l.left, r.right)
          } else {
            IncreaseCount(l.count + r.count, l.left, r.right)
          }
        },
        IncreaseCount(0,0,0))
    }

    val exampleInput = IList(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
      .map(n => IncreaseCount(0,n,n))

    val folded = Foldable[IList].fold(exampleInput)
    println(s"Step 1 example. Count is ${folded.count} expected 7")

    // Test with input 1
    val step1Input = Source.fromResource("org/justinhj/adventday1input1.txt")
      .mkString
      .split("\n")
      .toList.map {
        ns =>
          val n = ns.toInt
          IncreaseCount(0,n,n)
      }

    val step1InputList = IList.fromList(step1Input)
    val step1Folded = Foldable[IList].fold(step1InputList)
    println(s"Step 1. Count is ${step1Folded.count}")

    // Step 2

    val input2 = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
    val zipped = input2.zip(input2.tail.zip(input2.tail.tail))
    val sums = zipped.map {
      case (a,(b,c)) =>
        val s = a + b + c
        IncreaseCount(0, s, s)
    }
    val step2InputList = IList.fromList(sums)
    val step2Folded = Foldable[IList].fold(step2InputList)
    println(s"Step 2. Count is ${step2Folded.count}")

    val step2Input = Source.fromResource("org/justinhj/adventday1input1.txt")
      .mkString
      .split("\n")
      .toList.map(_.toInt)

    val zipped2 = step2Input.zip(step2Input.tail.zip(step2Input.tail.tail))

    val sums2 = zipped2.map {
      case (a,(b,c)) => {
        val s = a + b + c
        IncreaseCount(0, s, s)
      }
    }
    val step2InputList2 = IList.fromList(sums2)
    val step2Folded2 = Foldable[IList].fold(step2InputList2)
    println(s"Step 2. Count is ${step2Folded2.count}")


  }
}
