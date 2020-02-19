import cats._
import syntax._
import instances._

def flatMap[A,B](l : List[A], f: A => List[B]) = {

    l.flatMap(f)

}

def stringToNum(s: String) = List(s.size)

val r = flatMap(List("Justin", "Heather"), stringToNum)

val x = 2
val y = x + 2

val z = x + y

val l = List("Big", "and", "Hairy")
val s = l.mkString(" ")