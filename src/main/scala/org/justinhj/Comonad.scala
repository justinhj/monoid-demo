package org.justinhj

import cats._
import cats.data._
import cats.syntax.monad._
import cats.syntax.comonad._
import cats.implicits._
import cats.instances.all

object ComonadSample {

    sealed trait Tree[+A]
    case class Node[A](data: A, var left: Tree[A], var right: Tree[A]) extends Tree[A]
    case object EmptyNode extends Tree[Nothing]

    def insert[A : Order](a: A, t: Tree[A]) : Tree[A] = {
        t match {
            case EmptyNode => 
                Node(a, EmptyNode, EmptyNode)
            case Node(a1,l,r) if a < a1 => 
                Node(a1, insert(a, l), r)
            case Node(a1,l,r) if a > a1 => 
                Node(a1, l, insert(a, r))
            case n @ Node(_,_,_) => 
                n
        }
    }

    // kind projector in ammonite 
    // import $plugin.$ivy.`org.spire-math::kind-projector:0.9.6`

    val t1 = List(10,5,2,7,8,3,5,6,4,3).foldLeft[Tree[Int]](EmptyNode){case (acc, a) => insert(a, acc)}  

    implicit def treeFunctor = new Functor[Tree] {
        def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
            case EmptyNode => 
                EmptyNode
            case Node(a,l,r) =>
                Node(f(a), map(l)(f), map(r)(f))
        }
    }

    // TODO is binary tree a sensible monad?

    // implicit def treeMonad = new Monad[Tree] {
    //     def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = {
    //         fa match {
    //             case EmptyNode => 
    //                 EmptyNode
    //             case Node(a,l,r) =>
    //                 Node(f(a), map(l)(f), map(r)(f))
    //         }
    //     }

    //     def pure[A](x: A): Tree[A] = Node(x, EmptyNode, EmptyNode)

    //     def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = ???

    // }

    def reseed(long: Long) = {
        println("reseed")
        (long * 6364136223846793005L + 1442695040888963407L)
    }

    def nextLong = State[Long, Long](
        s => {
            val newSeed = reseed(s)
            (newSeed, newSeed)  
        }
    )

    // Monad[State[Long, ?]].iterateWhile(diceRoll)((a : Long) => a < 3).run(3).value._2 

    def diceRoll : State[Long, Long] = nextLong.map(n => Math.abs(n) % 6 + 1)

    val testRolls1 = for (
        seed1 : Long <- State.get;
        x1 <- diceRoll;
        x2 <- diceRoll;
        seed2 <- State.get;
        _ <- State.set(seed1);
        x3 <- diceRoll;
        x4 <- diceRoll
    ) yield (seed1,x1,x2,seed2,x3,x4)

    val tr1 = testRolls1.runA(90810928L).value

    println(s"testRolls1 $tr1")

    def main(args: Array[String]) : Unit = {
          println(s"t1 $t1")
    }
}