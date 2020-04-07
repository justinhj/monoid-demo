package org.justinhj

object ApplyPlay {

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit final class AnyOps[A](self: A) {
    def ===(other: A): Boolean = self == other
  }

  // This is the first part of exercise 12.2 from the Red Book
  // "Show that this formulation is equivalent in expressiveness
  // by defining map2 and map in terms of unit and apply."

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  // You can define map2 in terms of apply and unit

  trait Applicative[F[_]] extends Functor[F] {

    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

    def unit[A](a: A): F[A]

    // define in terms of apply and unit
    def map[A, B](fa: F[A])(f: A => B): F[B] = {
      apply(unit(f))(fa)
    }

    // From the book answers apply(map(fa)(f.curried), fb)
    // So I got it but did the currying manually

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {

      val fbc = map(fa)(f.curried)
      // Explicitly...
      // {
      //   a =>
      //     b =>
      //       f(a,b)
      // }

      apply(fbc)(fb)
    }

    def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
      val fbcd = map(fa)(f.curried)
      val fcd = apply(fbcd)(fb)
      val t1 = apply(fcd)(fc)
      t1
    }

    def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
      val fbcde = map(fa)(f.curried)
      val fcde = apply(fbcde)(fb)
      val fde = apply(fcde)(fc)
      apply(fde)(fd)
    }

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = {
      as.foldLeft(unit(List.empty[B])) { (acc, a) =>
        map2(acc, f(a)) { (listB, b) =>
          listB :+ b
        }
      }
    }

    def sequence[A](fas: List[F[A]]): F[List[A]] = {
      traverse(fas)(identity)
    }

    def replicateM[A](n: Int, fa: F[A]): F[List[A]] = {
      val as = List.fill(n)(fa)
      sequence(as)
    }

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
      //map2(fa, fb)((_,_))
      map2(fa,fb)(Tuple2.apply)
    }
  }

  // Applicative instance for Option
  val applicativeOption = new Applicative[Option] {
    def apply[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] = {
      for (f <- fab;
           a <- fa) yield f(a)
    }

    def unit[A](a: A): Option[A] = Some(a)
  }

  trait Monad[F[_]] extends Functor[F] with Applicative[F] {
    def flatMap[A,B](fa: F[A])(f: A => F[B]) : F[B]

    override def map[A, B](fa: F[A])(f: A => B): F[B] = {
      flatMap(fa)(a => unit(f(a)))
    }
  }

  val monadicList = new Monad[List] {
      def apply[A, B](fabs: List[A => B])(fa: List[A]): List[B] = {
        flatMap(fabs) { fab =>
           map(fa)(fab)
        }
      }

      def unit[A](a: A): List[A] = List(a)

      def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = {
         val mapped = fa.map { a =>
          f(a)
         }
         mapped.foldLeft(List.empty[B]) {
           (acc, bs) =>
              acc ++ bs
         }
      }
  }

  val applicativeList = new Applicative[List] {
    def apply[A, B](fab: List[A => B])(fa: List[A]): List[B] = {
      fab.flatMap { f =>
        fa.map { a =>
          f(a)
        }
      }
    }

    def unit[A](a: A): List[A] = List(a)

  }

  def main(args: Array[String]) {

    val o1: Option[Int] = Some(1)
    val o2: Option[Int] = Some(4)
    val o3 = applicativeOption.map2(o1, o2) { _ + _ }

    println(s"o1 + o2 = $o3")

    val t1 = applicativeOption.traverse(List(2, 4, 6))(n =>
      if (n % 2 === 1) None
      else Some(n)
    )
    println(t1)

    val s1 = applicativeOption.sequence(List(o1, o2))
    println(s1)

    val rm1 = applicativeOption.replicateM(10, Some(3))
    println(rm1)

    val p1 = applicativeOption.product(Some(3), Some(5))
    println(p1)

    val m3 = applicativeOption.map3(Some(3), Some(5), Some(7)){_ * _ * _}
    println(m3)

    val m4 = applicativeOption.map4(Some(2), Some(3), Some(5), Some(7)){_ * _ * _ * _}
    println(m4)

    // Applicative list

    // Gives Cartesian product
    val a1 = applicativeList.product(List(1,2,3), List("Ass", "Boobs"))
    println(a1)

    val a1map2 = applicativeList.map2(List(1,2,3), List(10,20,30))((a,b) => a * b)
    println(a1map2)

    val monadicA1Map2 = monadicList.map2(List(1,2,3), List(10,20,30))((a,b) => a * b)
    println(monadicA1Map2)

  }
}
