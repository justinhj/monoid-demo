package org.justinhj

object WhatsAp {

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit final class AnyOps[A](self: A) {
    def ===(other: A): Boolean = self == other
  }

  // This is exercise 12.1 from the Red Book
  // and the second part of 12.2

  trait Functor[F[_]] {
    def map[A,B](fa: F[A])(f: A => B): F[B]
  }

  trait Applicative[F[_]] extends Functor[F] {
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C]

    def unit[A](a: A): F[A]

    // derive map and traverse
    def map[A, B](fa: F[A])(f: A => B): F[B] = {
      map2(fa, unit(())){
        (a,b) => f(a)
      }
    }

    def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = {
      as.foldLeft(unit(List.empty[B])) {
        (acc,a) =>
          map2(acc, f(a)) {
            (listB, b) =>
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

    def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] = {
      map2(fa,fb)(Tuple2.apply)
    }

    // From the book is the slightly more concise map2(fab, fa)(_(_))

    // use map2 to define apply
    def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = {
      map2(fab, fa){
        (f,a) =>
          f(a)
      }
    }
  }

  // Applicative instance for Option
  val applicativeOption = new Applicative[Option] {
    def map2[A,B,C](fa: Option[A], fb: Option[B])(f: (A,B) => C): Option[C] = {
      for (
        a <- fa;
        b <- fb
      ) yield f(a,b)
    }

    def unit[A](a: A): Option[A] = Some(a)
  }

  def main(args : Array[String]) {

    val o1: Option[Int] = Some(1)
    val o2: Option[Int] = Some(4)
    val o3 = applicativeOption.map2(o1,o2){_ + _}

    println(s"o1 + o2 = $o3")

    val t1 = applicativeOption.traverse(List(2,4,6))(n =>
      if(n%2 === 1) None
      else Some(n))
    println(t1)

    val s1 = applicativeOption.sequence(List(o1,o2))
    println(s1)

    val rm1 = applicativeOption.replicateM(10, Some(3))
    println(rm1)

    val p1 = applicativeOption.product(Some(3), Some(5))
    println(p1)

    val a1 = applicativeOption.apply(Some((a: Int) => a + 1))(Some(10))
    println(a1)
  }
}