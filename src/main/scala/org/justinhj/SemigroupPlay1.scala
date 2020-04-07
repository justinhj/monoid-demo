package org.justinhj

object SemigroupPlay {

    // import cats.Semigroup
    // import cats.data.NonEmptyList
    // import cats.syntax.SemigroupOps
    // import cats.data.NonEmptyListInstances
    // import cats.data.NonEmptyList._

    import cats._
    import cats.implicits._

    //val catList = NonEmptyList.of(1,2,3).combine(NonEmptyList.of(4,5,6))

    val fa = 10.some
    val fb = 20.some
    val fc = 30.some

    val plus1 = Some((a: Int) => a + 1)

    val test = Apply[Option].ap(plus1)(fa)

    // Product for option
    def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = {
      (fa, fb) match {
        case (Some(a), Some(b)) => Some((a,b))
        case _ => None
      }
    }

    def main(args : Array[String]) {
        import scala.util.Random
        import scala.io.StdIn
        import scala.util.{Try,Success,Failure}

        println(test)

        // Ask the user for a number and keep asking until you get one.
        def inputNumber(prompt: String) : Int = {
            print(prompt)
            Try(StdIn.readInt) match {
                case Success(value) =>
                    value
                case Failure(err) =>
                    println(s"That's not a number, bud.")
                    inputNumber(prompt)
            }
        }

        println("Please pick a number from 1-50 to guess the computers number!")

        val random = new Random

        val num = random.nextInt(50)

        def answer(maxGuesses: Int) {
          var done = false
          var guessedNumbers = Set.empty[Int]
          var numGuesses = 0

          do {
            val guess = inputNumber("Give us a number!: ")

            numGuesses += 1

            if(guessedNumbers.contains(guess)) {
                println(s"Haha. You've already tried $guess.")
            }

            guessedNumbers = guessedNumbers + guess

            if(guess > num) {
              if(guess - num == 5)
                println("Close, but too high.")
              else
                println("Too high!")
              }
            else if(guess<num) {
              if(guess - num == -5)
                println("CLose, but too low.")
              else
                println("Too low!")
            }
            else if(guess == num) {
              println("Nice! That's the number!")
              done = true
            }

            if(numGuesses == maxGuesses) {
                println(s"None of your $numGuesses were correct. The number was $num.")
                done = true
            }
          } while(!done)
        }

        answer(10)

    }

}