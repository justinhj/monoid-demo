# Question

Chris: "Should Lagom entities ideally be monoids
(empty for the initial state + associativity of the commands)?"

## What is a Monoid?

trait Semigroup[A] {
  def append(f1: A, f2:A): A
}

trait Monoid[A] extends Semigroup[A] {
  def zero: A
}

import scalaz._, Scalaz._

Monoid[String].zero |+| "Yoppworks " |+| "in " |+| "the " |+| "house." |+| Monoid[String].zero

### Show left identity
("Yoppworks" |+| Monoid[String].zero) === "Yoppworks"

### Show right identity
(Monoid[String].zero |+| "Yoppworks") === "Yoppworks"

### Show associativity
(("a" |+| "b") |+| "c") === (("a" |+| ("b" |+| "c")))

## Check laws with property based checks

import scalaz.scalacheck.ScalazProperties._
monoid.laws[String].check()

## Folding nested data structures

val tuples = List((10, "Pure"), (20, " "),(12,"Functions"))
Foldable[List].fold(tuples)

## Scalaz - using tags and a more interesting Monoid example

### Monoid instance selection with tags

Tags.Multiplication(10) |+| Tags.Multiplication(10)

Tags.First(Option.empty[Int]) |+| Tags.First(20.some) |+| Tags.First(30.some)

Option.empty[Int] |+| 20.some |+| 30.some

implicit val intMaxInstance = Monoid.instance[Int @@ Tags.Max]({
    case (a : (Int @@ Tags.Max),b : (Int @@ Tags.Max)) =>
    val m : Int = Math.max(a.asInstanceOf[Int], b.asInstanceOf[Int])
    Tags.Max(m)
}, Tags.Max(Int.MinValue))

implicit def eqTagMax = new Equal[Int @@ Tags.Max] {
    def equal(x: Int @@ Tags.Max, y: Int @@ Tags.Max): Boolean = {
        x == y
    }
}

def word(w: String) = (1, Tags.Max(w.size), Map(w -> 1))
val words = List("Yoppworks", "Yoppworks", "technical", "pure", "functional", "pure", "pure", "pure")
val w = words.map(word)

### Get the word count, total characters and word frequency
Foldable[List].fold(w)

## What is Lagom persistent entity?

### Event sourced entity

Two stages:

applyCommand(Command, State) -> decision logic -> persist -> Zero or more events

events.applyEvent(Event, State) -> process events -> Updated State

## Monoid for persistent entity

This is still ok:

applyCommand(Command, State) -> decision logic -> persist -> Zero or more events

Event sourcing part now looks like this:

events.map(_.toState).fold

 implicit val monoidBankAccountStateOps = new Monoid[AccountState] {
        def zero = AccountState(0)
        def append(a1: AccountState, a2: => AccountState) : AccountState = {
            AccountState(a1.balance + a2.balance)
        }
    }

AccountState(balance = 100) |+| AccountState(balance = 120)

Notes
* The Monoid is of type State
* We still store events
* We can create the current state by converting events to states then combining them

### Conclusion

Monoids are a flexible abstraction for combining things

Monoids can be used to represent some persistent entities

Advantages
* parallelism of evaluation
* clearer code intent
* composition - we now have ability to combine entities and structures that contain entities, and process them in different ways
* testing - every event must map to a particular state, gives us a new angle on testing

Disadvantages
* requires state combination is associative and has a zero value
* it is an implementation detail, does not really help the user of the entity

# Cats version

import cats.Semigroup
import cats.implicits._

Monoid[String].empty |+| "Yoppworks " |+| "in " |+| "the " |+| "house." |+| Monoid[String].empty


@ Monoid[String].empty |+| "Yoppworks " |+| "in " |+| "the " |+| "house." |+| Monoid[String].empty

res2: String = "Yoppworks in the house."

// Combining bank accounts. We want to sum the balance but use last option for the address

{
 implicit final class LastOption[A](val opt: Option[A]) extends AnyVal

  object LastOption {
            implicit def lastOptionMonoid[A]: Monoid[LastOption[A]] = new Monoid[LastOption[A]] {
                def combine(a1: LastOption[A], a2: LastOption[A]): LastOption[A] =
                    LastOption(a2.opt.orElse(a1.opt))

                def empty: LastOption[A] = LastOption(None)
            }
            implicit def lastOptionEq[A]: Eq[LastOption[A]] = new Eq[LastOption[A]] {
                def eqv(a1: LastOption[A], a2: LastOption[A]): Boolean =
                  a1.opt == a2.opt
              }
        }
                case class AccountState(address: LastOption[String], balance: Int)
}

// see http://eed3si9n.com/herding-cats/Monoid.html

import org.scalacheck.ScalacheckShapeless._
import cats.tests.CatsSuite
import cats.kernel.laws.discipline.MonoidTests

@ LastOption("10 downing street".some) |+| LastOption("199 columbia".some)
res7: LastOption[String] = LastOption(Some("199 columbia"))

MonoidTests[LastOption[Int]].monoid.all.check()


@ AccountState(LastOption("200 Downing St".some), 100) |+| AccountState(LastOption("207 Downing St".some), 0)
res19: AccountState = AccountState(LastOption(Some("207 Downing St")), 100)

@ AccountState(LastOption("200 Downing St".some), 100) |+| AccountState(LastOption(None), 0)
res20: AccountState = AccountState(LastOption(Some("200 Downing St")), 100)

@ AccountState(LastOption("200 Downing St".some), 100) |+| AccountState(LastOption(None), 20)
res21: AccountState = AccountState(LastOption(Some("200 Downing St")), 120)

@ AccountState(LastOption("200 Downing St".some), 100) |+| AccountState(LastOption(None), -20)
res22: AccountSt




