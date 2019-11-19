## What is a Monoid

import scalaz._, Scalaz._

Monoid[String].zero |+| "Yoppworks " |+| "in " |+| "the " |+| "house." |+| Monoid[String].zero

### Show left identity
("Yoppworks" |+| Monoid[String].zero) === "Yoppworks"

### Show right identity
(Monoid[String].zero |+| "Yoppworks") === "Yoppworks"

### Show associativity
(("a" |+| "b") |+| "c") === (("a" |+| ("b" |+| "c")))

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




