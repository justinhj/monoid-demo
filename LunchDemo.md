# Demo of Monoids and Zio event sourcing

## What is a Monoid

import cats._
import cats.implicits._
import cats.Monoid

"Justin" |+| " was here" |+| Monoid[String].empty

## Folding nested data structures

val tuples = List((10, "Pure"), (20, " "),(12,"Functions"))
Foldable[List].fold(tuples)

## Custom monoid instances

See PersistentEntity.scala