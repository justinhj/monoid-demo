package org.justinhj

object Contra {


    class Animal
    class Zebra extends Animal
    // thanks to covariance
    val animal: Animal = new Zebra

    class Hotel[-T]
    def generic = new Hotel[Any]
    // thanks to contravariance
    val ah: Hotel[Int] = generic




}