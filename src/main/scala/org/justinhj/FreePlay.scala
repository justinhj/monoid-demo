package org.justinhj

object FreePlay {

    @SuppressWarnings(Array("org.wartremover.warts.Equals"))
    implicit final class AnyOps[A](self: A) {
      def ===(other: A): Boolean = self == other
    }

    import cats.free.Free
    import cats.free.Free.liftF

    sealed trait KVStoreA[A]
    final case class Put[T](key: String, value: T) extends KVStoreA[Unit]
    final case class Get[T](key: String) extends KVStoreA[Option[T]]
    final case class Delete(key: String) extends KVStoreA[Unit]

    type KVStore[A] = Free[KVStoreA, A]

    // Put returns nothing (i.e. Unit).
    def put[T](key: String, value: T): KVStore[Unit] =
        liftF[KVStoreA, Unit](Put[T](key, value))

    // Get returns a T value.
    def get[T](key: String): KVStore[Option[T]] =
        liftF[KVStoreA, Option[T]](Get[T](key))

    // Delete returns nothing (i.e. Unit).
    def delete(key: String): KVStore[Unit] =
        liftF(Delete(key))

    // Update composes get and set, and returns nothing.
    def update[T](key: String, f: T => T): KVStore[Unit] =
        for {
            vMaybe <- get[T](key)
            _ <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
        } yield ()

    def program: KVStore[Option[Int]] =
        for {
            _ <- put("wild-cats", 2)
            _ <- update[Int]("wild-cats", (_ + 12))
            _ <- put("tame-cats", 5)
            n <- get[Int]("wild-cats")
            _ <- delete("tame-cats")
        } yield n

    // Interpreter

    import cats.arrow.FunctionK
    import cats.Id
    import scala.collection.mutable

    // To interpret the DSL above we need a natural transformation
    // KVStoreA to Id in this case

    def impureCompiler: FunctionK[KVStoreA,Id]  =
        new FunctionK[KVStoreA,Id] {

        // a very simple (and imprecise) key-value store
        val kvs = mutable.Map.empty[String, Any]

        def apply[A](fa: KVStoreA[A]): Id[A] =
            fa match {
            case Put(key, value) =>
                println(s"put($key, $value)")
                kvs(key) = value
                ()
            case Get(key) =>
                println(s"get($key)")
                kvs.get(key).map(_.asInstanceOf[A])
            case Delete(key) =>
                println(s"delete($key)")
                kvs.remove(key)
                ()
            }
        }

}
