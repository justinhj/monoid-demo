package org.justinhj

package org.justinhj

import java.time.Instant

import cats._
import cats.implicits._
import cats.{Monoid,Eq,Show}
import zio._
import zio.Runtime
import zio.clock.Clock
import zio.console._
import zio.internal.Platform
import zio.blocking.Blocking
import zio.random.Random
import zio.system.System

// trait HttpClient {
//     val httpClient : HttpClient.Service[HttpClient with Blocking]
// }

// object httpclient extends HttpClient.Service[HttpClient with Blocking] {
//   final def get(url: String): ZIO[Any with HttpClient with Blocking, Throwable, String] =
//   ZIO.accessM(_.httpClient.get(url))
// }


// Define a runtime for production
trait LiveRuntime extends Runtime[Clock with Console with System with Random with Blocking] {
  type Environment = Clock with Console with System with Random with Blocking

  val Platform: Platform       = Platform
 // val Environment: Environment = new Clock.live with Console with System.Live with Random.Live with Blocking.Live
}

trait PersistentEntity[T <: PersistentEntity[T]] {
    type Command
    type Event
    type State

    def id: Int
    def state : State

    def processCommand(command: Command) : List[Event]
    def processEvent(event: Event) : T
}

sealed trait BankAccountCommand
final case class DepositCmd(time: Instant, amount: Int) extends BankAccountCommand
final case class PurchaseCmd(time: Instant, amount: Int) extends BankAccountCommand
final case class AssignAccountHolderCmd(time: Instant, accountHolder: String) extends BankAccountCommand

sealed trait BankAccountEvent
final case class DepositEvt(time: Instant, amount: Int) extends BankAccountEvent
final case class PurchaseEvt(time: Instant, amount: Int) extends BankAccountEvent
final case class AssignAccountHolderEvt(time: Instant, accountHolder: String) extends BankAccountEvent

object LastOptionHelper {
    object LastOption {
        implicit def lastOptionMonoid[A]: Monoid[LastOption[A]] = new Monoid[LastOption[A]] {
            def combine(a1: LastOption[A], a2: LastOption[A]): LastOption[A] =
                LastOption(a2.opt.orElse(a1.opt))

            def empty: LastOption[A] = LastOption(None)
        }

        @SuppressWarnings(Array("org.wartremover.warts.Equals"))
        implicit def lastOptionEq[A]: Eq[LastOption[A]] = new Eq[LastOption[A]] {
            def eqv(a1: LastOption[A], a2: LastOption[A]): Boolean =
            a1.opt == a2.opt
        }
        implicit def lastOptiowShow[A : Show]: Show[LastOption[A]] = new Show[LastOption[A]] {
            def show(a: LastOption[A]): String =
                a.opt match {
                    case Some(a) =>
                        a.show
                    case None =>
                        "None"
                }
        }
    }

    implicit final class LastOption[A](val opt: Option[A]) extends AnyVal
}

import LastOptionHelper._

final case class AccountState(balance: Int, accountHolder: LastOption[String])

object AccountState {

    implicit def accountStateShow[A] = new Show[AccountState] {
        def show(a: AccountState): String = {
            show"Balance: ${a.balance}\nAccount holder: ${a.accountHolder}"
        }
    }

    implicit val accountMonoid = new Monoid[AccountState] {
        def empty : AccountState = AccountState(0, None)
        def combine(p1: AccountState, p2: AccountState) : AccountState = {
            AccountState(p1.balance |+| p2.balance, p1.accountHolder |+| p2.accountHolder)
        }
    }
}

final case class AccountEntity(id: Int, state: AccountState)
    extends PersistentEntity[AccountEntity] {

    override type Command = BankAccountCommand
    override type Event = BankAccountEvent
    override type State = AccountState

    // Processing commands involves validating it can be done with the current
    // state, and if it can it returns a list of events
    // It also returns a response, which is defined in the Command
    def processCommand(command: Command) : List[Event] = {
        // TODO persist, TODO error
        command match {
            case DepositCmd(time, amount) =>
                List(DepositEvt(time, amount))
            case PurchaseCmd(time, amount) =>
                if(amount <= state.balance)
                    List(PurchaseEvt(time, amount))
                else
                    List.empty[Event]
            case AssignAccountHolderCmd(time, accountHolder) =>
                List(AssignAccountHolderEvt(time, accountHolder))
        }
    }

    def processEvent(event: Event) : AccountEntity = {
        event match {
            case DepositEvt(time, amount) =>
                AccountEntity(id, AccountState(state.balance + amount, state.accountHolder))
            case PurchaseEvt(time, amount) =>
                AccountEntity(id, AccountState(state.balance - amount, state.accountHolder))
            case AssignAccountHolderEvt(time, accountHolder) =>
                AccountEntity(id, AccountState(state.balance, accountHolder.some))
        }
    }

}

// To process a command we need a function
// processCommand(command: Command) : List(Event)
// processEvent(event: Event): PersistentEnity

// import org.justinhj._
// import java.time.Instant
//                             balance
// dep 100                     100
// purchase 120 (fail)         100
// dep 100                     200
// purchase 120 (succeed)      80

object Sample {

    def main(args: Array[String]): Unit = {
        val t1 = Instant.now

        // Create a sample account entity
        val sampleAccount = AccountEntity(1, AccountState(0, None))

        val commands = List[BankAccountCommand](
            DepositCmd(t1.plusSeconds(10), 100),
            PurchaseCmd(t1.plusSeconds(20), 120),
            AssignAccountHolderCmd(t1.plusSeconds(40), "Bob Johnson"),
            DepositCmd(t1.plusSeconds(40), 100),
            AssignAccountHolderCmd(t1.plusSeconds(50), "Ben Johnson"),
            PurchaseCmd(t1.plusSeconds(60), 120))

        // With processCommand and processEvent (PE)
        val finalState = commands.foldLeft(sampleAccount) {
            case (acc, cmd) =>
                val events = acc.processCommand(cmd)
                events.foldLeft(acc){
                    case (acc, evt) =>
                    acc.processEvent(evt)
                }
            }

        println(show"Final state ${finalState.state}\n")

        // With Monoids. We need a way to convert events to states
        def eventToState(event: BankAccountEvent): AccountState = {
            event match {
                case DepositEvt(time, amount) =>
                    AccountState(amount, None)
                case PurchaseEvt(time, amount) =>
                    AccountState(-amount, None)
                case AssignAccountHolderEvt(time, accountHolder) =>
                    AccountState(0, accountHolder.some)
            }
        }

        // Combine those states to get the current state

        val events = commands.foldLeft((sampleAccount, List.empty[BankAccountEvent])) {
            case ((acc, events), cmd) =>
                val newEvents = acc.processCommand(cmd)
                val newAcc = events.foldLeft(acc) {
                    case (acc,evt) =>
                        acc.processEvent(evt)
                }
                (newAcc, events ++ newEvents)
            }

        val allEvents = events._2

        println(s"$allEvents")

        val finalState2 = allEvents.map(eventToState).combineAll

        println(show"Final state $finalState2")

        // zio part

        val runtime = Runtime.default

        val program: ZIO[Console, Nothing, Unit] = for (
            _ <- putStrLn("Hello")
            ) yield ()

        runtime.unsafeRun(program)

        // Zio version goals
        // 1. Queue we can send commands to
        // 2. Commands applied and generate events
        // 3. Persist using a zio module, see below for interface
        // 4. Update state
        // 5. At any time can look at the state (in a ref)
        // 6. When starting the program the queue should load events from the store using the persist module

        trait EventRepository[EventType] {
            val eventRepository : EventRepository.Service[EventType, EventRepository[EventType]]
        }

        object EventRepository {
            trait Service[EventType, R <: EventRepository[EventType]] {
                // Note this should be a stream
                // Also make an error type
                def readAllEvents(): ZIO[R, Throwable, List[EventType]]
                def writeEvent(event: EventType): Task[Unit]
                def writeEvents(events: List[EventType]): Task[Unit]
            }
        }

    }
}