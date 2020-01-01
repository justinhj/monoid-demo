package org.justinhj

import java.time.Instant

import cats.implicits._
import cats.{Monoid, Id}

trait PersistentEntity[F[_], T <: PersistentEntity[F, T]] {
    type Command
    type Event
    type State

    def id: Int
    def state : State

    def processCommand(command: Command) : List[Event]
    def processEvent(event: Event) : T
}

sealed trait BankAccountCommand
case class DepositCmd(time: Instant, amount: Int) extends BankAccountCommand
case class PurchaseCmd(time: Instant, amount: Int) extends BankAccountCommand
case class AssignAccountHolderCmd(time: Instant, accountHolder: String) extends BankAccountCommand

sealed trait BankAccountEvent
case class DepositEvt(time: Instant, amount: Int) extends BankAccountEvent
case class PurchaseEvt(time: Instant, amount: Int) extends BankAccountEvent
case class AssignAccountHolderEvt(time: Instant, accountHolder: String) extends BankAccountEvent

case class AccountState(balance: Int, accountHolder: Option[String])

case class AccountEntity[F[_]](id: Int, state: AccountState)
    extends PersistentEntity[F, AccountEntity[F]] {

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
                    List.empty
            case AssignAccountHolderCmd(time, accountHolder) =>
                List(AssignAccountHolderEvt(time, accountHolder))
        }
    }

    def processEvent(event: Event) : AccountEntity[F] = {
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
        val commands = List(
            DepositCmd(t1.plusSeconds(10), 100),
            PurchaseCmd(t1.plusSeconds(20), 120),
            AssignAccountHolderCmd(t1.plusSeconds(40), "Bob Johnson"),
            DepositCmd(t1.plusSeconds(40), 100),
            AssignAccountHolderCmd(t1.plusSeconds(50), "Ben Johnson"),
            PurchaseCmd(t1.plusSeconds(60), 120))

        // with processCommand and processEvent
        val finalState = commands.foldLeft(AccountEntity(1, AccountState(0, None))) {
                case (acc, cmd) =>
                val events = acc.processCommand(cmd)
                events.foldLeft(acc){
                    case (acc, evt) =>
                    acc.processEvent(evt)
                }
            }

        println(s"Final state $finalState")

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

        // Create a persistent entity
        val tt = Instant.now

        val pe = AccountEntity(1, AccountState(0, None))

        // Apply a command yielding some events
        val events = pe.processCommand(AssignAccountHolderCmd(tt, "Nero Johnson"))

        // Convert those events to states
        val states = events.map(eventToState)

        // Combine those states to get the current state

        val accountMonoid : Monoid[AccountEntity[Id]] = {

        }

        //println(s"Final state $finalState2")
    }
}

// In Lagom we have an actor with events incoming in strict time order one at a time
// Command comes in
//   Query commands can just return data
//   Commands which change state should emit a list of events following writing the events to a db
//   Once persisted the events should playback, changing the state

// Question, can


