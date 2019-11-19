package org.justinhj

import java.time.Instant

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

sealed trait BankAccountEvent
case class DepositEvt(time: Instant, amount: Int) extends BankAccountEvent
case class PurchaseEvt(time: Instant, amount: Int) extends BankAccountEvent

case class AccountState(balance: Int)

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

        }
    }

    def processEvent(event: Event) : AccountEntity[F] = {
        event match {
            case DepositEvt(time, amount) =>
                AccountEntity(id, AccountState(state.balance + amount))
            case PurchaseEvt(time, amount) =>
                AccountEntity(id, AccountState(state.balance - amount))
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
    val a = List(1,2,3)

    val t1 = Instant.now
    val commands = List(
        DepositCmd(t1.plusSeconds(10), 100),
        PurchaseCmd(t1.plusSeconds(20), 120),
        DepositCmd(t1.plusSeconds(30), 100),
        PurchaseCmd(t1.plusSeconds(20), 120))

    val finalState = commands.foldLeft(AccountEntity(1, AccountState(0))) {
            case (acc, cmd) =>
              val events = acc.processCommand(cmd)
              events.foldLeft(acc){
                case (acc, evt) =>
                  acc.processEvent(evt)
            }
          }
}



// In Lagom we have an actor with events incoming in strict time order one at a time
// Command comes in
//   Query commands can just return data
//   Commands which change state should emit a list of events following writing the events to a db
//   Once persisted the events should playback, changing the state

// Question, can


