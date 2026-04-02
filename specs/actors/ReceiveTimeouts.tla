------------------------- MODULE ReceiveTimeouts -------------------------
EXTENDS Integers, Sequences, TLC, QueueUtils, ActorsCommon

\* A focused model of selective receive with a save queue and an optional
\* receive timeout. This slice is intentionally smaller than the integration
\* runtime model: one process, no workers, no links, and no syscall state.

CONSTANTS
  Messages,
  TimerIds,
  SelectorUniverse,
  NoMessage,
  NoTimer,
  NoSelector,
  MaxMailboxLen

ASSUME Messages # {}
ASSUME SelectorUniverse # {}
ASSUME \A selector \in SelectorUniverse : selector \subseteq Messages
ASSUME NoMessage \notin Messages
ASSUME NoTimer \notin TimerIds
ASSUME NoSelector \notin SelectorUniverse
ASSUME MaxMailboxLen \in Nat

VARIABLES
  procState,
  mailbox,
  saveQueue,
  waitingFilter,
  receiveTimerId,
  receiveTimeoutFired,
  timerActive,
  lastOutcome

vars ==
  << procState,
     mailbox,
     saveQueue,
     waitingFilter,
     receiveTimerId,
     receiveTimeoutFired,
     timerActive,
     lastOutcome >>

ReceiveStates == { "Running", "WaitingMessage" }

ReceiveOutcomes == { "Idle", "Matched", "Blocked", "TimedOut" }

CanAppendMailbox == Len(mailbox) < MaxMailboxLen

TypeOK ==
  /\ procState \in ReceiveStates
  /\ mailbox \in Seq(Messages)
  /\ saveQueue \in Seq(Messages)
  /\ waitingFilter \in SelectorUniverse \cup { NoSelector }
  /\ receiveTimerId \in TimerIds \cup { NoTimer }
  /\ receiveTimeoutFired \in BOOLEAN
  /\ timerActive \in [TimerIds -> BOOLEAN]
  /\ lastOutcome \in ReceiveOutcomes

TimeoutStateConsistent ==
  /\ receiveTimeoutFired => receiveTimerId # NoTimer
  /\ receiveTimerId = NoTimer => ~receiveTimeoutFired
  /\ procState = "WaitingMessage" => waitingFilter # NoSelector
  /\ waitingFilter = NoSelector => receiveTimerId = NoTimer
  /\ receiveTimerId # NoTimer => timerActive[receiveTimerId] \/ receiveTimeoutFired

\* Bounded smoke-run cutoff used only by the small passing config.
SmokeDepthBound == TLCGet("level") < 6

\* These laws describe the intended receive-timeout behavior. The current slice
\* intentionally violates them so the refactor still reproduces the historical
\* bug before we fix the model.
TimeoutMustWinOverUnmatchedMessages ==
  [][
    /\ procState = "Running"
    /\ waitingFilter # NoSelector
    /\ receiveTimeoutFired
    /\ LET scan == SelectiveReceiveScan(saveQueue, mailbox, waitingFilter, NoMessage)
       IN /\ ~scan.matched
          /\ Len(saveQueue) + Len(mailbox) > 0
    => waitingFilter' = NoSelector
  ]_vars

UnmatchedWakeupKeepsOriginalTimer ==
  [][
    /\ procState = "Running"
    /\ waitingFilter # NoSelector
    /\ receiveTimerId # NoTimer
    /\ ~receiveTimeoutFired
    /\ LET scan == SelectiveReceiveScan(saveQueue, mailbox, waitingFilter, NoMessage)
       IN ~scan.matched
    => receiveTimerId' = receiveTimerId
  ]_vars

Init ==
  /\ procState = "Running"
  /\ mailbox = <<>>
  /\ saveQueue = <<>>
  /\ waitingFilter = NoSelector
  /\ receiveTimerId = NoTimer
  /\ receiveTimeoutFired = FALSE
  /\ timerActive = [ tid \in TimerIds |-> FALSE ]
  /\ lastOutcome = "Idle"

DeliverMessage(msg) ==
  /\ msg \in Messages
  /\ CanAppendMailbox
  /\ mailbox' = Enqueue(mailbox, msg)
  /\ IF procState = "WaitingMessage"
     THEN procState' = "Running"
     ELSE procState' = procState
  /\ UNCHANGED << saveQueue, waitingFilter, receiveTimerId,
                  receiveTimeoutFired, timerActive, lastOutcome >>

StartReceiveNoWait(selector) ==
  /\ selector \in SelectorUniverse
  /\ procState = "Running"
  /\ waitingFilter = NoSelector
  /\ LET scan == SelectiveReceiveScan(saveQueue, mailbox, selector, NoMessage) IN
     /\ scan.matched
     /\ mailbox' = scan.newMailbox
     /\ saveQueue' = scan.newSave
  /\ lastOutcome' = "Matched"
  /\ UNCHANGED << procState, waitingFilter, receiveTimerId,
                  receiveTimeoutFired, timerActive >>

StartReceiveBlock(selector, tid) ==
  /\ selector \in SelectorUniverse
  /\ tid \in TimerIds \cup { NoTimer }
  /\ procState = "Running"
  /\ waitingFilter = NoSelector
  /\ LET scan == SelectiveReceiveScan(saveQueue, mailbox, selector, NoMessage) IN
     /\ ~scan.matched
     /\ mailbox' = scan.newMailbox
     /\ saveQueue' = scan.newSave
  /\ procState' = "WaitingMessage"
  /\ waitingFilter' = selector
  /\ receiveTimeoutFired' = FALSE
  /\ IF tid = NoTimer
     THEN
       /\ receiveTimerId' = NoTimer
       /\ timerActive' = timerActive
     ELSE
       /\ ~timerActive[tid]
       /\ receiveTimerId' = tid
       /\ timerActive' = [timerActive EXCEPT ![tid] = TRUE]
  /\ lastOutcome' = "Blocked"

FireReceiveTimeout(tid) ==
  /\ tid \in TimerIds
  /\ timerActive[tid]
  /\ receiveTimerId = tid
  /\ timerActive' = [timerActive EXCEPT ![tid] = FALSE]
  /\ receiveTimeoutFired' = TRUE
  /\ IF procState = "WaitingMessage"
     THEN procState' = "Running"
     ELSE procState' = procState
  /\ UNCHANGED << mailbox, saveQueue, waitingFilter, receiveTimerId, lastOutcome >>

ResumeBlockedReceiveMatched ==
  /\ procState = "Running"
  /\ waitingFilter # NoSelector
  /\ LET clearedTimers ==
           IF receiveTimerId = NoTimer
           THEN timerActive
           ELSE [timerActive EXCEPT ![receiveTimerId] = FALSE]
         scan == SelectiveReceiveScan(saveQueue, mailbox, waitingFilter, NoMessage)
     IN
     /\ scan.matched
     /\ mailbox' = scan.newMailbox
     /\ saveQueue' = scan.newSave
     /\ procState' = "Running"
     /\ waitingFilter' = NoSelector
     /\ receiveTimerId' = NoTimer
     /\ receiveTimeoutFired' = FALSE
     /\ timerActive' = clearedTimers
     /\ lastOutcome' = "Matched"

ResumeBlockedReceiveTimedOut ==
  /\ procState = "Running"
  /\ waitingFilter # NoSelector
  /\ receiveTimeoutFired
  /\ mailbox = <<>>
  /\ saveQueue = <<>>
  /\ mailbox' = mailbox
  /\ saveQueue' = saveQueue
  /\ procState' = "Running"
  /\ waitingFilter' = NoSelector
  /\ receiveTimerId' = NoTimer
  /\ receiveTimeoutFired' = FALSE
  /\ IF receiveTimerId = NoTimer
     THEN timerActive' = timerActive
     ELSE timerActive' = [timerActive EXCEPT ![receiveTimerId] = FALSE]
  /\ lastOutcome' = "TimedOut"

\* Buggy behavior:
\* - any unmatched wakeup cancels the original timeout id;
\* - if the timeout fired but unmatched messages exist, the receive re-blocks
\*   instead of completing the timeout.
ResumeBlockedReceiveRearm(newTid) ==
  /\ newTid \in TimerIds \cup { NoTimer }
  /\ procState = "Running"
  /\ waitingFilter # NoSelector
  /\ LET scan == SelectiveReceiveScan(saveQueue, mailbox, waitingFilter, NoMessage)
         cancelledTimers ==
           IF receiveTimerId = NoTimer
           THEN timerActive
           ELSE [timerActive EXCEPT ![receiveTimerId] = FALSE]
     IN
     /\ ~scan.matched
     /\ \/ ~receiveTimeoutFired
        \/ Len(saveQueue) + Len(mailbox) > 0
     /\ mailbox' = scan.newMailbox
     /\ saveQueue' = scan.newSave
     /\ procState' = "WaitingMessage"
     /\ waitingFilter' = waitingFilter
     /\ receiveTimeoutFired' = FALSE
     /\ IF receiveTimerId = NoTimer
        THEN
          /\ receiveTimerId' = NoTimer
          /\ timerActive' = timerActive
        ELSE
          /\ newTid \in TimerIds
          /\ newTid # receiveTimerId
          /\ ~timerActive[newTid]
          /\ receiveTimerId' = newTid
          /\ timerActive' = [cancelledTimers EXCEPT ![newTid] = TRUE]
     /\ lastOutcome' = "Blocked"

Next ==
  \/ \E msg \in Messages : DeliverMessage(msg)
  \/ \E selector \in SelectorUniverse : StartReceiveNoWait(selector)
  \/ \E selector \in SelectorUniverse, tid \in TimerIds \cup { NoTimer } :
       StartReceiveBlock(selector, tid)
  \/ \E tid \in TimerIds : FireReceiveTimeout(tid)
  \/ ResumeBlockedReceiveMatched
  \/ ResumeBlockedReceiveTimedOut
  \/ \E newTid \in TimerIds \cup { NoTimer } : ResumeBlockedReceiveRearm(newTid)

Spec == Init /\ [][Next]_vars

=============================================================================
