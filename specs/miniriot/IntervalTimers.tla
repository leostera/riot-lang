------------------------- MODULE IntervalTimers -------------------------
EXTENDS Integers, Sequences, TLC, QueueUtils, MiniriotCommon

\* A focused model of user-visible send-after and send-interval timers. It
\* deliberately ignores workers and process internals so the main question stays
\* obvious: does a timer keep the same identity across repeats, and does cancel
\* stop future deliveries?

CONSTANTS
  Processes,
  Messages,
  TimerIds,
  NoProcess,
  NoMessage,
  MaxMailboxLen

ASSUME Processes # {}
ASSUME Messages # {}
ASSUME NoProcess \notin Processes
ASSUME NoMessage \notin Messages
ASSUME MaxMailboxLen \in Nat

VARIABLES
  mailbox,
  timerActive,
  timerMode,
  timerTarget,
  timerMessage,
  deliveryLog

vars ==
  << mailbox,
     timerActive,
     timerMode,
     timerTarget,
     timerMessage,
     deliveryLog >>

CanAppendMailbox(p) == Len(mailbox[p]) < MaxMailboxLen

TypeOK ==
  /\ mailbox \in [Processes -> Seq(Messages)]
  /\ timerActive \in [TimerIds -> BOOLEAN]
  /\ timerMode \in [TimerIds -> TimerModes]
  /\ timerTarget \in [TimerIds -> Processes \cup { NoProcess }]
  /\ timerMessage \in [TimerIds -> Messages \cup { NoMessage }]
  /\ deliveryLog \in Seq(TimerIds \X Processes \X Messages)

InactiveTimersAreDetached ==
  \A tid \in TimerIds :
    ~timerActive[tid]
    =>
    /\ timerTarget[tid] = NoProcess
    /\ timerMessage[tid] = NoMessage

ActiveTimersAreWellFormed ==
  \A tid \in TimerIds :
    timerActive[tid]
    =>
    /\ timerTarget[tid] \in Processes
    /\ timerMessage[tid] \in Messages

\* Bounded smoke-run cutoff used only by the small passing config.
SmokeDepthBound == TLCGet("level") < 6

\* Intended law: if an interval delivery has happened and there is still an
\* active interval timer carrying the same target/message payload, that active
\* timer should still be the original timer id recorded in the delivery log.
\* The current slice intentionally violates this by rearming under a fresh id.
IntervalFireKeepsOriginalId ==
  \A i \in 1..Len(deliveryLog) :
    LET delivered == deliveryLog[i]
        deliveredTid == delivered[1]
        deliveredTarget == delivered[2]
        deliveredMessage == delivered[3]
    IN
    /\ timerMode[deliveredTid] = "Interval"
    /\ (\E activeTid \in TimerIds :
          /\ timerActive[activeTid]
          /\ timerMode[activeTid] = "Interval"
          /\ timerTarget[activeTid] = deliveredTarget
          /\ timerMessage[activeTid] = deliveredMessage)
    => timerActive[deliveredTid]

Init ==
  /\ mailbox = [ p \in Processes |-> <<>> ]
  /\ timerActive = [ tid \in TimerIds |-> FALSE ]
  /\ timerMode = [ tid \in TimerIds |-> "OneShot" ]
  /\ timerTarget = [ tid \in TimerIds |-> NoProcess ]
  /\ timerMessage = [ tid \in TimerIds |-> NoMessage ]
  /\ deliveryLog = <<>>

AddOneShotTimer(tid, target, msg) ==
  /\ tid \in TimerIds
  /\ target \in Processes
  /\ msg \in Messages
  /\ ~timerActive[tid]
  /\ timerTarget[tid] = NoProcess
  /\ timerMessage[tid] = NoMessage
  /\ timerActive' = [timerActive EXCEPT ![tid] = TRUE]
  /\ timerMode' = [timerMode EXCEPT ![tid] = "OneShot"]
  /\ timerTarget' = [timerTarget EXCEPT ![tid] = target]
  /\ timerMessage' = [timerMessage EXCEPT ![tid] = msg]
  /\ UNCHANGED << mailbox, deliveryLog >>

AddIntervalTimer(tid, target, msg) ==
  /\ tid \in TimerIds
  /\ target \in Processes
  /\ msg \in Messages
  /\ ~timerActive[tid]
  /\ timerTarget[tid] = NoProcess
  /\ timerMessage[tid] = NoMessage
  /\ timerActive' = [timerActive EXCEPT ![tid] = TRUE]
  /\ timerMode' = [timerMode EXCEPT ![tid] = "Interval"]
  /\ timerTarget' = [timerTarget EXCEPT ![tid] = target]
  /\ timerMessage' = [timerMessage EXCEPT ![tid] = msg]
  /\ UNCHANGED << mailbox, deliveryLog >>

CancelTimer(tid) ==
  /\ tid \in TimerIds
  /\ timerActive[tid]
  /\ timerActive' = [timerActive EXCEPT ![tid] = FALSE]
  /\ timerTarget' = [timerTarget EXCEPT ![tid] = NoProcess]
  /\ timerMessage' = [timerMessage EXCEPT ![tid] = NoMessage]
  /\ UNCHANGED << mailbox, timerMode, deliveryLog >>

FireOneShotTimer(tid) ==
  /\ tid \in TimerIds
  /\ timerActive[tid]
  /\ timerMode[tid] = "OneShot"
  /\ LET target == timerTarget[tid]
         msg == timerMessage[tid]
         delivered ==
           target \in Processes /\ msg \in Messages /\ CanAppendMailbox(target)
     IN
     /\ IF delivered
        THEN
          /\ mailbox' = [mailbox EXCEPT ![target] = Enqueue(@, msg)]
          /\ deliveryLog' = Enqueue(deliveryLog, << tid, target, msg >>)
        ELSE
          /\ mailbox' = mailbox
          /\ deliveryLog' = deliveryLog
     /\ timerActive' = [timerActive EXCEPT ![tid] = FALSE]
     /\ timerTarget' = [timerTarget EXCEPT ![tid] = NoProcess]
     /\ timerMessage' = [timerMessage EXCEPT ![tid] = NoMessage]
  /\ UNCHANGED timerMode

\* Buggy behavior: interval delivery consumes the old timer id and rearms a new
\* timer with the same target/message.
FireIntervalTimer(tid, rearmTid) ==
  /\ tid \in TimerIds
  /\ rearmTid \in TimerIds
  /\ tid # rearmTid
  /\ timerActive[tid]
  /\ timerMode[tid] = "Interval"
  /\ ~timerActive[rearmTid]
  /\ timerTarget[rearmTid] = NoProcess
  /\ timerMessage[rearmTid] = NoMessage
  /\ LET target == timerTarget[tid]
         msg == timerMessage[tid]
         delivered ==
           target \in Processes /\ msg \in Messages /\ CanAppendMailbox(target)
         clearedActive == [timerActive EXCEPT ![tid] = FALSE]
         clearedTarget == [timerTarget EXCEPT ![tid] = NoProcess]
         clearedMessage == [timerMessage EXCEPT ![tid] = NoMessage]
     IN
     /\ IF delivered
        THEN
          /\ mailbox' = [mailbox EXCEPT ![target] = Enqueue(@, msg)]
          /\ deliveryLog' = Enqueue(deliveryLog, << tid, target, msg >>)
        ELSE
          /\ mailbox' = mailbox
          /\ deliveryLog' = deliveryLog
     /\ timerActive' = [clearedActive EXCEPT ![rearmTid] = TRUE]
     /\ timerMode' = [timerMode EXCEPT ![rearmTid] = "Interval"]
     /\ timerTarget' = [clearedTarget EXCEPT ![rearmTid] = target]
     /\ timerMessage' = [clearedMessage EXCEPT ![rearmTid] = msg]

ConsumeMessage(pid) ==
  /\ pid \in Processes
  /\ mailbox[pid] # <<>>
  /\ mailbox' = [mailbox EXCEPT ![pid] = Suffix(@, 2)]
  /\ UNCHANGED << timerActive, timerMode, timerTarget, timerMessage, deliveryLog >>

Next ==
  \/ \E tid \in TimerIds, target \in Processes, msg \in Messages :
       AddOneShotTimer(tid, target, msg)
  \/ \E tid \in TimerIds, target \in Processes, msg \in Messages :
       AddIntervalTimer(tid, target, msg)
  \/ \E tid \in TimerIds : CancelTimer(tid)
  \/ \E tid \in TimerIds : FireOneShotTimer(tid)
  \/ \E tid \in TimerIds, rearmTid \in TimerIds : FireIntervalTimer(tid, rearmTid)
  \/ \E pid \in Processes : ConsumeMessage(pid)

Spec == Init /\ [][Next]_vars

=============================================================================
