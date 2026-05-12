------------------------- MODULE PendingActionDrain -------------------------
EXTENDS Naturals, FiniteSets, Sequences, TLC

\* Bounded semantic model for deterministic pending-action draining in zort.
\*
\* Source contracts:
\* - zort/spec/startup-domains-and-signals.md
\* - zort/spec/gc-control-and-stats.md
\*
\* How to read this model:
\* - "Actions" are abstract pending runtime callbacks such as queued signals or
\*   ready finalizers.
\* - "pending" is an ordered queue of not-yet-delivered actions.
\* - "delivered" is the set of actions already drained exactly once.
\* - domains drain pending work only at explicit checkpoints:
\*   scheduler safepoints, blocking enter, blocking exit, and STW pause
\*   acknowledgements.
\* - this model is about protocol ordering and losslessness, not callback body
\*   behavior or scheduler policy.

CONSTANTS Domains, Actions

ASSUME Domains # {}
ASSUME Actions # {}

Checkpoints == {"none", "scheduler", "blocking_enter", "blocking_exit", "stw_pause"}

VARIABLES
  known,
  pending,
  delivered,
  blocked,
  stwRequested,
  stwAcked,
  deliveryCheckpoint,
  lastDrainCheckpoint

vars ==
  << known,
     pending,
     delivered,
     blocked,
     stwRequested,
     stwAcked,
     deliveryCheckpoint,
     lastDrainCheckpoint >>

SeqSet(seq) ==
  { seq[i] : i \in 1..Len(seq) }

UniquePending ==
  \A i, j \in 1..Len(pending) : i # j => pending[i] # pending[j]

TypeOK ==
  /\ known \subseteq Actions
  /\ pending \in Seq(Actions)
  /\ delivered \subseteq Actions
  /\ blocked \subseteq Domains
  /\ stwRequested \subseteq Domains
  /\ stwAcked \subseteq Domains
  /\ deliveryCheckpoint \in [Actions -> Checkpoints \cup {"undelivered"}]
  /\ lastDrainCheckpoint \in Checkpoints

PendingAndDeliveredDisjoint ==
  SeqSet(pending) \cap delivered = {}

KnownActionsAreTracked ==
  known = SeqSet(pending) \cup delivered

EveryDeliveredActionHasExactlyOneCheckpoint ==
  \A action \in Actions :
    (action \in delivered) <=> (deliveryCheckpoint[action] # "undelivered")

DeliveredActionsOnlyUseLegalCheckpoints ==
  \A action \in delivered :
    deliveryCheckpoint[action] \in {"scheduler", "blocking_enter", "blocking_exit", "stw_pause"}

StopTheWorldAcknowledgementsAreRequested ==
  stwAcked \subseteq stwRequested

SmokeDepthBound ==
  TLCGet("level") < 7

Init ==
  /\ known = {}
  /\ pending = <<>>
  /\ delivered = {}
  /\ blocked = {}
  /\ stwRequested = {}
  /\ stwAcked = {}
  /\ deliveryCheckpoint = [action \in Actions |-> "undelivered"]
  /\ lastDrainCheckpoint = "none"

DrainAll(checkpoint) ==
  /\ checkpoint \in {"scheduler", "blocking_enter", "blocking_exit", "stw_pause"}
  /\ Len(pending) > 0
  /\ delivered' = delivered \cup SeqSet(pending)
  /\ pending' = <<>>
  /\ deliveryCheckpoint' =
      [action \in Actions |->
        IF action \in SeqSet(pending)
        THEN checkpoint
        ELSE deliveryCheckpoint[action]]
  /\ lastDrainCheckpoint' = checkpoint
  /\ UNCHANGED << known, blocked, stwRequested, stwAcked >>

EnqueueAction(action) ==
  /\ action \in Actions
  /\ action \notin known
  /\ known' = known \cup {action}
  /\ pending' = Append(pending, action)
  /\ UNCHANGED << delivered, blocked, stwRequested, stwAcked, deliveryCheckpoint, lastDrainCheckpoint >>

SchedulerSafepoint(domain) ==
  /\ domain \in Domains
  /\ domain \notin blocked
  /\ DrainAll("scheduler")

EnterBlocking(domain) ==
  /\ domain \in Domains
  /\ domain \notin blocked
  /\ IF Len(pending) > 0 THEN
       /\ DrainAll("blocking_enter")
       /\ blocked' = blocked \cup {domain}
       /\ UNCHANGED << known, stwRequested, stwAcked >>
     ELSE
       /\ blocked' = blocked \cup {domain}
       /\ UNCHANGED << known, pending, delivered, stwRequested, stwAcked, deliveryCheckpoint, lastDrainCheckpoint >>

ExitBlocking(domain) ==
  /\ domain \in blocked
  /\ IF Len(pending) > 0 THEN
       /\ DrainAll("blocking_exit")
       /\ blocked' = blocked \ {domain}
       /\ UNCHANGED << known, stwRequested, stwAcked >>
     ELSE
       /\ blocked' = blocked \ {domain}
       /\ UNCHANGED << known, pending, delivered, stwRequested, stwAcked, deliveryCheckpoint, lastDrainCheckpoint >>

RequestStopTheWorld ==
  /\ stwRequested = {}
  /\ stwAcked = {}
  /\ stwRequested' = Domains
  /\ stwAcked' = {}
  /\ UNCHANGED << known, pending, delivered, blocked, deliveryCheckpoint, lastDrainCheckpoint >>

AcknowledgePause(domain) ==
  /\ domain \in stwRequested \ stwAcked
  /\ IF Len(pending) > 0 THEN
       /\ DrainAll("stw_pause")
       /\ stwAcked' = stwAcked \cup {domain}
       /\ UNCHANGED << known, blocked, stwRequested >>
     ELSE
       /\ stwAcked' = stwAcked \cup {domain}
       /\ UNCHANGED << known, pending, delivered, blocked, stwRequested, deliveryCheckpoint, lastDrainCheckpoint >>

ResumeWorld ==
  /\ stwRequested # {}
  /\ stwRequested = stwAcked
  /\ stwRequested' = {}
  /\ stwAcked' = {}
  /\ UNCHANGED << known, pending, delivered, blocked, deliveryCheckpoint, lastDrainCheckpoint >>

Next ==
  \/ \E action \in Actions : EnqueueAction(action)
  \/ \E domain \in Domains : SchedulerSafepoint(domain)
  \/ \E domain \in Domains : EnterBlocking(domain)
  \/ \E domain \in Domains : ExitBlocking(domain)
  \/ RequestStopTheWorld
  \/ \E domain \in Domains : AcknowledgePause(domain)
  \/ ResumeWorld

Spec ==
  Init /\ [][Next]_vars

=============================================================================
