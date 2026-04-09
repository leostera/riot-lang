------------------------- MODULE RunnableTransfer -------------------------
EXTENDS Naturals, FiniteSets, TLC

\* Bounded semantic model for zort's explicit cross-domain runnable transfer
\* capability.  This models claimed scheduler lanes and ownership-preserving
\* transfer, but not balancing or work-stealing policy.

CONSTANTS
  Domains,
  Fibers,
  Workers,
  NoFiber,
  NoWorker

ASSUME Domains # {}
ASSUME Fibers # {}
ASSUME Workers # {}
ASSUME NoFiber \notin Fibers
ASSUME NoWorker \notin Workers

VARIABLES
  laneOwner,
  current,
  runnable,
  parked,
  suspended,
  fiberDomain,
  lastMutationClaimed

vars ==
  << laneOwner,
     current,
     runnable,
     parked,
     suspended,
     fiberDomain,
     lastMutationClaimed >>

OwnedByDomain(d) ==
  runnable[d]
  \cup parked[d]
  \cup suspended[d]
  \cup IF current[d] = NoFiber THEN {} ELSE {current[d]}

OwnershipCount(f) ==
  Cardinality({ d \in Domains : f \in OwnedByDomain(d) })

TypeOK ==
  /\ laneOwner \in [Domains -> (Workers \cup {NoWorker})]
  /\ current \in [Domains -> (Fibers \cup {NoFiber})]
  /\ runnable \in [Domains -> SUBSET Fibers]
  /\ parked \in [Domains -> SUBSET Fibers]
  /\ suspended \in [Domains -> SUBSET Fibers]
  /\ fiberDomain \in [Fibers -> Domains]
  /\ lastMutationClaimed \in BOOLEAN

FibersHaveExclusiveLaneOwnership ==
  \A f \in Fibers : OwnershipCount(f) <= 1

LaneContentsMatchFiberDomain ==
  \A d \in Domains :
    \A f \in OwnedByDomain(d) : fiberDomain[f] = d

CurrentFibersAreNotQueued ==
  \A d \in Domains :
    current[d] = NoFiber
    \/ /\ current[d] \notin runnable[d]
       /\ current[d] \notin parked[d]
       /\ current[d] \notin suspended[d]

LaneMutationsUseClaims ==
  lastMutationClaimed

SmokeDepthBound ==
  TLCGet("level") < 7

Init ==
  /\ laneOwner = [d \in Domains |-> NoWorker]
  /\ current = [d \in Domains |-> NoFiber]
  /\ runnable = [d \in Domains |-> {}]
  /\ parked = [d \in Domains |-> {}]
  /\ suspended = [d \in Domains |-> {}]
  /\ fiberDomain = [f \in Fibers |-> CHOOSE d \in Domains : TRUE]
  /\ lastMutationClaimed = TRUE

ClaimLane(worker, d) ==
  /\ worker \in Workers
  /\ d \in Domains
  /\ laneOwner[d] = NoWorker
  /\ laneOwner' = [laneOwner EXCEPT ![d] = worker]
  /\ UNCHANGED << current, runnable, parked, suspended, fiberDomain, lastMutationClaimed >>

ReleaseLane(worker, d) ==
  /\ worker \in Workers
  /\ d \in Domains
  /\ laneOwner[d] = worker
  /\ laneOwner' = [laneOwner EXCEPT ![d] = NoWorker]
  /\ UNCHANGED << current, runnable, parked, suspended, fiberDomain, lastMutationClaimed >>

EnqueueRunnable(worker, d, f) ==
  /\ worker \in Workers
  /\ d \in Domains
  /\ f \in Fibers
  /\ laneOwner[d] = worker
  /\ OwnershipCount(f) = 0
  /\ runnable' = [runnable EXCEPT ![d] = @ \cup {f}]
  /\ fiberDomain' = [fiberDomain EXCEPT ![f] = d]
  /\ lastMutationClaimed' = TRUE
  /\ UNCHANGED << laneOwner, current, parked, suspended >>

ScheduleRunnable(worker, d, f) ==
  /\ worker \in Workers
  /\ d \in Domains
  /\ f \in runnable[d]
  /\ laneOwner[d] = worker
  /\ current[d] = NoFiber
  /\ runnable' = [runnable EXCEPT ![d] = @ \ {f}]
  /\ current' = [current EXCEPT ![d] = f]
  /\ lastMutationClaimed' = TRUE
  /\ UNCHANGED << laneOwner, parked, suspended, fiberDomain >>

YieldCurrent(worker, d) ==
  /\ worker \in Workers
  /\ d \in Domains
  /\ laneOwner[d] = worker
  /\ current[d] # NoFiber
  /\ runnable' = [runnable EXCEPT ![d] = @ \cup {current[d]}]
  /\ current' = [current EXCEPT ![d] = NoFiber]
  /\ lastMutationClaimed' = TRUE
  /\ UNCHANGED << laneOwner, parked, suspended, fiberDomain >>

ParkCurrent(worker, d) ==
  /\ worker \in Workers
  /\ d \in Domains
  /\ laneOwner[d] = worker
  /\ current[d] # NoFiber
  /\ parked' = [parked EXCEPT ![d] = @ \cup {current[d]}]
  /\ current' = [current EXCEPT ![d] = NoFiber]
  /\ lastMutationClaimed' = TRUE
  /\ UNCHANGED << laneOwner, runnable, suspended, fiberDomain >>

SuspendCurrent(worker, d) ==
  /\ worker \in Workers
  /\ d \in Domains
  /\ laneOwner[d] = worker
  /\ current[d] # NoFiber
  /\ suspended' = [suspended EXCEPT ![d] = @ \cup {current[d]}]
  /\ current' = [current EXCEPT ![d] = NoFiber]
  /\ lastMutationClaimed' = TRUE
  /\ UNCHANGED << laneOwner, runnable, parked, fiberDomain >>

UnparkFiber(worker, d, f) ==
  /\ worker \in Workers
  /\ d \in Domains
  /\ f \in parked[d]
  /\ laneOwner[d] = worker
  /\ parked' = [parked EXCEPT ![d] = @ \ {f}]
  /\ runnable' = [runnable EXCEPT ![d] = @ \cup {f}]
  /\ lastMutationClaimed' = TRUE
  /\ UNCHANGED << laneOwner, current, suspended, fiberDomain >>

TransferRunnable(workerSrc, workerDst, src, dst, f) ==
  /\ workerSrc \in Workers
  /\ workerDst \in Workers
  /\ src \in Domains
  /\ dst \in Domains
  /\ src # dst
  /\ f \in runnable[src]
  /\ laneOwner[src] = workerSrc
  /\ laneOwner[dst] = workerDst
  /\ runnable' =
      [runnable EXCEPT
        ![src] = @ \ {f},
        ![dst] = @ \cup {f}]
  /\ fiberDomain' = [fiberDomain EXCEPT ![f] = dst]
  /\ lastMutationClaimed' = TRUE
  /\ UNCHANGED << laneOwner, current, parked, suspended >>

Next ==
  \/ \E worker \in Workers, d \in Domains : ClaimLane(worker, d)
  \/ \E worker \in Workers, d \in Domains : ReleaseLane(worker, d)
  \/ \E worker \in Workers, d \in Domains, f \in Fibers : EnqueueRunnable(worker, d, f)
  \/ \E worker \in Workers, d \in Domains, f \in Fibers : ScheduleRunnable(worker, d, f)
  \/ \E worker \in Workers, d \in Domains : YieldCurrent(worker, d)
  \/ \E worker \in Workers, d \in Domains : ParkCurrent(worker, d)
  \/ \E worker \in Workers, d \in Domains : SuspendCurrent(worker, d)
  \/ \E worker \in Workers, d \in Domains, f \in Fibers : UnparkFiber(worker, d, f)
  \/ \E workerSrc \in Workers, workerDst \in Workers,
       src \in Domains, dst \in Domains, f \in Fibers :
       TransferRunnable(workerSrc, workerDst, src, dst, f)

Spec ==
  Init /\ [][Next]_vars

=============================================================================
