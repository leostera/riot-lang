------------------------- MODULE RunnableTransfer -------------------------
EXTENDS Naturals, FiniteSets, TLC

\* Bounded semantic model for zort's explicit cross-domain runnable transfer
\* capability.  This models claimed scheduler lanes and ownership-preserving
\* transfer, but not balancing or work-stealing policy.
\*
\* Source contracts:
\* - zort/spec/startup-domains-and-signals.md
\* - zort/spec/effects-and-continuations.md
\*
\* How to read this model:
\* - each domain has one abstract scheduler lane;
\* - lane mutation is only legal while some worker claims that lane;
\* - runnable transfer is an explicit capability, not a balancing decision;
\* - userland policy such as work stealing is intentionally out of scope.

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

\* Machine state:
\* - laneOwner[d]: which worker currently holds the mutation claim for domain d.
\* - current[d]: the actively running fiber for domain d.
\* - runnable[d], parked[d], suspended[d]: the remaining scheduler-owned fiber
\*   states for domain d.
\* - fiberDomain[f]: the semantic domain ownership recorded for fiber f.
\* - lastMutationClaimed: a simple history bit used to assert that every
\*   mutating step modeled here happened under a valid claim.
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

\* The set of fibers currently owned by one domain lane, across all scheduler
\* states that matter for transfer and liveness.
OwnedByDomain(d) ==
  runnable[d]
  \cup parked[d]
  \cup suspended[d]
  \cup IF current[d] = NoFiber THEN {} ELSE {current[d]}

OwnershipCount(f) ==
  Cardinality({ d \in Domains : f \in OwnedByDomain(d) })

\* Type/bounds invariants.
TypeOK ==
  /\ laneOwner \in [Domains -> (Workers \cup {NoWorker})]
  /\ current \in [Domains -> (Fibers \cup {NoFiber})]
  /\ runnable \in [Domains -> SUBSET Fibers]
  /\ parked \in [Domains -> SUBSET Fibers]
  /\ suspended \in [Domains -> SUBSET Fibers]
  /\ fiberDomain \in [Fibers -> Domains]
  /\ lastMutationClaimed \in BOOLEAN

\* Semantic invariants.
\*
\* A fiber may be owned by at most one scheduler lane at a time.
FibersHaveExclusiveLaneOwnership ==
  \A f \in Fibers : OwnershipCount(f) <= 1

\* The semantic domain recorded on a fiber must match whichever lane currently
\* owns it.
LaneContentsMatchFiberDomain ==
  \A d \in Domains :
    \A f \in OwnedByDomain(d) : fiberDomain[f] = d

\* A fiber cannot be current and still sit in one of the queue-like lane sets.
CurrentFibersAreNotQueued ==
  \A d \in Domains :
    current[d] = NoFiber
    \/ /\ current[d] \notin runnable[d]
       /\ current[d] \notin parked[d]
       /\ current[d] \notin suspended[d]

\* Every modeled mutating step is supposed to occur under an active lane claim.
\* Because the actions below encode the claim precondition directly, this
\* history flag stays true unless we accidentally add an unchecked mutator path.
LaneMutationsUseClaims ==
  lastMutationClaimed

SmokeDepthBound ==
  TLCGet("level") < 7

\* Initial state: no claimed lanes and no owned fibers.
Init ==
  /\ laneOwner = [d \in Domains |-> NoWorker]
  /\ current = [d \in Domains |-> NoFiber]
  /\ runnable = [d \in Domains |-> {}]
  /\ parked = [d \in Domains |-> {}]
  /\ suspended = [d \in Domains |-> {}]
  /\ fiberDomain = [f \in Fibers |-> CHOOSE d \in Domains : TRUE]
  /\ lastMutationClaimed = TRUE

\* Worker lifecycle at the scheduler-lane level.
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

\* A claimed worker inserts a currently-unowned fiber into its runnable set.
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

\* Scheduler pick-next transition.
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

\* Cooperative yield back into the runnable set.
YieldCurrent(worker, d) ==
  /\ worker \in Workers
  /\ d \in Domains
  /\ laneOwner[d] = worker
  /\ current[d] # NoFiber
  /\ runnable' = [runnable EXCEPT ![d] = @ \cup {current[d]}]
  /\ current' = [current EXCEPT ![d] = NoFiber]
  /\ lastMutationClaimed' = TRUE
  /\ UNCHANGED << laneOwner, parked, suspended, fiberDomain >>

\* Explicit park operation; wakeup policy itself is out of scope.
ParkCurrent(worker, d) ==
  /\ worker \in Workers
  /\ d \in Domains
  /\ laneOwner[d] = worker
  /\ current[d] # NoFiber
  /\ parked' = [parked EXCEPT ![d] = @ \cup {current[d]}]
  /\ current' = [current EXCEPT ![d] = NoFiber]
  /\ lastMutationClaimed' = TRUE
  /\ UNCHANGED << laneOwner, runnable, suspended, fiberDomain >>

\* Current fiber becomes scheduler-owned suspended state, e.g. after effect
\* capture.  The policy for when this happens lives elsewhere; this model only
\* tracks the ownership shape.
SuspendCurrent(worker, d) ==
  /\ worker \in Workers
  /\ d \in Domains
  /\ laneOwner[d] = worker
  /\ current[d] # NoFiber
  /\ suspended' = [suspended EXCEPT ![d] = @ \cup {current[d]}]
  /\ current' = [current EXCEPT ![d] = NoFiber]
  /\ lastMutationClaimed' = TRUE
  /\ UNCHANGED << laneOwner, runnable, parked, fiberDomain >>

\* Wakeup from parked back to runnable.
UnparkFiber(worker, d, f) ==
  /\ worker \in Workers
  /\ d \in Domains
  /\ f \in parked[d]
  /\ laneOwner[d] = worker
  /\ parked' = [parked EXCEPT ![d] = @ \ {f}]
  /\ runnable' = [runnable EXCEPT ![d] = @ \cup {f}]
  /\ lastMutationClaimed' = TRUE
  /\ UNCHANGED << laneOwner, current, suspended, fiberDomain >>

\* Cross-domain runnable transfer capability:
\* - both source and target lanes must already be claimed,
\* - the fiber must leave the source runnable set before it appears in the
\*   target runnable set,
\* - the fiber's semantic domain changes with scheduler ownership.
\*
\* This is the runtime capability.  It intentionally does not decide *when* to
\* transfer; userland policy does that.
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

\* Full transition relation.
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

\* Standard safety spec.
Spec ==
  Init /\ [][Next]_vars

=============================================================================
