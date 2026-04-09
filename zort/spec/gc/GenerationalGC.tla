------------------------- MODULE GenerationalGC -------------------------
EXTENDS Naturals, FiniteSets, TLC

\* Bounded semantic model for zort's generational collector protocol.
\*
\* Source contracts:
\* - zort/spec/gc-strategy.md
\* - zort/spec/gc-roots.md
\*
\* How to read this model:
\* - "Objects" are abstract heap identities.  They are not concrete blocks or
\*   slots.
\* - "edges" is the abstract object graph seen by the collector.
\* - "roots" is the abstract union of every active RootProvider.
\* - "remembered" is the major-to-nursery edge summary maintained by the
\*   mutator and by promotion itself.
\* - "phase" is the externally visible collector protocol, not a full
\*   implementation of OCaml's internal phase machine.
\*
\* This is intentionally smaller than the runtime:
\*
\* - it models domains, roots, nursery/major generations, remembered edges,
\*   and the stop-the-world gate around collection phases;
\* - it does not model object payload layout, forwarding-pointer bits,
\*   ephemerons, finalizer queues, or concrete allocator structures.

CONSTANTS Objects, Domains

ASSUME Objects # {}
ASSUME Domains # {}

Generations == {"nursery", "major"}
ObjectStates == {"live", "reclaimed"}
Phases == {"idle", "stw", "enumerate", "promote", "sweep"}

\* Machine state:
\* - objectGen[o]: current generation for object o.
\* - objectState[o]: whether object o is still live or has been reclaimed.
\* - edges[o]: the outgoing references stored in object o.
\* - roots[d]: the objects currently exposed by domain d's root providers.
\* - remembered: the summarized major-to-nursery edges the collector relies on
\*   during minor collection.
\* - phase: coarse collector phase.
\* - stwRequested / stwAcked: the domains asked to stop and the domains that
\*   have acknowledged that request.
VARIABLES
  objectGen,
  objectState,
  edges,
  roots,
  remembered,
  phase,
  stwRequested,
  stwAcked

vars ==
  << objectGen,
     objectState,
     edges,
     roots,
     remembered,
     phase,
     stwRequested,
     stwAcked >>

\* Convenience views used by the safety properties.
LiveObjects == { o \in Objects : objectState[o] = "live" }

AllRoots == UNION { roots[d] : d \in Domains }

EdgePairs ==
  { <<src, dst>> \in Objects \X Objects : dst \in edges[src] }

LiveEdgePairs ==
  { <<src, dst>> \in EdgePairs :
      /\ objectState[src] = "live"
      /\ objectState[dst] = "live" }

MajorToNurseryEdges ==
  { <<src, dst>> \in LiveEdgePairs :
      /\ objectGen[src] = "major"
      /\ objectGen[dst] = "nursery" }

RECURSIVE ReachAtDepth(_, _)
ReachAtDepth(frontier, depth) ==
  IF depth = 0 THEN
    frontier
  ELSE
    LET prev ==
          ReachAtDepth(frontier, depth - 1)
        expansion ==
          UNION { edges[o] : o \in (prev \cap LiveObjects) }
    IN
      prev \cup expansion

ReachableObjects ==
  ReachAtDepth(AllRoots, Cardinality(Objects))

StopTheWorldComplete ==
  stwRequested = stwAcked

\* Type/bounds invariants.
TypeOK ==
  /\ objectGen \in [Objects -> Generations]
  /\ objectState \in [Objects -> ObjectStates]
  /\ edges \in [Objects -> SUBSET Objects]
  /\ roots \in [Domains -> SUBSET Objects]
  /\ remembered \subseteq (Objects \X Objects)
  /\ phase \in Phases
  /\ stwRequested \subseteq Domains
  /\ stwAcked \subseteq Domains

\* Semantic invariants.
\*
\* Remembered-set coverage is the key minor-GC obligation: every live
\* major-to-nursery edge must be present in the remembered set, whether it was
\* created by a mutator write or by promotion during collection.
RememberedSetCoversMajorToNurseryEdges ==
  MajorToNurseryEdges \subseteq remembered

\* The collector must never reclaim an object that is still reachable through
\* the abstract roots plus the abstract heap graph.
ReachableObjectsRemainLive ==
  \A o \in ReachableObjects : objectState[o] = "live"

\* Once collection leaves "idle", the abstract STW gate must already be closed.
CollectionPhasesRequireStopTheWorld ==
  phase \in {"enumerate", "promote", "sweep"} => StopTheWorldComplete

\* Tiny TLC smoke cutoff only.
SmokeDepthBound ==
  TLCGet("level") < 7

\* Initial state: everything is nursery-resident, live, and unrooted.
Init ==
  /\ objectGen = [o \in Objects |-> "nursery"]
  /\ objectState = [o \in Objects |-> "live"]
  /\ edges = [o \in Objects |-> {}]
  /\ roots = [d \in Domains |-> {}]
  /\ remembered = {}
  /\ phase = "idle"
  /\ stwRequested = {}
  /\ stwAcked = {}

\* Root-provider bookkeeping in the mutator.
AddRoot(d, o) ==
  /\ phase = "idle"
  /\ d \in Domains
  /\ o \in LiveObjects
  /\ roots' = [roots EXCEPT ![d] = @ \cup {o}]
  /\ UNCHANGED << objectGen, objectState, edges, remembered, phase, stwRequested, stwAcked >>

\* Root-provider removal.
DropRoot(d, o) ==
  /\ phase = "idle"
  /\ d \in Domains
  /\ o \in roots[d]
  /\ roots' = [roots EXCEPT ![d] = @ \ {o}]
  /\ UNCHANGED << objectGen, objectState, edges, remembered, phase, stwRequested, stwAcked >>

\* Heap mutation.  Only major-to-nursery writes affect remembered-set state.
WriteEdge(src, dst) ==
  /\ phase = "idle"
  /\ src \in LiveObjects
  /\ dst \in LiveObjects
  /\ edges' = [edges EXCEPT ![src] = @ \cup {dst}]
  /\ remembered' =
      IF /\ objectGen[src] = "major"
         /\ objectGen[dst] = "nursery"
      THEN remembered \cup {<<src, dst>>}
      ELSE remembered
  /\ UNCHANGED << objectGen, objectState, roots, phase, stwRequested, stwAcked >>

\* The runtime requests a stop-the-world minor collection.
StartMinorCollection ==
  /\ phase = "idle"
  /\ phase' = "stw"
  /\ stwRequested' = Domains
  /\ stwAcked' = {}
  /\ UNCHANGED << objectGen, objectState, edges, roots, remembered >>

\* One participating domain has acknowledged the STW request.
AcknowledgeStopTheWorld(d) ==
  /\ phase = "stw"
  /\ d \in stwRequested \ stwAcked
  /\ stwAcked' = stwAcked \cup {d}
  /\ UNCHANGED << objectGen, objectState, edges, roots, remembered, phase, stwRequested >>

\* Collection may only enumerate roots once every requested domain has stopped.
BeginRootEnumeration ==
  /\ phase = "stw"
  /\ StopTheWorldComplete
  /\ phase' = "enumerate"
  /\ UNCHANGED << objectGen, objectState, edges, roots, remembered, stwRequested, stwAcked >>

\* Promotion moves one reachable nursery object to the major generation.
\*
\* The important subtlety is that promotion itself can create a new
\* major-to-nursery edge, so this action also updates the remembered set for
\* the promoted object's still-young children.
PromoteReachableNursery(o) ==
  /\ phase \in {"enumerate", "promote"}
  /\ o \in ReachableObjects
  /\ objectState[o] = "live"
  /\ objectGen[o] = "nursery"
  /\ objectGen' = [objectGen EXCEPT ![o] = "major"]
  /\ remembered' =
      remembered
      \cup { <<o, child>> :
               child \in { dst \in edges[o] :
                             /\ objectState[dst] = "live"
               /\ objectGen[dst] = "nursery" } }
  /\ phase' = "promote"
  /\ UNCHANGED << objectState, edges, roots, stwRequested, stwAcked >>

\* The collector can only start sweeping once no reachable nursery object
\* remains to be promoted.
BeginSweep ==
  /\ phase \in {"enumerate", "promote"}
  /\ \A o \in ReachableObjects :
       ~(objectState[o] = "live" /\ objectGen[o] = "nursery")
  /\ phase' = "sweep"
  /\ UNCHANGED << objectGen, objectState, edges, roots, remembered, stwRequested, stwAcked >>

\* Minor sweep only reclaims unreachable nursery objects.  It also removes the
\* reclaimed object from every abstract adjacency structure so later reachability
\* calculations stay finite and readable.
ReclaimUnreachableNursery(o) ==
  /\ phase = "sweep"
  /\ objectState[o] = "live"
  /\ objectGen[o] = "nursery"
  /\ o \notin ReachableObjects
  /\ objectState' = [objectState EXCEPT ![o] = "reclaimed"]
  /\ edges' =
      [src \in Objects |->
        IF src = o THEN {} ELSE edges[src] \ {o}]
  /\ roots' =
      [d \in Domains |->
        roots[d] \ {o}]
  /\ remembered' =
      { pair \in remembered : /\ pair[1] # o /\ pair[2] # o }
  /\ UNCHANGED << objectGen, phase, stwRequested, stwAcked >>

\* Once no live nursery object remains, the minor collection ends and the STW
\* bookkeeping resets.
FinishMinorCollection ==
  /\ phase = "sweep"
  /\ \A o \in Objects :
       ~(objectState[o] = "live" /\ objectGen[o] = "nursery")
  /\ phase' = "idle"
  /\ stwRequested' = {}
  /\ stwAcked' = {}
  /\ UNCHANGED << objectGen, objectState, edges, roots, remembered >>

\* The full transition relation.  This keeps the model readable by naming each
\* semantic step separately instead of inlining one giant action.
Next ==
  \/ \E d \in Domains, o \in Objects : AddRoot(d, o)
  \/ \E d \in Domains, o \in Objects : DropRoot(d, o)
  \/ \E src \in Objects, dst \in Objects : WriteEdge(src, dst)
  \/ StartMinorCollection
  \/ \E d \in Domains : AcknowledgeStopTheWorld(d)
  \/ BeginRootEnumeration
  \/ \E o \in Objects : PromoteReachableNursery(o)
  \/ BeginSweep
  \/ \E o \in Objects : ReclaimUnreachableNursery(o)
  \/ FinishMinorCollection

\* Standard safety specification: initialize, then repeatedly take one of the
\* bounded protocol steps above.
Spec ==
  Init /\ [][Next]_vars

=============================================================================
