------------------------- MODULE GenerationalGC -------------------------
EXTENDS Naturals, FiniteSets, TLC

\* Bounded semantic model for zort's generational collector protocol.
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

TypeOK ==
  /\ objectGen \in [Objects -> Generations]
  /\ objectState \in [Objects -> ObjectStates]
  /\ edges \in [Objects -> SUBSET Objects]
  /\ roots \in [Domains -> SUBSET Objects]
  /\ remembered \subseteq (Objects \X Objects)
  /\ phase \in Phases
  /\ stwRequested \subseteq Domains
  /\ stwAcked \subseteq Domains

RememberedSetCoversMajorToNurseryEdges ==
  MajorToNurseryEdges \subseteq remembered

ReachableObjectsRemainLive ==
  \A o \in ReachableObjects : objectState[o] = "live"

CollectionPhasesRequireStopTheWorld ==
  phase \in {"enumerate", "promote", "sweep"} => StopTheWorldComplete

MajorGenerationIsMonotonic ==
  \A o \in Objects :
    objectGen[o] = "major"
    => objectState[o] # "reclaimed" \/ objectState[o] = "reclaimed"

\* Tiny TLC smoke cutoff only.
SmokeDepthBound ==
  TLCGet("level") < 7

Init ==
  /\ objectGen = [o \in Objects |-> "nursery"]
  /\ objectState = [o \in Objects |-> "live"]
  /\ edges = [o \in Objects |-> {}]
  /\ roots = [d \in Domains |-> {}]
  /\ remembered = {}
  /\ phase = "idle"
  /\ stwRequested = {}
  /\ stwAcked = {}

AddRoot(d, o) ==
  /\ phase = "idle"
  /\ d \in Domains
  /\ o \in LiveObjects
  /\ roots' = [roots EXCEPT ![d] = @ \cup {o}]
  /\ UNCHANGED << objectGen, objectState, edges, remembered, phase, stwRequested, stwAcked >>

DropRoot(d, o) ==
  /\ phase = "idle"
  /\ d \in Domains
  /\ o \in roots[d]
  /\ roots' = [roots EXCEPT ![d] = @ \ {o}]
  /\ UNCHANGED << objectGen, objectState, edges, remembered, phase, stwRequested, stwAcked >>

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

StartMinorCollection ==
  /\ phase = "idle"
  /\ phase' = "stw"
  /\ stwRequested' = Domains
  /\ stwAcked' = {}
  /\ UNCHANGED << objectGen, objectState, edges, roots, remembered >>

AcknowledgeStopTheWorld(d) ==
  /\ phase = "stw"
  /\ d \in stwRequested \ stwAcked
  /\ stwAcked' = stwAcked \cup {d}
  /\ UNCHANGED << objectGen, objectState, edges, roots, remembered, phase, stwRequested >>

BeginRootEnumeration ==
  /\ phase = "stw"
  /\ StopTheWorldComplete
  /\ phase' = "enumerate"
  /\ UNCHANGED << objectGen, objectState, edges, roots, remembered, stwRequested, stwAcked >>

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

BeginSweep ==
  /\ phase \in {"enumerate", "promote"}
  /\ \A o \in ReachableObjects :
       ~(objectState[o] = "live" /\ objectGen[o] = "nursery")
  /\ phase' = "sweep"
  /\ UNCHANGED << objectGen, objectState, edges, roots, remembered, stwRequested, stwAcked >>

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

FinishMinorCollection ==
  /\ phase = "sweep"
  /\ \A o \in Objects :
       ~(objectState[o] = "live" /\ objectGen[o] = "nursery")
  /\ phase' = "idle"
  /\ stwRequested' = {}
  /\ stwAcked' = {}
  /\ UNCHANGED << objectGen, objectState, edges, roots, remembered >>

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

Spec ==
  Init /\ [][Next]_vars

=============================================================================
