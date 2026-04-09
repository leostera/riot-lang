------------------------- MODULE Continuations -------------------------
EXTENDS Naturals, FiniteSets, TLC

\* Bounded semantic model for one-shot resumable continuations in zort.
\* This models:
\*
\* - fibers, domains, handler visibility, callback boundaries,
\* - continuation capture / resume / drop,
\* - cross-domain resume and one-shot ownership.
\*
\* It does not model:
\*
\* - native stack frames,
\* - exact backtrace payloads,
\* - assembly stack switching,
\* - scheduler queue policy.

CONSTANTS
  Fibers,
  Domains,
  Continuations,
  Effects,
  NoFiber,
  NoDomain

ASSUME Fibers # {}
ASSUME Domains # {}
ASSUME Continuations # {}
ASSUME Effects # {}
ASSUME NoFiber \notin Fibers
ASSUME NoDomain \notin Domains

FiberStates == {"idle", "active", "suspended", "done"}
ContinuationStates == {"empty", "captured", "resumed", "dropped"}
SearchModes == {"none", "perform", "reperform"}
Errors == {"none", "UnhandledEffect", "AlreadyResumed"}

VARIABLES
  fiberState,
  fiberDomain,
  parent,
  callbackBoundary,
  handlers,
  current,
  contState,
  contFiber,
  contRootsOwned,
  contResumeDomain,
  contUsed,
  lastSearchMode,
  lastSearchStart,
  lastVisibleParentAtSearch,
  lastCapturedFiber,
  lastError

vars ==
  << fiberState,
     fiberDomain,
     parent,
     callbackBoundary,
     handlers,
     current,
     contState,
     contFiber,
     contRootsOwned,
     contResumeDomain,
     contUsed,
     lastSearchMode,
     lastSearchStart,
     lastVisibleParentAtSearch,
     lastCapturedFiber,
     lastError >>

VisibleParent(f) ==
  IF callbackBoundary[f] THEN NoFiber ELSE parent[f]

RECURSIVE SearchAtDepth(_, _, _)
SearchAtDepth(f, e, depth) ==
  IF /\ f = NoFiber
     \/ depth = 0
  THEN
    FALSE
  ELSE IF e \in handlers[f] THEN
    TRUE
  ELSE
    SearchAtDepth(VisibleParent(f), e, depth - 1)

HandlerVisibleFrom(f, e) ==
  SearchAtDepth(f, e, Cardinality(Fibers))

FiberCurrentCount(f) ==
  Cardinality({ d \in Domains : current[d] = f })

TypeOK ==
  /\ fiberState \in [Fibers -> FiberStates]
  /\ fiberDomain \in [Fibers -> Domains]
  /\ parent \in [Fibers -> (Fibers \cup {NoFiber})]
  /\ callbackBoundary \in [Fibers -> BOOLEAN]
  /\ handlers \in [Fibers -> SUBSET Effects]
  /\ current \in [Domains -> (Fibers \cup {NoFiber})]
  /\ contState \in [Continuations -> ContinuationStates]
  /\ contFiber \in [Continuations -> (Fibers \cup {NoFiber})]
  /\ contRootsOwned \in [Continuations -> BOOLEAN]
  /\ contResumeDomain \in [Continuations -> (Domains \cup {NoDomain})]
  /\ contUsed \in [Continuations -> BOOLEAN]
  /\ lastSearchMode \in SearchModes
  /\ lastSearchStart \in Fibers \cup {NoFiber}
  /\ lastVisibleParentAtSearch \in Fibers \cup {NoFiber}
  /\ lastCapturedFiber \in Fibers \cup {NoFiber}
  /\ lastError \in Errors

CurrentFibersAreUnique ==
  \A f \in Fibers : FiberCurrentCount(f) <= 1

CapturedContinuationsOwnRoots ==
  \A c \in Continuations :
    contState[c] = "captured" <=> contRootsOwned[c]

CapturedFibersAreNotCurrent ==
  \A c \in Continuations :
    contState[c] = "captured"
    =>
    \A d \in Domains : current[d] # contFiber[c]

CurrentFibersMatchTheirDomains ==
  \A d \in Domains :
    current[d] = NoFiber \/ fiberDomain[current[d]] = d

UsedContinuationsDoNotReturnToCaptured ==
  \A c \in Continuations :
    contUsed[c] => contState[c] # "captured"

CallbackBoundariesCutParentTraversal ==
  \A f \in Fibers :
    callbackBoundary[f] => VisibleParent(f) = NoFiber

PerformStartsSearchAtCurrentFiber ==
  lastSearchMode = "perform" => lastSearchStart = lastCapturedFiber

ReperformStartsSearchAtVisibleParent ==
  lastSearchMode = "reperform" => lastSearchStart = lastVisibleParentAtSearch

SmokeDepthBound ==
  TLCGet("level") < 7

Init ==
  /\ fiberState = [f \in Fibers |-> "idle"]
  /\ fiberDomain = [f \in Fibers |-> CHOOSE d \in Domains : TRUE]
  /\ parent = [f \in Fibers |-> NoFiber]
  /\ callbackBoundary = [f \in Fibers |-> FALSE]
  /\ handlers = [f \in Fibers |-> {}]
  /\ current = [d \in Domains |-> NoFiber]
  /\ contState = [c \in Continuations |-> "empty"]
  /\ contFiber = [c \in Continuations |-> NoFiber]
  /\ contRootsOwned = [c \in Continuations |-> FALSE]
  /\ contResumeDomain = [c \in Continuations |-> NoDomain]
  /\ contUsed = [c \in Continuations |-> FALSE]
  /\ lastSearchMode = "none"
  /\ lastSearchStart = NoFiber
  /\ lastVisibleParentAtSearch = NoFiber
  /\ lastCapturedFiber = NoFiber
  /\ lastError = "none"

ActivateFiber(d, f) ==
  /\ d \in Domains
  /\ f \in Fibers
  /\ current[d] = NoFiber
  /\ fiberState[f] = "idle"
  /\ FiberCurrentCount(f) = 0
  /\ current' = [current EXCEPT ![d] = f]
  /\ fiberState' = [fiberState EXCEPT ![f] = "active"]
  /\ fiberDomain' = [fiberDomain EXCEPT ![f] = d]
  /\ UNCHANGED
       << parent,
          callbackBoundary,
          handlers,
          contState,
          contFiber,
          contRootsOwned,
          contResumeDomain,
          contUsed,
          lastSearchMode,
          lastSearchStart,
          lastVisibleParentAtSearch,
          lastCapturedFiber,
          lastError >>

AttachParent(child, parentFiber) ==
  /\ child \in Fibers
  /\ parentFiber \in Fibers \cup {NoFiber}
  /\ child # parentFiber
  /\ parent' = [parent EXCEPT ![child] = parentFiber]
  /\ UNCHANGED
       << fiberState,
          fiberDomain,
          callbackBoundary,
          handlers,
          current,
          contState,
          contFiber,
          contRootsOwned,
          contResumeDomain,
          contUsed,
          lastSearchMode,
          lastSearchStart,
          lastVisibleParentAtSearch,
          lastCapturedFiber,
          lastError >>

InstallHandler(f, e) ==
  /\ f \in Fibers
  /\ e \in Effects
  /\ handlers' = [handlers EXCEPT ![f] = @ \cup {e}]
  /\ UNCHANGED
       << fiberState,
          fiberDomain,
          parent,
          callbackBoundary,
          current,
          contState,
          contFiber,
          contRootsOwned,
          contResumeDomain,
          contUsed,
          lastSearchMode,
          lastSearchStart,
          lastVisibleParentAtSearch,
          lastCapturedFiber,
          lastError >>

EnterCallbackBoundary(f) ==
  /\ f \in Fibers
  /\ callbackBoundary' = [callbackBoundary EXCEPT ![f] = TRUE]
  /\ UNCHANGED
       << fiberState,
          fiberDomain,
          parent,
          handlers,
          current,
          contState,
          contFiber,
          contRootsOwned,
          contResumeDomain,
          contUsed,
          lastSearchMode,
          lastSearchStart,
          lastVisibleParentAtSearch,
          lastCapturedFiber,
          lastError >>

ExitCallbackBoundary(f) ==
  /\ f \in Fibers
  /\ callbackBoundary[f]
  /\ callbackBoundary' = [callbackBoundary EXCEPT ![f] = FALSE]
  /\ UNCHANGED
       << fiberState,
          fiberDomain,
          parent,
          handlers,
          current,
          contState,
          contFiber,
          contRootsOwned,
          contResumeDomain,
          contUsed,
          lastSearchMode,
          lastSearchStart,
          lastVisibleParentAtSearch,
          lastCapturedFiber,
          lastError >>

PerformHandled(d, c, e) ==
  LET f == current[d] IN
  /\ d \in Domains
  /\ c \in Continuations
  /\ e \in Effects
  /\ f # NoFiber
  /\ contState[c] = "empty"
  /\ ~contUsed[c]
  /\ HandlerVisibleFrom(f, e)
  /\ current' = [current EXCEPT ![d] = NoFiber]
  /\ fiberState' = [fiberState EXCEPT ![f] = "suspended"]
  /\ contState' = [contState EXCEPT ![c] = "captured"]
  /\ contFiber' = [contFiber EXCEPT ![c] = f]
  /\ contRootsOwned' = [contRootsOwned EXCEPT ![c] = TRUE]
  /\ contResumeDomain' = [contResumeDomain EXCEPT ![c] = NoDomain]
  /\ lastSearchMode' = "perform"
  /\ lastSearchStart' = f
  /\ lastVisibleParentAtSearch' = VisibleParent(f)
  /\ lastCapturedFiber' = f
  /\ lastError' = "none"
  /\ UNCHANGED << fiberDomain, parent, callbackBoundary, handlers, contUsed >>

PerformUnhandled(d, c, e) ==
  LET f == current[d] IN
  /\ d \in Domains
  /\ c \in Continuations
  /\ e \in Effects
  /\ f # NoFiber
  /\ contState[c] = "empty"
  /\ ~contUsed[c]
  /\ ~HandlerVisibleFrom(f, e)
  /\ lastSearchMode' = "perform"
  /\ lastSearchStart' = f
  /\ lastVisibleParentAtSearch' = VisibleParent(f)
  /\ lastCapturedFiber' = f
  /\ lastError' = "UnhandledEffect"
  /\ UNCHANGED
       << fiberState,
          fiberDomain,
          parent,
          callbackBoundary,
          handlers,
          current,
          contState,
          contFiber,
          contRootsOwned,
          contResumeDomain,
          contUsed >>

ReperformHandled(d, c, e) ==
  LET f == current[d] IN
  /\ d \in Domains
  /\ c \in Continuations
  /\ e \in Effects
  /\ f # NoFiber
  /\ contState[c] = "empty"
  /\ ~contUsed[c]
  /\ HandlerVisibleFrom(VisibleParent(f), e)
  /\ current' = [current EXCEPT ![d] = NoFiber]
  /\ fiberState' = [fiberState EXCEPT ![f] = "suspended"]
  /\ contState' = [contState EXCEPT ![c] = "captured"]
  /\ contFiber' = [contFiber EXCEPT ![c] = f]
  /\ contRootsOwned' = [contRootsOwned EXCEPT ![c] = TRUE]
  /\ contResumeDomain' = [contResumeDomain EXCEPT ![c] = NoDomain]
  /\ lastSearchMode' = "reperform"
  /\ lastSearchStart' = VisibleParent(f)
  /\ lastVisibleParentAtSearch' = VisibleParent(f)
  /\ lastCapturedFiber' = f
  /\ lastError' = "none"
  /\ UNCHANGED << fiberDomain, parent, callbackBoundary, handlers, contUsed >>

ResumeContinuation(c, d) ==
  /\ c \in Continuations
  /\ d \in Domains
  /\ contState[c] = "captured"
  /\ current[d] = NoFiber
  /\ contFiber[c] # NoFiber
  /\ current' = [current EXCEPT ![d] = contFiber[c]]
  /\ fiberState' = [fiberState EXCEPT ![contFiber[c]] = "active"]
  /\ fiberDomain' = [fiberDomain EXCEPT ![contFiber[c]] = d]
  /\ contState' = [contState EXCEPT ![c] = "resumed"]
  /\ contRootsOwned' = [contRootsOwned EXCEPT ![c] = FALSE]
  /\ contResumeDomain' = [contResumeDomain EXCEPT ![c] = d]
  /\ contUsed' = [contUsed EXCEPT ![c] = TRUE]
  /\ lastError' = "none"
  /\ UNCHANGED
       << parent,
          callbackBoundary,
          handlers,
          contFiber,
          lastSearchMode,
          lastSearchStart,
          lastVisibleParentAtSearch,
          lastCapturedFiber >>

ResumeAlreadyUsed(c, d) ==
  /\ c \in Continuations
  /\ d \in Domains
  /\ contUsed[c]
  /\ lastError' = "AlreadyResumed"
  /\ UNCHANGED
       << fiberState,
          fiberDomain,
          parent,
          callbackBoundary,
          handlers,
          current,
          contState,
          contFiber,
          contRootsOwned,
          contResumeDomain,
          contUsed,
          lastSearchMode,
          lastSearchStart,
          lastVisibleParentAtSearch,
          lastCapturedFiber >>

DropCapturedContinuation(c) ==
  /\ c \in Continuations
  /\ contState[c] = "captured"
  /\ contFiber[c] # NoFiber
  /\ contState' = [contState EXCEPT ![c] = "dropped"]
  /\ contRootsOwned' = [contRootsOwned EXCEPT ![c] = FALSE]
  /\ contResumeDomain' = [contResumeDomain EXCEPT ![c] = NoDomain]
  /\ contUsed' = [contUsed EXCEPT ![c] = TRUE]
  /\ fiberState' = [fiberState EXCEPT ![contFiber[c]] = "done"]
  /\ lastError' = "none"
  /\ UNCHANGED
       << fiberDomain,
          parent,
          callbackBoundary,
          handlers,
          current,
          contFiber,
          lastSearchMode,
          lastSearchStart,
          lastVisibleParentAtSearch,
          lastCapturedFiber >>

Next ==
  \/ \E d \in Domains, f \in Fibers : ActivateFiber(d, f)
  \/ \E child \in Fibers, parentFiber \in Fibers \cup {NoFiber} :
       AttachParent(child, parentFiber)
  \/ \E f \in Fibers, e \in Effects : InstallHandler(f, e)
  \/ \E f \in Fibers : EnterCallbackBoundary(f)
  \/ \E f \in Fibers : ExitCallbackBoundary(f)
  \/ \E d \in Domains, c \in Continuations, e \in Effects : PerformHandled(d, c, e)
  \/ \E d \in Domains, c \in Continuations, e \in Effects : PerformUnhandled(d, c, e)
  \/ \E d \in Domains, c \in Continuations, e \in Effects : ReperformHandled(d, c, e)
  \/ \E c \in Continuations, d \in Domains : ResumeContinuation(c, d)
  \/ \E c \in Continuations, d \in Domains : ResumeAlreadyUsed(c, d)
  \/ \E c \in Continuations : DropCapturedContinuation(c)

Spec ==
  Init /\ [][Next]_vars

=============================================================================
