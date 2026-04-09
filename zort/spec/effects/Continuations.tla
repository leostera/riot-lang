------------------------- MODULE Continuations -------------------------
EXTENDS Naturals, FiniteSets, TLC

\* Bounded semantic model for one-shot resumable continuations in zort.
\*
\* Source contracts:
\* - zort/spec/effects-and-continuations.md
\* - zort/spec/exceptions-callbacks-and-backtraces.md
\*
\* How to read this model:
\* - a "fiber" is an abstract unit of managed-stack execution state;
\* - a "continuation" is a one-shot handle that may temporarily own a
\*   suspended fiber and its roots;
\* - "handlers[f]" is only the visible set of handled effect labels for fiber f;
\* - callback boundaries are modeled as a cut in parent traversal, not as a
\*   separate native-stack subsystem.
\*
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

\* Machine state:
\* - fiberState / fiberDomain: abstract control-state and current domain
\*   ownership for each fiber.
\* - parent: the visible parent-fiber chain used for handler search.
\* - callbackBoundary: whether parent traversal is cut at this fiber.
\* - handlers: which effect labels this fiber currently handles.
\* - current[d]: the active fiber for domain d, if any.
\* - contState / contFiber / contRootsOwned / contResumeDomain / contUsed:
\*   the one-shot continuation protocol state.
\* - last*: history/debug fields used only to state search-start invariants
\*   explicitly.  They are not meant to represent full runtime state.
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

\* Callback boundaries deliberately hide the parent chain from effect search.
VisibleParent(f) ==
  IF callbackBoundary[f] THEN NoFiber ELSE parent[f]

\* Bounded search for a matching effect handler.
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

\* Type/bounds invariants.
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

\* Semantic invariants.
\*
\* No fiber may be the active current fiber of multiple domains at once.
CurrentFibersAreUnique ==
  \A f \in Fibers : FiberCurrentCount(f) <= 1

\* A captured continuation owns suspended roots exactly while it is in the
\* "captured" state.
CapturedContinuationsOwnRoots ==
  \A c \in Continuations :
    contState[c] = "captured" <=> contRootsOwned[c]

\* Once a continuation has captured a fiber, that fiber is no longer allowed to
\* remain current in any domain.
CapturedFibersAreNotCurrent ==
  \A c \in Continuations :
    contState[c] = "captured"
    =>
    \A d \in Domains : current[d] # contFiber[c]

\* Current fibers must agree with the domain slot they occupy.  This is the
\* stable state property behind cross-domain resume.
CurrentFibersMatchTheirDomains ==
  \A d \in Domains :
    current[d] = NoFiber \/ fiberDomain[current[d]] = d

\* One-shot usage is tracked by contUsed.  Once a continuation has been resumed
\* or dropped, it must never become "captured" again.
UsedContinuationsDoNotReturnToCaptured ==
  \A c \in Continuations :
    contUsed[c] => contState[c] # "captured"

\* Callback boundaries must cut parent traversal exactly at the marked fiber.
CallbackBoundariesCutParentTraversal ==
  \A f \in Fibers :
    callbackBoundary[f] => VisibleParent(f) = NoFiber

\* These next two invariants use the history fields to make the search contract
\* explicit and reviewable.
PerformStartsSearchAtCurrentFiber ==
  lastSearchMode = "perform" => lastSearchStart = lastCapturedFiber

ReperformStartsSearchAtVisibleParent ==
  lastSearchMode = "reperform" => lastSearchStart = lastVisibleParentAtSearch

SmokeDepthBound ==
  TLCGet("level") < 7

\* Initial state: no current fibers, no handlers, no captured continuations.
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

\* A domain activates an idle fiber as its current execution context.
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

\* This is the abstract parent-fiber linkage manipulated by perform/reperform
\* and by callback-boundary setup in the real runtime.
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

\* Install a handler for one effect label on the target fiber.
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

\* Enter and exit callback boundaries.  These are explicit because callback
\* boundaries are part of the runtime-visible effect contract.
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

\* `perform` when a visible handler exists:
\* - the current fiber becomes suspended,
\* - the continuation captures that fiber,
\* - the history fields remember where the search started.
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

\* `perform` when no visible handler exists.  The main semantic effect here is
\* that the search still starts at the current fiber, but the result is the
\* explicit `UnhandledEffect` error instead of hidden boundary crossing.
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

\* `reperform` is modeled as a distinct search rule:
\* - it still captures the current fiber into a fresh continuation,
\* - but it starts handler search at the visible parent instead of the current
\*   fiber.
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

\* Resume is the one-shot ownership handoff:
\* - the captured fiber becomes current again,
\* - the target domain becomes that fiber's domain,
\* - the continuation stops owning roots and is marked used.
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

\* Reusing a consumed continuation must fail explicitly.
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

\* Dropping a captured continuation consumes it without resuming the fiber.
\* In this bounded model, the captured fiber becomes `done` to make the loss of
\* ownership explicit and easy to check.
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

\* Full transition relation for the continuation protocol.
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

\* Standard safety spec.
Spec ==
  Init /\ [][Next]_vars

=============================================================================
