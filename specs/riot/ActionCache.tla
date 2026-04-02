---------------------------- MODULE ActionCache ----------------------------
EXTENDS FiniteSets, Naturals, Sequences, TLC

\* Readable PlusCal slice for the current action-level cache contract in:
\*
\* - packages/riot-planner/src/action.ml
\* - packages/riot-planner/src/action_node.ml
\* - packages/riot-executor/src/action_executor.ml
\* - packages/riot-store/src/store.ml
\*
\* This first slice focuses only on `BuildForeignDependency` actions.  We keep
\* the fields that matter to the current cache-key question:
\*
\* - `StableKeyFields[a]` abstracts the other hashed action fields
\*   (name, path, outputs, env).
\* - `BuildCmd[a]` is modeled explicitly because the current OCaml hash sorts
\*   the command list before hashing it.
\*
\* In the real system the key is a SHA256 digest.  In this slice we model the
\* digest by the normalized value it is derived from so hash-equivalence is
\* directly visible in TLC.

CONSTANTS
  Actions,
  Commands,
  StableFieldIds,
  StableKeyFields,
  BuildCmd,
  NoArtifact,
  NoAction

RECURSIVE CountInSeq(_, _)

CountInSeq(seq, value) ==
  IF Len(seq) = 0 THEN
    0
  ELSE
    (IF seq[1] = value THEN 1 ELSE 0) + CountInSeq(Tail(seq), value)

CommandBag(cmds) ==
  [cmd \in Commands |-> CountInSeq(cmds, cmd)]

ActionResult(a) ==
  BuildCmd[a]

ActionHash(a) ==
  << StableKeyFields[a], CommandBag(BuildCmd[a]) >>

Hashes ==
  { ActionHash(a) : a \in Actions }

ActionOutputs ==
  { ActionResult(a) : a \in Actions }

HashCollisionExists ==
  \E a \in Actions :
    \E b \in Actions :
      /\ a # b
      /\ StableKeyFields[a] = StableKeyFields[b]
      /\ BuildCmd[a] # BuildCmd[b]
      /\ ActionHash(a) = ActionHash(b)

ASSUME Actions # {}
ASSUME Commands # {}
ASSUME StableFieldIds # {}
ASSUME StableKeyFields \in [Actions -> StableFieldIds]
ASSUME \A a \in Actions : BuildCmd[a] \in Seq(Commands)
ASSUME \A a \in Actions : Len(BuildCmd[a]) > 0
ASSUME NoAction \notin Actions
ASSUME NoArtifact \notin ActionOutputs

\* Small named model presets so the `.cfg` files can stay simple and readable.
SmokeActions ==
  {"BuildA", "BuildB"}

SmokeCommands ==
  {"Prep", "Compile"}

SmokeStableFieldIds ==
  {"FieldA", "FieldB"}

SmokeStableKeyFields ==
  [action \in SmokeActions |->
    IF action = "BuildA" THEN "FieldA" ELSE "FieldB"]

SmokeBuildCmd ==
  [action \in SmokeActions |->
    IF action = "BuildA" THEN <<"Prep", "Compile">> ELSE <<"Compile">>]

SmokeNoArtifact ==
  <<>>

SmokeNoAction ==
  "NoAction"

CommandOrderBugActions ==
  {"FirstBuild", "SecondBuild"}

CommandOrderBugCommands ==
  {"Prep", "Compile"}

CommandOrderBugStableFieldIds ==
  {"SharedFields"}

CommandOrderBugStableKeyFields ==
  [action \in CommandOrderBugActions |-> "SharedFields"]

CommandOrderBugBuildCmd ==
  [action \in CommandOrderBugActions |->
    IF action = "FirstBuild"
      THEN <<"Prep", "Compile">>
      ELSE <<"Compile", "Prep">>]

CommandOrderBugNoArtifact ==
  <<>>

CommandOrderBugNoAction ==
  "NoAction"

(* --algorithm ActionCache
variables
  pending = Actions,
  cache = [h \in Hashes |-> NoArtifact],
  cacheOwner = [h \in Hashes |-> NoAction],
  materialized = [a \in Actions |-> NoArtifact],
  completion = [a \in Actions |-> "Pending"],
  current = NoAction,
  history = <<>>;

begin
  Loop:
    while pending # {} do
    ChooseAction:
      with nextAction \in pending do
        current := nextAction;
        pending := pending \ {nextAction};
      end with;

    LookupCache:
      if cache[ActionHash(current)] # NoArtifact then
        CacheHit:
          materialized[current] := cache[ActionHash(current)];
          completion[current] := "Cached";
          history := Append(
            history,
            [ kind |-> "Cached",
              action |-> current,
              hash |-> ActionHash(current),
              owner |-> cacheOwner[ActionHash(current)],
              output |-> cache[ActionHash(current)] ]
          );
      else
        CacheMiss:
          cache := [cache EXCEPT ![ActionHash(current)] = ActionResult(current)];
          cacheOwner := [cacheOwner EXCEPT ![ActionHash(current)] = current];
          materialized[current] := ActionResult(current);
          completion[current] := "Fresh";
          history := Append(
            history,
            [ kind |-> "Fresh",
              action |-> current,
              hash |-> ActionHash(current),
              owner |-> current,
              output |-> ActionResult(current) ]
          );
      end if;
  end while;

  Finished:
    skip;
end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "c73585f0" /\ chksum(tla) = "72a4ea2e")
VARIABLES pending, cache, cacheOwner, materialized, completion, current, 
          history, pc

vars == << pending, cache, cacheOwner, materialized, completion, current, 
           history, pc >>

Init == (* Global variables *)
        /\ pending = Actions
        /\ cache = [h \in Hashes |-> NoArtifact]
        /\ cacheOwner = [h \in Hashes |-> NoAction]
        /\ materialized = [a \in Actions |-> NoArtifact]
        /\ completion = [a \in Actions |-> "Pending"]
        /\ current = NoAction
        /\ history = <<>>
        /\ pc = "Loop"

Loop == /\ pc = "Loop"
        /\ IF pending # {}
              THEN /\ pc' = "ChooseAction"
              ELSE /\ pc' = "Finished"
        /\ UNCHANGED << pending, cache, cacheOwner, materialized, completion, 
                        current, history >>

ChooseAction == /\ pc = "ChooseAction"
                /\ \E nextAction \in pending:
                     /\ current' = nextAction
                     /\ pending' = pending \ {nextAction}
                /\ pc' = "LookupCache"
                /\ UNCHANGED << cache, cacheOwner, materialized, completion, 
                                history >>

LookupCache == /\ pc = "LookupCache"
               /\ IF cache[ActionHash(current)] # NoArtifact
                     THEN /\ pc' = "CacheHit"
                     ELSE /\ pc' = "CacheMiss"
               /\ UNCHANGED << pending, cache, cacheOwner, materialized, 
                               completion, current, history >>

CacheHit == /\ pc = "CacheHit"
            /\ materialized' = [materialized EXCEPT ![current] = cache[ActionHash(current)]]
            /\ completion' = [completion EXCEPT ![current] = "Cached"]
            /\ history' =            Append(
                            history,
                            [ kind |-> "Cached",
                              action |-> current,
                              hash |-> ActionHash(current),
                              owner |-> cacheOwner[ActionHash(current)],
                              output |-> cache[ActionHash(current)] ]
                          )
            /\ pc' = "Loop"
            /\ UNCHANGED << pending, cache, cacheOwner, current >>

CacheMiss == /\ pc = "CacheMiss"
             /\ cache' = [cache EXCEPT ![ActionHash(current)] = ActionResult(current)]
             /\ cacheOwner' = [cacheOwner EXCEPT ![ActionHash(current)] = current]
             /\ materialized' = [materialized EXCEPT ![current] = ActionResult(current)]
             /\ completion' = [completion EXCEPT ![current] = "Fresh"]
             /\ history' =            Append(
                             history,
                             [ kind |-> "Fresh",
                               action |-> current,
                               hash |-> ActionHash(current),
                               owner |-> current,
                               output |-> ActionResult(current) ]
                           )
             /\ pc' = "Loop"
             /\ UNCHANGED << pending, current >>

Finished == /\ pc = "Finished"
            /\ TRUE
            /\ pc' = "Done"
            /\ UNCHANGED << pending, cache, cacheOwner, materialized, 
                            completion, current, history >>

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == pc = "Done" /\ UNCHANGED vars

Next == Loop \/ ChooseAction \/ LookupCache \/ CacheHit \/ CacheMiss
           \/ Finished
           \/ Terminating

Spec == Init /\ [][Next]_vars

Termination == <>(pc = "Done")

\* END TRANSLATION 

HistoryKinds ==
  {"Fresh", "Cached"}

HistoryEntrySet ==
  { [kind |-> kind,
     action |-> action,
     hash |-> hash,
     owner |-> owner,
     output |-> output] :
      kind \in HistoryKinds,
      action \in Actions,
      hash \in Hashes,
      owner \in (Actions \cup {NoAction}),
      output \in ActionOutputs }

TypeOK ==
  /\ pending \subseteq Actions
  /\ cache \in [Hashes -> (ActionOutputs \cup {NoArtifact})]
  /\ cacheOwner \in [Hashes -> (Actions \cup {NoAction})]
  /\ materialized \in [Actions -> (ActionOutputs \cup {NoArtifact})]
  /\ completion \in [Actions -> {"Pending", "Fresh", "Cached"}]
  /\ current \in (Actions \cup {NoAction})
  /\ history \in Seq(HistoryEntrySet)

CacheSlotsAgree ==
  \A h \in Hashes :
    (cache[h] = NoArtifact) <=> (cacheOwner[h] = NoAction)

CacheValueMatchesOwner ==
  \A h \in Hashes :
    cacheOwner[h] # NoAction
    =>
    /\ cache[h] # NoArtifact
    /\ cache[h] = ActionResult(cacheOwner[h])

FreshExecutionsMatchOwnResult ==
  \A a \in Actions :
    completion[a] = "Fresh"
    =>
    materialized[a] = ActionResult(a)

CachedExecutionsReuseStoredValue ==
  \A a \in Actions :
    completion[a] = "Cached"
    =>
    materialized[a] = cache[ActionHash(a)]

CachePreservesActionSemantics ==
  \A a \in Actions :
    completion[a] \in {"Fresh", "Cached"}
    =>
    materialized[a] = ActionResult(a)

SemanticSoundness ==
  []CachePreservesActionSemantics

=============================================================================
