---------------- MODULE PlanBundleToolchainInvalidation ----------------
EXTENDS Naturals, Sequences, TLC

\* Readable PlusCal slice for the mismatch between:
\*
\* - packages/riot-planner/src/package_planner.ml (`compute_input_hash`)
\* - packages/riot-planner/src/action_node.ml (`Action_node.make`)
\*
\* The current planner bundle key ignores toolchain identity, but each action
\* node hash includes the toolchain hash. If the toolchain changes and the plan
\* bundle key does not, a warm-plan cache hit can restore stale action hashes.

CONSTANTS
  StableInputs,
  FirstToolchain,
  SecondToolchain

ASSUME StableInputs \in STRING
ASSUME FirstToolchain \in STRING
ASSUME SecondToolchain \in STRING

\* Passing smoke model: no toolchain change, so reusing the stored action hash
\* is still sound.
SmokeStableInputs == "pkg+profile+deps"
SmokeFirstToolchain == "toolchain-v1"
SmokeSecondToolchain == "toolchain-v1"

\* Bug model: the planner inputs stay the same while the toolchain changes.
ToolchainChangeStableInputs == "pkg+profile+deps"
ToolchainChangeFirstToolchain == "toolchain-v1"
ToolchainChangeSecondToolchain == "toolchain-v2"

PlanBundleKey(toolchain) ==
  StableInputs

ActionHash(toolchain) ==
  <<StableInputs, toolchain>>

ToolchainChanged ==
  FirstToolchain # SecondToolchain

(* --algorithm PlanBundleToolchainInvalidation
variables
  stored_bundle_present = FALSE,
  stored_action_hash = <<>>,
  current_plan_key = "",
  cache_decision = "pending",
  restored_action_hash = <<>>;

begin
  FirstPlan:
    \* Mirrors the initial cold-plan path: action hashes are computed with the
    \* first toolchain and persisted under a planner bundle key derived only
    \* from the stable planner inputs.
    current_plan_key := PlanBundleKey(FirstToolchain);
    stored_action_hash := ActionHash(FirstToolchain);
    stored_bundle_present := TRUE;

  SecondPlan:
    \* Mirrors the warm-plan path: planner bundle lookup uses the current
    \* `compute_input_hash` shape, which currently ignores toolchain identity.
    if stored_bundle_present
       /\ PlanBundleKey(SecondToolchain) = current_plan_key then
      cache_decision := "hit";
      restored_action_hash := stored_action_hash;
    else
      cache_decision := "miss";
      restored_action_hash := ActionHash(SecondToolchain);
    end if;

  Finished:
    skip;
end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "e5434f28" /\ chksum(tla) = "9d3b0abb")
VARIABLES stored_bundle_present, stored_action_hash, current_plan_key, 
          cache_decision, restored_action_hash, pc

vars == << stored_bundle_present, stored_action_hash, current_plan_key, 
           cache_decision, restored_action_hash, pc >>

Init == (* Global variables *)
        /\ stored_bundle_present = FALSE
        /\ stored_action_hash = <<>>
        /\ current_plan_key = ""
        /\ cache_decision = "pending"
        /\ restored_action_hash = <<>>
        /\ pc = "FirstPlan"

FirstPlan == /\ pc = "FirstPlan"
             /\ current_plan_key' = PlanBundleKey(FirstToolchain)
             /\ stored_action_hash' = ActionHash(FirstToolchain)
             /\ stored_bundle_present' = TRUE
             /\ pc' = "SecondPlan"
             /\ UNCHANGED << cache_decision, restored_action_hash >>

SecondPlan == /\ pc = "SecondPlan"
              /\ IF stored_bundle_present
                    /\ PlanBundleKey(SecondToolchain) = current_plan_key
                    THEN /\ cache_decision' = "hit"
                         /\ restored_action_hash' = stored_action_hash
                    ELSE /\ cache_decision' = "miss"
                         /\ restored_action_hash' = ActionHash(SecondToolchain)
              /\ pc' = "Finished"
              /\ UNCHANGED << stored_bundle_present, stored_action_hash, 
                              current_plan_key >>

Finished == /\ pc = "Finished"
            /\ TRUE
            /\ pc' = "Done"
            /\ UNCHANGED << stored_bundle_present, stored_action_hash, 
                            current_plan_key, cache_decision, 
                            restored_action_hash >>

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == pc = "Done" /\ UNCHANGED vars

Next == FirstPlan \/ SecondPlan \/ Finished
           \/ Terminating

Spec == Init /\ [][Next]_vars

Termination == <>(pc = "Done")

\* END TRANSLATION

TypeOK ==
  /\ stored_bundle_present \in BOOLEAN
  /\ stored_action_hash \in Seq(STRING)
  /\ current_plan_key \in STRING
  /\ cache_decision \in {"pending", "hit", "miss"}
  /\ restored_action_hash \in Seq(STRING)

Settled ==
  pc = "Done"

StableToolchainCanReusePlanBundle ==
  Settled
  /\ ~ToolchainChanged
  =>
  cache_decision = "hit"
  /\ restored_action_hash = ActionHash(SecondToolchain)

ToolchainChangeMustInvalidateOrRehash ==
  Settled
  /\ ToolchainChanged
  =>
  cache_decision = "miss"
  \/ restored_action_hash = ActionHash(SecondToolchain)

=============================================================================
