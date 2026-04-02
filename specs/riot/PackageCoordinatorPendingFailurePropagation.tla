---------------- MODULE PackageCoordinatorPendingFailurePropagation ----------------
EXTENDS Naturals, FiniteSets, TLC

\* Readable PlusCal slice for the pending-package wakeup path in:
\*
\* - packages/riot-executor/src/coordinator.ml (`try_plan_pending_packages`)
\*
\* This slice narrows to one coordinator interaction:
\*
\* - a package sits in `pending_planning`
\* - one dependency result becomes available
\* - the coordinator revisits the pending package
\* - success removes it from pending and stages/plans it
\* - dependency failure removes it from pending and inserts a failed
\*   `package_results` entry
\*
\* The current code leaves the package graph node unchanged in that failure
\* branch. The stronger law we want is: once a pending package is resolved to a
\* failed result because of failed dependencies, the returned `package_graph`
\* should no longer say that package is `Unplanned`.

CONSTANT DependencyOutcome

GraphStates ==
  {"unplanned", "planned", "failed", "skipped"}

ResultStates ==
  {"pending", "success", "failed"}

ASSUME DependencyOutcome \in {"success", "failed"}

\* Passing smoke model: dependency success wakes the package and moves its graph
\* node out of `unplanned`.
SmokeDependencyOutcome ==
  "success"

\* Bug model: dependency failure resolves the package result but leaves the
\* graph state unchanged.
PendingFailureBugDependencyOutcome ==
  "failed"

(* --algorithm PackageCoordinatorPendingFailurePropagation
variables
  pending = {"Pkg"},
  package_results = [pkg \in {"Dep", "Pkg"} |->
                       CASE pkg = "Dep" -> DependencyOutcome
                         [] OTHER -> "pending"],
  package_graph_state = [pkg \in {"Dep", "Pkg"} |->
                           CASE pkg = "Dep" -> "planned"
                             [] OTHER -> "unplanned"];

begin
  RevisitPendingPackage:
    if DependencyOutcome = "failed" then
      \* Mirrors the `deps_failed` branch in `try_plan_pending_packages`:
      \* the package gets a failed result and is removed from `pending_planning`
      \* but the graph node is left unchanged.
      package_results["Pkg"] := "failed";
      pending := pending \ {"Pkg"};
    else
      \* Mirrors the successful wakeup path: the package leaves pending and its
      \* graph node advances out of `Unplanned`.
      pending := pending \ {"Pkg"};
      package_graph_state["Pkg"] := "planned";
    end if;

  Finished:
    skip;
end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "9f51b545" /\ chksum(tla) = "5de98d2a")
VARIABLES pending, package_results, package_graph_state, pc

vars == << pending, package_results, package_graph_state, pc >>

Init == (* Global variables *)
        /\ pending = {"Pkg"}
        /\ package_results = [pkg \in {"Dep", "Pkg"} |->
                                CASE pkg = "Dep" -> DependencyOutcome
                                  [] OTHER -> "pending"]
        /\ package_graph_state = [pkg \in {"Dep", "Pkg"} |->
                                    CASE pkg = "Dep" -> "planned"
                                      [] OTHER -> "unplanned"]
        /\ pc = "RevisitPendingPackage"

RevisitPendingPackage == /\ pc = "RevisitPendingPackage"
                         /\ IF DependencyOutcome = "failed"
                               THEN /\ package_results' = [package_results EXCEPT !["Pkg"] = "failed"]
                                    /\ pending' = pending \ {"Pkg"}
                                    /\ UNCHANGED package_graph_state
                               ELSE /\ pending' = pending \ {"Pkg"}
                                    /\ package_graph_state' = [package_graph_state EXCEPT !["Pkg"] = "planned"]
                                    /\ UNCHANGED package_results
                         /\ pc' = "Finished"

Finished == /\ pc = "Finished"
            /\ TRUE
            /\ pc' = "Done"
            /\ UNCHANGED << pending, package_results, package_graph_state >>

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == pc = "Done" /\ UNCHANGED vars

Next == RevisitPendingPackage \/ Finished
           \/ Terminating

Spec == Init /\ [][Next]_vars

Termination == <>(pc = "Done")

\* END TRANSLATION

TypeOK ==
  /\ pending \subseteq {"Pkg"}
  /\ package_results \in [{"Dep", "Pkg"} -> ResultStates]
  /\ package_graph_state \in [{"Dep", "Pkg"} -> GraphStates]

Settled ==
  pc = "Done"

PendingSuccessLeavesUnplanned ==
  (Settled
   /\ DependencyOutcome = "success")
  =>
  (pending = {}
   /\ package_graph_state["Pkg"] = "planned")

FailedPendingPackageMustNotStayUnplanned ==
  (Settled
   /\ DependencyOutcome = "failed"
   /\ package_results["Pkg"] = "failed")
  =>
  package_graph_state["Pkg"] # "unplanned"

=============================================================================
