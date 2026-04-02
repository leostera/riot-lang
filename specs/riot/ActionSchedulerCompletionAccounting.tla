---------------- MODULE ActionSchedulerCompletionAccounting ----------------
EXTENDS Naturals, Sequences, FiniteSets, TLC

\* Readable PlusCal slice for the interaction between:
\*
\* - packages/riot-executor/src/action_queue.ml (`next`)
\* - packages/riot-executor/src/action_executor.ml (`execute`)
\*
\* The current interaction bug is not in either function alone. It appears when:
\*
\* - `Action_queue.next` marks a node `Skipped` immediately after seeing a
\*   failed dependency
\* - `action_executor.execute` only increments `completed_count` when a worker
\*   sends `ActionCompleted`
\*
\* That means skipped nodes become recorded in `completed`, but do not increase
\* `completed_count`. The production loop then has no ready work, no busy
\* workers, and still waits for more worker messages that will never arrive.
\*
\* Abstraction boundary:
\* - one worker is enough to expose the bug
\* - `later_queue` and `requeue_with_deps` are omitted
\* - the initial ready order is assumed topological, so every node is either
\*   runnable or skippable when it reaches the head of the queue

CONSTANTS
  Nodes,
  Dependencies,
  WorkerOutcome,
  InitialReady

StatusUniverse ==
  {"pending", "cached", "executed", "failed", "skipped"}

SuccessfulStatuses ==
  {"cached", "executed"}

FailureStatuses ==
  {"failed", "skipped"}

SeqToSet(seq) ==
  { seq[i] : i \in 1..Len(seq) }

PriorReadyNodes(seq, i) ==
  { seq[j] : j \in 1..(i - 1) }

TopologicalReadyOrder(seq) ==
  \A i \in 1..Len(seq) :
    Dependencies[seq[i]] \subseteq PriorReadyNodes(seq, i)

ASSUME Nodes # {}
ASSUME Dependencies \in [Nodes -> SUBSET Nodes]
ASSUME WorkerOutcome \in [Nodes -> {"executed", "failed"}]
ASSUME InitialReady \in Seq(Nodes)
ASSUME SeqToSet(InitialReady) = Nodes
ASSUME TopologicalReadyOrder(InitialReady)

\* Passing smoke model: one independent node runs and increments the worker
\* completion counter normally.
SmokeNodes ==
  {"A"}

SmokeDependencies ==
  [n \in SmokeNodes |-> {}]

SmokeWorkerOutcome ==
  [n \in SmokeNodes |-> "executed"]

SmokeInitialReady ==
  <<"A">>

\* Bug model: `A` fails, `B` depends on `A`, and `B` is marked skipped inside
\* `Action_queue.next` without incrementing `completed_count`.
SkipAccountingBugNodes ==
  {"A", "B"}

SkipAccountingBugDependencies ==
  [n \in SkipAccountingBugNodes |->
      CASE n = "B" -> {"A"}
        [] OTHER -> {}]

SkipAccountingBugWorkerOutcome ==
  [n \in SkipAccountingBugNodes |->
      CASE n = "A" -> "failed"
        [] OTHER -> "executed"]

SkipAccountingBugInitialReady ==
  <<"A", "B">>

(* --algorithm ActionSchedulerCompletionAccounting
variables
  ready = InitialReady,
  completed = [n \in Nodes |-> "pending"],
  completed_count = 0,
  worker_busy = FALSE,
  worker_node = CHOOSE n \in Nodes : TRUE,
  current_node = CHOOSE n \in Nodes : TRUE,
  quiescent_under_counted = FALSE;

begin
  DispatchLoop:
    while ready # <<>> \/ worker_busy do
      if ~worker_busy then
        SchedulerScan:
          while ready # <<>> /\ ~worker_busy do
            current_node := Head(ready);
            ready := Tail(ready);

            if \E dep \in Dependencies[current_node] :
                 completed[dep] \in FailureStatuses then
              \* Mirrors `Action_queue.next`: skipped nodes are recorded in the
              \* completed table immediately.
              completed[current_node] := "skipped";
            else
              if \A dep \in Dependencies[current_node] :
                   completed[dep] \in SuccessfulStatuses then
                worker_busy := TRUE;
                worker_node := current_node;
              end if;
            end if;
          end while;
      end if;

    MaybeCompleteWorker:
      if worker_busy then
        \* Mirrors `action_executor.execute`: only worker completions increment
        \* `completed_count`.
        completed[worker_node] := WorkerOutcome[worker_node];
        completed_count := completed_count + 1;
        worker_busy := FALSE;
      end if;
    end while;

  DetectQuiescence:
    \* The production loop would now wait forever on `receive_any` if
    \* `completed_count` is still short. The model records that quiescent
    \* under-counted state explicitly so TLC can check it as a safety property.
    if completed_count # Cardinality({n \in Nodes : completed[n] # "pending"}) then
      quiescent_under_counted := TRUE;
    end if;

  Finished:
    skip;
end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "dc2a09fe" /\ chksum(tla) = "b54e9460")
VARIABLES ready, completed, completed_count, worker_busy, worker_node, 
          current_node, quiescent_under_counted, pc

vars == << ready, completed, completed_count, worker_busy, worker_node, 
           current_node, quiescent_under_counted, pc >>

Init == (* Global variables *)
        /\ ready = InitialReady
        /\ completed = [n \in Nodes |-> "pending"]
        /\ completed_count = 0
        /\ worker_busy = FALSE
        /\ worker_node = (CHOOSE n \in Nodes : TRUE)
        /\ current_node = (CHOOSE n \in Nodes : TRUE)
        /\ quiescent_under_counted = FALSE
        /\ pc = "DispatchLoop"

DispatchLoop == /\ pc = "DispatchLoop"
                /\ IF ready # <<>> \/ worker_busy
                      THEN /\ IF ~worker_busy
                                 THEN /\ pc' = "SchedulerScan"
                                 ELSE /\ pc' = "MaybeCompleteWorker"
                      ELSE /\ pc' = "DetectQuiescence"
                /\ UNCHANGED << ready, completed, completed_count, worker_busy, 
                                worker_node, current_node, 
                                quiescent_under_counted >>

MaybeCompleteWorker == /\ pc = "MaybeCompleteWorker"
                       /\ IF worker_busy
                             THEN /\ completed' = [completed EXCEPT ![worker_node] = WorkerOutcome[worker_node]]
                                  /\ completed_count' = completed_count + 1
                                  /\ worker_busy' = FALSE
                             ELSE /\ TRUE
                                  /\ UNCHANGED << completed, completed_count, 
                                                  worker_busy >>
                       /\ pc' = "DispatchLoop"
                       /\ UNCHANGED << ready, worker_node, current_node, 
                                       quiescent_under_counted >>

SchedulerScan == /\ pc = "SchedulerScan"
                 /\ IF ready # <<>> /\ ~worker_busy
                       THEN /\ current_node' = Head(ready)
                            /\ ready' = Tail(ready)
                            /\ IF \E dep \in Dependencies[current_node'] :
                                    completed[dep] \in FailureStatuses
                                  THEN /\ completed' = [completed EXCEPT ![current_node'] = "skipped"]
                                       /\ UNCHANGED << worker_busy, 
                                                       worker_node >>
                                  ELSE /\ IF \A dep \in Dependencies[current_node'] :
                                               completed[dep] \in SuccessfulStatuses
                                             THEN /\ worker_busy' = TRUE
                                                  /\ worker_node' = current_node'
                                             ELSE /\ TRUE
                                                  /\ UNCHANGED << worker_busy, 
                                                                  worker_node >>
                                       /\ UNCHANGED completed
                            /\ pc' = "SchedulerScan"
                       ELSE /\ pc' = "MaybeCompleteWorker"
                            /\ UNCHANGED << ready, completed, worker_busy, 
                                            worker_node, current_node >>
                 /\ UNCHANGED << completed_count, quiescent_under_counted >>

DetectQuiescence == /\ pc = "DetectQuiescence"
                    /\ IF completed_count # Cardinality({n \in Nodes : completed[n] # "pending"})
                          THEN /\ quiescent_under_counted' = TRUE
                          ELSE /\ TRUE
                               /\ UNCHANGED quiescent_under_counted
                    /\ pc' = "Finished"
                    /\ UNCHANGED << ready, completed, completed_count, 
                                    worker_busy, worker_node, current_node >>

Finished == /\ pc = "Finished"
            /\ TRUE
            /\ pc' = "Done"
            /\ UNCHANGED << ready, completed, completed_count, worker_busy, 
                            worker_node, current_node, quiescent_under_counted >>

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == pc = "Done" /\ UNCHANGED vars

Next == DispatchLoop \/ MaybeCompleteWorker \/ SchedulerScan
           \/ DetectQuiescence \/ Finished
           \/ Terminating

Spec == Init /\ [][Next]_vars

Termination == <>(pc = "Done")

\* END TRANSLATION

TypeOK ==
  /\ ready \in Seq(Nodes)
  /\ completed \in [Nodes -> StatusUniverse]
  /\ completed_count \in 0..Cardinality(Nodes)
  /\ worker_busy \in BOOLEAN
  /\ worker_node \in Nodes
  /\ current_node \in Nodes
  /\ quiescent_under_counted \in BOOLEAN

Settled ==
  pc = "Done"

SingleNodeCompletionIsCounted ==
  Settled
  /\ Cardinality(Nodes) = 1
  =>
  completed_count = Cardinality(Nodes)
  /\ ~quiescent_under_counted

QuiescentSchedulerMustBeFullyCounted ==
  Settled
  =>
  ~quiescent_under_counted
  /\ completed_count = Cardinality({n \in Nodes : completed[n] # "pending"})

=============================================================================
