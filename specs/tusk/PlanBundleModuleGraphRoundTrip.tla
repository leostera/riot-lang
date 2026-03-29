---------------- MODULE PlanBundleModuleGraphRoundTrip ----------------
EXTENDS Naturals, Sequences, TLC

\* Readable PlusCal slice for the module-graph bundle round-trip in:
\*
\* - packages/tusk-planner/src/package_planner.ml
\*
\* This model narrows to one fidelity question inside the persisted plan bundle:
\*
\* - `module_graph_to_json` serializes module nodes
\* - `module_graph_of_json` restores them
\* - `open_modules` should survive that round-trip if the bundle is meant to
\*   restore the original module graph rather than only a shape-compatible shell
\*
\* The current extracted serializer writes `"opens": []` for every node and the
\* current deserializer reconstructs `open_modules = []` for every node, so any
\* non-empty open-module context is lost across a warm-plan cache hit.

CONSTANTS
  Nodes,
  OriginalOpens

ASSUME Nodes # {}
ASSUME OriginalOpens \in [Nodes -> SUBSET Nodes]

\* Passing smoke model: every node already has an empty open-module context.
SmokeNodes ==
  {"Root", "Main", "Util"}

SmokeOriginalOpens ==
  [ n \in SmokeNodes |-> {} ]

\* Bug model: `Main` depends on alias-open context, so its open-module set is
\* non-empty before serialization.
OpenModulesBugNodes ==
  {"Root", "Main", "UtilAliases"}

OpenModulesBugOriginalOpens ==
  [ n \in OpenModulesBugNodes |->
      CASE n = "Main" -> {"UtilAliases"}
        [] OTHER -> {} ]

SeqToSet(seq) ==
  { seq[i] : i \in 1..Len(seq) }

(* --algorithm PlanBundleModuleGraphRoundTrip
variables
  pending = Nodes,
  serializedOpens = [n \in Nodes |-> <<>>],
  restoredOpens = [n \in Nodes |-> {}];

begin
  NextNode:
    while pending # {} do
      with n \in pending do
        pending := pending \ {n};

        \* Mirrors `module_graph_to_json`: every node is serialized with
        \* `("opens", Array [])`.
        serializedOpens[n] := <<>>;

        \* Mirrors `module_graph_of_json`: every node is reconstructed with
        \* `open_modules = []`.
        restoredOpens[n] := {};
      end with;
    end while;

  Finished:
    skip;
end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "22ac364c" /\ chksum(tla) = "d8cd7266")
VARIABLES pending, serializedOpens, restoredOpens, pc

vars == << pending, serializedOpens, restoredOpens, pc >>

Init == (* Global variables *)
        /\ pending = Nodes
        /\ serializedOpens = [n \in Nodes |-> <<>>]
        /\ restoredOpens = [n \in Nodes |-> {}]
        /\ pc = "NextNode"

NextNode == /\ pc = "NextNode"
            /\ IF pending # {}
                  THEN /\ \E n \in pending:
                            /\ pending' = pending \ {n}
                            /\ serializedOpens' = [serializedOpens EXCEPT ![n] = <<>>]
                            /\ restoredOpens' = [restoredOpens EXCEPT ![n] = {}]
                       /\ pc' = "NextNode"
                  ELSE /\ pc' = "Finished"
                       /\ UNCHANGED << pending, serializedOpens, restoredOpens >>

Finished == /\ pc = "Finished"
            /\ TRUE
            /\ pc' = "Done"
            /\ UNCHANGED << pending, serializedOpens, restoredOpens >>

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == pc = "Done" /\ UNCHANGED vars

Next == NextNode \/ Finished
           \/ Terminating

Spec == Init /\ [][Next]_vars

Termination == <>(pc = "Done")

\* END TRANSLATION 

TypeOK ==
  /\ pending \subseteq Nodes
  /\ serializedOpens \in [Nodes -> Seq(Nodes)]
  /\ restoredOpens \in [Nodes -> SUBSET Nodes]

RoundTripSettled ==
  pending = {}

SerializedOpenListsStaySequences ==
  \A n \in Nodes :
    serializedOpens[n] \in Seq(Nodes)

EmptyOpenModulesRoundTrip ==
  RoundTripSettled
  =>
  \A n \in Nodes :
    OriginalOpens[n] = {}
    =>
    restoredOpens[n] = {}

OpenModulesRoundTripPreserved ==
  RoundTripSettled
  =>
  \A n \in Nodes :
    restoredOpens[n] = OriginalOpens[n]

=============================================================================
