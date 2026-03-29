------------------------- MODULE ModuleGraphWiring -------------------------
EXTENDS Naturals, Sequences, TLC

\* Readable PlusCal slice for the `wire_dependencies` phase in:
\*
\* - packages/tusk-planner/src/module_graph.ml
\* - packages/tusk-planner/src/module_registry.ml
\*
\* This model intentionally starts after `ocamldep` has already produced
\* dependency names for one source file.  It only captures the current rules
\* for turning resolved dependency names into graph edges:
\*
\* - look up candidate target nodes by module name in the registry
\* - skip self-edges
\* - skip `MLI -> ML`
\* - otherwise add an edge to every matching registry entry
\*
\* The later bug config checks the stronger planner law encoded by
\* `dependency_resolution_tests.ml`: when both interface and implementation
\* nodes exist for a referenced module, downstream users should prefer the
\* implementation node.

CONSTANTS
  Nodes,
  ModuleRefs,
  Kinds,
  Registry,
  SourceNode,
  SourceDeps

ASSUME Nodes # {}
ASSUME ModuleRefs # {}
ASSUME Kinds \in [Nodes -> {"ML", "MLI"}]
ASSUME Registry \in [ModuleRefs -> Seq(Nodes)]
ASSUME SourceNode \in Nodes
ASSUME SourceDeps \in Seq(ModuleRefs)

\* Passing smoke model: an interface source depends on itself and on a module
\* that only has an interface node.  This exercises self-edge filtering and the
\* "use what exists" path without the implementation-preference conflict.
SmokeNodes ==
  {"BarMLI", "BarML", "FooMLI"}

SmokeModuleRefs ==
  {"Bar", "Foo"}

SmokeKinds ==
  [ node \in SmokeNodes |->
      CASE node = "BarML" -> "ML"
        [] OTHER -> "MLI" ]

SmokeRegistry ==
  [ ref \in SmokeModuleRefs |->
      CASE ref = "Bar" -> <<"BarML", "BarMLI">>
        [] ref = "Foo" -> <<"FooMLI">>
        [] OTHER -> <<>> ]

SmokeSourceNode ==
  "BarMLI"

SmokeSourceDeps ==
  <<"Bar", "Foo">>

\* Failing bug model: an interface source references `Foo`, and the registry
\* contains both `Foo.ml` and `Foo.mli`.  The current implementation order is
\* implementation first, because the interface is scanned first and the later
\* implementation registration is prepended in `Module_registry.register`.
PreferenceBugNodes ==
  {"BarMLI", "FooML", "FooMLI"}

PreferenceBugModuleRefs ==
  {"Foo"}

PreferenceBugKinds ==
  [ node \in PreferenceBugNodes |->
      CASE node = "FooML" -> "ML"
        [] OTHER -> "MLI" ]

PreferenceBugRegistry ==
  [ ref \in PreferenceBugModuleRefs |->
      CASE ref = "Foo" -> <<"FooML", "FooMLI">>
        [] OTHER -> <<>> ]

PreferenceBugSourceNode ==
  "BarMLI"

PreferenceBugSourceDeps ==
  <<"Foo">>

CandidateTargets(depName) ==
  { Registry[depName][i] : i \in 1..Len(Registry[depName]) }

ReferencedModules ==
  { SourceDeps[i] : i \in 1..Len(SourceDeps) }

DepTargets(depName, edges) ==
  { target \in CandidateTargets(depName) : <<SourceNode, target>> \in edges }

ImplementationTargets(depName) ==
  { target \in CandidateTargets(depName) : Kinds[target] = "ML" }

InterfaceTargets(depName) ==
  { target \in CandidateTargets(depName) : Kinds[target] = "MLI" }

PreferredTargets(depName) ==
  IF ImplementationTargets(depName) # {}
    THEN ImplementationTargets(depName)
    ELSE InterfaceTargets(depName)

(* --algorithm ModuleGraphWiring
variables
  remainingDeps = SourceDeps,
  remainingCandidates = <<>>,
  currentDep = CHOOSE ref \in ModuleRefs : TRUE,
  currentCandidate = CHOOSE node \in Nodes : TRUE,
  edges = {};

begin
  NextDependency:
    while remainingDeps # <<>> do
      currentDep := Head(remainingDeps);
      remainingDeps := Tail(remainingDeps);
      remainingCandidates := Registry[currentDep];

      NextCandidate:
        while remainingCandidates # <<>> do
          currentCandidate := Head(remainingCandidates);
          remainingCandidates := Tail(remainingCandidates);

          if currentCandidate # SourceNode then
            if ~(Kinds[SourceNode] = "MLI" /\ Kinds[currentCandidate] = "ML") then
              edges := edges \cup {<<SourceNode, currentCandidate>>};
            end if;
          end if;
        end while;
    end while;

  Finished:
    skip;
end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "66435fd3" /\ chksum(tla) = "2e87cdfc")
VARIABLES remainingDeps, remainingCandidates, currentDep, currentCandidate, 
          edges, pc

vars == << remainingDeps, remainingCandidates, currentDep, currentCandidate, 
           edges, pc >>

Init == (* Global variables *)
        /\ remainingDeps = SourceDeps
        /\ remainingCandidates = <<>>
        /\ currentDep = (CHOOSE ref \in ModuleRefs : TRUE)
        /\ currentCandidate = (CHOOSE node \in Nodes : TRUE)
        /\ edges = {}
        /\ pc = "NextDependency"

NextDependency == /\ pc = "NextDependency"
                  /\ IF remainingDeps # <<>>
                        THEN /\ currentDep' = Head(remainingDeps)
                             /\ remainingDeps' = Tail(remainingDeps)
                             /\ remainingCandidates' = Registry[currentDep']
                             /\ pc' = "NextCandidate"
                        ELSE /\ pc' = "Finished"
                             /\ UNCHANGED << remainingDeps, 
                                             remainingCandidates, currentDep >>
                  /\ UNCHANGED << currentCandidate, edges >>

NextCandidate == /\ pc = "NextCandidate"
                 /\ IF remainingCandidates # <<>>
                       THEN /\ currentCandidate' = Head(remainingCandidates)
                            /\ remainingCandidates' = Tail(remainingCandidates)
                            /\ IF currentCandidate' # SourceNode
                                  THEN /\ IF ~(Kinds[SourceNode] = "MLI" /\ Kinds[currentCandidate'] = "ML")
                                             THEN /\ edges' = (edges \cup {<<SourceNode, currentCandidate'>>})
                                             ELSE /\ TRUE
                                                  /\ edges' = edges
                                  ELSE /\ TRUE
                                       /\ edges' = edges
                            /\ pc' = "NextCandidate"
                       ELSE /\ pc' = "NextDependency"
                            /\ UNCHANGED << remainingCandidates, 
                                            currentCandidate, edges >>
                 /\ UNCHANGED << remainingDeps, currentDep >>

Finished == /\ pc = "Finished"
            /\ TRUE
            /\ pc' = "Done"
            /\ UNCHANGED << remainingDeps, remainingCandidates, currentDep, 
                            currentCandidate, edges >>

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == pc = "Done" /\ UNCHANGED vars

Next == NextDependency \/ NextCandidate \/ Finished
           \/ Terminating

Spec == Init /\ [][Next]_vars

Termination == <>(pc = "Done")

\* END TRANSLATION 

TypeOK ==
  /\ remainingDeps \in Seq(ModuleRefs)
  /\ remainingCandidates \in Seq(Nodes)
  /\ currentDep \in ModuleRefs
  /\ currentCandidate \in Nodes
  /\ edges \subseteq ({SourceNode} \X Nodes)

NoSelfEdges ==
  \A edge \in edges :
    edge[1] # edge[2]

CurrentImplementationSkipsMLIToML ==
  \A edge \in edges :
    ~(Kinds[edge[1]] = "MLI" /\ Kinds[edge[2]] = "ML")

EdgesOnlyUseReferencedRegisteredCandidates ==
  \A edge \in edges :
    \E depName \in ReferencedModules :
      edge[2] \in CandidateTargets(depName)

WiringSettled ==
  /\ remainingDeps = <<>>
  /\ remainingCandidates = <<>>

PreferImplementationWhenBothExist ==
  WiringSettled
  =>
  \A depName \in ReferencedModules :
    ImplementationTargets(depName) # {}
    =>
    DepTargets(depName, edges) = PreferredTargets(depName)

=============================================================================
