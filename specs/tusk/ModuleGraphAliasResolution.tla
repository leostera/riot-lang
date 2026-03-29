-------------------- MODULE ModuleGraphAliasResolution --------------------
EXTENDS Naturals, Sequences, TLC

\* Readable PlusCal slice for alias-based dependency resolution around
\* `wire_dependencies` in:
\*
\* - packages/tusk-planner/src/module_graph.ml
\* - packages/tusk-planner/src/alias_module.ml
\* - packages/tusk-planner/src/action_graph.ml
\*
\* This model focuses on one narrow discrepancy:
\*
\* - scan-time graph construction records `open_modules` alias nodes on modules
\* - action generation later turns those alias nodes into `-open` compiler flags
\* - but the current `wire_dependencies` loop does not consult alias modules when
\*   deciding which graph edges to add
\*
\* In the older `minitusk` design, alias modules narrowed resolution to a
\* specific qualified target when a simple module name was ambiguous.

CONSTANTS
  Nodes,
  SimpleRefs,
  QualifiedRefs,
  QualifiedNameOf,
  Registry,
  SourceNode,
  SourceDeps,
  SourceAliasTargets

ASSUME Nodes # {}
ASSUME SimpleRefs # {}
ASSUME QualifiedRefs # {}
ASSUME QualifiedNameOf \in [Nodes -> QualifiedRefs]
ASSUME Registry \in [SimpleRefs -> Seq(Nodes)]
ASSUME SourceNode \in Nodes
ASSUME SourceDeps \in Seq(SimpleRefs)
ASSUME SourceAliasTargets \in [SimpleRefs -> SUBSET QualifiedRefs]

\* Passing smoke model: the source has an alias context, but the simple-name
\* registry for `Foo` already contains only the aliased target, so the current
\* implementation shape is harmless here.
SmokeNodes ==
  {"MainML", "UtilFooMLI"}

SmokeSimpleRefs ==
  {"Foo"}

SmokeQualifiedRefs ==
  {"Pkg__Main", "Pkg__Util__Foo"}

SmokeQualifiedNameOf ==
  [ node \in SmokeNodes |->
      CASE node = "MainML" -> "Pkg__Main"
        [] OTHER -> "Pkg__Util__Foo" ]

SmokeRegistry ==
  [ ref \in SmokeSimpleRefs |->
      CASE ref = "Foo" -> <<"UtilFooMLI">>
        [] OTHER -> <<>> ]

SmokeSourceNode ==
  "MainML"

SmokeSourceDeps ==
  <<"Foo">>

SmokeSourceAliasTargets ==
  [ ref \in SmokeSimpleRefs |->
      CASE ref = "Foo" -> {"Pkg__Util__Foo"}
        [] OTHER -> {} ]

\* Bug model: the source's alias context says `Foo` should resolve to
\* `Pkg__Util__Foo`, but current simple-name lookup returns both `Pkg__Foo` and
\* `Pkg__Util__Foo`, and the current machine wires both.
AliasBugNodes ==
  {"MainML", "RootFooMLI", "UtilFooMLI"}

AliasBugSimpleRefs ==
  {"Foo"}

AliasBugQualifiedRefs ==
  {"Pkg__Main", "Pkg__Foo", "Pkg__Util__Foo"}

AliasBugQualifiedNameOf ==
  [ node \in AliasBugNodes |->
      CASE node = "MainML" -> "Pkg__Main"
        [] node = "RootFooMLI" -> "Pkg__Foo"
        [] OTHER -> "Pkg__Util__Foo" ]

AliasBugRegistry ==
  [ ref \in AliasBugSimpleRefs |->
      CASE ref = "Foo" -> <<"RootFooMLI", "UtilFooMLI">>
        [] OTHER -> <<>> ]

AliasBugSourceNode ==
  "MainML"

AliasBugSourceDeps ==
  <<"Foo">>

AliasBugSourceAliasTargets ==
  [ ref \in AliasBugSimpleRefs |->
      CASE ref = "Foo" -> {"Pkg__Util__Foo"}
        [] OTHER -> {} ]

CandidateTargets(depName) ==
  { Registry[depName][i] : i \in 1..Len(Registry[depName]) }

ReferencedModules ==
  { SourceDeps[i] : i \in 1..Len(SourceDeps) }

DepTargets(depName, edges) ==
  { target \in CandidateTargets(depName) : <<SourceNode, target>> \in edges }

AliasMatchedTargets(depName) ==
  { target \in CandidateTargets(depName) :
      QualifiedNameOf[target] \in SourceAliasTargets[depName] }

(* --algorithm ModuleGraphAliasResolution
variables
  remainingDeps = SourceDeps,
  remainingCandidates = <<>>,
  currentDep = CHOOSE ref \in SimpleRefs : TRUE,
  currentCandidate = CHOOSE node \in Nodes : TRUE,
  observedAliasTargets = <<>>,
  edges = {};

begin
  NextDependency:
    while remainingDeps # <<>> do
      currentDep := Head(remainingDeps);
      remainingDeps := Tail(remainingDeps);
      observedAliasTargets := Append(observedAliasTargets, SourceAliasTargets[currentDep]);
      remainingCandidates := Registry[currentDep];

      NextCandidate:
        while remainingCandidates # <<>> do
          currentCandidate := Head(remainingCandidates);
          remainingCandidates := Tail(remainingCandidates);

          if currentCandidate # SourceNode then
            edges := edges \cup {<<SourceNode, currentCandidate>>};
          end if;
        end while;
    end while;

  Finished:
    skip;
end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "1946de01" /\ chksum(tla) = "4d9ec5b")
VARIABLES remainingDeps, remainingCandidates, currentDep, currentCandidate, 
          observedAliasTargets, edges, pc

vars == << remainingDeps, remainingCandidates, currentDep, currentCandidate, 
           observedAliasTargets, edges, pc >>

Init == (* Global variables *)
        /\ remainingDeps = SourceDeps
        /\ remainingCandidates = <<>>
        /\ currentDep = (CHOOSE ref \in SimpleRefs : TRUE)
        /\ currentCandidate = (CHOOSE node \in Nodes : TRUE)
        /\ observedAliasTargets = <<>>
        /\ edges = {}
        /\ pc = "NextDependency"

NextDependency == /\ pc = "NextDependency"
                  /\ IF remainingDeps # <<>>
                        THEN /\ currentDep' = Head(remainingDeps)
                             /\ remainingDeps' = Tail(remainingDeps)
                             /\ observedAliasTargets' = Append(observedAliasTargets, SourceAliasTargets[currentDep'])
                             /\ remainingCandidates' = Registry[currentDep']
                             /\ pc' = "NextCandidate"
                        ELSE /\ pc' = "Finished"
                             /\ UNCHANGED << remainingDeps, 
                                             remainingCandidates, currentDep, 
                                             observedAliasTargets >>
                  /\ UNCHANGED << currentCandidate, edges >>

NextCandidate == /\ pc = "NextCandidate"
                 /\ IF remainingCandidates # <<>>
                       THEN /\ currentCandidate' = Head(remainingCandidates)
                            /\ remainingCandidates' = Tail(remainingCandidates)
                            /\ IF currentCandidate' # SourceNode
                                  THEN /\ edges' = (edges \cup {<<SourceNode, currentCandidate'>>})
                                  ELSE /\ TRUE
                                       /\ edges' = edges
                            /\ pc' = "NextCandidate"
                       ELSE /\ pc' = "NextDependency"
                            /\ UNCHANGED << remainingCandidates, 
                                            currentCandidate, edges >>
                 /\ UNCHANGED << remainingDeps, currentDep, 
                                 observedAliasTargets >>

Finished == /\ pc = "Finished"
            /\ TRUE
            /\ pc' = "Done"
            /\ UNCHANGED << remainingDeps, remainingCandidates, currentDep, 
                            currentCandidate, observedAliasTargets, edges >>

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == pc = "Done" /\ UNCHANGED vars

Next == NextDependency \/ NextCandidate \/ Finished
           \/ Terminating

Spec == Init /\ [][Next]_vars

Termination == <>(pc = "Done")

\* END TRANSLATION 

TypeOK ==
  /\ remainingDeps \in Seq(SimpleRefs)
  /\ remainingCandidates \in Seq(Nodes)
  /\ currentDep \in SimpleRefs
  /\ currentCandidate \in Nodes
  /\ observedAliasTargets \in Seq(SUBSET QualifiedRefs)
  /\ edges \subseteq ({SourceNode} \X Nodes)

NoSelfEdges ==
  \A edge \in edges :
    edge[1] # edge[2]

EdgesOnlyUseSimpleNameRegistry ==
  \A edge \in edges :
    \E depName \in ReferencedModules :
      edge[2] \in CandidateTargets(depName)

ObservedAliasTargetsMatchProcessedDependencies ==
  /\ Len(observedAliasTargets) <= Len(SourceDeps)
  /\ \A i \in 1..Len(observedAliasTargets) :
       observedAliasTargets[i] = SourceAliasTargets[SourceDeps[i]]

AliasResolutionSettled ==
  /\ remainingDeps = <<>>
  /\ remainingCandidates = <<>>

PreferAliasMatchedTargetsWhenPresent ==
  AliasResolutionSettled
  =>
  \A depName \in ReferencedModules :
    AliasMatchedTargets(depName) # {}
    =>
    DepTargets(depName, edges) = AliasMatchedTargets(depName)

=============================================================================
