------------------ MODULE ModuleGraphNamespaceResolution ------------------
EXTENDS Naturals, Sequences, TLC

\* Readable PlusCal slice for the namespace-reconstruction part of
\* `wire_dependencies` in:
\*
\* - packages/tusk-planner/src/module_graph.ml
\* - packages/tusk-planner/src/module_registry.ml
\*
\* This model intentionally stops before alias handling and before the
\* `MLI -> ML` filter.  It only captures the smaller question:
\*
\* - a source file reconstructs a qualified dependency name from its own
\*   nested namespace
\* - the current registry lookup still uses only the simple module name
\* - every registry candidate for that simple name is then wired
\*
\* The bug model checks the stronger law we actually want:
\* if a dependency exists in the source file's own namespace, resolution should
\* prefer that namespaced target over same-simple-name modules from elsewhere.

CONSTANTS
  Nodes,
  SimpleRefs,
  QualifiedRefs,
  QualifiedNameOf,
  QualifiedRefOf,
  Registry,
  SourceNode,
  SourceDeps

ASSUME Nodes # {}
ASSUME SimpleRefs # {}
ASSUME QualifiedRefs # {}
ASSUME QualifiedNameOf \in [Nodes -> QualifiedRefs]
ASSUME QualifiedRefOf \in [SimpleRefs -> QualifiedRefs]
ASSUME Registry \in [SimpleRefs -> Seq(Nodes)]
ASSUME SourceNode \in Nodes
ASSUME SourceDeps \in Seq(SimpleRefs)

\* Passing smoke model: the source reconstructs `Pkg__Sub__Foo` and the simple
\* name registry for `Foo` contains only that one candidate.
SmokeNodes ==
  {"SubBarML", "SubFooMLI"}

SmokeSimpleRefs ==
  {"Foo"}

SmokeQualifiedRefs ==
  {"Pkg__Sub__Bar", "Pkg__Sub__Foo"}

SmokeQualifiedNameOf ==
  [ node \in SmokeNodes |->
      CASE node = "SubBarML" -> "Pkg__Sub__Bar"
        [] OTHER -> "Pkg__Sub__Foo" ]

SmokeQualifiedRefOf ==
  [ ref \in SmokeSimpleRefs |->
      CASE ref = "Foo" -> "Pkg__Sub__Foo"
        [] OTHER -> "Pkg__Sub__Foo" ]

SmokeRegistry ==
  [ ref \in SmokeSimpleRefs |->
      CASE ref = "Foo" -> <<"SubFooMLI">>
        [] OTHER -> <<>> ]

SmokeSourceNode ==
  "SubBarML"

SmokeSourceDeps ==
  <<"Foo">>

\* Bug model: the source reconstructs `Pkg__Sub__Foo`, but simple-name lookup
\* returns both `Pkg__Foo` and `Pkg__Sub__Foo`, so the current machine wires
\* both.
NamespaceBugNodes ==
  {"SubBarML", "RootFooMLI", "SubFooMLI"}

NamespaceBugSimpleRefs ==
  {"Foo"}

NamespaceBugQualifiedRefs ==
  {"Pkg__Sub__Bar", "Pkg__Foo", "Pkg__Sub__Foo"}

NamespaceBugQualifiedNameOf ==
  [ node \in NamespaceBugNodes |->
      CASE node = "SubBarML" -> "Pkg__Sub__Bar"
        [] node = "RootFooMLI" -> "Pkg__Foo"
        [] OTHER -> "Pkg__Sub__Foo" ]

NamespaceBugQualifiedRefOf ==
  [ ref \in NamespaceBugSimpleRefs |->
      CASE ref = "Foo" -> "Pkg__Sub__Foo"
        [] OTHER -> "Pkg__Sub__Foo" ]

NamespaceBugRegistry ==
  [ ref \in NamespaceBugSimpleRefs |->
      CASE ref = "Foo" -> <<"RootFooMLI", "SubFooMLI">>
        [] OTHER -> <<>> ]

NamespaceBugSourceNode ==
  "SubBarML"

NamespaceBugSourceDeps ==
  <<"Foo">>

CandidateTargets(depName) ==
  { Registry[depName][i] : i \in 1..Len(Registry[depName]) }

ReferencedModules ==
  { SourceDeps[i] : i \in 1..Len(SourceDeps) }

DepTargets(depName, edges) ==
  { target \in CandidateTargets(depName) : <<SourceNode, target>> \in edges }

PreferredNamespaceTargets(depName) ==
  { target \in CandidateTargets(depName) :
      QualifiedNameOf[target] = QualifiedRefOf[depName] }

(* --algorithm ModuleGraphNamespaceResolution
variables
  remainingDeps = SourceDeps,
  remainingCandidates = <<>>,
  currentDep = CHOOSE ref \in SimpleRefs : TRUE,
  currentQualifiedRef = CHOOSE q \in QualifiedRefs : TRUE,
  currentCandidate = CHOOSE node \in Nodes : TRUE,
  resolvedQualifiedRefs = <<>>,
  edges = {};

begin
  NextDependency:
    while remainingDeps # <<>> do
      currentDep := Head(remainingDeps);
      remainingDeps := Tail(remainingDeps);
      currentQualifiedRef := QualifiedRefOf[currentDep];
      resolvedQualifiedRefs := Append(resolvedQualifiedRefs, currentQualifiedRef);
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
\* BEGIN TRANSLATION (chksum(pcal) = "b98d7fc" /\ chksum(tla) = "ab621e79")
VARIABLES remainingDeps, remainingCandidates, currentDep, currentQualifiedRef, 
          currentCandidate, resolvedQualifiedRefs, edges, pc

vars == << remainingDeps, remainingCandidates, currentDep, 
           currentQualifiedRef, currentCandidate, resolvedQualifiedRefs, 
           edges, pc >>

Init == (* Global variables *)
        /\ remainingDeps = SourceDeps
        /\ remainingCandidates = <<>>
        /\ currentDep = (CHOOSE ref \in SimpleRefs : TRUE)
        /\ currentQualifiedRef = (CHOOSE q \in QualifiedRefs : TRUE)
        /\ currentCandidate = (CHOOSE node \in Nodes : TRUE)
        /\ resolvedQualifiedRefs = <<>>
        /\ edges = {}
        /\ pc = "NextDependency"

NextDependency == /\ pc = "NextDependency"
                  /\ IF remainingDeps # <<>>
                        THEN /\ currentDep' = Head(remainingDeps)
                             /\ remainingDeps' = Tail(remainingDeps)
                             /\ currentQualifiedRef' = QualifiedRefOf[currentDep']
                             /\ resolvedQualifiedRefs' = Append(resolvedQualifiedRefs, currentQualifiedRef')
                             /\ remainingCandidates' = Registry[currentDep']
                             /\ pc' = "NextCandidate"
                        ELSE /\ pc' = "Finished"
                             /\ UNCHANGED << remainingDeps, 
                                             remainingCandidates, currentDep, 
                                             currentQualifiedRef, 
                                             resolvedQualifiedRefs >>
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
                                 currentQualifiedRef, resolvedQualifiedRefs >>

Finished == /\ pc = "Finished"
            /\ TRUE
            /\ pc' = "Done"
            /\ UNCHANGED << remainingDeps, remainingCandidates, currentDep, 
                            currentQualifiedRef, currentCandidate, 
                            resolvedQualifiedRefs, edges >>

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
  /\ currentQualifiedRef \in QualifiedRefs
  /\ currentCandidate \in Nodes
  /\ resolvedQualifiedRefs \in Seq(QualifiedRefs)
  /\ edges \subseteq ({SourceNode} \X Nodes)

NoSelfEdges ==
  \A edge \in edges :
    edge[1] # edge[2]

EdgesOnlyUseSimpleNameRegistry ==
  \A edge \in edges :
    \E depName \in ReferencedModules :
      edge[2] \in CandidateTargets(depName)

ResolvedQualifiedRefsMatchProcessedDependencies ==
  /\ Len(resolvedQualifiedRefs) <= Len(SourceDeps)
  /\ \A i \in 1..Len(resolvedQualifiedRefs) :
       resolvedQualifiedRefs[i] = QualifiedRefOf[SourceDeps[i]]

ResolutionSettled ==
  /\ remainingDeps = <<>>
  /\ remainingCandidates = <<>>

PreferOwnNamespaceTargetsWhenPresent ==
  ResolutionSettled
  =>
  \A depName \in ReferencedModules :
    PreferredNamespaceTargets(depName) # {}
    =>
    DepTargets(depName, edges) = PreferredNamespaceTargets(depName)

=============================================================================
