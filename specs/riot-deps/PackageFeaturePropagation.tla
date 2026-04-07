---------------------- MODULE PackageFeaturePropagation ----------------------
EXTENDS Naturals, FiniteSets, Sequences, TLC

\* Readable feature-propagation slice for a future Riot package-feature system.
\*
\* This model focuses only on graph semantics:
\*
\* - packages declare named features plus a default feature set
\* - dependency edges may request explicit features on their target package
\* - dependency edges may allow or cut ambient default propagation
\* - explicit requests unify additively across all reachable incoming edges
\* - defaults propagate down dependency paths until an edge sets
\*   `default_features = false`
\*
\* The model intentionally does NOT cover:
\*
\* - version solving
\* - feature forwarding such as `std/net -> kernel/net`
\* - source-level `cfg(...)` syntax
\* - root package self-activation flags
\*
\* The purpose of this slice is to make the package-graph contract executable.

CONSTANTS
  Packages,
  Features,
  Roots,
  Edges,
  EdgeFrom,
  EdgeTo,
  EdgeAllowsDefaults,
  EdgeFeatures,
  DeclaredFeatures,
  DefaultFeatures,
  ExpectedDefaultReachable,
  ExpectedEffective

ASSUME Packages # {}
ASSUME Roots \subseteq Packages
ASSUME Edges # {}
ASSUME Features # {}
ASSUME EdgeFrom \in [Edges -> Packages]
ASSUME EdgeTo \in [Edges -> Packages]
ASSUME EdgeAllowsDefaults \in [Edges -> BOOLEAN]
ASSUME DeclaredFeatures \in [Packages -> SUBSET Features]
ASSUME DefaultFeatures \in [Packages -> SUBSET Features]
ASSUME \A p \in Packages : DefaultFeatures[p] \subseteq DeclaredFeatures[p]
ASSUME EdgeFeatures \in [Edges -> SUBSET Features]
ASSUME \A e \in Edges : EdgeFeatures[e] \subseteq DeclaredFeatures[EdgeTo[e]]
ASSUME ExpectedDefaultReachable \subseteq Packages
ASSUME ExpectedEffective \in [Packages -> SUBSET Features]
ASSUME \A p \in Packages : ExpectedEffective[p] \subseteq DeclaredFeatures[p]

\* Bound path search to the finite edge set so TLC can enumerate paths directly.
BoundedEdgePaths ==
  UNION { [1..n -> Edges] : n \in 1..Cardinality(Edges) }

PathConnected(path) ==
  \A i \in 1..(Len(path) - 1) : EdgeTo[path[i]] = EdgeFrom[path[i + 1]]

PathStartsAtRoot(path) ==
  EdgeFrom[path[1]] \in Roots

PathEndsAt(path, pkg) ==
  EdgeTo[path[Len(path)]] = pkg

ReachableByPath(pkg) ==
  pkg \in Roots
  \/ \E path \in BoundedEdgePaths :
       /\ PathStartsAtRoot(path)
       /\ PathConnected(path)
       /\ PathEndsAt(path, pkg)

DefaultReachableByPath(pkg) ==
  pkg \in Roots
  \/ \E path \in BoundedEdgePaths :
       /\ PathStartsAtRoot(path)
       /\ PathConnected(path)
       /\ PathEndsAt(path, pkg)
       /\ \A i \in 1..Len(path) : EdgeAllowsDefaults[path[i]]

RequestedFromEdges(edgeSet, pkg) ==
  UNION { EdgeFeatures[e] : e \in { edge \in edgeSet : EdgeTo[edge] = pkg } }

RequestedFromReachableEdges(pkg) ==
  UNION {
    EdgeFeatures[e] :
      e \in {
        edge \in Edges :
          /\ ReachableByPath(EdgeFrom[edge])
          /\ EdgeTo[edge] = pkg
      }
  }

\* Small named presets so the `.cfg` file can stay focused on the intended
\* package-graph scenarios discussed for the RFD:
\*
\* Foo depends on Bar and a narrow Std.
\* Bar depends on Pepo and patches Std with explicit io/test support.
\* Pepo forgets to request the Std features it really needs.
\* The Bar -> Pepo edge cuts ambient defaults, so Pepo's `std = "*"` does not
\* silently pull in all Std defaults.

StdFeatureUniverse ==
  {"net", "io", "test", "compress", "tls"}

SmokePackages ==
  {"Foo", "Bar", "Pepo", "Std"}

SmokeFeatures ==
  StdFeatureUniverse

SmokeRoots ==
  {"Foo"}

SmokeEdges ==
  {"FooToBar", "FooToStd", "BarToPepo", "BarToStd", "PepoToStd"}

SmokeEdgeFrom ==
  [e \in SmokeEdges |->
    CASE e = "FooToBar" -> "Foo"
      [] e = "FooToStd" -> "Foo"
      [] e = "BarToPepo" -> "Bar"
      [] e = "BarToStd" -> "Bar"
      [] e = "PepoToStd" -> "Pepo"]

SmokeEdgeTo ==
  [e \in SmokeEdges |->
    CASE e = "FooToBar" -> "Bar"
      [] e = "FooToStd" -> "Std"
      [] e = "BarToPepo" -> "Pepo"
      [] e = "BarToStd" -> "Std"
      [] e = "PepoToStd" -> "Std"]

SmokeEdgeAllowsDefaults ==
  [e \in SmokeEdges |->
    CASE e = "FooToBar" -> TRUE
      [] e = "FooToStd" -> FALSE
      [] e = "BarToPepo" -> FALSE
      [] e = "BarToStd" -> FALSE
      [] e = "PepoToStd" -> TRUE]

SmokeDeclaredFeatures ==
  [p \in SmokePackages |->
    CASE p = "Std" -> {"net", "io", "test", "compress", "tls"}
      [] OTHER -> {}]

SmokeDefaultFeatures ==
  [p \in SmokePackages |->
    CASE p = "Std" -> {"compress", "tls"}
      [] OTHER -> {}]

SmokeEdgeFeatures ==
  [e \in SmokeEdges |->
    CASE e = "FooToStd" -> {"net"}
      [] e = "BarToStd" -> {"io", "test"}
      [] OTHER -> {}]

SmokeExpectedStdFeatures ==
  {"net", "io", "test"}

SmokeExpectedDefaultReachable ==
  {"Foo", "Bar"}

SmokeExpectedEffective ==
  [p \in SmokePackages |->
    CASE p = "Std" -> SmokeExpectedStdFeatures
      [] OTHER -> {}]

\* If there is at least one defaults-allowed path to a dependency, that package
\* still gets defaults even when another path cuts them.

AlternatePathPackages ==
  {"Foo", "Bar", "Std"}

AlternatePathFeatures ==
  StdFeatureUniverse

AlternatePathRoots ==
  {"Foo"}

AlternatePathEdges ==
  {"FooToBar", "FooToStd", "BarToStd"}

AlternatePathEdgeFrom ==
  [e \in AlternatePathEdges |->
    CASE e = "FooToBar" -> "Foo"
      [] e = "FooToStd" -> "Foo"
      [] e = "BarToStd" -> "Bar"]

AlternatePathEdgeTo ==
  [e \in AlternatePathEdges |->
    CASE e = "FooToBar" -> "Bar"
      [] e = "FooToStd" -> "Std"
      [] e = "BarToStd" -> "Std"]

AlternatePathEdgeAllowsDefaults ==
  [e \in AlternatePathEdges |->
    CASE e = "FooToBar" -> FALSE
      [] e = "FooToStd" -> TRUE
      [] e = "BarToStd" -> TRUE]

AlternatePathDeclaredFeatures ==
  [p \in AlternatePathPackages |->
    CASE p = "Std" -> StdFeatureUniverse
      [] OTHER -> {}]

AlternatePathDefaultFeatures ==
  [p \in AlternatePathPackages |->
    CASE p = "Std" -> {"compress", "tls"}
      [] OTHER -> {}]

AlternatePathEdgeFeatures ==
  [e \in AlternatePathEdges |->
    CASE e = "BarToStd" -> {"net"}
      [] OTHER -> {}]

AlternatePathExpectedDefaultReachable ==
  {"Foo", "Std"}

AlternatePathExpectedEffective ==
  [p \in AlternatePathPackages |->
    CASE p = "Std" -> {"net", "compress", "tls"}
      [] OTHER -> {}]

\* Once an edge cuts defaults, lower edges on that same path cannot re-enable
\* them ambiently.

StickyCutPackages ==
  {"Foo", "Bar", "Baz", "Std"}

StickyCutFeatures ==
  StdFeatureUniverse

StickyCutRoots ==
  {"Foo"}

StickyCutEdges ==
  {"FooToBar", "BarToBaz", "BazToStd"}

StickyCutEdgeFrom ==
  [e \in StickyCutEdges |->
    CASE e = "FooToBar" -> "Foo"
      [] e = "BarToBaz" -> "Bar"
      [] e = "BazToStd" -> "Baz"]

StickyCutEdgeTo ==
  [e \in StickyCutEdges |->
    CASE e = "FooToBar" -> "Bar"
      [] e = "BarToBaz" -> "Baz"
      [] e = "BazToStd" -> "Std"]

StickyCutEdgeAllowsDefaults ==
  [e \in StickyCutEdges |->
    CASE e = "FooToBar" -> FALSE
      [] OTHER -> TRUE]

StickyCutDeclaredFeatures ==
  [p \in StickyCutPackages |->
    CASE p = "Std" -> StdFeatureUniverse
      [] OTHER -> {}]

StickyCutDefaultFeatures ==
  [p \in StickyCutPackages |->
    CASE p = "Std" -> {"compress", "tls"}
      [] OTHER -> {}]

StickyCutEdgeFeatures ==
  [e \in StickyCutEdges |-> {}]

StickyCutExpectedDefaultReachable ==
  {"Foo"}

StickyCutExpectedEffective ==
  [p \in StickyCutPackages |-> {}]

\* Multiple roots contribute to one shared effective feature set.

MultipleRootsPackages ==
  {"App", "Tool", "Std"}

MultipleRootsFeatures ==
  StdFeatureUniverse

MultipleRootsRoots ==
  {"App", "Tool"}

MultipleRootsEdges ==
  {"AppToStd", "ToolToStd"}

MultipleRootsEdgeFrom ==
  [e \in MultipleRootsEdges |->
    CASE e = "AppToStd" -> "App"
      [] e = "ToolToStd" -> "Tool"]

MultipleRootsEdgeTo ==
  [e \in MultipleRootsEdges |-> "Std"]

MultipleRootsEdgeAllowsDefaults ==
  [e \in MultipleRootsEdges |->
    CASE e = "AppToStd" -> FALSE
      [] e = "ToolToStd" -> TRUE]

MultipleRootsDeclaredFeatures ==
  [p \in MultipleRootsPackages |->
    CASE p = "Std" -> StdFeatureUniverse
      [] OTHER -> {}]

MultipleRootsDefaultFeatures ==
  [p \in MultipleRootsPackages |->
    CASE p = "Std" -> {"compress", "tls"}
      [] OTHER -> {}]

MultipleRootsEdgeFeatures ==
  [e \in MultipleRootsEdges |->
    CASE e = "AppToStd" -> {"net"}
      [] OTHER -> {}]

MultipleRootsExpectedDefaultReachable ==
  {"App", "Tool", "Std"}

MultipleRootsExpectedEffective ==
  [p \in MultipleRootsPackages |->
    CASE p = "Std" -> {"net", "compress", "tls"}
      [] OTHER -> {}]

\* A root may patch an underspecified transitive dependency by adding explicit
\* features on the shared package.

RootPatchPackages ==
  {"Foo", "Bar", "Pepo", "Std"}

RootPatchFeatures ==
  StdFeatureUniverse

RootPatchRoots ==
  {"Foo"}

RootPatchEdges ==
  {"FooToBar", "BarToPepo", "PepoToStd", "FooToStd"}

RootPatchEdgeFrom ==
  [e \in RootPatchEdges |->
    CASE e = "FooToBar" -> "Foo"
      [] e = "BarToPepo" -> "Bar"
      [] e = "PepoToStd" -> "Pepo"
      [] e = "FooToStd" -> "Foo"]

RootPatchEdgeTo ==
  [e \in RootPatchEdges |->
    CASE e = "FooToBar" -> "Bar"
      [] e = "BarToPepo" -> "Pepo"
      [] e = "PepoToStd" -> "Std"
      [] e = "FooToStd" -> "Std"]

RootPatchEdgeAllowsDefaults ==
  [e \in RootPatchEdges |->
    CASE e = "FooToBar" -> TRUE
      [] e = "BarToPepo" -> FALSE
      [] e = "PepoToStd" -> TRUE
      [] e = "FooToStd" -> FALSE]

RootPatchDeclaredFeatures ==
  [p \in RootPatchPackages |->
    CASE p = "Std" -> StdFeatureUniverse
      [] OTHER -> {}]

RootPatchDefaultFeatures ==
  [p \in RootPatchPackages |->
    CASE p = "Std" -> {"compress", "tls"}
      [] OTHER -> {}]

RootPatchEdgeFeatures ==
  [e \in RootPatchEdges |->
    CASE e = "FooToStd" -> {"io"}
      [] OTHER -> {}]

RootPatchExpectedDefaultReachable ==
  {"Foo", "Bar"}

RootPatchExpectedEffective ==
  [p \in RootPatchPackages |->
    CASE p = "Std" -> {"io"}
      [] OTHER -> {}]

\* Explicit features can be requested together with ambient defaults on the same
\* edge.

ExplicitPlusDefaultsPackages ==
  {"Foo", "Std"}

ExplicitPlusDefaultsFeatures ==
  StdFeatureUniverse

ExplicitPlusDefaultsRoots ==
  {"Foo"}

ExplicitPlusDefaultsEdges ==
  {"FooToStd"}

ExplicitPlusDefaultsEdgeFrom ==
  [e \in ExplicitPlusDefaultsEdges |-> "Foo"]

ExplicitPlusDefaultsEdgeTo ==
  [e \in ExplicitPlusDefaultsEdges |-> "Std"]

ExplicitPlusDefaultsEdgeAllowsDefaults ==
  [e \in ExplicitPlusDefaultsEdges |-> TRUE]

ExplicitPlusDefaultsDeclaredFeatures ==
  [p \in ExplicitPlusDefaultsPackages |->
    CASE p = "Std" -> StdFeatureUniverse
      [] OTHER -> {}]

ExplicitPlusDefaultsDefaultFeatures ==
  [p \in ExplicitPlusDefaultsPackages |->
    CASE p = "Std" -> {"compress", "tls"}
      [] OTHER -> {}]

ExplicitPlusDefaultsEdgeFeatures ==
  [e \in ExplicitPlusDefaultsEdges |-> {"net"}]

ExplicitPlusDefaultsExpectedDefaultReachable ==
  {"Foo", "Std"}

ExplicitPlusDefaultsExpectedEffective ==
  [p \in ExplicitPlusDefaultsPackages |->
    CASE p = "Std" -> {"net", "compress", "tls"}
      [] OTHER -> {}]

\* Unreachable subgraphs should contribute nothing.

UnreachablePackages ==
  {"Foo", "Dead", "Std"}

UnreachableFeatures ==
  StdFeatureUniverse

UnreachableRoots ==
  {"Foo"}

UnreachableEdges ==
  {"FooToStd", "DeadToStd"}

UnreachableEdgeFrom ==
  [e \in UnreachableEdges |->
    CASE e = "FooToStd" -> "Foo"
      [] e = "DeadToStd" -> "Dead"]

UnreachableEdgeTo ==
  [e \in UnreachableEdges |-> "Std"]

UnreachableEdgeAllowsDefaults ==
  [e \in UnreachableEdges |->
    CASE e = "FooToStd" -> FALSE
      [] e = "DeadToStd" -> TRUE]

UnreachableDeclaredFeatures ==
  [p \in UnreachablePackages |->
    CASE p = "Std" -> StdFeatureUniverse
      [] OTHER -> {}]

UnreachableDefaultFeatures ==
  [p \in UnreachablePackages |->
    CASE p = "Std" -> {"compress", "tls"}
      [] OTHER -> {}]

UnreachableEdgeFeatures ==
  [e \in UnreachableEdges |->
    CASE e = "FooToStd" -> {"net"}
      [] e = "DeadToStd" -> {"io"}]

UnreachableExpectedDefaultReachable ==
  {"Foo"}

UnreachableExpectedEffective ==
  [p \in UnreachablePackages |->
    CASE p = "Std" -> {"net"}
      [] OTHER -> {}]

\* Duplicate explicit requests are idempotent because effective features are a
\* set, not a multiset.

DuplicatePackages ==
  {"Foo", "A", "B", "Std"}

DuplicateFeatures ==
  StdFeatureUniverse

DuplicateRoots ==
  {"Foo"}

DuplicateEdges ==
  {"FooToA", "FooToB", "AToStd", "BToStd"}

DuplicateEdgeFrom ==
  [e \in DuplicateEdges |->
    CASE e = "FooToA" -> "Foo"
      [] e = "FooToB" -> "Foo"
      [] e = "AToStd" -> "A"
      [] e = "BToStd" -> "B"]

DuplicateEdgeTo ==
  [e \in DuplicateEdges |->
    CASE e = "FooToA" -> "A"
      [] e = "FooToB" -> "B"
      [] OTHER -> "Std"]

DuplicateEdgeAllowsDefaults ==
  [e \in DuplicateEdges |->
    CASE e = "FooToA" -> TRUE
      [] e = "FooToB" -> TRUE
      [] OTHER -> FALSE]

DuplicateDeclaredFeatures ==
  [p \in DuplicatePackages |->
    CASE p = "Std" -> StdFeatureUniverse
      [] OTHER -> {}]

DuplicateDefaultFeatures ==
  [p \in DuplicatePackages |->
    CASE p = "Std" -> {"compress", "tls"}
      [] OTHER -> {}]

DuplicateEdgeFeatures ==
  [e \in DuplicateEdges |->
    CASE e = "AToStd" -> {"net"}
      [] e = "BToStd" -> {"net"}
      [] OTHER -> {}]

DuplicateExpectedDefaultReachable ==
  {"Foo", "A", "B"}

DuplicateExpectedEffective ==
  [p \in DuplicatePackages |->
    CASE p = "Std" -> {"net"}
      [] OTHER -> {}]

\* Defaults can propagate transitively through libraries when no edge on the
\* path cuts them.

TransitiveDefaultsPackages ==
  {"Foo", "Bar", "Std"}

TransitiveDefaultsFeatures ==
  StdFeatureUniverse

TransitiveDefaultsRoots ==
  {"Foo"}

TransitiveDefaultsEdges ==
  {"FooToBar", "BarToStd"}

TransitiveDefaultsEdgeFrom ==
  [e \in TransitiveDefaultsEdges |->
    CASE e = "FooToBar" -> "Foo"
      [] e = "BarToStd" -> "Bar"]

TransitiveDefaultsEdgeTo ==
  [e \in TransitiveDefaultsEdges |->
    CASE e = "FooToBar" -> "Bar"
      [] e = "BarToStd" -> "Std"]

TransitiveDefaultsEdgeAllowsDefaults ==
  [e \in TransitiveDefaultsEdges |-> TRUE]

TransitiveDefaultsDeclaredFeatures ==
  [p \in TransitiveDefaultsPackages |->
    CASE p = "Std" -> StdFeatureUniverse
      [] OTHER -> {}]

TransitiveDefaultsDefaultFeatures ==
  [p \in TransitiveDefaultsPackages |->
    CASE p = "Std" -> {"compress", "tls"}
      [] OTHER -> {}]

TransitiveDefaultsEdgeFeatures ==
  [e \in TransitiveDefaultsEdges |-> {}]

TransitiveDefaultsExpectedDefaultReachable ==
  {"Foo", "Bar", "Std"}

TransitiveDefaultsExpectedEffective ==
  [p \in TransitiveDefaultsPackages |->
    CASE p = "Std" -> {"compress", "tls"}
      [] OTHER -> {}]

VARIABLES
  processed,
  reachable,
  defaultReachable,
  requested

vars ==
  << processed, reachable, defaultReachable, requested >>

EnabledEdges ==
  { e \in Edges \ processed : EdgeFrom[e] \in reachable }

Done ==
  EnabledEdges = {}

EffectiveFeatures(pkg) ==
  requested[pkg]
  \cup (IF pkg \in defaultReachable THEN DefaultFeatures[pkg] ELSE {})

ExpectedEffectiveFeatures(pkg) ==
  RequestedFromReachableEdges(pkg)
  \cup (IF DefaultReachableByPath(pkg) THEN DefaultFeatures[pkg] ELSE {})

Init ==
  /\ processed = {}
  /\ reachable = Roots
  /\ defaultReachable = Roots
  /\ requested = [p \in Packages |-> {}]

ProcessEdge(e) ==
  /\ e \in EnabledEdges
  /\ processed' = processed \cup {e}
  /\ reachable' = reachable \cup {EdgeTo[e]}
  /\ requested' = [requested EXCEPT ![EdgeTo[e]] = @ \cup EdgeFeatures[e]]
  /\ defaultReachable' =
      IF EdgeFrom[e] \in defaultReachable /\ EdgeAllowsDefaults[e]
        THEN defaultReachable \cup {EdgeTo[e]}
        ELSE defaultReachable

Next ==
  \/ \E e \in EnabledEdges : ProcessEdge(e)
  \/ /\ Done
     /\ UNCHANGED vars

Spec ==
  Init /\ [][Next]_vars

TypeOK ==
  /\ processed \subseteq Edges
  /\ reachable \subseteq Packages
  /\ defaultReachable \subseteq Packages
  /\ defaultReachable \subseteq reachable
  /\ requested \in [Packages -> SUBSET Features]
  /\ \A p \in Packages : requested[p] \subseteq DeclaredFeatures[p]

ProcessedEdgesComeFromReachableSources ==
  \A e \in processed : EdgeFrom[e] \in reachable

ExplicitRequestsMatchProcessedEdges ==
  requested = [p \in Packages |-> RequestedFromEdges(processed, p)]

DoneReachabilityMatchesPathSemantics ==
  Done =>
    reachable = { p \in Packages : ReachableByPath(p) }

DoneDefaultsMatchPathSemantics ==
  Done =>
    defaultReachable = { p \in Packages : DefaultReachableByPath(p) }

DoneEffectiveFeaturesMatchPathSemantics ==
  Done =>
    \A p \in Packages : EffectiveFeatures(p) = ExpectedEffectiveFeatures(p)

ScenarioMatchesExpectedDefaultReachability ==
  ~Done \/ defaultReachable = ExpectedDefaultReachable

ScenarioMatchesExpectedEffectiveFeatures ==
  ~Done \/ \A p \in Packages : EffectiveFeatures(p) = ExpectedEffective[p]

=============================================================================
