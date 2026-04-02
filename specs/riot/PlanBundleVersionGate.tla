---------------- MODULE PlanBundleVersionGate ----------------
EXTENDS Naturals, TLC

\* Readable PlusCal slice for the warm-plan cache gate in:
\*
\* - packages/riot-planner/src/package_planner.ml
\*
\* This model narrows to the decision boundary after `Store.load_plan_bundle`:
\*
\* - missing bundles rebuild
\* - decode exceptions rebuild
\* - wrong bundle version rebuilds
\* - wrong package identity rebuilds
\* - module-graph or action-graph parse failures rebuild
\* - only a fully accepted bundle yields a warm cache hit

CONSTANTS
  BundlePresent,
  BundleVersion,
  ExpectedVersion,
  PackageMatches,
  DecodeRaises,
  ModuleGraphParses,
  ActionGraphParses

ASSUME BundlePresent \in BOOLEAN
ASSUME BundleVersion \in Nat
ASSUME ExpectedVersion \in Nat
ASSUME PackageMatches \in BOOLEAN
ASSUME DecodeRaises \in BOOLEAN
ASSUME ModuleGraphParses \in BOOLEAN
ASSUME ActionGraphParses \in BOOLEAN

\* Passing smoke model: a fresh compatible bundle hits the cache.
SmokeBundlePresent == TRUE
SmokeBundleVersion == 1
SmokeExpectedVersion == 1
SmokePackageMatches == TRUE
SmokeDecodeRaises == FALSE
SmokeModuleGraphParses == TRUE
SmokeActionGraphParses == TRUE

\* Stale bundle model: the cached bundle exists but has the wrong version, so
\* the planner must rebuild its graphs.
StaleVersionBundlePresent == TRUE
StaleVersionBundleVersion == 0
StaleVersionExpectedVersion == 1
StaleVersionPackageMatches == TRUE
StaleVersionDecodeRaises == FALSE
StaleVersionModuleGraphParses == TRUE
StaleVersionActionGraphParses == TRUE

FreshBundleAccepted ==
  BundlePresent
  /\ ~DecodeRaises
  /\ BundleVersion = ExpectedVersion
  /\ PackageMatches
  /\ ModuleGraphParses
  /\ ActionGraphParses

(* --algorithm PlanBundleVersionGate
variables
  bundle_state = "unknown",
  parse_state = "unknown",
  outcome = "pending";

begin
  LoadBundle:
    if ~BundlePresent then
      bundle_state := "missing";
      outcome := "rebuild";
      goto Finished;
    end if;

  BundleLoaded:
    bundle_state := "loaded";

  DecodeBundle:
    if DecodeRaises then
      parse_state := "decode_exception";
      outcome := "rebuild";
      goto Finished;
    end if;

  CheckVersion:
    if BundleVersion # ExpectedVersion then
      parse_state := "stale_version";
      outcome := "rebuild";
      goto Finished;
    end if;

  CheckPackage:
    if ~PackageMatches then
      parse_state := "wrong_package";
      outcome := "rebuild";
      goto Finished;
    end if;

  CheckGraphs:
    if ~ModuleGraphParses \/ ~ActionGraphParses then
      parse_state := "graph_parse_failed";
      outcome := "rebuild";
      goto Finished;
    end if;

  AcceptBundle:
    parse_state := "accepted";
    outcome := "cache_hit";

  Finished:
    skip;
end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "9b930ba6" /\ chksum(tla) = "383b3a0c")
VARIABLES bundle_state, parse_state, outcome, pc

vars == << bundle_state, parse_state, outcome, pc >>

Init == (* Global variables *)
        /\ bundle_state = "unknown"
        /\ parse_state = "unknown"
        /\ outcome = "pending"
        /\ pc = "LoadBundle"

LoadBundle == /\ pc = "LoadBundle"
              /\ IF ~BundlePresent
                    THEN /\ bundle_state' = "missing"
                         /\ outcome' = "rebuild"
                         /\ pc' = "Finished"
                    ELSE /\ pc' = "BundleLoaded"
                         /\ UNCHANGED << bundle_state, outcome >>
              /\ UNCHANGED parse_state

BundleLoaded == /\ pc = "BundleLoaded"
                /\ bundle_state' = "loaded"
                /\ pc' = "DecodeBundle"
                /\ UNCHANGED << parse_state, outcome >>

DecodeBundle == /\ pc = "DecodeBundle"
                /\ IF DecodeRaises
                      THEN /\ parse_state' = "decode_exception"
                           /\ outcome' = "rebuild"
                           /\ pc' = "Finished"
                      ELSE /\ pc' = "CheckVersion"
                           /\ UNCHANGED << parse_state, outcome >>
                /\ UNCHANGED bundle_state

CheckVersion == /\ pc = "CheckVersion"
                /\ IF BundleVersion # ExpectedVersion
                      THEN /\ parse_state' = "stale_version"
                           /\ outcome' = "rebuild"
                           /\ pc' = "Finished"
                      ELSE /\ pc' = "CheckPackage"
                           /\ UNCHANGED << parse_state, outcome >>
                /\ UNCHANGED bundle_state

CheckPackage == /\ pc = "CheckPackage"
                /\ IF ~PackageMatches
                      THEN /\ parse_state' = "wrong_package"
                           /\ outcome' = "rebuild"
                           /\ pc' = "Finished"
                      ELSE /\ pc' = "CheckGraphs"
                           /\ UNCHANGED << parse_state, outcome >>
                /\ UNCHANGED bundle_state

CheckGraphs == /\ pc = "CheckGraphs"
               /\ IF ~ModuleGraphParses \/ ~ActionGraphParses
                     THEN /\ parse_state' = "graph_parse_failed"
                          /\ outcome' = "rebuild"
                          /\ pc' = "Finished"
                     ELSE /\ pc' = "AcceptBundle"
                          /\ UNCHANGED << parse_state, outcome >>
               /\ UNCHANGED bundle_state

AcceptBundle == /\ pc = "AcceptBundle"
                /\ parse_state' = "accepted"
                /\ outcome' = "cache_hit"
                /\ pc' = "Finished"
                /\ UNCHANGED bundle_state

Finished == /\ pc = "Finished"
            /\ TRUE
            /\ pc' = "Done"
            /\ UNCHANGED << bundle_state, parse_state, outcome >>

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == pc = "Done" /\ UNCHANGED vars

Next == LoadBundle \/ BundleLoaded \/ DecodeBundle \/ CheckVersion
           \/ CheckPackage \/ CheckGraphs \/ AcceptBundle \/ Finished
           \/ Terminating

Spec == Init /\ [][Next]_vars

Termination == <>(pc = "Done")

\* END TRANSLATION

TypeOK ==
  /\ bundle_state \in {"unknown", "missing", "loaded"}
  /\ parse_state \in {
       "unknown",
       "decode_exception",
       "stale_version",
       "wrong_package",
       "graph_parse_failed",
       "accepted"
     }
  /\ outcome \in {"pending", "rebuild", "cache_hit"}

Settled ==
  pc = "Done"

FreshBundleHitsCache ==
  Settled
  /\ FreshBundleAccepted
  =>
  outcome = "cache_hit"

CacheHitOnlyWhenBundleAccepted ==
  Settled
  /\ outcome = "cache_hit"
  =>
  FreshBundleAccepted /\ parse_state = "accepted"

StaleVersionForcesRebuild ==
  Settled
  /\ BundlePresent
  /\ ~DecodeRaises
  /\ BundleVersion # ExpectedVersion
  =>
  outcome = "rebuild" /\ parse_state = "stale_version"

=============================================================================
