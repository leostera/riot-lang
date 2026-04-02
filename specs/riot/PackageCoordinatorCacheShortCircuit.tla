---------------- MODULE PackageCoordinatorCacheShortCircuit ----------------
EXTENDS Naturals, FiniteSets, TLC

\* Readable PlusCal slice for the package-level cache short-circuit path in:
\*
\* - packages/riot-executor/src/coordinator.ml (`maybe_short_circuit_cached_package`)
\* - packages/riot-store/src/store.ml (`materialize_package_exports`)
\*
\* This slice narrows to one coordinator/store interaction:
\*
\* - the package hash artifact exists in the store
\* - some package exports are missing from the target directory
\* - the coordinator asks the store to materialize exports
\* - the store currently returns `Ok ()` even when an export source is missing
\* - the coordinator then marks the package `Cached`
\*
\* The semantic law we want is stronger: a package-level cache short-circuit
\* should only succeed if all declared exports are present in the target
\* directory afterwards.

CONSTANTS
  Exports,
  PackageArtifactPresent,
  InitiallyMaterialized,
  ExportSourcePresent

ASSUME Exports # {}
ASSUME PackageArtifactPresent \in BOOLEAN
ASSUME InitiallyMaterialized \subseteq Exports
ASSUME ExportSourcePresent \in [Exports -> BOOLEAN]

\* Passing smoke model: the package artifact exists and every missing export can
\* be materialized successfully.
SmokeExports ==
  {"lib.cmxa", "lib.cmxs"}

SmokePackageArtifactPresent ==
  TRUE

SmokeInitiallyMaterialized ==
  {}

SmokeExportSourcePresent ==
  [e \in SmokeExports |-> TRUE]

\* Bug model: the package hash artifact exists, but one export source is absent
\* from the action-level store. `materialize_package_exports` still returns
\* success, so the coordinator marks the package cached with an incomplete
\* target directory.
MissingExportBugExports ==
  {"lib.cmxa", "lib.cmxs"}

MissingExportBugPackageArtifactPresent ==
  TRUE

MissingExportBugInitiallyMaterialized ==
  {}

MissingExportBugExportSourcePresent ==
  [e \in MissingExportBugExports |->
      CASE e = "lib.cmxa" -> TRUE
        [] OTHER -> FALSE]

AllExportsPresent(target_exports) ==
  target_exports = Exports

(* --algorithm PackageCoordinatorCacheShortCircuit
variables
  target_exports = InitiallyMaterialized,
  package_status = "planning",
  short_circuit_taken = FALSE;

begin
  CheckPackageArtifact:
    if ~PackageArtifactPresent then
      package_status := "needs_execution";
      goto Finished;
    end if;

  CheckLocalExports:
    if AllExportsPresent(target_exports) then
      short_circuit_taken := TRUE;
      package_status := "cached";
      goto Finished;
    end if;

  MaterializeMissingExports:
    with missing = Exports \ target_exports do
      \* Mirrors `materialize_package_exports`: present sources are copied,
      \* missing sources only log a warning and do not fail the call.
      target_exports :=
        target_exports \cup {e \in missing : ExportSourcePresent[e]};

      \* Mirrors `maybe_short_circuit_cached_package`: any `Ok ()` result marks
      \* the package cached immediately.
      short_circuit_taken := TRUE;
      package_status := "cached";
    end with;

  Finished:
    skip;
end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "6f6c5963" /\ chksum(tla) = "ef0ffa6f")
VARIABLES target_exports, package_status, short_circuit_taken, pc

vars == << target_exports, package_status, short_circuit_taken, pc >>

Init == (* Global variables *)
        /\ target_exports = InitiallyMaterialized
        /\ package_status = "planning"
        /\ short_circuit_taken = FALSE
        /\ pc = "CheckPackageArtifact"

CheckPackageArtifact == /\ pc = "CheckPackageArtifact"
                        /\ IF ~PackageArtifactPresent
                              THEN /\ package_status' = "needs_execution"
                                   /\ pc' = "Finished"
                              ELSE /\ pc' = "CheckLocalExports"
                                   /\ UNCHANGED package_status
                        /\ UNCHANGED << target_exports, short_circuit_taken >>

CheckLocalExports == /\ pc = "CheckLocalExports"
                     /\ IF AllExportsPresent(target_exports)
                           THEN /\ short_circuit_taken' = TRUE
                                /\ package_status' = "cached"
                                /\ pc' = "Finished"
                           ELSE /\ pc' = "MaterializeMissingExports"
                                /\ UNCHANGED << package_status, 
                                                short_circuit_taken >>
                     /\ UNCHANGED target_exports

MaterializeMissingExports == /\ pc = "MaterializeMissingExports"
                             /\ LET missing == Exports \ target_exports IN
                                  /\ target_exports' = (target_exports \cup {e \in missing : ExportSourcePresent[e]})
                                  /\ short_circuit_taken' = TRUE
                                  /\ package_status' = "cached"
                             /\ pc' = "Finished"

Finished == /\ pc = "Finished"
            /\ TRUE
            /\ pc' = "Done"
            /\ UNCHANGED << target_exports, package_status, 
                            short_circuit_taken >>

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == pc = "Done" /\ UNCHANGED vars

Next == CheckPackageArtifact \/ CheckLocalExports
           \/ MaterializeMissingExports \/ Finished
           \/ Terminating

Spec == Init /\ [][Next]_vars

Termination == <>(pc = "Done")

\* END TRANSLATION

TypeOK ==
  /\ target_exports \subseteq Exports
  /\ package_status \in {"planning", "needs_execution", "cached"}
  /\ short_circuit_taken \in BOOLEAN

Settled ==
  pc = "Done"

CompleteMaterializationCanShortCircuit ==
  (Settled
   /\ PackageArtifactPresent
   /\ \A e \in Exports \ InitiallyMaterialized : ExportSourcePresent[e])
  =>
  (package_status = "cached"
   /\ target_exports = Exports)

CachedPackageMustExposeAllExports ==
  (Settled
   /\ package_status = "cached")
  =>
  target_exports = Exports

=============================================================================
