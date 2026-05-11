open Global

(**
   Fixture metadata attached to a test context when a case comes from a shared
   fixture runner.
*)
type fixture = {
  (** Absolute fixture path. *)
  path: Path.t;
  (** Fixture path relative to the fixture root. *)
  relpath: Path.t;
  (** Human-readable fixture name. *)
  name: string;
  (** Snapshot path associated with this fixture, when any. *)
  snapshot_path: Path.t option;
}
type built_binary = {
  (** Binary name as declared or autodiscovered for an available runtime binary. *)
  name: string;
  (** Absolute built binary path. *)
  path: Path.t;
}
type snapshot_mode =
  | External
  | Inline
type snapshot_format =
  | Text
  | Json
type snapshot_mismatch_reason =
  | Missing_approved
  | Pending_exists
  | Mismatch
type progress =
  | PropertyIterationPassed of { current: int; total: int; size: int }
  | PropertyAssumptionRejected of { current: int; total: int; size: int; rejected_count: int }
  | PropertyCounterExampleFound of { current: int; total: int; size: int }
  | PropertyShrinkStep of { current: int; total: int; step: int; max_steps: int }
  | SnapshotAssertionStarted of {
      mode: snapshot_mode;
      format: snapshot_format;
      approved_path: Path.t option;
      pending_path: Path.t option;
    }
  | SnapshotAssertionMatched of {
      mode: snapshot_mode;
      format: snapshot_format;
      approved_path: Path.t option;
    }
  | SnapshotAssertionMismatch of {
      mode: snapshot_mode;
      format: snapshot_format;
      approved_path: Path.t option;
      pending_path: Path.t option;
      reason: snapshot_mismatch_reason;
    }
type progress_handler = progress -> unit

module Store: sig
  type t = Collections.TypedKeyHashMap.t
  type 'a key = 'a Collections.TypedKeyHashMap.key

  val create: unit -> t

  val key: unit -> 'a key

  val insert: t -> 'a key -> 'a -> 'a option

  val get: t -> 'a key -> 'a option

  val remove: t -> 'a key -> 'a option
end

type 'a key = 'a Store.key

val key: unit -> 'a key

(**
   Stable per-test metadata supplied by the shared test runner.

   This context intentionally carries enough identity to support snapshot
   storage, fixture-backed cases, and future temp-artifact helpers without
   relying on hidden global state.
*)
type t = {
  (** Human-readable suite name. *)
  suite_name: string;
  (** Suite-scoped typed context shared by setup, tests, and teardown. *)
  context_store: Store.t;
  (** Human-readable test name. *)
  test_name: string;
  (** Position of the test in the suite. *)
  test_index: int;
  (** Source file defining the test, when known. *)
  source_file: Path.t option;
  (** Built test binary path, when known. *)
  binary_path: Path.t option;
  (** Built runtime binaries for the suite package's reachable runtime dependency closure, when any. *)
  built_binaries: built_binary list;
  (** Workspace root, when available from the runner. *)
  workspace_root: Path.t option;
  (** Owning package name, when available. *)
  package_name: string option;
  (** Fixture metadata for fixture-backed tests. *)
  fixture: fixture option;
  (** Internal progress callback used by shared property/snapshot helpers. *)
  progress_handler: progress_handler;
}

(** Attach fixture metadata to an existing test context. *)
val with_fixture: t -> fixture -> t

(** Replace the progress callback on an existing test context. *)
val with_progress_handler: t -> progress_handler -> t

(** Emit a structured progress event for the current test case. *)
val emit_progress: t -> progress -> unit

(** Default no-op progress handler. *)
val no_progress_handler: progress_handler

(** Read a suite-scoped typed context value. *)
val get: t -> 'a key -> 'a option

(** Find an available built binary by name. *)
val find_binary: t -> string -> Path.t option

(** Require an available built binary by name. *)
val require_binary: t -> string -> (Path.t, string) result
