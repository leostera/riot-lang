open Global

(** Fixture metadata attached to a test context when a case comes from a shared
    fixture runner. *)
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
(** Stable per-test metadata supplied by the shared test runner.

    This context intentionally carries enough identity to support snapshot
    storage, fixture-backed cases, and future temp-artifact helpers without
    relying on hidden global state.
*)
type t = {
  (** Human-readable suite name. *)
  suite_name: string;
  (** Human-readable test name. *)
  test_name: string;
  (** Position of the test in the suite. *)
  test_index: int;
  (** Source file defining the test, when known. *)
  source_file: Path.t option;
  (** Built test binary path, when known. *)
  binary_path: Path.t option;
  (** Workspace root, when available from the runner. *)
  workspace_root: Path.t option;
  (** Owning package name, when available. *)
  package_name: string option;
  (** Fixture metadata for fixture-backed tests. *)
  fixture: fixture option;
}

(** Attach fixture metadata to an existing test context. *)
val with_fixture: t -> fixture -> t
