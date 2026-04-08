open Global

(** Shared test execution context. *)
type ctx = Test_context.t = {
  suite_name: string;
  test_name: string;
  test_index: int;
  source_file: Path.t option;
  binary_path: Path.t option;
  workspace_root: Path.t option;
  package_name: string option;
  fixture: Test_context.fixture option;
}

(** Helpers for working with test contexts and fixtures. *)
module Context: sig
  type t = ctx
  type fixture = Test_context.fixture = {
    path: Path.t;
    relpath: Path.t;
    name: string;
    snapshot_path: Path.t option;
  }

  (** Attach fixture metadata to a test context. *)
  val with_fixture: t -> fixture -> t
end

(** Snapshot assertions for golden-file and inline snapshots. *)
module Snapshot = Snapshot

(** Shared fixture discovery and test expansion helpers. *)
module FixtureRunner = Fixture_runner

(** The type of test: regular unit test or property test with example count. *)
type test_type =
  | UnitTest
  | Property of { examples: int }
type size = Test_case.size =
  | Small
  | Large
type reliability = Test_case.reliability =
  | Stable
  | Flaky of { retry_attempts: int }
(** Public representation of a test case. *)
type test_case = Test_case.t

(** [case name fn] creates a regular unit test. *)
val case: ?size:size -> ?reliability:reliability -> string -> (ctx -> (unit, string) result) -> test_case

(** [property name ~examples fn] creates a property test.
    Use this for property-based tests to show the number of examples tested.
    
    Example:
    {[
      Test.property "my property" ~examples:1000 (fun _ctx ->
        (* property checking logic *)
        Ok ()
      )
    ]}
*)
val property:
  ?size:size ->
  ?reliability:reliability ->
  string ->
  examples:int ->
  (ctx -> (unit, string) result) ->
  test_case

(** [skip name fn] creates a skipped test. *)
val skip: ?size:size -> ?reliability:reliability -> string -> (ctx -> (unit, string) result) -> test_case

(** [todo name] creates a placeholder test marked as todo. *)
val todo: ?size:size -> ?reliability:reliability -> string -> test_case

(** Assert that [actual] equals [expected]. Raises on failure. *)
val assert_equal: expected:'a -> actual:'a -> unit

(** Assert that a result is [Error _]. Raises on success. *)
val assert_error: ('a, 'b) result -> unit

(** Assert that a boolean is [false]. Raises otherwise. *)
val assert_false: bool -> unit

(** Assert that a result is [Ok _]. Raises on error. *)
val assert_ok: ('a, 'b) result -> unit

(** Assert that a boolean is [true]. Raises otherwise. *)
val assert_true: bool -> unit

(** CLI helpers for test binaries. *)
module Cli: sig
  (** Main entry point for test binaries with CLI support. *)
  val main:
    name:string -> tests:test_case list -> args:string list -> (unit, Actors.Process.exit_reason) result
end
