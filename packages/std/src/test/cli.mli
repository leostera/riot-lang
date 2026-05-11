open Global

type execution_mode =
  | Concurrent
  | Linear
(** Suite-scoped typed context store shared by setup, teardown, and cases. *)
type suite_context = Test_context.Store.t
(** A suite-level hook run by the shared test CLI. *)
type suite_hook = suite_context -> (unit, string) result

val main:
  ?execution_mode:execution_mode ->
  (** [setup] runs once before selected tests and can populate the suite context. If it fails, the suite fails without running tests. *)
  ?setup:suite_hook ->
  (** [teardown] runs once after selected tests and receives the suite context. If it fails, the active reporter emits a warning. *)
  ?teardown:suite_hook ->
  name:string ->
  tests:Test_case.t list ->
  args:string list ->
  unit ->
  (unit, Runtime.Actor.exit_reason) result
