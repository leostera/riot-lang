open Global

type execution_mode =
  | Concurrent
  | Linear
(** A suite-level hook run by the shared test CLI. *)
type suite_hook = unit -> (unit, string) result

val main:
  ?execution_mode:execution_mode ->
  (** [setup] runs once before selected tests. If it fails, the suite fails without running tests. *)
  ?setup:suite_hook ->
  (** [teardown] runs once after selected tests. If it fails, the active reporter emits a warning. *)
  ?teardown:suite_hook ->
  name:string ->
  tests:Test_case.t list ->
  args:string list ->
  unit ->
  (unit, Runtime.Actor.exit_reason) result
