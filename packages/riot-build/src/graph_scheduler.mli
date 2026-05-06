open Std

module Node_id: sig
  type t

  val equal: t -> t -> bool

  val compare: t -> t -> Order.t

  val to_int: t -> int
end

module Run_config: sig
  type mode =
    | Fail_fast
    | Continue_on_failure
  type t

  val make: parallelism:int -> mode:mode -> unit -> t

  val parallelism: t -> int

  val mode: t -> mode
end

module Graph: sig
  type ('work, 'mutation) t

  val create:
    apply_mutation:(('work, 'mutation) t -> 'mutation -> unit) ->
    unit ->
    ('work, 'mutation) t

  val add_node: ('work, 'mutation) t -> payload:'work -> Node_id.t

  val add_dependency: ('work, 'mutation) t -> node:Node_id.t -> depends_on:Node_id.t -> unit

  val payload: ('work, 'mutation) t -> Node_id.t -> 'work option

  val dependencies: ('work, 'mutation) t -> Node_id.t -> Node_id.t list
end

module Handle: sig
  type ('work, 'mutation, 'event, 'result, 'error) t

  val add_node: ('work, 'mutation, 'event, 'result, 'error) t -> payload:'work -> Node_id.t

  val add_dependency:
    ('work, 'mutation, 'event, 'result, 'error) t ->
    node:Node_id.t ->
    depends_on:Node_id.t ->
    unit

  val record: ('work, 'mutation, 'event, 'result, 'error) t -> 'mutation -> unit

  val emit_event: ('work, 'mutation, 'event, 'result, 'error) t -> 'event -> unit

  val complete_node:
    ('work, 'mutation, 'event, 'result, 'error) t ->
    node:Node_id.t ->
    outcome:('result, 'error) result ->
    unit
end

type ('work, 'result, 'error) node_result = {
  node: Node_id.t;
  payload: 'work;
  outcome: ('result, 'error) result;
}
type ('work, 'result, 'error) run_result = {
  results: ('work, 'result, 'error) node_result list;
}

val run:
  config:Run_config.t ->
  on_event:('event -> unit) ->
  graph:('work, 'mutation) Graph.t ->
  execute:(
    graph:('work, 'mutation, 'event, 'result, 'error) Handle.t ->
    node:Node_id.t ->
    payload:'work ->
    ('result, 'error) result
  ) ->
  ('work, 'result, 'error) run_result
