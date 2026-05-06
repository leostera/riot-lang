open Std
open Std.Collections

module DynamicWorkerPool = WorkerPool.DynamicWorkerPool

module Node_id = struct
  type t = int

  let equal = fun left right -> left = right

  let compare = Int.compare

  let to_int = fun node_id -> node_id
end

module Run_config = struct
  type mode =
    | Fail_fast
    | Continue_on_failure

  type t = {
    parallelism: int;
    mode: mode;
  }

  let make = fun ~parallelism ~mode () -> { parallelism = Int.max 1 parallelism; mode }

  let parallelism = fun config -> config.parallelism

  let mode = fun config -> config.mode
end

type 'work node = {
  id: Node_id.t;
  payload: 'work;
  deps: Node_id.t HashSet.t;
  dependents: Node_id.t HashSet.t;
}

module Graph = struct
  type ('work, 'mutation) t = {
    nodes: (Node_id.t, 'work node) HashMap.t;
    apply_mutation: ('work, 'mutation) t -> 'mutation -> unit;
    mutable next_id: int;
  }

  let create = fun ~apply_mutation () -> {
    nodes = HashMap.create ();
    apply_mutation;
    next_id = 1;
  }

  let find_node = fun graph node_id ->
    match HashMap.get graph.nodes ~key:node_id with
    | Some node -> node
    | None -> panic ("graph scheduler: missing node " ^ Int.to_string node_id)

  let add_node_with_id = fun graph ~id ~payload ->
    let node = {
      id;
      payload;
      deps = HashSet.create ();
      dependents = HashSet.create ();
    }
    in
    let _ = HashMap.insert graph.nodes ~key:id ~value:node in
    node

  let add_node = fun graph ~payload ->
    let node_id = graph.next_id in
    graph.next_id <- graph.next_id + 1;
    let _ = add_node_with_id graph ~id:node_id ~payload in
    node_id

  let add_dependency_internal = fun graph ~node ~depends_on ->
    let dependent = find_node graph node in
    let dependency = find_node graph depends_on in
    let inserted = HashSet.insert dependent.deps ~value:depends_on in
    if inserted then (
      let _ = HashSet.insert dependency.dependents ~value:node in
      ()
    );
    inserted

  let add_dependency = fun graph ~node ~depends_on ->
    ignore
      (add_dependency_internal graph ~node ~depends_on)

  let payload = fun graph node_id ->
    HashMap.get graph.nodes ~key:node_id
    |> Option.map ~fn:(fun node -> node.payload)

  let dependencies = fun graph node_id ->
    let node = find_node graph node_id in
    HashSet.to_list node.deps
end

type ('work, 'mutation, 'event, 'result, 'error) command =
  | Add_node of {
      local_id: Node_id.t;
      payload: 'work;
    }
  | Add_dependency of {
      node: Node_id.t;
      depends_on: Node_id.t;
    }
  | Record_mutation of 'mutation
  | Emit_event of 'event
  | Complete_node of {
      node: Node_id.t;
      outcome: ('result, 'error) result;
    }

module Handle = struct
  type ('work, 'mutation, 'event, 'result, 'error) t = {
    mutable next_local_id: int;
    mutable commands: ('work, 'mutation, 'event, 'result, 'error) command list;
    emit: 'event -> unit;
  }

  let create = fun ~emit_event () -> {
    next_local_id = (-1);
    commands = [];
    emit = emit_event;
  }

  let push = fun handle command -> handle.commands <- command :: handle.commands

  let add_node = fun handle ~payload ->
    let node_id = handle.next_local_id in
    handle.next_local_id <- handle.next_local_id - 1;
    push handle (Add_node { local_id = node_id; payload });
    node_id

  let add_dependency = fun handle ~node ~depends_on ->
    push
      handle
      (Add_dependency { node; depends_on })

  let record = fun handle mutation -> push handle (Record_mutation mutation)

  let emit_event = fun handle event -> handle.emit event

  let complete_node = fun handle ~node ~outcome ->
    push handle (Complete_node { node; outcome })
end

type ('work, 'result, 'error) node_result = {
  node: Node_id.t;
  payload: 'work;
  outcome: ('result, 'error) result;
}

type ('work, 'result, 'error) run_result = {
  results: ('work, 'result, 'error) node_result list;
}

type (
  'work,
  'mutation,
  'event,
  'result,
  'error
) task_result = {
  node: Node_id.t;
  payload: 'work;
  outcome: ('result, 'error) result;
  commands: ('work, 'mutation, 'event, 'result, 'error) command list;
}

type ('work, 'result, 'error) runtime_node = {
  payload: 'work;
  mutable unresolved_dependencies: int;
  mutable status: [`Pending | `Running | `Completed of ('result, 'error) result];
}

type Message.t +=
  | GraphNodeResult: {
      result: ('work, 'mutation, 'event, 'result, 'error) task_result;
      result_ref: ('work, 'mutation, 'event, 'result, 'error) task_result Ref.t;
    } -> Message.t
  | GraphRunEvent: {
      event: 'event;
      event_ref: 'event Ref.t;
    } -> Message.t
  | GraphRunCompleted: {
      results: ('work, 'result, 'error) run_result;
      run_ref: (('work, 'result, 'error) run_result) Ref.t;
    } -> Message.t
  | GraphRunFailed: {
      exn: exn;
      run_ref: (('work, 'result, 'error) run_result) Ref.t;
    } -> Message.t

type (
  'work,
  'mutation,
  'event,
  'result,
  'error
) state = {
  config: Run_config.t;
  graph: ('work, 'mutation) Graph.t;
  pool: (Node_id.t * 'work) DynamicWorkerPool.t;
  ready_queue: (Node_id.t * 'work) Queue.t;
  idle_workers: (Node_id.t * 'work) DynamicWorkerPool.worker Queue.t;
  result_ref: ('work, 'mutation, 'event, 'result, 'error) task_result Ref.t;
  runtime_nodes: (Node_id.t, ('work, 'result, 'error) runtime_node) HashMap.t;
  on_event: 'event -> unit;
  mutable tasks_in_flight: int;
  mutable fail_fast_triggered: bool;
}

let should_block_new_work = fun state ->
  state.fail_fast_triggered && match Run_config.mode state.config with
  | Run_config.Fail_fast -> true
  | Run_config.Continue_on_failure -> false

let find_runtime_node = fun state node_id ->
  match HashMap.get state.runtime_nodes ~key:node_id with
  | Some node -> node
  | None -> panic ("graph scheduler: missing runtime node " ^ Int.to_string node_id)

let enqueue_if_ready = fun state node_id ->
  if should_block_new_work state then
    ()
  else
    let runtime_node = find_runtime_node state node_id in
    match runtime_node.status with
    | `Pending when runtime_node.unresolved_dependencies = 0 ->
        Queue.push state.ready_queue ~value:(node_id, runtime_node.payload)
    | `Pending
    | `Running
    | `Completed _ -> ()

let rec pop_dispatchable = fun state ->
  if should_block_new_work state then
    None
  else
    match Queue.pop state.ready_queue with
    | None -> None
    | Some (node_id, payload) ->
        let runtime_node = find_runtime_node state node_id in
        if runtime_node.unresolved_dependencies != 0 then
          pop_dispatchable state
        else
          match runtime_node.status with
          | `Pending ->
              runtime_node.status <- `Running;
              Some (node_id, payload)
          | `Running
          | `Completed _ -> pop_dispatchable state

let rec has_dispatchable = fun state ->
  if should_block_new_work state then
    false
  else
    match Queue.pop state.ready_queue with
    | None -> false
    | Some (node_id, payload) ->
        let runtime_node = find_runtime_node state node_id in
        if runtime_node.unresolved_dependencies != 0 then
          has_dispatchable state
        else
          match runtime_node.status with
          | `Pending ->
              Queue.push state.ready_queue ~value:(node_id, payload);
              true
          | `Running
          | `Completed _ -> has_dispatchable state

let dispatch_available = fun state ->
  let rec loop () =
    match Queue.pop state.idle_workers with
    | None -> ()
    | Some worker -> (
        match pop_dispatchable state with
        | Some task ->
            state.tasks_in_flight <- state.tasks_in_flight + 1;
            DynamicWorkerPool.send_task state.pool worker task;
            loop ()
        | None -> Queue.push state.idle_workers ~value:worker
      )
  in
  loop ()

let resolve_node_ref = fun locals node_id ->
  if Node_id.to_int node_id > 0 then
    node_id
  else
    match HashMap.get locals ~key:node_id with
    | Some resolved -> resolved
    | None ->
        panic ("graph scheduler: unresolved local node " ^ Int.to_string (Node_id.to_int node_id))

let mark_dependents_settled = fun state node_id ->
  let node = Graph.find_node state.graph node_id in
  HashSet.fold_left
    node.dependents
    ~init:[]
    ~fn:(fun acc dependent_id ->
      let dependent = find_runtime_node state dependent_id in
      if dependent.unresolved_dependencies > 0 then
        dependent.unresolved_dependencies <- dependent.unresolved_dependencies - 1;
      dependent_id :: acc)

let apply_command = fun state locals touched ->
  fun __tmp1 ->
    match __tmp1 with
    | Add_node { local_id; payload } ->
        let node_id = Graph.add_node state.graph ~payload in
        let _ = HashMap.insert locals ~key:local_id ~value:node_id in
        let _ =
          HashMap.insert
            state.runtime_nodes
            ~key:node_id
            ~value:{
              payload;
              unresolved_dependencies = 0;
              status = `Pending;
            }
        in
        node_id :: touched
    | Add_dependency { node; depends_on } ->
        let node = resolve_node_ref locals node in
        let depends_on = resolve_node_ref locals depends_on in
        if Graph.add_dependency_internal state.graph ~node ~depends_on then (
          let runtime_node = find_runtime_node state node in
          (
            match runtime_node.status with
            | `Pending -> ()
            | `Running
            | `Completed _ ->
                panic
                  ("graph scheduler: cannot add dependencies to active node "
                  ^ Int.to_string (Node_id.to_int node))
          );
          let dependency = find_runtime_node state depends_on in
          (
            match dependency.status with
            | `Completed _ -> ()
            | `Pending
            | `Running ->
                runtime_node.unresolved_dependencies <- runtime_node.unresolved_dependencies + 1
          );
          node :: touched
        ) else
          touched
    | Record_mutation mutation ->
        state.graph.apply_mutation state.graph mutation;
        touched
    | Emit_event event ->
        state.on_event event;
        touched
    | Complete_node { node; outcome } ->
        let node = resolve_node_ref locals node in
        let runtime_node = find_runtime_node state node in
        (
          match runtime_node.status with
          | `Pending ->
              runtime_node.status <- `Completed outcome;
              mark_dependents_settled state node @ touched
          | `Running ->
              panic
                ("graph scheduler: cannot complete running node "
                ^ Int.to_string (Node_id.to_int node))
          | `Completed _ -> touched
        )

let apply_commands = fun state commands ->
  let locals: (Node_id.t, Node_id.t) HashMap.t = HashMap.create () in
  List.fold_left
    commands
    ~init:[]
    ~fn:(apply_command state locals)

let completed_results = fun state ->
  HashMap.to_list state.runtime_nodes
  |> List.sort ~compare:(fun (left, _) (right, _) -> Node_id.compare left right)
  |> List.filter_map
    ~fn:(fun (node_id, runtime_node) ->
      match runtime_node.status with
      | `Completed outcome -> Some { node = node_id; payload = runtime_node.payload; outcome }
      | `Pending
      | `Running -> None)

let is_complete = fun state ->
  state.tasks_in_flight = 0 && (should_block_new_work state || not (has_dispatchable state))

let rec loop:
  type work mutation event result error. (work, mutation, event, result, error) state ->
  (work, result, error) run_result = fun state ->
  dispatch_available state;
  if is_complete state then
    { results = completed_results state }
  else
    let selector:
      ([
        | `WorkerReady of (Node_id.t * work) DynamicWorkerPool.worker
        | `NodeResult of (work, mutation, event, result, error) task_result
      ]) selector = fun msg ->
      match msg with
      | DynamicWorkerPool.WorkerReady worker -> (
          let worker_ref = DynamicWorkerPool.get_worker_task_ref worker in
          if Ref.equal state.pool.task_ref worker_ref then
            match Ref.type_equal state.pool.task_ref worker_ref with
            | Some Type.Equal -> Select (`WorkerReady worker)
            | None -> Skip
          else
            Skip
        )
      | GraphNodeResult { result; result_ref } -> (
          match Ref.type_equal state.result_ref result_ref with
          | Some Type.Equal -> Select (`NodeResult result)
          | None -> Skip
        )
      | _ -> Skip
    in
    match receive ~selector () with
    | `WorkerReady worker ->
        Queue.push state.idle_workers ~value:worker;
        loop state
    | `NodeResult result ->
        state.tasks_in_flight <- state.tasks_in_flight - 1;
        let runtime_node = find_runtime_node state result.node in
        runtime_node.status <- `Completed result.outcome;
        (
          match result.outcome with
          | Error _ ->
              if Run_config.mode state.config = Run_config.Fail_fast then
                state.fail_fast_triggered <- true
          | Ok _ -> ()
        );
        let touched = apply_commands state result.commands in
        let dependents = mark_dependents_settled state result.node in
        List.for_each (List.concat [ touched; dependents ]) ~fn:(enqueue_if_ready state);
        loop state

let run = fun ~config ~on_event ~graph ~execute ->
  if HashMap.length graph.Graph.nodes = 0 then
    { results = [] }
  else
    let owner = self () in
    let event_ref: 'event Ref.t = Ref.make () in
    let run_ref: (('work, 'result, 'error) run_result) Ref.t = Ref.make () in
    let init_run () =
      let runtime_nodes: (Node_id.t, ('work, 'result, 'error) runtime_node) HashMap.t =
        HashMap.create ()
      in
      HashMap.for_each
        graph.Graph.nodes
        ~fn:(fun node_id node ->
          let _ =
            HashMap.insert
              runtime_nodes
              ~key:node_id
              ~value:{
                payload = node.payload;
                unresolved_dependencies = HashSet.length node.deps;
                status = `Pending;
              }
          in
          ());
      let result_ref = Ref.make () in
      let emit_event = fun event -> send owner (GraphRunEvent { event; event_ref }) in
      let worker_owner = self () in
      let worker_fn ~owner ~task:(node, payload) =
        let handle = Handle.create ~emit_event () in
        let outcome = execute ~graph:handle ~node ~payload in
        let result = {
          node;
          payload;
          outcome;
          commands = List.reverse handle.commands;
        }
        in
        send owner (GraphNodeResult { result; result_ref })
      in
      let pool =
        DynamicWorkerPool.start
          ~concurrency:(Run_config.parallelism config)
          ~owner:worker_owner
          ~worker_fn
          ()
      in
      let state = {
        config;
        graph;
        pool;
        ready_queue = Queue.create ();
        idle_workers = Queue.create ();
        result_ref;
        runtime_nodes;
        on_event = emit_event;
        tasks_in_flight = 0;
        fail_fast_triggered = false;
      }
      in
      HashMap.for_each
        runtime_nodes
        ~fn:(fun node_id runtime_node ->
          if runtime_node.unresolved_dependencies = 0 then
            Queue.push state.ready_queue ~value:(node_id, runtime_node.payload));
      match loop state with
      | results ->
          send owner (GraphRunCompleted { results; run_ref });
          Ok ()
      | exception exn ->
          send owner (GraphRunFailed { exn; run_ref });
          Ok ()
    in
    let _ = spawn init_run in
    let rec await () =
      let selector:
        ([`Event of 'event | `Completed of ('work, 'result, 'error) run_result | `Failed of exn]) selector = fun
        __tmp1 ->
        match __tmp1 with
        | GraphRunEvent { event; event_ref = ref } -> (
            match Ref.cast ref event_ref event with
            | Some event -> Select (`Event event)
            | None -> Skip
          )
        | GraphRunCompleted { results; run_ref = ref } -> (
            match Ref.cast ref run_ref results with
            | Some results -> Select (`Completed results)
            | None -> Skip
          )
        | GraphRunFailed { exn; run_ref = ref } -> (
            if Ref.equal run_ref ref then
              Select (`Failed exn)
            else
              Skip
          )
        | _ -> Skip
      in
      match receive ~selector () with
      | `Event event ->
          on_event event;
          await ()
      | `Completed results -> results
      | `Failed exn -> raise exn
    in
    await ()
