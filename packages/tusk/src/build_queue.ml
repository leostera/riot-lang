(** Build queue management - Three-state system for dependency ordering *)

type t = {
  ready_queue : Build_node.t Queue.t; (* Nodes ready to build *)
  waiting_queue : Build_node.t Queue.t; (* Nodes waiting on dependencies *)
  busy_nodes : (string, Build_node.t) Hashtbl.t;
      (* pkg_name -> currently building node *)
}

(** Create a new build queue *)
let create _ =
  {
    ready_queue = Queue.create ();
    waiting_queue = Queue.create ();
    busy_nodes = Hashtbl.create 32;
  }

(** Add a node to the ready queue *)
let add_ready t node =
  let pkg_name = node.Build_node.package.name in
  (* Don't add if already busy or already in ready queue *)
  if not (Hashtbl.mem t.busy_nodes pkg_name) then (
    (* Check if already in ready queue *)
    let already_queued = ref false in
    Queue.iter
      (fun queued_node ->
        let queued_pkg = queued_node.Build_node.package.name in
        if queued_pkg = pkg_name then already_queued := true)
      t.ready_queue;
    if not !already_queued then Queue.add node t.ready_queue)

(** Add a node to the waiting queue *)
let add_waiting t node =
  let pkg_name = node.Build_node.package.name in
  if not (Hashtbl.mem t.busy_nodes pkg_name) then Queue.add node t.waiting_queue

(** Add a node to the queue *)
let queue t node =
  (* For now, just add to ready queue - in reality we'd check deps *)
  add_ready t node

(** Queue a node with dependencies *)
let queue_with_deps t node ~deps =
  (* First queue all dependencies *)
  List.iter (fun dep -> add_ready t dep) deps;
  (* Then add the node to waiting *)
  add_waiting t node

(** Get the next node from the ready queue *)
let rec next t =
  if Queue.is_empty t.ready_queue then None
  else
    let node = Queue.take t.ready_queue in
    let pkg_name = node.Build_node.package.name in
    (* Mark as busy *)
    Hashtbl.add t.busy_nodes pkg_name node;
    Some node

(** Mark a node as no longer busy *)
let mark_done t node =
  let pkg_name = node.Build_node.package.name in
  Hashtbl.remove t.busy_nodes pkg_name;
  (* Check if any waiting nodes can now be promoted to ready *)
  (* Move all waiting nodes to ready for now - proper dependency checking would be better *)
  let waiting_nodes = ref [] in
  while not (Queue.is_empty t.waiting_queue) do
    waiting_nodes := Queue.take t.waiting_queue :: !waiting_nodes
  done;
  List.iter (fun n -> add_ready t n) (List.rev !waiting_nodes)

(** Get queue statistics *)
let get_stats t =
  let ready = Queue.length t.ready_queue in
  let waiting = Queue.length t.waiting_queue in
  let busy = Hashtbl.length t.busy_nodes in
  (ready, waiting, busy)
