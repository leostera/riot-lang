(** Build queue management - Three-state system for dependency ordering *)

(** Queue type that manages build tasks in different states *)
type t = {
  ready_queue : Build_messages.build_task Queue.t;    (* Tasks ready to build *)
  waiting_queue : Build_messages.build_task Queue.t;  (* Tasks waiting on dependencies *)  
  busy_tasks : (string, Build_messages.build_task) Hashtbl.t; (* pkg_name -> currently building task *)
  build_results : Build_results.t; (* Reference to build results for automatic filtering *)
}

(** Create a new build queue *)
let create build_results = {
  ready_queue = Queue.create ();
  waiting_queue = Queue.create ();
  busy_tasks = Hashtbl.create 32;
  build_results;
}

(** Add a task to the ready queue *)
let add_ready t task =
  let pkg_name = task.Build_messages.node.Build_node.package.name in
  (* Don't add if already busy or already in ready queue *)
  if not (Hashtbl.mem t.busy_tasks pkg_name) then (
    (* Check if already in ready queue *)
    let already_queued = ref false in
    Queue.iter (fun queued_task ->
      let queued_pkg = queued_task.Build_messages.node.Build_node.package.name in
      if queued_pkg = pkg_name then already_queued := true
    ) t.ready_queue;
    if not !already_queued then (
      Printf.printf "[BuildQueue] Adding %s to ready queue\n" pkg_name;
      Queue.add task t.ready_queue
    ) else
      Printf.printf "[BuildQueue] Skipping %s - already in ready queue\n" pkg_name
  ) else
    Printf.printf "[BuildQueue] Skipping %s - already busy\n" pkg_name

(** Add a task to the waiting queue *)
let add_waiting t task =
  let pkg_name = task.Build_messages.node.Build_node.package.name in
  if not (Hashtbl.mem t.busy_tasks pkg_name) then
    Queue.add task t.waiting_queue

(** Check if there's any work available in either queue *)
let is_empty t =
  Queue.is_empty t.ready_queue && Queue.is_empty t.waiting_queue

(** Check if the ready queue has work *)
let has_ready_work t =
  not (Queue.is_empty t.ready_queue)

(** Helper to promote tasks from waiting queue when dependencies are ready *)
let rec promote_from_waiting t =
  if Queue.is_empty t.waiting_queue then
    ()
  else
    let waiting_items = ref [] in
    (* Take all items from waiting queue *)
    while not (Queue.is_empty t.waiting_queue) do
      waiting_items := Queue.take t.waiting_queue :: !waiting_items
    done;
    
    (* Check each task and either promote or keep waiting *)
    List.iter (fun task ->
      let pkg_name = task.Build_messages.node.Build_node.package.name in
      (* Get dependencies for this package - we need the build graph context *)
      (* For now, we'll use a simple check based on build results *)
      (* In a real implementation, we'd need the dependency information *)
      match Build_results.get_status t.build_results pkg_name with
      | Some (Built _) -> () (* Already built, skip *)
      | _ -> 
          (* For now, add back to waiting queue - this needs dependency checking *)
          Queue.add task t.waiting_queue
    ) !waiting_items

(** Get the next task from the ready queue and mark it as busy *)
let rec take_ready t =
  (* First try to promote tasks from waiting queue *)
  promote_from_waiting t;
  
  if Queue.is_empty t.ready_queue then
    None
  else
    let task = Queue.take t.ready_queue in
    let pkg_name = task.Build_messages.node.Build_node.package.name in
    (* Check if already busy *)
    if Hashtbl.mem t.busy_tasks pkg_name then (
      take_ready t (* Skip and try next *)
    ) else
      (* Check if already built *)
      match Build_results.get_status t.build_results pkg_name with
      | Some (Built _) -> 
          take_ready t (* Skip and try next *)
      | _ -> 
          (* Mark as busy *)
          Printf.printf "[BuildQueue] Taking %s from ready queue\n" pkg_name;
          Hashtbl.replace t.busy_tasks pkg_name task;
          Some task


(** Get statistics about the queue *)
let stats t =
  let ready_size = Queue.length t.ready_queue in
  let waiting_size = Queue.length t.waiting_queue in
  let busy_size = Hashtbl.length t.busy_tasks in
  (ready_size, waiting_size, busy_size)

(** Clear all queues *)
let clear t =
  Queue.clear t.ready_queue;
  Queue.clear t.waiting_queue;
  Hashtbl.clear t.busy_tasks

(** Peek at what's in the waiting queue without removing *)
let peek_waiting t =
  let items = ref [] in
  Queue.iter (fun task -> 
    let pkg_name = task.Build_messages.node.Build_node.package.name in
    items := pkg_name :: !items
  ) t.waiting_queue;
  List.rev !items

(** Peek at what's in the ready queue without removing *)
let peek_ready t =
  let items = ref [] in
  Queue.iter (fun task -> 
    let pkg_name = task.Build_messages.node.Build_node.package.name in
    items := pkg_name :: !items
  ) t.ready_queue;
  List.rev !items

(** Check if a package is in the waiting queue *)
let is_waiting t pkg_name =
  let waiting_items = peek_waiting t in
  List.mem pkg_name waiting_items

(** Move a task from waiting to ready queue *)
let move_to_ready t pkg_name =
  (* Take all items from waiting queue *)
  let waiting_items = ref [] in
  while not (Queue.is_empty t.waiting_queue) do
    waiting_items := Queue.take t.waiting_queue :: !waiting_items
  done;
  
  (* Put back items except the one we're moving *)
  List.iter (fun task ->
    let task_pkg_name = task.Build_messages.node.Build_node.package.name in
    if task_pkg_name = pkg_name then
      Queue.add task t.ready_queue (* Move to ready queue *)
    else
      Queue.add task t.waiting_queue   (* Keep in waiting queue *)
  ) (List.rev !waiting_items)

(** Mark a task as completed and remove from busy queue **)
let mark_completed t pkg_name =
  Hashtbl.remove t.busy_tasks pkg_name

(** Check if a task is currently busy **)
let is_busy t pkg_name =
  Hashtbl.mem t.busy_tasks pkg_name