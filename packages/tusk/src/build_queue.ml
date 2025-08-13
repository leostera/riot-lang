(** Build queue management - Two-queue system for dependency ordering *)

(** Queue type that manages packages ready to build and packages waiting on dependencies *)
type t = {
  current_queue : string Queue.t;  (* Packages ready to build *)
  later_queue : string Queue.t;     (* Packages waiting on dependencies *)
  build_results : Build_results.t; (* Reference to build results for automatic filtering *)
}

(** Create a new build queue *)
let create build_results = {
  current_queue = Queue.create ();
  later_queue = Queue.create ();
  build_results;
}

(** Add a package to the current (ready) queue *)
let add_ready t pkg_name =
  Queue.add pkg_name t.current_queue

(** Add a package to the later (waiting) queue *)
let add_waiting t pkg_name =
  Queue.add pkg_name t.later_queue

(** Check if there's any work available in either queue *)
let is_empty t =
  Queue.is_empty t.current_queue && Queue.is_empty t.later_queue

(** Check if the current queue has work *)
let has_ready_work t =
  not (Queue.is_empty t.current_queue)

(** Helper to promote packages from later queue when dependencies are ready *)
let rec promote_from_later t =
  if Queue.is_empty t.later_queue then
    ()
  else
    let later_items = ref [] in
    (* Take all items from later queue *)
    while not (Queue.is_empty t.later_queue) do
      later_items := Queue.take t.later_queue :: !later_items
    done;
    
    (* Check each package and either promote or keep waiting *)
    List.iter (fun pkg_name ->
      (* Get dependencies for this package - we need the build graph context *)
      (* For now, we'll use a simple check based on build results *)
      (* In a real implementation, we'd need the dependency information *)
      match Build_results.get_status t.build_results pkg_name with
      | Some (Built _) -> () (* Already built, skip *)
      | _ -> 
          (* For now, add back to later queue - this needs dependency checking *)
          Queue.add pkg_name t.later_queue
    ) !later_items

(** Get the next package from the current queue *)
let rec take_ready t =
  (* First try to promote packages from later queue *)
  promote_from_later t;
  
  if Queue.is_empty t.current_queue then
    None
  else
    let pkg = Queue.take t.current_queue in
    (* Check if already built *)
    match Build_results.get_status t.build_results pkg with
    | Some (Built _) -> take_ready t (* Skip and try next *)
    | _ -> Some pkg


(** Get statistics about the queue *)
let stats t =
  let current_size = Queue.length t.current_queue in
  let later_size = Queue.length t.later_queue in
  (current_size, later_size)

(** Clear both queues *)
let clear t =
  Queue.clear t.current_queue;
  Queue.clear t.later_queue

(** Peek at what's in the later queue without removing *)
let peek_waiting t =
  let items = ref [] in
  Queue.iter (fun item -> items := item :: !items) t.later_queue;
  List.rev !items

(** Peek at what's in the current queue without removing *)
let peek_ready t =
  let items = ref [] in
  Queue.iter (fun item -> items := item :: !items) t.current_queue;
  List.rev !items

(** Check if a package is in the waiting queue *)
let is_waiting t pkg_name =
  let waiting_items = peek_waiting t in
  List.mem pkg_name waiting_items

(** Move a package from waiting to ready queue *)
let move_to_ready t pkg_name =
  (* Take all items from later queue *)
  let later_items = ref [] in
  while not (Queue.is_empty t.later_queue) do
    later_items := Queue.take t.later_queue :: !later_items
  done;
  
  (* Put back items except the one we're moving *)
  List.iter (fun item ->
    if item = pkg_name then
      Queue.add item t.current_queue (* Move to ready queue *)
    else
      Queue.add item t.later_queue   (* Keep in waiting queue *)
  ) (List.rev !later_items)