(** Build queue management - Two-queue system for dependency ordering *)

(** Queue type that manages packages ready to build and packages waiting on dependencies *)
type t = {
  current_queue : string Queue.t;  (* Packages ready to build *)
  later_queue : string Queue.t;     (* Packages waiting on dependencies *)
}

(** Create a new build queue *)
let create () = {
  current_queue = Queue.create ();
  later_queue = Queue.create ();
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

(** Get the next package from the current queue *)
let take_ready t =
  if Queue.is_empty t.current_queue then
    None
  else
    Some (Queue.take t.current_queue)

(** Move packages from later queue to current queue based on a predicate 
    Returns the number of packages moved *)
let promote_ready t is_ready =
  if Queue.is_empty t.later_queue then
    0
  else
    let later_items = ref [] in
    (* Take all items from later queue *)
    while not (Queue.is_empty t.later_queue) do
      later_items := Queue.take t.later_queue :: !later_items
    done;
    
    (* Partition into ready and still-waiting *)
    let moved = ref 0 in
    List.iter (fun pkg_name ->
      if is_ready pkg_name then (
        Queue.add pkg_name t.current_queue;
        incr moved
      ) else
        Queue.add pkg_name t.later_queue
    ) !later_items;
    !moved

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