(** Worker pool management - Handles spawning, tracking, and shutting down build workers *)

open Miniriot

(** Worker pool state *)
type t = {
  workers : Pid.t list;           (* List of active worker PIDs *)
  idle_workers : Pid.t Queue.t;   (* Queue of workers waiting for tasks *)
  max_workers : int;              (* Maximum number of workers *)
  server_pid : Pid.t;             (* PID of the server process *)
}

(** Create a new worker pool *)
let create ~server_pid ~max_workers = {
  workers = [];
  idle_workers = Queue.create ();
  max_workers;
  server_pid;
}

(** Check if the pool has any workers *)
let has_workers t =
  t.workers <> []

(** Get the number of active workers *)
let worker_count t =
  List.length t.workers

(** Get the number of idle workers *)
let idle_count t =
  Queue.length t.idle_workers

(** Check if there are idle workers available *)
let has_idle_workers t =
  not (Queue.is_empty t.idle_workers)

(** Spawn workers if not already spawned *)
let ensure_workers t spawn_fn =
  if t.workers = [] then
    let workers = spawn_fn t.server_pid t.max_workers in
    { t with workers }
  else
    t

(** Add a worker to the idle queue *)
let mark_idle t worker_pid =
  if List.mem worker_pid t.workers then (
    Queue.add worker_pid t.idle_workers;
    t
  ) else
    t (* Ignore workers not in our pool *)

(** Get an idle worker if available *)
let get_idle_worker t =
  if Queue.is_empty t.idle_workers then
    None, t
  else
    let worker = Queue.take t.idle_workers in
    Some worker, t

(** Check if a worker belongs to this pool *)
let is_pool_member t worker_pid =
  List.mem worker_pid t.workers

(** Shutdown all workers in the pool *)
let shutdown_all t =
  (* Clear idle queue first *)
  Queue.clear t.idle_workers;
  
  (* Send shutdown to all workers *)
  List.iter (fun w -> send w Build_messages.Shutdown) t.workers;
  
  (* Return empty pool *)
  { t with workers = []; idle_workers = Queue.create () }

(** Remove a specific worker from the pool *)
let remove_worker t worker_pid =
  let workers = List.filter (fun w -> w <> worker_pid) t.workers in
  (* Also remove from idle queue if present *)
  let new_idle = Queue.create () in
  Queue.iter (fun w -> 
    if w <> worker_pid then Queue.add w new_idle
  ) t.idle_workers;
  { t with workers; idle_workers = new_idle }

(** Get statistics about the pool *)
let stats t =
  let total = List.length t.workers in
  let idle = Queue.length t.idle_workers in
  let busy = total - idle in
  (total, busy, idle)