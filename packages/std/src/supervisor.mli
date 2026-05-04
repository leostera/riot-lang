(* # Supervisor - OTP-style process supervision

   Supervisors are processes that monitor and restart child processes
   according to configurable strategies.

   Based on Erlang/OTP supervisor behavior with all standard strategies:
   - **One_for_one**: Restart only the failed child
   - **One_for_all**: Restart all children when one fails
   - **Rest_for_one**: Restart failed child and all started after it
   - **Simple_one_for_one**: All children use the same child spec

   ## Example

   ```ocaml
   open Std

   let worker_spec id =
     Supervisor.child_spec
       ~id:(format "worker_%d" id)
       ~start:(fun () -> spawn_worker id)
       ~restart:Permanent
       ()

   let supervisor =
     Supervisor.start_link
       ~strategy:OneForOne
       ~intensity:{ max_restarts = 5; window = Duration.from_sec 10 }
       ~children:[
         worker_spec 1;
         worker_spec 2;
         worker_spec 3;
       ]
       ()
   ```

   ## Restart Strategies

   - **Permanent**: Always restart the child (default for most services)
   - **Temporary**: Never restart the child
   - **Transient**: Restart only on abnormal exit (errors)

   ## Shutdown Behavior

   - **Brutal_kill**: Kill immediately
   - **Timeout**: Wait N seconds, then kill
   - **Infinity**: Wait forever (use for supervisors)

   ## Significant Children (OTP 24+)

   A child marked as `significant:true` will cause the supervisor to
   terminate if that child terminates, regardless of the supervision strategy.
*)
open Global

(** A supervisor process. *)
type t

(** Convert supervisor to Pid. *)
val to_pid: t -> Pid.t

type strategy =
  | OneForOne
  (** If one child fails, only that child is restarted *)
  | OneForAll
  (** If one child fails, all children are terminated and restarted *)
  | RestForOne
  (**
     If one child fails, that child and all children started after it
     are terminated and restarted
  *)
  | SimpleOneForOne
(**
   Simplified supervisor where all children use the same child_spec.
   Children are added dynamically.
*)
type restart =
  | Permanent
  (** Always restart the child, regardless of exit reason *)
  | Temporary
  (** Never restart the child *)
  | Transient
(** Restart only if the child terminates abnormally (with error) *)
type shutdown =
  | BrutalKill
  (** Terminate immediately with no cleanup *)
  | Timeout of Time.Duration.t
  (** Wait for duration for graceful shutdown, then kill *)
  | Infinity
(** Wait forever for graceful shutdown (use for supervisors) *)
type child_type =
  | Worker
  (** A regular worker process *)
  | Supervisor
(** A nested supervisor *)
type child_spec = {
  id: string;
  (** Unique identifier for this child *)
  start: unit -> Pid.t;
  (** Function to start the child process *)
  restart: restart;
  (** When to restart this child *)
  shutdown: shutdown;
  (** How to shutdown this child *)
  child_type: child_type;
  (** Worker or Supervisor *)
  significant: bool;
  (** If true, supervisor terminates when this child terminates *)
}

(**
   Create a child specification with sensible defaults.

   Defaults:
   - restart: [Permanent]
   - shutdown: [Timeout 5.0]
   - child_type: [Worker]
   - significant: [false]

   Example:
   ```ocaml
   let worker =
     Supervisor.child_spec
       ~id:"my_worker"
       ~start:(fun () -> spawn my_worker_fn)
       ()
   ```
*)
val child_spec:
  id:string ->
  start:(unit -> Pid.t) ->
  ?restart:restart ->
  ?shutdown:shutdown ->
  ?child_type:child_type ->
  ?significant:bool ->
  unit ->
  child_spec

(**
   Maximum restarts within a time window.

   Example:
   ```ocaml
   { max_restarts = 5; window = Time.Duration.from_secs 10 }
   ```

   If this limit is exceeded, the supervisor terminates.
*)
type intensity = {
  max_restarts: int;
  window: Time.Duration.t;
}

(**
   Start a supervisor linked to the current process.

   If the current process exits, the supervisor (and all children) exit.
   If the supervisor exceeds restart intensity, it exits.

   Example:
   ```ocaml
   let sup = Supervisor.start_link
     ~strategy:OneForOne
     ~intensity:{ max_restarts = 3; window = Duration.from_sec 5 }
     ~children:[worker1; worker2]
     ()
   ```
*)
val start_link: strategy:strategy -> ?intensity:intensity -> children:child_spec list -> unit -> t

(**
   Start a supervisor without linking.

   The supervisor continues running even if the caller exits.
*)
val start: strategy:strategy -> ?intensity:intensity -> children:child_spec list -> unit -> t

type child_info = {
  id: string;
  pid: Pid.t option;
  (** [None] if child is not running *)
  child_type: child_type;
  restart: restart;
}

(**
   Get list of all children (running or not).

   Example:
   ```ocaml
   let children = Supervisor.which_children sup in
   List.iter (fun info ->
     match info.pid with
     | Some pid -> println "Child %s running: %s" info.id (Pid.to_string pid)
     | None -> println "Child %s not running" info.id
   ) children
   ```
*)
val which_children: t -> child_info list

type child_count = {
  specs: int;
  (** Total number of child specs *)
  active: int;
  (** Number of actively running children *)
  supervisors: int;
  (** Number of supervisor children *)
  workers: int;
  (** Number of worker children *)
}
(** Alias for compatibility *)
type count = child_count

(**
   Count children by type and status.

   Example:
   ```ocaml
   let count = Supervisor.count_children sup in
   println "Active: %d/%d workers, %d supervisors"
     count.active count.specs count.supervisors
   ```
*)
val count_children: t -> child_count

(**
   Remove a child specification.

   The child must not be running. Use [terminate_child] first if needed.

   Returns [Error] if:
   - Child is still running
   - Child ID not found
   - Strategy is [Simple_one_for_one] (not supported)

   Example:
   ```ocaml
   match Supervisor.delete_child sup ~id:"worker_1" with
   | Ok () -> println "Child spec removed"
   | Error msg -> println "Failed: %s" msg
   ```
*)
val delete_child: t -> id:string -> (unit, string) Kernel.result

(**
   Restart a child that is not currently running.

   Returns [Error] if:
   - Child is already running
   - Child ID not found
   - Strategy is [Simple_one_for_one] (not supported)

   Example:
   ```ocaml
   match Supervisor.restart_child sup ~id:"worker_1" with
   | Ok pid -> println "Restarted: %s" (Pid.to_string pid)
   | Error msg -> println "Failed: %s" msg
   ```
*)
val restart_child: t -> id:string -> (Pid.t, string) Kernel.result

(**
   Terminate a running child according to its shutdown spec.

   The child spec remains, so the child can be restarted with [restart_child].

   Returns [Error] if:
   - Child ID not found
   - Strategy is [Simple_one_for_one] (not supported)

   Example:
   ```ocaml
   match Supervisor.terminate_child sup ~id:"worker_1" with
   | Ok () -> println "Child terminated"
   | Error msg -> println "Failed: %s" msg
   ```
*)
val terminate_child: t -> id:string -> (unit, string) Kernel.result

(**
   Stop the supervisor and all children gracefully.

   Children are stopped in reverse order of startup.
   Each child is stopped according to its shutdown specification.
*)
val stop: t -> unit

module Dynamic: sig
  (**
     Dynamic supervisor for managing many children at runtime.

     Optimized for scenarios with thousands or millions of children.
     Only supports [OneForOne] strategy.

     Example:
     ```ocaml
     let sup = Supervisor.Dynamic.start_link
       ~max_children:(Some 1000)
       ()

     (* Add children dynamically *)
     match Supervisor.Dynamic.start_child sup
       ~start:(fun () -> spawn_worker 42)
       () with
     | Ok pid -> println "Worker started: %s" (Pid.to_string pid)
     | Error msg -> println "Failed: %s" msg
     ```
  *)
  type t

  (** Convert dynamic supervisor to Pid. *)
  val to_pid: t -> Pid.t

  (**
     Start a dynamic supervisor.

     - [intensity]: Default [{ max_restarts = 3; window = Duration.from_sec 5 }]
     - [max_children]: Optional limit on number of children

     Example:
     ```ocaml
     let sup = Supervisor.Dynamic.start_link
       ~max_children:(Some 10_000)
       ()
     ```
  *)
  val start_link: ?intensity:intensity -> ?max_children:int -> unit -> t

  (** Start a dynamic supervisor without linking. *)
  val start: ?intensity:intensity -> ?max_children:int -> unit -> t

  (**
     Start a new child process.

     Returns [Error] if [max_children] limit is reached.

     Example:
     ```ocaml
     match Supervisor.Dynamic.start_child sup
       ~start:(fun () -> spawn my_worker)
       ~restart:Transient
       () with
     | Ok pid -> (* child started *)
     | Error "max_children_reached" -> (* too many children *)
     ```
  *)
  val start_child:
    t ->
    start:(unit -> Pid.t) ->
    ?restart:restart ->
    ?shutdown:shutdown ->
    unit ->
    (Pid.t, string) Kernel.result

  (**
     Terminate a child by PID.

     The child spec is removed (unlike regular supervisor).

     Returns [Error "not_found"] if PID is not a child.
  *)
  val terminate_child: t -> Pid.t -> (unit, string) Kernel.result

  (** Get list of all running child PIDs. *)
  val which_children: t -> Pid.t list

  (** Count running children. *)
  val count_children: t -> child_count
end
