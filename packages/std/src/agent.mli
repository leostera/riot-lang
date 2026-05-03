(**
   Lightweight state server for concurrent access.

   Agents provide a simple wrapper around state that can be accessed
   concurrently from different processes. This version uses parametric
   polymorphism instead of functors for ease of use.

   ## Example

   ```ocaml
   open Std

   (* Create a counter agent *)
   let counter = Agent.start (fun () -> 0) in

   (* Update the counter *)
   Agent.update counter (fun n -> n + 1);

   (* Get the current value *)
   let value = Agent.get counter (fun n -> n) in
   (* value = 1 *)

   (* Get and update atomically *)
   let old_value = Agent.get_and_update counter (fun n -> n, n + 10) in
   (* old_value = 1, new value = 11 *)
   ```
*)

(** Agent handle parametrized by state type *)
type 'state t

(** Start an agent with the given initial state *)
val start: fn:(unit -> 'state) -> 'state t

(** Start an agent linked to the current process *)
val start_link: fn:(unit -> 'state) -> 'state t

(** Get a value computed from the agent's state *)
val get: 'state t -> fn:('state -> 'reply) -> 'reply

(** Update the agent's state synchronously *)
val update: 'state t -> fn:('state -> 'state) -> unit

(** Get a value and update state atomically *)
val get_and_update: 'state t -> fn:('state -> 'reply * 'state) -> 'reply

(** Update the agent's state asynchronously (fire and forget) *)
val cast: 'state t -> fn:('state -> 'state) -> unit

(** Stop the agent *)
val stop: 'state t -> unit
