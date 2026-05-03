(**
   Commonly used utility functions.

   Global utility functions available throughout `std`. This module owns
   runtime-facing process helpers plus a small amount of formatting, printing,
   and panic support.

   ## Examples

   Printing output:

   ```ocaml open Std

   print "Hello, world!"

   println "Value: 42" ```

   String formatting:

   ```ocaml
   let msg = format Format.[
     str "Error at line ";
     int 42;
     str ": syntax error";
   ] in
   (* msg = "Error at line 42: syntax error" *)
   ```

   Development helpers:

   ```ocaml let implement_later () = todo "Need to implement caching"

   let not_ready () = unimplemented () ```

   Mutable cells:

   ```ocaml let counter = cell 0 in Sync.Cell.set counter 1; Sync.Cell.get counter (* 1
   *) ```
*)
module Format = Format

type format = Format.t
type ('value, 'error) result = ('value, 'error) Kernel.result =
  | Ok of 'value
  | Error of 'error

val format: format list -> string

val ( = ): 'value -> 'value -> bool

val compare: 'value -> 'value -> Order.t

val min: 'value -> 'value -> 'value

val max: 'value -> 'value -> 'value

val ( != ): 'value -> 'value -> bool

val ( < ): 'value -> 'value -> bool

val ( > ): 'value -> 'value -> bool

val ( <= ): 'value -> 'value -> bool

val ( >= ): 'value -> 'value -> bool

val ( ~- ): int -> int

val ( + ): int -> int -> int

val ( - ): int -> int -> int

val ( * ): int -> int -> int

val ( / ): int -> int -> int

val ( mod ): int -> int -> int

val ( land ): int -> int -> int

val ( lor ): int -> int -> int

val ( lxor ): int -> int -> int

val lnot: int -> int

val ( lsl ): int -> int -> int

val ( lsr ): int -> int -> int

val ( asr ): int -> int -> int

val ( ~-. ): float -> float

val ( +. ): float -> float -> float

val ( -. ): float -> float -> float

val ( *. ): float -> float -> float

val ( /. ): float -> float -> float

val ( @@ ): ('value -> 'result) -> 'value -> 'result

val ( |> ): 'value -> ('value -> 'result) -> 'result

val ( ^ ): string -> string -> string

val ( @ ): 'value list -> 'value list -> 'value list

val ( ** ): float -> float -> float

val float_of_int: int -> float

val int_of_float: float -> int

val float: int -> float

val string_of_int: int -> string

val string_of_float: ?precision:int -> float -> string

val abs: int -> int

val mod_float: float -> float -> float

val sqrt: float -> float

val floor: float -> float

val ceil: float -> float

val not: bool -> bool

val ( && ): bool -> bool -> bool

val ( || ): bool -> bool -> bool

val raise: exn -> 'value

val raise_notrace: exn -> 'value

val ignore: 'value -> unit

exception Receive_timeout

exception Syscall_timeout

(** Mailbox selector type used by receive operations. *)
type 'msg selection = 'msg Runtime.selection =
  | Select of 'msg
  | Skip
type 'msg selector = 'msg Runtime.selector

(** Get the PID of the currently running process. *)
val self: unit -> Runtime.Pid.t

(** Spawn a new process *)
val spawn: (unit -> (unit, Runtime.Actor.exit_reason) Kernel.result) -> Runtime.Pid.t

(** Spawn a new process linked to the current process *)
val spawn_link: (unit -> (unit, Runtime.Actor.exit_reason) Kernel.result) -> Runtime.Pid.t

(** Send a message to a process *)
val send: Runtime.Pid.t -> Runtime.Message.t -> unit

(** Receive a message using a selector *)

(** Receive any message *)
val receive: selector:'value Runtime.selector -> ?timeout:Time.Duration.t -> unit -> 'value

val receive_any: ?timeout:Time.Duration.t -> unit -> Runtime.Message.t

(** Sleeps the current process for at least the specified duration *)
val sleep: Time.Duration.t -> unit

(** Yield control to the scheduler *)
val yield: unit -> unit

(** Shutdown the runtime with the given exit status *)
val shutdown: status:int -> unit

(**
   Raises a panic exception with the given message. Program terminates unless
   caught.

   ## Examples

   ```ocaml if config_missing then panic "Configuration file not found" ```

   ## Use Cases

   - Irrecoverable errors
   - Invariant violations
   - Programmer errors (use assertions instead when possible)
*)
val panic: string -> 'a

val ( ! ): 'a Sync.Cell.t -> 'a

val ( := ): 'a Sync.Cell.t -> 'a -> unit

val ref: 'a -> 'a Sync.Cell.t

(**
   Creates a mutable cell containing the given value.

   ## Examples

   ```ocaml let counter = cell 0 in Sync.Cell.update counter (fun n -> n + 1);
   Sync.Cell.get counter (* 1 *) ```

   ## See Also

   - [Sync.Cell] for full cell API
*)
val cell: 'a -> 'a Sync.Cell.t

(**
   Prints to stdout with immediate flush (no newline).

   ## Examples

   ```ocaml print "Processing..." (* Output: Processing... *) ```
*)
val print: string -> unit

(**
   Prints to stdout with newline and immediate flush.

   ## Examples

   ```ocaml println "Operation complete" (* Output: Operation complete\n *) ```
*)
val println: string -> unit

(**
   Prints to stderr with immediate flush (no newline).

   ## Examples

   ```ocaml eprint "Debug: processing item" (* Output to stderr: Debug: processing item *) ```
*)
val eprint: string -> unit

(**
   Prints to stderr with newline and immediate flush.

   ## Examples

   ```ocaml eprintln "Error: file not found" (* Output to stderr: Error: file not found\n *) ```
*)
val eprintln: string -> unit

(**
   Marks code as TODO, panicking with the given message when called.

   ## Examples

   ```ocaml let cache_lookup key = todo "Implement Redis caching" ```

   ## Use Cases

   - Placeholder for future implementation
   - Self-documenting incomplete code
   - Fails fast if accidentally called
*)

(**
   Marks code as unimplemented, panicking when called.

   ## Examples

   ```ocaml let complex_algorithm () = unimplemented () ```
*)
val todo: string -> 'a

val unimplemented: unit -> 'a
