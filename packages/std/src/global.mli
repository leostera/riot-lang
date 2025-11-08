(** # Global - Commonly used utility functions

    Global utility functions available throughout the Std library. Includes
    formatting, printing, and development helpers.

    ## Examples

    Printing output:

    ```ocaml open Std

    print "Hello, %s!" "world" (* Prints: Hello, world! *)

    println "Value: %d" 42 (* Prints: Value: 42\n *) ```

    String formatting:

    ```ocaml let msg = format "Error at line %d: %s" 42 "syntax error" in (* msg
    = "Error at line 42: syntax error" *) ```

    Development helpers:

    ```ocaml let implement_later () = todo "Need to implement caching"

    let not_ready () = unimplemented () ```

    Mutable cells:

    ```ocaml let counter = cell 0 in Sync.Cell.set counter 1; Sync.Cell.get counter (* 1
    *) ``` *)

include module type of Kernel.Types
include module type of Kernel.Global

(** {1 Process Management} *)

exception Receive_timeout
exception Syscall_timeout
type 'msg selector = 'msg Miniriot.selector

val self : unit -> Miniriot.Pid.t
(** Get the PID of the currently running process *)

val spawn : (unit -> (unit, Miniriot.Process.exit_reason) Kernel.result) -> Miniriot.Pid.t
(** Spawn a new process *)

val spawn_link : (unit -> (unit, Miniriot.Process.exit_reason) Kernel.result) -> Miniriot.Pid.t
(** Spawn a new process linked to the current process *)

val send : Miniriot.Pid.t -> Miniriot.Message.t -> unit
(** Send a message to a process *)

val receive : selector:'value Miniriot.selector -> ?timeout:Time.Duration.t -> unit -> 'value
(** Receive a message using a selector *)

val receive_any : ?timeout:Time.Duration.t -> unit -> Miniriot.Message.t
(** Receive any message *)

val sleep : Time.Duration.t -> unit
(** Sleeps the current process for at least the specified duration *)

val yield : unit -> unit
(** Yield control to the scheduler *)

val shutdown : status:int -> unit
(** Shutdown the runtime with the given exit status *)


open Kernel

(** {1 Collection Types and Constructors} *)

type 'a vec = 'a Kernel.vec
(** Vector type alias - dynamically-sized array *)

type 'a queue = 'a Kernel.queue
(** Queue type alias - FIFO queue *)

type 'a set = 'a Kernel.set
(** Set type alias - hash-based set *)

type ('k, 'v) map = ('k, 'v) Kernel.map
(** Map type alias - hash-based map *)

val vec : 'a list -> 'a vec
(** Create a vector from a list.
    
    ## Examples
    
    ```ocaml
    let v: int vec = vec [1; 2; 3] in
    (* v is a Vector.t containing [1; 2; 3] *)
    ```
*)

val queue : 'a list -> 'a queue
(** Create a queue from a list.
    
    ## Examples
    
    ```ocaml
    let q: int queue = queue [1; 2; 3] in
    (* q is a Queue.t containing [1; 2; 3] *)
    ```
*)

val set : 'a list -> 'a set
(** Create a set from a list.
    
    ## Examples
    
    ```ocaml
    let s: int set = set [1; 2; 3] in
    (* s is a HashSet.t containing {1, 2, 3} *)
    ```
*)

val map : ('k * 'v) list -> ('k, 'v) map
(** Create a map from a list of key-value pairs.
    
    ## Examples
    
    ```ocaml
    let m: (string, int) map = map [("a", 1); ("b", 2)] in
    (* m is a HashMap.t containing {"a" -> 1, "b" -> 2} *)
    ```
*)

val panic : string -> 'a
(** Raises a panic exception with the given message. Program terminates unless
    caught.

    ## Examples

    ```ocaml if config_missing then panic "Configuration file not found" ```

    ## Use Cases

    - Irrecoverable errors
    - Invariant violations
    - Programmer errors (use assertions instead when possible) *)

val ( ! ) : 'a Sync.Cell.t -> 'a
val ( := ) : 'a Sync.Cell.t -> 'a -> unit
val ref : 'a -> 'a Sync.Cell.t

val cell : 'a -> 'a Sync.Cell.t
(** Creates a mutable cell containing the given value.

    ## Examples

    ```ocaml let counter = cell 0 in Sync.Cell.update counter (fun n -> n + 1);
    Sync.Cell.get counter (* 1 *) ```

    ## See Also

    - [Sync.Cell] for full cell API *)

val print : string -> unit
(** Prints to stdout with immediate flush (no newline).

    ## Examples

    ```ocaml print "Processing..." (* Output: Processing... *) ``` *)

val println : string -> unit
(** Prints to stdout with newline and immediate flush.

    ## Examples

    ```ocaml println "Operation complete" (* Output: Operation complete\n *) ```
*)

val eprint : string -> unit
(** Prints to stderr with immediate flush (no newline).

    ## Examples

    ```ocaml eprint "Debug: processing item" (* Output to stderr: Debug: processing item *) ```
*)

val eprintln : string -> unit
(** Prints to stderr with newline and immediate flush.

    ## Examples

    ```ocaml eprintln "Error: file not found" (* Output to stderr: Error: file not found\n *) ```
*)

val todo : string -> 'a
(** Marks code as TODO, panicking with the given message when called.

    ## Examples

    ```ocaml let cache_lookup key = todo "Implement Redis caching" ```

    ## Use Cases

    - Placeholder for future implementation
    - Self-documenting incomplete code
    - Fails fast if accidentally called *)

val unimplemented : unit -> 'a
(** Marks code as unimplemented, panicking when called.

    ## Examples

    ```ocaml let complex_algorithm () = unimplemented () ``` *)
