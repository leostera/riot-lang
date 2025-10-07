(** # MutIterator - Mutable iteration protocol
    
    Mutable iterator protocol for efficient sequence processing. Calling
    [next] mutates the internal state, similar to iterators in imperative
    languages.
    
    ## Examples
    
    Creating a custom mutable iterator:
    
    ```ocaml
    open Std
    
    module CountIter = struct
      type state = { mutable current : int; stop : int }
      type item = int
      
      let next state =
        if state.current >= state.stop then
          None
        else begin
          let value = state.current in
          state.current <- state.current + 1;
          Some value
        end
      
      let size state = max 0 (state.stop - state.current)
      
      let clone state = { current = state.current; stop = state.stop }
    end
    
    let counter start stop =
      let module I = CountIter in
      MutIterator.make (module I) { I.current = start; I.stop }
    ```
    
    Using a mutable iterator:
    
    ```ocaml
    let iter = counter 0 5 in
    
    let rec process () =
      match MutIterator.next iter with
      | Some x ->
          Printf.printf "%d " x;
          process ()
      | None -> ()
    
    process ()  (* Prints: 0 1 2 3 4 *)
    ```
    
    Collecting to list:
    
    ```ocaml
    let iter = counter 0 5 in
    let items = MutIterator.to_list iter
    (* [0; 1; 2; 3; 4] *)
    ```
    
    ## Differences from Iterator
    
    | MutIterator | Iterator |
    |-------------|----------|
    | Mutates in place | Returns new state |
    | More efficient | Immutable/pure |
    | Can't backtrack | Can backtrack |
    | Single pass | Multiple passes possible |
    
    ## When to Use
    
    - Performance-critical iteration
    - Single-pass processing
    - Interfacing with imperative code
    - Memory-constrained scenarios
*)

(** Interface that mutable iterators must implement. *)
module type Intf = sig
  type state
  (** The mutable iterator state *)

  type item
  (** The type of items produced *)

  val next : state -> item option
  (** Returns the next item, mutating internal state. Returns None when
      exhausted. *)

  val size : state -> int
  (** Returns the number of remaining items (may be approximate). *)

  val clone : state -> state
  (** Creates an independent copy of the iterator state. *)
end

type ('item, 'state) iter =
  (module Intf with type item = 'item and type state = 'state)
(** First-class module type for mutable iterators. *)

type 'item t =
  | Iter : (('item, 'state) iter * 'state) -> 'item t
      (** A mutable iterator over items of type ['item]. *)

val make : ('item, 'state) iter -> 'state -> 'item t
(** Creates a mutable iterator from a module and initial state.

    ## Examples

    ```ocaml let iter = MutIterator.make (module MyIter) initial_state ``` *)

val next : 'item t -> 'item option
(** Returns the next item, mutating the iterator's internal state.

    ## Examples

    ```ocaml match MutIterator.next iter with | Some x -> process x | None -> ()
    (* Iterator exhausted *) ``` *)

val size : 'item t -> int
(** Returns the number of remaining items (may be approximate).

    ## Examples

    ```ocaml let remaining = MutIterator.size iter in ``` *)

val clone : 'item t -> 'item t
(** Creates an independent copy of the iterator.

    ## Examples

    ```ocaml let iter2 = MutIterator.clone iter in (* iter and iter2 can now be
    advanced independently *) ``` *)

val collect : 'item t -> 'item list -> 'item list
(** Collects remaining items, prepending to the given list.

    ## Examples

    ```ocaml let items = MutIterator.collect iter [] ``` *)

val to_list : 'item t -> 'item list
(** Consumes the iterator and collects all items into a list.

    ## Examples

    ```ocaml let items = MutIterator.to_list iter ``` *)
