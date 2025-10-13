(** # Iterator - Immutable iteration protocol
    
    Immutable iterator protocol for lazy sequence processing. Each call
    to [next] returns a new iterator, leaving the original unchanged.
    
    ## Examples
    
    Creating a custom iterator:
    
    ```ocaml
    open Std
    
    module RangeIter = struct
      type state = { current : int; stop : int }
      type item = int
      
      let next state =
        if state.current >= state.stop then
          (None, state)
        else
          (Some state.current, { state with current = state.current + 1 })
      
      let size state = max 0 (state.stop - state.current)
    end
    
    let range start stop =
      let module I = RangeIter in
      Iterator.make (module I) { I.current = start; I.stop }
    
    let iter = range 0 5 in
    let items = Iterator.to_list iter
    (* [0; 1; 2; 3; 4] *)
    ```
    
    Using an iterator:
    
    ```ocaml
    let rec consume iter =
      let (item, iter') = Iterator.next iter in
      match item with
      | Some x ->
          Printf.printf "%d " x;
          consume iter'
      | None -> ()
    ```
    
    ## Immutability
    
    Unlike [MutIterator], calling [next] returns both the item and a
    new iterator state, allowing backtracking and multiple iterations.
*)

(** Interface that iterators must implement. *)
module type Intf = sig
  type state
  (** The internal iterator state *)

  type item
  (** The type of items produced *)

  val next : state -> item option * state
  (** Returns the next item and new state. Returns (None, state) when exhausted.
  *)

  val size : state -> int
  (** Returns the number of remaining items (may be approximate). *)
end

type ('item, 'state) iter =
  (module Intf with type item = 'item and type state = 'state)
(** First-class module type for iterators. *)

type 'item t
(** An immutable iterator over items of type ['item]. *)

val make : ('item, 'state) iter -> 'state -> 'item t
(** Creates an iterator from a module and initial state.

    ## Examples

    ```ocaml let iter = Iterator.make (module MyIter) initial_state ``` *)

val next : 'item t -> 'item option * 'item t
(** Returns the next item and a new iterator.

    ## Examples

    ```ocaml let (item, iter') = Iterator.next iter in match item with | Some x
    -> process x | None -> () (* Iterator exhausted *) ``` *)

val size : 'item t -> int
(** Returns the number of remaining items (may be approximate).

    ## Examples

    ```ocaml let remaining = Iterator.size iter in Log.info "Items left: %d"
    remaining ``` *)

val to_list : 'item t -> 'item list
(** Consumes the iterator and collects all items into a list.

    ## Examples

    ```ocaml let items = Iterator.to_list iter ``` *)
