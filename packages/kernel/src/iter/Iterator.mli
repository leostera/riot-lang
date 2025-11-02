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

(** {1 Transformation} *)

val map : 'a t -> fn:('a -> 'b) -> 'b t
(** Transforms each element using the provided function.
    
    ## Examples
    
    ```ocaml
    iter
    |> Iterator.map ~fn:(fun x -> x * 2)
    |> Iterator.to_list
    (* [2; 4; 6; 8] if iter was [1; 2; 3; 4] *)
    ```
*)

val filter : 'a t -> fn:('a -> bool) -> 'a t
(** Keeps only elements that satisfy the predicate.
    
    ## Examples
    
    ```ocaml
    iter
    |> Iterator.filter ~fn:(fun x -> x mod 2 = 0)
    |> Iterator.to_list
    (* [2; 4] if iter was [1; 2; 3; 4] *)
    ```
*)

val filter_map : 'a t -> fn:('a -> 'b option) -> 'b t
(** Maps and filters in one operation. Elements mapping to None are dropped.
    
    ## Examples
    
    ```ocaml
    iter
    |> Iterator.filter_map ~fn:(fun x -> if x > 0 then Some (x * 2) else None)
    |> Iterator.to_list
    ```
*)

(** {1 Reduction} *)

val fold : 'a t -> init:'acc -> fn:('a -> 'acc -> 'acc) -> 'acc
(** Reduces the iterator to a single value.
    
    ## Examples
    
    ```ocaml
    iter
    |> Iterator.fold ~init:0 ~fn:(fun x acc -> acc + x)
    (* Sum of all elements *)
    ```
*)

val reduce : 'a t -> fn:('a -> 'a -> 'a) -> 'a option
(** Reduces using the first element as initial value.
    
    ## Examples
    
    ```ocaml
    iter
    |> Iterator.reduce ~fn:(fun x acc -> acc + x)
    (* Same as fold but returns None if empty *)
    ```
*)

val count : 'a t -> int
(** Counts the number of elements.
    
    ## Examples
    
    ```ocaml Iterator.count iter (* 4 *) ```
*)

(** {1 Search} *)

val find : 'a t -> fn:('a -> bool) -> 'a option
(** Returns the first element satisfying the predicate.
    
    ## Examples
    
    ```ocaml
    iter |> Iterator.find ~fn:(fun x -> x > 10)
    (* Some(11) or None *)
    ```
*)

val any : 'a t -> fn:('a -> bool) -> bool
(** Returns true if any element satisfies the predicate.
    
    ## Examples
    
    ```ocaml iter |> Iterator.any ~fn:(fun x -> x < 0) ```
*)

val all : 'a t -> fn:('a -> bool) -> bool
(** Returns true if all elements satisfy the predicate.
    
    ## Examples
    
    ```ocaml iter |> Iterator.all ~fn:(fun x -> x > 0) ```
*)

(** {1 Combinators} *)

val take : 'a t -> int -> 'a t
(** Takes at most n elements.
    
    ## Examples
    
    ```ocaml
    iter |> Iterator.take 3 |> Iterator.to_list
    (* [1; 2; 3] *)
    ```
*)

val drop : 'a t -> int -> 'a t
(** Skips the first n elements.
    
    ## Examples
    
    ```ocaml
    iter |> Iterator.drop 2 |> Iterator.to_list
    (* [3; 4; 5] if iter was [1; 2; 3; 4; 5] *)
    ```
*)

val enumerate : 'a t -> (int * 'a) t
(** Adds indices to elements, starting from 0.
    
    ## Examples
    
    ```ocaml
    iter |> Iterator.enumerate |> Iterator.to_list
    (* [(0, 'a'); (1, 'b'); (2, 'c')] *)
    ```
*)

val zip : 'a t -> 'b t -> ('a * 'b) t
(** Combines two iterators into pairs. Stops when either is exhausted.
    
    ## Examples
    
    ```ocaml
    Iterator.zip iter1 iter2 |> Iterator.to_list
    (* [(1, 'a'); (2, 'b'); (3, 'c')] *)
    ```
*)

val chain : 'a t -> 'a t -> 'a t  
(** Chains two iterators together.
    
    ## Examples
    
    ```ocaml
    Iterator.chain iter1 iter2 |> Iterator.to_list
    (* [1; 2; 3; 4; 5; 6] *)
    ```
*)

(** {1 Side Effects} *)

val for_each : 'a t -> fn:('a -> unit) -> unit
(** Applies a function to each element for side effects.
    
    ## Examples
    
    ```ocaml
    iter |> Iterator.for_each ~fn:(fun x -> print_int x)
    ```
*)
