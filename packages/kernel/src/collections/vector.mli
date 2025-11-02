(** # Collections.Vector - Dynamic arrays

    A contiguous growable array type with O(1) indexing and amortized O(1)
    push/pop. Similar to Rust's `Vec`, C++'s `std::vector`, or Java's
    `ArrayList`.

    ## Examples

    Basic usage:

    ```ocaml open Std.Collections

    let vec = Vector.create () in Vector.push vec 1; Vector.push vec 2;
    Vector.push vec 3;

    match Vector.pop vec with | Some x -> Printf.printf "Popped: %d\n" x (* 3 *)
    | None -> ()

    (* Index access *) match Vector.get vec 0 with | Some x -> Printf.printf
    "First: %d\n" x | None -> () ```

    ## When to Use Vector

    - Need O(1) indexed access to elements
    - Frequently adding/removing from the end
    - Need a growable array
    - Want cache-friendly iteration

    ## Performance Characteristics

    - Push (end): O(1) amortized
    - Pop (end): O(1)
    - Insert (middle): O(n)
    - Remove (middle): O(n)
    - Index access: O(1)
    - Iteration: O(n) with good cache locality *)

type 'a t
(** The type of vectors containing elements of type `'a` *)

(** # Creation *)

val create : unit -> 'a t
(** Creates a new empty vector.

    ## Examples

    ```ocaml let vec = Vector.create () in assert (Vector.is_empty vec) ``` *)

val with_capacity : int -> 'a t
(** Creates a new empty vector with specified initial capacity.

    Pre-allocating capacity can improve performance when the size is known in
    advance.

    ## Examples

    ```ocaml (* Pre-allocate for 1000 elements *) let vec = Vector.with_capacity
    1000 in for i = 0 to 999 do Vector.push vec i (* No reallocation needed *)
    done ``` *)

val of_list : 'a list -> 'a t
(** Creates a vector from a list of elements.

    ## Examples

    ```ocaml let vec = Vector.of_list [1; 2; 3; 4; 5] in assert (Vector.len vec
    = 5); assert (Vector.get vec 2 = Some 3) ``` *)

(** # Basic Operations *)

val push : 'a t -> 'a -> unit
(** [push vector value] adds an element to the end of the vector *)

val pop : 'a t -> 'a option
(** [pop vector] removes and returns the last element. Returns [Some element] if
    the vector is not empty, [None] otherwise. *)

val insert : 'a t -> int -> 'a -> unit
(** [insert vector index value] inserts an element at the given index *)

val remove : 'a t -> int -> 'a option
(** [remove vector index] removes and returns the element at the given index.
    Returns [Some element] if the index is valid, [None] otherwise. *)

val get : 'a t -> int -> 'a option
(** [get vector index] returns the element at the given index. Returns
    [Some element] if the index is valid, [None] otherwise. *)

val set : 'a t -> int -> 'a -> unit
(** [set vector index value] sets the element at the given index *)

(** {1 Collection Information} *)

val len : 'a t -> int
(** [len vector] returns the number of elements in the vector *)

val is_empty : 'a t -> bool
(** [is_empty vector] returns [true] if the vector contains no elements *)

val capacity : 'a t -> int
(** [capacity vector] returns the current capacity of the vector *)

val clear : 'a t -> unit
(** [clear vector] removes all elements from the vector *)

(** {1 Iteration} *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f vector] applies function [f] to each element *)

val fold : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
(** [fold f vector acc] folds over all elements *)

val to_list : 'a t -> 'a list
(** [to_list vector] returns all elements as a list *)

val to_mut_iter : 'a t -> 'a Iter.MutIterator.t
(** Returns a mutable iterator over the vector's elements.

    ## Examples

    ```ocaml let vec = Vector.of_list [1; 2; 3; 4; 5] in let iter =
    Vector.to_mut_iter vec in

    match Iter.MutIterator.next iter with | Some x -> (* 1 *) | None -> () ```
*)

(** {1 Additional Operations} *)

val contains : 'a t -> 'a -> bool
(** [contains vector value] returns [true] if the value exists in the vector *)

val append : 'a t -> 'a t -> unit
(** [append vector1 vector2] moves all elements from [vector2] into [vector1] *)

val split_off : 'a t -> int -> 'a t
(** [split_off vector index] splits the vector into two at the given index.
    Returns a new vector containing elements from [index] onwards. *)

val sort : 'a t -> unit
(** [sort vector] sorts the vector in-place using the default comparison *)

val sort_by : 'a t -> ('a -> 'a -> int) -> unit
(** [sort_by vector compare] sorts the vector in-place using a custom comparison
    function *)

val reverse : 'a t -> unit
(** [reverse vector] reverses the order of elements in-place *)

val first : 'a t -> 'a option
(** [first vector] returns the first element without removing it *)

val last : 'a t -> 'a option
(** [last vector] returns the last element without removing it *)

(** # Iteration *)

val into_iter : 'a t -> 'a Iter.Iterator.t
(** Converts the vector into an immutable iterator.
    
    ## Examples
    
    ```ocaml
    let vec = Vector.of_list [1; 2; 3; 4] in
    vec
    |> Vector.into_iter
    |> Iterator.map ~fn:(fun x -> x * 2)
    |> Iterator.filter ~fn:(fun x -> x > 4)
    |> Iterator.to_list
    (* [6; 8] *)
    ```
*)

val to_mut_iter : 'a t -> 'a Iter.MutIterator.t
(** Converts the vector into a mutable iterator.
    
    ## Examples
    
    ```ocaml
    let vec = Vector.of_list [1; 2; 3] in
    vec
    |> Vector.to_mut_iter
    |> MutIterator.map ~fn:(fun x -> x * 2)
    |> MutIterator.to_list
    (* [2; 4; 6] *)
    ```
*)
