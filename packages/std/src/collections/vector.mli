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

(** The type of vectors containing elements of type `'value` *)
type 'value t

(** Creates a new empty vector.

    ## Examples

    ```ocaml let vec = Vector.create () in assert (Vector.is_empty vec) ``` *)
val create: unit -> 'value t

(** Creates a new empty vector with specified initial capacity.

    Pre-allocating capacity can improve performance when the size is known in
    advance.

    ## Examples

    ```ocaml (* Pre-allocate for 1000 elements *) let vec = Vector.with_capacity
    1000 in for i = 0 to 999 do Vector.push vec i (* No reallocation needed *)
    done ``` *)
val with_capacity: int -> 'value t

(** Creates a vector from a list of elements.

    ## Examples

    ```ocaml let vec = Vector.of_list [1; 2; 3; 4; 5] in assert (Vector.len vec
    = 5); assert (Vector.get vec 2 = Some 3) ``` *)
val of_list: 'value list -> 'value t

(** # Basic Operations *)
(** [push vector value] adds an element to the end of the vector *)
val push: 'value t -> 'value -> unit

(** [pop vector] removes and returns the last element. Returns [Some element] if
    the vector is not empty, [None] otherwise. *)
val pop: 'value t -> 'value option

(** [insert vector index value] inserts an element at the given index *)
val insert: 'value t -> int -> 'value -> unit

(** [remove vector index] removes and returns the element at the given index.
    Returns [Some element] if the index is valid, [None] otherwise. *)
val remove: 'value t -> int -> 'value option

(** [get vector index] returns the element at the given index. Returns
    [Some element] if the index is valid, [None] otherwise. *)
val get: 'value t -> int -> 'value option

(** [get_unchecked vector index] returns the element at the given index without
    performing bounds checks. *)
val get_unchecked: 'value t -> int -> 'value

(** [set vector index value] sets the element at the given index *)
val set: 'value t -> int -> 'value -> unit

(** [set_unchecked vector index value] sets the element at the given index
    without performing bounds checks. *)
val set_unchecked: 'value t -> int -> 'value -> unit

(** {1 Collection Information} *)
(** [len vector] returns the number of elements in the vector *)
val len: 'value t -> int

(** [is_empty vector] returns [true] if the vector contains no elements *)
val is_empty: 'value t -> bool

(** [capacity vector] returns the current capacity of the vector *)
val capacity: 'value t -> int

(** [clear vector] removes all elements from the vector *)
val clear: 'value t -> unit

(** [to_array vector] copies the vector contents into a compact array. *)
val to_array: 'value t -> 'value array

(** [reserve vector additional] ensures the vector can hold at least
    [additional] more elements without reallocating. *)
val reserve: 'value t -> int -> unit

(** {1 Iteration} *)
(** [iter f vector] applies function [f] to each element *)
val iter: ('value -> unit) -> 'value t -> unit

(** Returns a mutable iterator over the vector's elements.

    ## Examples

    ```ocaml let vec = Vector.of_list [1; 2; 3; 4; 5] in let iter =
    Vector.to_mut_iter vec in

    match Iter.MutIterator.next iter with | Some x -> (* 1 *) | None -> () ```
*)
val to_mut_iter: 'value t -> 'value Iter.MutIterator.t

(** {1 Additional Operations} *)
(** [append vector1 vector2] moves all elements from [vector2] into [vector1] *)
val append: 'value t -> 'value t -> unit

(** [split_off vector index] splits the vector into two at the given index.
    Returns a new vector containing elements from [index] onwards. *)
val split_off: 'value t -> int -> 'value t

(** [sort vector] sorts the vector in-place using the default comparison *)
val sort: 'value t -> unit

(** [sort_by vector compare] sorts the vector in-place using a custom comparison
    function *)
val sort_by: 'value t -> ('value -> 'value -> int) -> unit

(** [reverse vector] reverses the order of elements in-place *)
val reverse: 'value t -> unit

(** [first vector] returns the first element without removing it *)
val first: 'value t -> 'value option

(** [last vector] returns the last element without removing it *)
val last: 'value t -> 'value option

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
val into_iter: 'value t -> 'value Iter.Iterator.t
