(** # Collections.Deque - Double-ended queue

    A growable ring buffer supporting efficient insertion and removal at both
    ends. Similar to Rust's [VecDeque], Python's [collections.deque], or C++'s
    [std::deque].

    ## Examples

    Basic double-ended operations:

    ```ocaml open Std.Collections

    let deque = Deque.create () in

    (* Add to both ends *) Deque.push_back deque 2; Deque.push_front deque 1;
    Deque.push_back deque 3; (* deque: [1, 2, 3] *)

    (* Remove from both ends *) Deque.pop_front deque (* Some 1 *)
    Deque.pop_back deque (* Some 3 *) (* deque: [2] *) ```

    Using as a queue (FIFO):

    ```ocaml let queue = Deque.create () in

    (* Enqueue *) Deque.push_back queue "first"; Deque.push_back queue "second";
    Deque.push_back queue "third";

    (* Dequeue *) Deque.pop_front queue (* Some "first" *) Deque.pop_front queue
    (* Some "second" *) ```

    Using as a stack (LIFO):

    ```ocaml let stack = Deque.create () in

    (* Push *) Deque.push_back stack 1; Deque.push_back stack 2;

    (* Pop *) Deque.pop_back stack (* Some 2 *) Deque.pop_back stack (* Some 1
    *) ```

    ## When to Use Deque

    - Need efficient operations at both ends
    - Implementing queues or stacks
    - Sliding window algorithms
    - BFS/DFS with backtracking
    - Undo/redo functionality

    ## Performance Characteristics

    - Push/pop front: O(1) amortized
    - Push/pop back: O(1) amortized
    - Index access: O(1)
    - Insert/remove middle: O(n)
    - Iteration: O(n) *)

type 'a t
(** The type of double-ended queues containing elements of type ['a].
    Implemented as a growable ring buffer for efficient operations at both ends.
*)

(** {1 Creation} *)

val create : unit -> 'a t
(** Creates a new empty deque.

    ## Examples

    ```ocaml let deque = Deque.create () in assert (Deque.is_empty deque) ``` *)

val with_capacity : int -> 'a t
(** Creates a new empty deque with specified initial capacity.

    Pre-allocating capacity improves performance when the approximate size is
    known in advance.

    ## Examples

    ```ocaml let deque = Deque.with_capacity 1000 in (* Can push ~1000 elements
    without reallocation *) ``` *)

val of_list : 'a list -> 'a t
(** Creates a deque from a list. The first list element becomes the front of the
    deque.

    ## Examples

    ```ocaml let deque = Deque.of_list [1; 2; 3] in Deque.front deque (* Some 1
    *) Deque.back deque (* Some 3 *) ```

    ## Complexity

    - Time: O(n)
    - Space: O(n) *)

(** {1 Adding Elements} *)

val push_front : 'a t -> 'a -> unit
(** Adds an element to the front of the deque.

    ## Examples

    ```ocaml let deque = Deque.of_list [2; 3] in Deque.push_front deque 1; (*
    deque: [1, 2, 3] *) ```

    ## Complexity

    - Time: O(1) amortized *)

val push_back : 'a t -> 'a -> unit
(** Adds an element to the back of the deque.

    ## Examples

    ```ocaml let deque = Deque.of_list [1; 2] in Deque.push_back deque 3; (*
    deque: [1, 2, 3] *) ```

    ## Complexity

    - Time: O(1) amortized *)

val insert : 'a t -> int -> 'a -> unit
(** Inserts an element at the given index. Existing elements at and after this
    index are shifted back.

    ## Examples

    ```ocaml let deque = Deque.of_list [1; 3; 4] in Deque.insert deque 1 2; (*
    deque: [1, 2, 3, 4] *) ```

    ## Complexity

    - Time: O(n) where n is distance from nearest end *)

(** {1 Removing Elements} *)

val pop_front : 'a t -> 'a option
(** Removes and returns the front element. Returns [Some element] if the deque
    is not empty, [None] otherwise.

    ## Examples

    ```ocaml let deque = Deque.of_list [1; 2; 3] in Deque.pop_front deque (*
    Some 1 *) Deque.pop_front deque (* Some 2 *) ```

    ## Complexity

    - Time: O(1) *)

val pop_back : 'a t -> 'a option
(** Removes and returns the back element. Returns [Some element] if the deque is
    not empty, [None] otherwise.

    ## Examples

    ```ocaml let deque = Deque.of_list [1; 2; 3] in Deque.pop_back deque (* Some
    3 *) Deque.pop_back deque (* Some 2 *) ```

    ## Complexity

    - Time: O(1) *)

val remove : 'a t -> int -> 'a option
(** Removes and returns the element at the given index. Returns [Some element]
    if the index is valid, [None] otherwise.

    ## Examples

    ```ocaml let deque = Deque.of_list [1; 2; 3; 4] in Deque.remove deque 1 (*
    Some 2 *) (* deque: [1, 3, 4] *) ```

    ## Complexity

    - Time: O(n) where n is distance from nearest end *)

val clear : 'a t -> unit
(** Removes all elements from the deque.

    ## Examples

    ```ocaml let deque = Deque.of_list [1; 2; 3] in Deque.clear deque; assert
    (Deque.is_empty deque) ```

    ## Complexity

    - Time: O(1)
    - Space: Capacity is preserved for reuse *)

(** {1 Accessing Elements} *)

val front : 'a t -> 'a option
(** Returns the front element without removing it.

    ## Examples

    ```ocaml let deque = Deque.of_list [1; 2; 3] in Deque.front deque (* Some 1
    *) (* deque unchanged *) ```

    ## Complexity

    - Time: O(1) *)

val back : 'a t -> 'a option
(** Returns the back element without removing it.

    ## Examples

    ```ocaml let deque = Deque.of_list [1; 2; 3] in Deque.back deque (* Some 3
    *) (* deque unchanged *) ```

    ## Complexity

    - Time: O(1) *)

val get : 'a t -> int -> 'a option
(** Returns the element at the given index without removing it.

    ## Examples

    ```ocaml let deque = Deque.of_list ["a"; "b"; "c"] in Deque.get deque 0 (*
    Some "a" *) Deque.get deque 1 (* Some "b" *) Deque.get deque 5 (* None *)
    ```

    ## Complexity

    - Time: O(1) *)

(** {1 Collection Information} *)

val len : 'a t -> int
(** Returns the number of elements in the deque.

    ## Examples

    ```ocaml let deque = Deque.of_list [1; 2; 3] in Deque.len deque (* 3 *) ```

    ## Complexity

    - Time: O(1) *)

val is_empty : 'a t -> bool
(** Returns [true] if the deque contains no elements.

    ## Examples

    ```ocaml let deque = Deque.create () in Deque.is_empty deque (* true *)
    Deque.push_back deque 1; Deque.is_empty deque (* false *) ```

    ## Complexity

    - Time: O(1) *)

(** {1 Iteration} *)

val iter : ('a -> unit) -> 'a t -> unit
(** Applies function [f] to each element from front to back.

    ## Examples

    ```ocaml let deque = Deque.of_list [1; 2; 3] in Deque.iter (fun x ->
    Printf.printf "%d " x) deque (* Prints: 1 2 3 *) ```

    ## Complexity

    - Time: O(n) *)

val fold : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
(** Folds over all elements from front to back.

    ## Examples

    ```ocaml let deque = Deque.of_list [1; 2; 3; 4] in let sum = Deque.fold (fun
    x acc -> x + acc) deque 0 in (* sum = 10 *) ```

    ## Complexity

    - Time: O(n) *)

val to_list : 'a t -> 'a list
(** Converts the deque to a list in front-to-back order.

    ## Examples

    ```ocaml let deque = Deque.of_list [1; 2; 3] in Deque.push_front deque 0;
    Deque.to_list deque (* [0; 1; 2; 3] *) ```

    ## Complexity

    - Time: O(n)
    - Space: O(n) *)

(** {1 Additional Operations} *)

val contains : 'a t -> 'a -> bool
(** Returns [true] if the value exists anywhere in the deque.

    ## Examples

    ```ocaml let deque = Deque.of_list [1; 2; 3] in Deque.contains deque 2 (*
    true *) Deque.contains deque 5 (* false *) ```

    ## Complexity

    - Time: O(n) *)

val append : 'a t -> 'a t -> unit
(** Moves all elements from [deque2] to the back of [deque1]. After this
    operation, [deque2] is empty.

    ## Examples

    ```ocaml let deque1 = Deque.of_list [1; 2] in let deque2 = Deque.of_list
    [3; 4] in Deque.append deque1 deque2; (* deque1: [1, 2, 3, 4] *) (* deque2:
    [] *) ```

    ## Complexity

    - Time: O(m) where m = len(deque2) *)

val split_off : 'a t -> int -> 'a t
(** Splits the deque at the given index. Elements from [index] onwards are moved
    to a new deque and returned.

    ## Examples

    ```ocaml let deque = Deque.of_list [1; 2; 3; 4; 5] in let second_half =
    Deque.split_off deque 3 in Deque.to_list deque (* [1; 2; 3] *) Deque.to_list
    second_half (* [4; 5] *) ```

    ## Complexity

    - Time: O(n) where n = len - index
    - Space: O(n) *)

val to_mut_iter : 'a t -> 'a Iter.MutIterator.t
(** Returns a mutable iterator over the deque's elements from front to back.
    
    ## Examples
    
    ```ocaml
    let deque = Deque.of_list [1; 2; 3] in
    let iter = Deque.to_mut_iter deque in
    ```
*)
