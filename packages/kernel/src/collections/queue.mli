(** # Collections.Queue - FIFO queue

    A first-in, first-out (FIFO) queue for sequential processing. Elements are
    added to the back and removed from the front.

    ## Examples

    Basic queue operations:

    ```ocaml open Std.Collections

    let queue = Queue.create () in

    (* Add items *) Queue.enqueue queue "first"; Queue.enqueue queue "second";
    Queue.enqueue queue "third";

    (* Remove in FIFO order *) Queue.dequeue queue (* Some "first" *)
    Queue.dequeue queue (* Some "second" *) Queue.dequeue queue (* Some "third"
    *) Queue.dequeue queue (* None - empty *) ```

    Processing tasks in order:

    ```ocaml let task_queue = Queue.create () in

    (* Add tasks *) List.iter (Queue.enqueue task_queue)
    [ "process_file_1.txt"; "process_file_2.txt"; "process_file_3.txt" ];

    (* Process tasks in order *) let rec process_tasks () = match Queue.dequeue
    task_queue with | Some task -> handle_task task; process_tasks () | None ->
    Log.info "All tasks complete" in process_tasks () ```

    ## When to Use Queue

    - Task processing in arrival order
    - Message buffering
    - BFS algorithms
    - Producer-consumer patterns

    ## When to Use Alternatives

    - Need LIFO (stack) behavior → Use [Vector] with push/pop
    - Need access to both ends → Use [Deque]
    - Priority ordering → Implement a priority queue

    ## Performance Characteristics

    - Enqueue: O(1) amortized
    - Dequeue: O(1)
    - Front (peek): O(1)
    - Contains: O(n) *)

type 'a t
(** A FIFO queue containing elements of type ['a]. Elements are added to the
    back and removed from the front. *)

(** {1 Creation} *)

val create : unit -> 'a t
(** Creates a new empty queue.

    ## Examples

    ```ocaml let queue = Queue.create () in assert (Queue.is_empty queue) ``` *)

val with_capacity : int -> 'a t
(** Creates an empty queue with specified initial capacity.

    Pre-allocating capacity can improve performance when the approximate size is
    known in advance.

    ## Examples

    ```ocaml let queue = Queue.with_capacity 1000 in (* Can enqueue ~1000 items
    without reallocation *) ``` *)

val of_list : 'a list -> 'a t
(** Creates a queue from a list. The first list element becomes the front of the
    queue.

    ## Examples

    ```ocaml let queue = Queue.of_list [1; 2; 3] in Queue.dequeue queue (* Some
    1 *) Queue.dequeue queue (* Some 2 *) ```

    ## Complexity

    - Time: O(n)
    - Space: O(n) *)

(** {1 Basic Operations} *)

val enqueue : 'a t -> 'a -> unit
(** Adds an element to the back of the queue.

    ## Examples

    ```ocaml let queue = Queue.create () in Queue.enqueue queue "first";
    Queue.enqueue queue "second"; (* queue: ["first", "second"] - front to back
    *) ```

    ## Complexity

    - Time: O(1) amortized *)

val dequeue : 'a t -> 'a option
(** Removes and returns the front element. Returns [Some element] if the queue
    is not empty, [None] otherwise.

    ## Examples

    ```ocaml let queue = Queue.of_list [1; 2; 3] in Queue.dequeue queue (* Some
    1 *) Queue.dequeue queue (* Some 2 *)

    let empty = Queue.create () in Queue.dequeue empty (* None *) ```

    ## Complexity

    - Time: O(1) *)

val front : 'a t -> 'a option
(** Returns the front element without removing it.

    ## Examples

    ```ocaml let queue = Queue.of_list ["a"; "b"; "c"] in Queue.front queue (*
    Some "a" *) Queue.front queue (* Some "a" - still there *) ```

    ## Complexity

    - Time: O(1) *)

val len : 'a t -> int
(** Returns the number of elements in the queue.

    ## Examples

    ```ocaml let queue = Queue.of_list [1; 2; 3] in Queue.len queue (* 3 *) ```

    ## Complexity

    - Time: O(1) *)

val is_empty : 'a t -> bool
(** Returns [true] if the queue contains no elements.

    ## Examples

    ```ocaml let queue = Queue.create () in Queue.is_empty queue (* true *)
    Queue.enqueue queue 1; Queue.is_empty queue (* false *) ```

    ## Complexity

    - Time: O(1) *)

val clear : 'a t -> unit
(** Removes all elements from the queue.

    ## Examples

    ```ocaml let queue = Queue.of_list [1; 2; 3] in Queue.clear queue; assert
    (Queue.is_empty queue) ```

    ## Complexity

    - Time: O(1)
    - Space: Capacity is preserved for reuse *)

(** {1 Iteration} *)

val iter : ('a -> unit) -> 'a t -> unit
(** Applies function [f] to each element from front to back.

    ## Examples

    ```ocaml let queue = Queue.of_list [1; 2; 3] in Queue.iter (fun x ->
    Printf.printf "%d " x) queue (* Prints: 1 2 3 *) ```

    ## Complexity

    - Time: O(n) *)

val fold : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
(** Folds over all elements from front to back.

    ## Examples

    ```ocaml let queue = Queue.of_list [1; 2; 3; 4] in let sum = Queue.fold (fun
    x acc -> x + acc) queue 0 in (* sum = 10 *) ```

    ## Complexity

    - Time: O(n) *)

val to_list : 'a t -> 'a list
(** Converts the queue to a list in front-to-back order.

    ## Examples

    ```ocaml let queue = Queue.of_list [1; 2; 3] in Queue.enqueue queue 4;
    Queue.to_list queue (* [1; 2; 3; 4] *) ```

    ## Complexity

    - Time: O(n)
    - Space: O(n) *)

(** {1 Additional Operations} *)

val contains : 'a t -> 'a -> bool
(** Returns [true] if the value exists anywhere in the queue.

    ## Examples

    ```ocaml let queue = Queue.of_list ["a"; "b"; "c"] in Queue.contains queue
    "b" (* true *) Queue.contains queue "z" (* false *) ```

    ## Complexity

    - Time: O(n) *)

val append : 'a t -> 'a t -> unit
(** Moves all elements from [queue2] to the back of [queue1]. After this
    operation, [queue2] is empty.

    ## Examples

    ```ocaml let q1 = Queue.of_list [1; 2] in let q2 = Queue.of_list [3; 4] in
    Queue.append q1 q2; (* q1: [1, 2, 3, 4] *) (* q2: [] *) ```

    ## Complexity

    - Time: O(m) where m = len(queue2) *)

val into_iter : 'a t -> 'a Iter.Iterator.t
(** Converts this queue into an immutable iterator over its elements in FIFO order.

    ## Examples

    ```ocaml
    let queue = Queue.of_list [1; 2; 3; 4; 5] in
    queue
    |> Queue.into_iter
    |> Iterator.map ~fn:(fun x -> x * 2)
    |> Iterator.filter ~fn:(fun x -> x > 5)
    |> Iterator.collect
    (* [6; 8; 10] *)
    ```

    ## Complexity

    - Time: O(1) to create iterator
    - Space: O(1) *)

val to_mut_iter : 'a t -> 'a Iter.MutIterator.t
(** Returns a mutable iterator over the queue's elements in FIFO order.

    ## Examples

    ```ocaml let queue = Queue.of_list [1; 2; 3] in let iter = Queue.to_mut_iter
    queue in ``` *)

val transfer : src:'a t -> dst:'a t -> unit
(** Efficiently transfers all elements from [src] to the back of [dst]. After
    this operation, [src] is empty. This is more efficient than [append] as it
    directly manipulates internal pointers rather than re-enqueueing elements.

    ## Examples

    ```ocaml let src = Queue.of_list [3; 4; 5] in let dst = Queue.of_list [1; 2]
    in Queue.transfer ~src ~dst; (* src: [] *) (* dst: [1, 2, 3, 4, 5] *) ```

    ## Complexity

    - Time: O(1)
    - Space: O(1) *)
