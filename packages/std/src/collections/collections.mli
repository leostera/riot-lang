(** # Collections - Data structure modules

    Collection data structures for efficient data storage and manipulation. All
    modules provide ergonomic APIs with good performance characteristics.

    ## Available Collections

    - [HashMap] - Hash table with O(1) average operations
    - [HashSet] - Hash set for unique values
    - [Vector] - Growable array with O(1) indexed access
    - [Deque] - Double-ended queue for efficient push/pop at both ends
    - [Queue] - FIFO queue for sequential processing
    - [Heap] - Binary heap for priority queue operations

    ## Quick Start

    ```ocaml open Std.Collections

    (* Hash maps for key-value storage *) let map = HashMap.create () in
    HashMap.insert map "key" "value" |> ignore

    (* Hash sets for unique values *) let set = HashSet.of_list [1; 2; 3; 2; 1]
    in HashSet.len set (* 3 - duplicates removed *)

    (* Vectors for indexed access *) let vec = Vector.of_list [1; 2; 3] in
    Vector.get vec 1 (* Some 2 *)

    (* Queues for FIFO processing *) let queue = Queue.create () in
    Queue.enqueue queue "task1"; Queue.dequeue queue (* Some "task1" *) ```

    ## Choosing the Right Collection

    | Need | Use | |------|-----| | Key-value lookups | [HashMap] | | Unique
    values | [HashSet] | | Indexed access | [Vector] | | Push/pop at both ends |
    [Deque] | | FIFO processing | [Queue] | | Priority queue | [Heap] | | Small ordered collection | [List]
    | *)

module HashMap = Hashmap
(** Hash table with O(1) average-case operations. See [HashMap]. *)

module HashSet = Hashset
(** Hash set for storing unique values. See [HashSet]. *)

module Queue = Queue
(** FIFO queue for sequential task processing. See [Queue]. *)

module Deque = Deque
(** Double-ended queue for efficient operations at both ends. See [Deque]. *)

module Vector = Vector
(** Growable array with O(1) indexed access. See [Vector]. *)

module Heap = Heap
(** Binary heap for priority queue operations. See [Heap]. *)
