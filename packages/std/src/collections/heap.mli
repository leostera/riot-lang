(** # Collections.Heap - Priority queue implementation
    
    Binary heap implementation providing efficient priority queue operations.
    Supports both min-heap (smallest element first) and max-heap (largest 
    element first) variants.
    
    ## Examples
    
    Min-heap (default):
    
    ```ocaml
    open Std.Collections
    
    let heap = Heap.create () in
    Heap.push heap 5;
    Heap.push heap 3;
    Heap.push heap 7;
    Heap.push heap 1;
    
    Heap.pop heap  (* Some 1 - smallest element *)
    Heap.pop heap  (* Some 3 *)
    Heap.peek heap  (* Some 5 - next element without removing *)
    ```
    
    Max-heap:
    
    ```ocaml
    let heap = Heap.create_max () in
    Heap.push heap 5;
    Heap.push heap 3;
    Heap.push heap 7;
    
    Heap.pop heap  (* Some 7 - largest element *)
    Heap.pop heap  (* Some 5 *)
    ```
    
    Custom comparison:
    
    ```ocaml
    type task = { priority : int; name : string }
    
    let by_priority a b = compare a.priority b.priority in
    let heap = Heap.create_with ~compare:by_priority () in
    
    Heap.push heap { priority = 5; name = "medium" };
    Heap.push heap { priority = 1; name = "urgent" };
    Heap.push heap { priority = 10; name = "low" };
    
    Heap.pop heap  (* Some { priority = 1; name = "urgent" } *)
    ```
    
    Building from list:
    
    ```ocaml
    let heap = Heap.of_list [5; 2; 8; 1; 9] in
    Heap.to_list heap  (* [1; 2; 5; 8; 9] - sorted *)
    ```
    
    ## Use Cases
    
    - Task scheduling by priority
    - Dijkstra's shortest path algorithm
    - Huffman coding
    - Event-driven simulation
    - K-th largest/smallest element problems
    - Merge k sorted lists
    
    ## Performance
    
    - Push: O(log n)
    - Pop: O(log n)
    - Peek: O(1)
    - Size: O(1)
*)

type 'a t
(** A binary heap containing elements of type ['a]. *)

(** {1 Creation} *)

val create : unit -> 'a t
(** Creates an empty min-heap using [compare].
    
    ## Examples
    
    ```ocaml
    let heap = Heap.create () in
    Heap.push heap 5;
    Heap.push heap 3;
    Heap.pop heap  (* Some 3 - min-heap returns smallest *)
    ```
*)

val create_max : unit -> 'a t
(** Creates an empty max-heap using reversed [compare].
    
    ## Examples
    
    ```ocaml
    let heap = Heap.create_max () in
    Heap.push heap 5;
    Heap.push heap 3;
    Heap.pop heap  (* Some 5 - max-heap returns largest *)
    ```
*)

val create_with : compare:('a -> 'a -> int) -> unit -> 'a t
(** Creates an empty heap with custom comparison function.
    
    The comparison function should return:
    - negative if first argument is less than second
    - zero if arguments are equal
    - positive if first argument is greater than second
    
    ## Examples
    
    ```ocaml
    (* Min-heap by absolute value *)
    let heap = Heap.create_with ~compare:(fun a b -> 
      compare (abs a) (abs b)
    ) () in
    
    Heap.push heap (-5);
    Heap.push heap 3;
    Heap.pop heap  (* Some 3 - smallest absolute value *)
    ```
*)

val of_list : 'a list -> 'a t
(** Creates a min-heap from a list. O(n) time complexity.
    
    ## Examples
    
    ```ocaml
    let heap = Heap.of_list [5; 2; 8; 1] in
    Heap.peek heap  (* Some 1 *)
    ```
*)

val of_list_with : compare:('a -> 'a -> int) -> 'a list -> 'a t
(** Creates a heap from a list with custom comparison. *)

(** {1 Operations} *)

val push : 'a t -> 'a -> unit
(** Adds an element to the heap. O(log n).
    
    ## Examples
    
    ```ocaml
    let heap = Heap.create () in
    Heap.push heap 5;
    Heap.push heap 3;
    Heap.size heap  (* 2 *)
    ```
*)

val pop : 'a t -> 'a option
(** Removes and returns the top element (min or max depending on heap type).
    Returns [None] if heap is empty. O(log n).
    
    ## Examples
    
    ```ocaml
    let heap = Heap.of_list [3; 1; 4] in
    Heap.pop heap  (* Some 1 *)
    Heap.pop heap  (* Some 3 *)
    Heap.pop heap  (* Some 4 *)
    Heap.pop heap  (* None - empty *)
    ```
*)

val peek : 'a t -> 'a option
(** Returns the top element without removing it. O(1).
    
    ## Examples
    
    ```ocaml
    let heap = Heap.of_list [3; 1; 4] in
    Heap.peek heap  (* Some 1 *)
    Heap.peek heap  (* Some 1 - still there *)
    ```
*)

val pop_exn : 'a t -> 'a
(** Removes and returns the top element. Raises [Not_found] if empty.
    
    ## Examples
    
    ```ocaml
    let heap = Heap.of_list [3; 1] in
    Heap.pop_exn heap  (* 1 *)
    ```
*)

val peek_exn : 'a t -> 'a
(** Returns the top element. Raises [Not_found] if empty. *)

(** {1 Query} *)

val is_empty : 'a t -> bool
(** Returns [true] if the heap is empty.
    
    ## Examples
    
    ```ocaml
    let heap = Heap.create () in
    Heap.is_empty heap  (* true *)
    Heap.push heap 1;
    Heap.is_empty heap  (* false *)
    ```
*)

val size : 'a t -> int
(** Returns the number of elements in the heap. O(1).
    
    ## Examples
    
    ```ocaml
    let heap = Heap.of_list [1; 2; 3] in
    Heap.size heap  (* 3 *)
    ```
*)

val clear : 'a t -> unit
(** Removes all elements from the heap.
    
    ## Examples
    
    ```ocaml
    let heap = Heap.of_list [1; 2; 3] in
    Heap.clear heap;
    Heap.is_empty heap  (* true *)
    ```
*)

(** {1 Conversion} *)

val to_list : 'a t -> 'a list
(** Returns all elements as a sorted list. Empties the heap. O(n log n).
    
    ## Examples
    
    ```ocaml
    let heap = Heap.of_list [5; 2; 8; 1] in
    Heap.to_list heap  (* [1; 2; 5; 8] *)
    Heap.is_empty heap  (* true - heap is now empty *)
    ```
*)

val to_list_unordered : 'a t -> 'a list
(** Returns all elements in arbitrary order without emptying the heap. O(n).
    
    ## Examples
    
    ```ocaml
    let heap = Heap.of_list [5; 2; 8; 1] in
    let items = Heap.to_list_unordered heap in
    Heap.size heap  (* 4 - heap unchanged *)
    ```
*)

val iter : ('a -> unit) -> 'a t -> unit
(** Iterates over elements in heap order, emptying the heap. O(n log n).
    
    ## Examples
    
    ```ocaml
    let heap = Heap.of_list [3; 1; 4] in
    Heap.iter (fun x -> println "%d" x) heap
    (* Prints: 1, 3, 4 *)
    ```
*)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** Folds over elements in heap order, emptying the heap. O(n log n).
    
    ## Examples
    
    ```ocaml
    let heap = Heap.of_list [1; 2; 3] in
    Heap.fold (fun acc x -> acc + x) 0 heap  (* 6 *)
    ```
*)

val to_mut_iter : 'a t -> 'a Iter.MutIterator.t
(** Returns a mutable iterator over the heap's elements in priority order.
    Note: This consumes the heap as you iterate.
    
    ## Examples
    
    ```ocaml
    let heap = Heap.of_list [5; 2; 8; 1] in
    let iter = Heap.to_mut_iter heap in
    Iter.MutIterator.to_list iter  (* [1; 2; 5; 8] - sorted order *)
    (* heap is now empty *)
    ```
*)
