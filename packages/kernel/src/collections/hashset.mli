(** # Collections.HashSet - Hash-based sets
    
    A hash set for storing unique values with O(1) average-case operations.
    Similar to Rust's [HashSet], Python's [set], or Java's [HashSet].
    
    ## Examples
    
    Basic set operations:
    
    ```ocaml
    open Std.Collections
    
    let set = HashSet.create () in
    
    (* Insert returns true if newly added *)
    HashSet.insert set "apple" |> ignore;  (* true *)
    HashSet.insert set "apple" |> ignore;  (* false - already exists *)
    
    HashSet.insert set "banana" |> ignore;
    HashSet.insert set "cherry" |> ignore;
    
    (* Check membership *)
    HashSet.contains set "apple";  (* true *)
    HashSet.contains set "grape";  (* false *)
    
    (* Get size *)
    HashSet.len set  (* 3 *)
    ```
    
    Set algebra operations:
    
    ```ocaml
    let set1 = HashSet.of_list [1; 2; 3; 4] in
    let set2 = HashSet.of_list [3; 4; 5; 6] in
    
    let u = HashSet.union set1 set2 in  (* {1, 2, 3, 4, 5, 6} *)
    let i = HashSet.intersection set1 set2 in  (* {3, 4} *)
    let d = HashSet.difference set1 set2 in  (* {1, 2} *)
    let s = HashSet.symmetric_difference set1 set2 in  (* {1, 2, 5, 6} *)
    ```
    
    ## When to Use HashSet
    
    - Need to track unique values
    - Fast membership testing O(1)
    - No ordering required
    - Implementing deduplication
    - Set algebra operations (union, intersection, etc.)
    
    ## Performance Characteristics
    
    - Insert: O(1) average, O(n) worst case
    - Remove: O(1) average, O(n) worst case
    - Contains: O(1) average, O(n) worst case
    - Union/Intersection: O(n + m) where n, m are set sizes
    
    ## Migration from OCaml Stdlib
    
    ```ocaml
    (* Old: Stdlib Set module with functors *)
    module StringSet = Set.Make(String)
    let set = StringSet.empty |> StringSet.add "foo"
    let contains = StringSet.mem "foo" set
    
    (* New: HashSet with simpler API *)
    let set = HashSet.create () in
    HashSet.insert set "foo" |> ignore;
    let contains = HashSet.contains set "foo"
    ```
*)

type 'a t
(** The type of hash sets containing elements of type ['a]. Elements are stored
    in an unordered fashion with uniqueness guaranteed by hashing. *)

(** {1 Creation} *)

val create : unit -> 'a t
(** Creates a new empty hash set.

    ## Examples

    ```ocaml let set = HashSet.create () in assert (HashSet.is_empty set) ``` *)

val with_capacity : int -> 'a t
(** Creates a new empty hash set with specified initial capacity.

    Pre-allocating capacity can improve performance when the approximate size is
    known in advance.

    ## Examples

    ```ocaml let set = HashSet.with_capacity 1000 in (* Can insert ~1000
    elements without rehashing *) ``` *)

val of_list : 'a list -> 'a t
(** Creates a hash set from a list of elements. Duplicate elements in the input
    list are automatically deduplicated.

    ## Examples

    ```ocaml let set = HashSet.of_list [1; 2; 2; 3; 3; 3] in HashSet.len set; (*
    3 - duplicates removed *) HashSet.to_list set (* [1; 2; 3] - order
    unspecified *) ``` *)

(** {1 Basic Operations} *)

val insert : 'a t -> 'a -> bool
(** Adds a value to the set. Returns [true] if the value was newly inserted,
    [false] if it already existed.

    ## Examples

    ```ocaml let set = HashSet.create () in HashSet.insert set "new"; (* true -
    newly added *) HashSet.insert set "new" (* false - already exists *) ```

    ## Complexity

    - Time: O(1) average, O(n) worst case during rehashing
    - Space: O(1) amortized *)

val remove : 'a t -> 'a -> bool
(** Removes a value from the set. Returns [true] if the value was present and
    removed, [false] if it wasn't in the set.

    ## Examples

    ```ocaml let set = HashSet.of_list [1; 2; 3] in HashSet.remove set 2; (*
    true *) HashSet.remove set 5 (* false - not in set *) ```

    ## Complexity

    - Time: O(1) average, O(n) worst case *)

val contains : 'a t -> 'a -> bool
(** Returns [true] if the value exists in the set.

    ## Examples

    ```ocaml let set = HashSet.of_list ["a"; "b"; "c"] in HashSet.contains set
    "b"; (* true *) HashSet.contains set "z" (* false *) ```

    ## Complexity

    - Time: O(1) average, O(n) worst case *)

val len : 'a t -> int
(** Returns the number of unique elements in the set.

    ## Examples

    ```ocaml let set = HashSet.of_list [1; 2; 3] in HashSet.len set (* 3 *) ```

    ## Complexity

    - Time: O(1) *)

val is_empty : 'a t -> bool
(** Returns [true] if the set contains no elements.

    ## Examples

    ```ocaml let set = HashSet.create () in HashSet.is_empty set; (* true *)
    HashSet.insert set 1 |> ignore; HashSet.is_empty set (* false *) ```

    ## Complexity

    - Time: O(1) *)

val clear : 'a t -> unit
(** Removes all elements from the set.

    ## Examples

    ```ocaml let set = HashSet.of_list [1; 2; 3] in HashSet.clear set;
    HashSet.is_empty set (* true *) ```

    ## Complexity

    - Time: O(1)
    - Space: Capacity is preserved for reuse *)

(** {1 Iteration} *)

val iter : 'a t -> fn:('a -> unit) -> unit
(** Applies function [fn] to each element in the set. The iteration order is
    unspecified and may change between runs.

    ## Examples

    ```ocaml let set = HashSet.of_list [1; 2; 3] in HashSet.iter set ~fn:(fun x
    -> Printf.printf "%d " x) (* Prints: 1 2 3 (order not guaranteed) *) ```

    ## Complexity

    - Time: O(n) *)

val into_iter : 'a t -> 'a Iter.Iterator.t
(** Converts the set into an immutable iterator.

    ## Examples

    ```ocaml
    set
    |> HashSet.into_iter
    |> Iterator.filter ~fn:(fun x -> x mod 2 = 0)
    |> Iterator.map ~fn:(fun x -> x * 2)
    |> Iterator.to_list
    ``` *)

val to_mut_iter : 'a t -> 'a Iter.MutIterator.t
(** Returns a mutable iterator over the set's elements.

    ## Examples

    ```ocaml let set = HashSet.of_list [1; 2; 3] in let iter =
    HashSet.to_mut_iter set in Iter.MutIterator.to_list iter (* [1; 2; 3] -
    order not guaranteed *) ``` *)

val fold : 'a t -> init:'acc -> fn:('acc -> 'a -> 'acc) -> 'acc
(** Folds over all elements in the set. The iteration order is unspecified.

    ## Examples

    ```ocaml let set = HashSet.of_list [1; 2; 3; 4] in let sum = HashSet.fold
    set ~init:0 ~fn:(fun acc x -> acc + x) in (* sum = 10 *) ```

    ## Complexity

    - Time: O(n) *)

val to_list : 'a t -> 'a list
(** Converts the set to a list. The order of elements is unspecified.

    ## Examples

    ```ocaml let set = HashSet.of_list [3; 1; 2] in HashSet.to_list set (*
    [1; 2; 3] or any permutation *) ```

    ## Complexity

    - Time: O(n)
    - Space: O(n) *)

(** {1 Set Operations} *)

val union : 'a t -> 'a t -> 'a t
(** Returns a new set containing all elements from both sets.

    ## Examples

    ```ocaml let set1 = HashSet.of_list [1; 2; 3] in let set2 = HashSet.of_list
    [3; 4; 5] in let u = HashSet.union set1 set2 in HashSet.to_list u |>
    List.sort compare (* [1; 2; 3; 4; 5] *) ```

    ## Complexity

    - Time: O(n + m) where n = len(set1), m = len(set2)
    - Space: O(n + m) *)

val intersection : 'a t -> 'a t -> 'a t
(** Returns a new set containing only elements present in both sets.

    ## Examples

    ```ocaml let set1 = HashSet.of_list [1; 2; 3; 4] in let set2 =
    HashSet.of_list [3; 4; 5; 6] in let i = HashSet.intersection set1 set2 in
    HashSet.to_list i |> List.sort compare (* [3; 4] *) ```

    ## Complexity

    - Time: O(min(n, m))
    - Space: O(min(n, m)) *)

val difference : 'a t -> 'a t -> 'a t
(** Returns a new set containing elements in [set1] but not in [set2].

    ## Examples

    ```ocaml let set1 = HashSet.of_list [1; 2; 3; 4] in let set2 =
    HashSet.of_list [3; 4; 5] in let d = HashSet.difference set1 set2 in
    HashSet.to_list d |> List.sort compare (* [1; 2] *) ```

    ## Complexity

    - Time: O(n) where n = len(set1)
    - Space: O(n) *)

val symmetric_difference : 'a t -> 'a t -> 'a t
(** Returns a new set containing elements in either set, but not both.
    Equivalent to [(set1 ∪ set2) - (set1 ∩ set2)].

    ## Examples

    ```ocaml let set1 = HashSet.of_list [1; 2; 3] in let set2 = HashSet.of_list
    [2; 3; 4] in let s = HashSet.symmetric_difference set1 set2 in
    HashSet.to_list s |> List.sort compare (* [1; 4] *) ```

    ## Complexity

    - Time: O(n + m)
    - Space: O(n + m) *)

val is_subset : 'a t -> 'a t -> bool
(** Returns [true] if all elements of [set1] are in [set2].

    ## Examples

    ```ocaml let set1 = HashSet.of_list [1; 2] in let set2 = HashSet.of_list
    [1; 2; 3; 4] in HashSet.is_subset set1 set2 (* true *) HashSet.is_subset
    set2 set1 (* false *) ```

    ## Complexity

    - Time: O(n) where n = len(set1) *)

val is_superset : 'a t -> 'a t -> bool
(** Returns [true] if [set1] contains all elements of [set2]. Equivalent to
    [is_subset set2 set1].

    ## Examples

    ```ocaml let set1 = HashSet.of_list [1; 2; 3; 4] in let set2 =
    HashSet.of_list [2; 3] in HashSet.is_superset set1 set2 (* true *) ```

    ## Complexity

    - Time: O(m) where m = len(set2) *)

val is_disjoint : 'a t -> 'a t -> bool
(** Returns [true] if the sets have no common elements.

    ## Examples

    ```ocaml let set1 = HashSet.of_list [1; 2; 3] in let set2 = HashSet.of_list
    [4; 5; 6] in let set3 = HashSet.of_list [3; 4; 5] in

    HashSet.is_disjoint set1 set2 (* true *) HashSet.is_disjoint set1 set3 (*
    false - share 3 *) ```

    ## Complexity

    - Time: O(min(n, m)) *)
