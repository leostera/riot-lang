(** # List - Extended list utilities

    A singly-linked list with additional utility functions beyond [Stdlib.List].
    Includes all standard list functions plus extra combinators for common
    patterns.

    ## Examples

    Basic list operations:

    ```ocaml open Std

    let numbers = [1; 2; 3; 4; 5] in

    (* Take/drop operations *) let first_three = List.take 3 numbers in (*
    [1; 2; 3] *) let rest = List.drop 2 numbers in (* [3; 4; 5] *)

    (* Find and transform *) let first_even = List.find_map (fun x -> if x mod 2
    = 0 then Some (x * 10) else None ) numbers in (* Some 20 *)

    (* Group consecutive elements *) let data = [1; 1; 2; 2; 2; 3; 1] in let
    groups = List.group (=) data in (* [[1; 1]; [2; 2; 2]; [3]; [1]] *) ```

    ## When to Use Lists

    - Sequential processing with pattern matching
    - Small collections (< 100 elements)
    - Need persistent/immutable data structures
    - Heavy use of recursion

    ## When to Use Alternatives

    - Need O(1) random access → [Collections.Vector]
    - Large collections with frequent appends → [Collections.Vector] or
      [Collections.Deque]
    - Need uniqueness guarantees → [Collections.HashSet]
    - Key-value lookups → [Collections.HashMap]

    ## Performance Characteristics

    - Prepend (::): O(1)
    - Append (@): O(n) where n is length of first list
    - Pattern match head: O(1)
    - Access by index: O(n)
    - Length: O(n) *)

include module type of Stdlib.List

(** {1 Construction} *)

val make : len:int -> fn:(int -> 'a) -> 'a list
(** Creates a new list by calling [fn] with indices 0 to [len-1].

    ## Examples

    ```ocaml let squares = List.make ~len:5 ~fn:(fun i -> i * i) in (*
    [0; 1; 4; 9; 16] *)

    let indexed = List.make ~len:3 ~fn:(fun i -> (i, format "item_%d" i)) in (*
    [(0, "item_0"); (1, "item_1"); (2, "item_2")] *) ```

    ## Complexity

    - Time: O(n) where n is [len]
    - Space: O(n) *)

(** {1 Search and Transform} *)

val find_map : ('a -> 'b option) -> 'a list -> 'b option
(** Searches for the first element where [fn] returns [Some value], returning
    that value immediately without processing remaining elements.

    More efficient than [List.find] followed by [List.map] when you need to both
    locate and transform an element.

    ## Examples

    ```ocaml let users = [ ("alice", 25); ("bob", 30); ("charlie", 35) ] in

    (* Find first user over 30 and get their name *) let result = List.find_map
    (fun (name, age) -> if age > 30 then Some name else None ) users in (* Some
    "charlie" *)

    (* Parse first valid integer *) let strings = ["foo"; "42"; "bar"; "100"] in
    List.find_map int_of_string_opt strings (* Some 42 *) ```

    ## Complexity

    - Time: O(n) worst case, O(1) best case
    - Space: O(1) *)

val filter_map : ('a -> 'b option) -> 'a list -> 'b list
(** Filters and transforms in a single pass. Only elements where [fn] returns
    [Some value] are included in the result.

    More efficient than [List.filter] followed by [List.map].

    ## Examples

    ```ocaml (* Parse and keep only valid integers *) let strings =
    ["1"; "foo"; "2"; "bar"; "3"] in let numbers = List.filter_map
    int_of_string_opt strings in (* [1; 2; 3] *)

    (* Extract and transform in one pass *) let data =
    [Some 1; None; Some 2; None; Some 3] in let doubled = List.filter_map
    (Option.map (fun x -> x * 2)) data in (* [2; 4; 6] *) ```

    ## Complexity

    - Time: O(n)
    - Space: O(n) for result list *)

(** {1 Subsequences} *)

val split_at : int -> 'a list -> 'a list * 'a list
(** Splits a list at the given index, returning [(prefix, suffix)].

    ## Examples

    ```ocaml let items = [1; 2; 3; 4; 5] in let (left, right) = List.split_at 2
    items in (* left = [1; 2], right = [3; 4; 5] *)

    (* Split at 0 *) List.split_at 0 items (* ([], [1; 2; 3; 4; 5]) *)

    (* Split past end *) List.split_at 10 items (* ([1; 2; 3; 4; 5], []) *) ```

    ## Complexity

    - Time: O(n) where n is the split index
    - Space: O(1) - shares structure with original list *)

val take : int -> 'a list -> 'a list
(** Returns the first [n] elements. If the list has fewer than [n] elements,
    returns the entire list.

    ## Examples

    ```ocaml List.take 3 [1; 2; 3; 4; 5] (* [1; 2; 3] *) List.take 10 [1; 2; 3]
    (* [1; 2; 3] *) List.take 0 [1; 2; 3] (* [] *) ```

    ## Complexity

    - Time: O(min(n, length))
    - Space: O(min(n, length)) *)

val drop : int -> 'a list -> 'a list
(** Returns the list without the first [n] elements. If the list has fewer than
    [n] elements, returns an empty list.

    ## Examples

    ```ocaml List.drop 2 [1; 2; 3; 4; 5] (* [3; 4; 5] *) List.drop 10 [1; 2; 3]
    (* [] *) List.drop 0 [1; 2; 3] (* [1; 2; 3] *) ```

    ## Complexity

    - Time: O(min(n, length))
    - Space: O(1) - shares structure with original list *)

val take_while : ('a -> bool) -> 'a list -> 'a list
(** Returns the longest prefix where [predicate] returns [true]. Stops at the
    first element that doesn't satisfy the predicate.

    ## Examples

    ```ocaml let numbers = [2; 4; 6; 7; 8; 10] in List.take_while (fun x -> x
    mod 2 = 0) numbers (* [2; 4; 6] - stops at 7 *)

    let words = ["hello"; "world"; ""; "foo"] in List.take_while (fun s ->
    String.length s > 0) words (* ["hello"; "world"] *) ```

    ## Complexity

    - Time: O(n) worst case, O(1) best case
    - Space: O(n) for result *)

val drop_while : ('a -> bool) -> 'a list -> 'a list
(** Drops elements from the start while [predicate] returns [true]. Returns the
    remainder once the predicate fails.

    ## Examples

    ```ocaml let numbers = [2; 4; 6; 7; 8; 10] in List.drop_while (fun x -> x
    mod 2 = 0) numbers (* [7; 8; 10] *)

    (* Skip whitespace at start *) let chars = [' '; ' '; 'h'; 'i'] in
    List.drop_while (fun c -> c = ' ') chars (* ['h'; 'i'] *) ```

    ## Complexity

    - Time: O(n) worst case, O(1) best case
    - Space: O(1) - shares structure with original list *)

(** {1 Grouping and Deduplication} *)

val group : ('a -> 'a -> bool) -> 'a list -> 'a list list
(** Groups consecutive equal elements together using the provided equality
    function.

    ## Examples

    ```ocaml (* Group consecutive duplicates *) let data =
    [1; 1; 2; 2; 2; 3; 1; 1] in List.group (=) data (*
    [[1; 1]; [2; 2; 2]; [3]; [1; 1]] *)

    (* Group by property *) let numbers = [1; 3; 2; 4; 6; 5] in List.group (fun
    a b -> (a mod 2) = (b mod 2)) numbers (* [[1; 3]; [2; 4; 6]; [5]] - groups
    odd/even *) ```

    ## Complexity

    - Time: O(n)
    - Space: O(n) *)

val uniq : ('a -> 'a -> bool) -> 'a list -> 'a list
(** Removes consecutive duplicate elements, keeping only the first occurrence of
    each sequence.

    ## Examples

    ```ocaml let data = [1; 1; 2; 2; 2; 3; 1; 1] in List.uniq (=) data (*
    [1; 2; 3; 1] *)

    (* Note: Only removes consecutive duplicates *) (* For global uniqueness,
    use Collections.HashSet *) ```

    ## Complexity

    - Time: O(n)
    - Space: O(n) *)

val intersperse : 'a -> 'a list -> 'a list
(** Inserts a separator element between each pair of list elements.

    ## Examples

    ```ocaml List.intersperse 0 [1; 2; 3] (* [1; 0; 2; 0; 3] *) List.intersperse
    "," ["a"; "b"; "c"] (* ["a"; ","; "b"; ","; "c"] *) List.intersperse 0 [] (*
    [] *) List.intersperse 0 [1] (* [1] *) ```

    ## Use Cases

    - Building comma-separated lists
    - Adding delimiters between items
    - Formatting output

    ## Complexity

    - Time: O(n)
    - Space: O(n) *)

(** {1 Predicates and Access} *)

val is_empty : 'a list -> bool
(** Returns [true] if the list contains no elements.

    ## Examples

    ```ocaml List.is_empty [] (* true *) List.is_empty [1] (* false *) ```

    ## Complexity

    - Time: O(1)
    - Space: O(1) *)

val last : 'a list -> 'a option
(** Returns the last element of the list.

    ## Examples

    ```ocaml List.last [1; 2; 3] (* Some 3 *) List.last [] (* None *) List.last
    [42] (* Some 42 *) ```

    ## Complexity

    - Time: O(n)
    - Space: O(1)

    ## Note

    For frequent access to the last element, consider using [Collections.Vector]
    which provides O(1) access. *)

val init : 'a list -> 'a list option
(** Returns all elements except the last one. Returns [None] if the list is
    empty.

    ## Examples

    ```ocaml List.init [1; 2; 3] (* Some [1; 2] *) List.init [1] (* Some [] *)
    List.init [] (* None *) ```

    ## Complexity

    - Time: O(n)
    - Space: O(n) *)
