include module type of Stdlib.List

(** Creates a new list by calling [fn] with indices 0 to [len-1].

    ## Examples

    ```ocaml 
    let squares = List.make ~len:5 ~fn:(fun i -> i * i) in
    (* [0; 1; 4; 9; 16] *)

    let indexed = List.make ~len:3 ~fn:(fun i -> (i, "item_" ^ string_of_int i)) in
    (* [(0, "item_0"); (1, "item_1"); (2, "item_2")] *)
    ```

    ## Complexity

    - Time: O(n) where n is [len]
    - Space: O(n) *)
val make: len:int -> fn:(int -> 'a) -> 'a list

(** Returns a list with duplicate elements removed, preserving order.
    Keeps the first occurrence of each element.
    Uses structural equality (=) to compare elements.

    ## Examples

    ```ocaml
    List.unique [1; 2; 2; 3; 1; 4]  (* [1; 2; 3; 4] *)
    List.unique ["a"; "b"; "a"]     (* ["a"; "b"] *)
    List.unique []                  (* [] *)
    ```

    ## Complexity

    - Time: O(n²) where n is the list length
    - Space: O(n) *)
val unique: 'a list -> 'a list
