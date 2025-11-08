open Global0

include module type of Stdlib.List

val make : len:int -> fn:(int -> 'a) -> 'a list
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
