include module type of Stdlib.Array

val into_iter : 'a array -> 'a Iter.Iterator.t
(** Converts this array into an immutable iterator.

    ## Examples

    ```ocaml
    let arr = [|1; 2; 3; 4; 5|] in
    arr
    |> Array.into_iter
    |> Iterator.map ~fn:(fun x -> x * 2)
    |> Iterator.filter ~fn:(fun x -> x > 5)
    |> Iterator.collect
    (* [6; 8; 10] *)
    ```

    ## Complexity

    - Time: O(1) to create iterator
    - Space: O(1) *)

val to_mut_iter : 'a array -> 'a Iter.MutIterator.t
(** Converts this array into a mutable iterator.

    ## Examples

    ```ocaml
    let arr = [|1; 2; 3; 4; 5|] in
    arr
    |> Array.to_mut_iter
    |> MutIterator.map ~fn:(fun x -> x * 2)
    |> MutIterator.filter ~fn:(fun x -> x > 5)
    |> MutIterator.collect
    (* [6; 8; 10] *)
    ```

    ## Complexity

    - Time: O(1) to create iterator
    - Space: O(1) *)
