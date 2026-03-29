include module type of Stdlib.Seq

(** Converts this stream into an immutable iterator.

    ## Examples

    ```ocaml
    let seq = Seq.of_list [1; 2; 3; 4; 5] in
    seq
    |> Stream.into_iter
    |> Iterator.map ~fn:(fun x -> x * 2)
    |> Iterator.filter ~fn:(fun x -> x > 5)
    |> Iterator.collect
    (* [6; 8; 10] *)
    ```

    ## Complexity

    - Time: O(1) to create iterator
    - Space: O(1) *)
val into_iter : 'a t -> 'a Iter.Iterator.t

(** Converts this stream into a mutable iterator.

    ## Examples

    ```ocaml
    let seq = Seq.of_list [1; 2; 3; 4; 5] in
    seq
    |> Stream.to_mut_iter
    |> MutIterator.map ~fn:(fun x -> x * 2)
    |> MutIterator.filter ~fn:(fun x -> x > 5)
    |> MutIterator.collect
    (* [6; 8; 10] *)
    ```

    ## Complexity

    - Time: O(1) to create iterator
    - Space: O(1) *)
val to_mut_iter : 'a t -> 'a Iter.MutIterator.t
