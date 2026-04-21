(** Examples demonstrating property testing for collection operations *)
open Std
open Propane

(* Property: Vector push/pop round-trip *)

let vector_push_pop_prop =
  property "vector push then pop returns the element" Arbitrary.(pair int (vector int))
    (fun ((x, vec)) ->
      let vec_copy = Collections.Vector.from_list
        (Collections.Vector.iter vec |> Iter.Iterator.to_list)
      in
      Collections.Vector.push vec_copy ~value:x;
      match Collections.Vector.pop vec_copy with
      | Some y -> x = y
      | None -> false)

(* Property: HashMap insert/get round-trip *)

let hashmap_insert_get_prop =
  property "hashmap insert then get returns the value" Arbitrary.(triple
    string
    int
    (hashmap string int))
    (fun ((key, value, map)) ->
      let _ = Collections.HashMap.insert map ~key ~value in
      match Collections.HashMap.get map ~key with
      | Some v -> v = value
      | None -> false)

(* Property: HashSet contains after insert *)

let hashset_insert_contains_prop =
  property "hashset contains element after insert" Arbitrary.(pair int (hashset int))
    (fun ((x, set)) ->
      let _ = Collections.HashSet.insert set ~value:x in
      Collections.HashSet.contains set ~value:x)

(* Property: Queue push/pop FIFO order *)

let queue_fifo_prop =
  property "queue maintains FIFO order" Arbitrary.(list int)
    (fun items ->
      let q = Collections.Queue.create () in
      List.for_each items ~fn:(fun item -> Collections.Queue.push q ~value:item);
      (* Pop all and check order *)
      let rec pop_all acc =
        match Collections.Queue.pop q with
        | Some x -> pop_all (x :: acc)
        | None -> List.reverse acc
      in
      let popped = pop_all [] in
      popped = items)

(* Property: Deque push_back/pop_back LIFO *)

let deque_lifo_prop =
  property "deque push_back/pop_back acts as stack" Arbitrary.(list int)
    (fun items ->
      let d = Collections.Deque.create () in
      List.for_each items ~fn:(fun item -> Collections.Deque.push_back d ~value:item);
      let rec pop_all acc =
        match Collections.Deque.pop_back d with
        | Some x -> pop_all (x :: acc)
        | None -> acc
      in
      let popped = pop_all [] in
      popped = items)

(* Property: Array length after of_list *)

let array_length_prop =
  property "array length equals list length" Arbitrary.(list int)
    (fun lst ->
      let arr = Collections.Array.from_list lst in
      Collections.Array.length arr = List.length lst)

(* Property: Vector sort is sorted *)

let vector_sort_prop =
  property "vector sort produces sorted result" Arbitrary.(vector int)
    (fun vec ->
      Collections.Vector.sort vec;
      let lst = Collections.Vector.iter vec |> Iter.Iterator.to_list in
      let rec is_sorted = function
        | []
        | [ _ ] -> true
        | x :: y :: rest -> x <= y && is_sorted (y :: rest)
      in
      is_sorted lst)

let tests = [
  vector_push_pop_prop;
  hashmap_insert_get_prop;
  hashset_insert_contains_prop;
  queue_fifo_prop;
  deque_lifo_prop;
  array_length_prop;
  vector_sort_prop;
]

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"propane-collections-examples" ~tests ~args ())
    ~args:Env.args
    ()
