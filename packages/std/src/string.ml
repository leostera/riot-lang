open Iter

include Stdlib.String

module MutIter = struct
  type state = { source : string; mutable current_pos : int }
  type item = Uchar.t

  let next state =
    if state.current_pos < length state.source then (
      let utf_decoded = get_utf_8_uchar state.source state.current_pos in
      let char_size = Uchar.utf_decode_length utf_decoded in
      let item = Uchar.utf_decode_uchar utf_decoded in
      state.current_pos <- state.current_pos + char_size;
      Some item)
    else None

  let size { current_pos; source } = length source - current_pos
  let clone { source; current_pos } = { source; current_pos }
end

let into_mut_iter source =
  MutIterator.make (module MutIter) { source; current_pos = 0 }

module Iter = struct
  type state = { source : string; current_pos : int }
  type item = Uchar.t

  let next ({ source; current_pos } as state) =
    if current_pos < length source then
      let utf_decoded = get_utf_8_uchar source current_pos in
      let char_size = Uchar.utf_decode_length utf_decoded in
      let item = Uchar.utf_decode_uchar utf_decoded in
      (Some item, { state with current_pos = current_pos + char_size })
    else (None, state)

  let size { current_pos; source } = length source - current_pos
end

let into_iter source = Iterator.make (module Iter) { source; current_pos = 0 }

(*
module Tests = struct
  let%test "empty iterator has size 0" =
    let iter = into_iter "" in
    Iterator.size iter = 0
  ;;

  let%test "iterator has size 3" =
    let iter = into_iter "abc" in
    Iterator.size iter = 3
  ;;

  let%test "iterator size decreases" =
    let iter = into_iter "abc" in
    let _next, iter = Iterator.next iter in
    Iterator.size iter = 2
  ;;

  let%test "iterator size reaches zero" =
    let iter = into_iter "abc" in
    let _next, iter = Iterator.next iter in
    let _next, iter = Iterator.next iter in
    let _next, iter = Iterator.next iter in
    Iterator.size iter = 0
  ;;

  let next iter =
    let item, state = Iterator.next iter in
    Option.get item, state
  ;;

  let%test "iterator returns None after its exhausted" =
    let iter = into_iter "abc" in
    let a, iter = Iterator.next iter in
    let b, iter = Iterator.next iter in
    let c, iter = Iterator.next iter in
    let r1, iter = Iterator.next iter in
    let r2, iter = Iterator.next iter in
    let r3, _ = Iterator.next iter in
    a != None && b != None && c != None && r1 = None && r2 = None && r3 = None
  ;;

  let%test "iterator returns different items" =
    let iter = into_iter "abc" in
    let a, iter = next iter in
    let b, iter = next iter in
    let c, _iter = next iter in
    Uchar.(equal a (of_char 'a') && equal b (of_char 'b') && equal c (of_char 'c'))
  ;;

  let%test "iterates over latin9 characters correctly" =
    let iter = into_iter "Élégant" in
    let rec validate_sequence chars iter =
      match chars with
      | [] -> true
      | expected :: chars ->
        (* Printf.printf "expecting: %d\n%!" (Uchar.to_int expected); *)
        let actual, iter = next iter in
        let result = Uchar.(equal actual expected) in
        (* Printf.printf *)
        (*   "%d == %d -> %b\n%!" *)
        (*   (Uchar.to_int actual) *)
        (*   (Uchar.to_int expected) *)
        (*   result; *)
        result && validate_sequence chars iter
    in
    validate_sequence
      Uchar.
        [ of_char 'E'
        ; of_int 0x0301
        ; of_char 'l'
        ; of_char 'e'
        ; of_int 0x0301
        ; of_char 'g'
        ; of_char 'a'
        ; of_char 'n'
        ; of_char 't'
        ]
      iter
  ;;
end
*)
