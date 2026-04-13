open Std
open Std.Collections

(* A shrinker is a function that takes a value and returns a list of smaller values *)

type 'value t = 'value -> 'value list

(* === BASIC SHRINKERS === *)

let nil = fun _value -> []

let towards = fun target ->
  fun value ->
    if value = target then
      []
    else
      (* Simple binary shrinking towards target *)
      let diff = value - target in
      let abs_diff =
        if diff < 0 then
          -diff
        else
          diff
      in
      let rec halve n acc =
        if n = 0 then
          acc
        else
          let half = n / 2 in
          halve half (n :: acc)
      in
      (* Generate sequence, remove last element (original value) *)
      let steps = halve abs_diff [] in
      let steps = List.reverse steps in
      (* Reverse to get decreasing order *)
      let steps =
        match steps with
        | _ :: rest -> rest
        | [] -> []
      in
      List.map steps
        ~fn:(fun step ->
          if diff > 0 then
            target + step
          else
            target - step)

(* === PRIMITIVE SHRINKERS === *)

let int = towards 0

let int_towards = fun target ->
  fun value ->
    if value = target then
      []
    else
      let diff = Int.abs (value - target) in
      let rec halve n acc =
        if n = 0 then
          acc
        else
          halve (n / 2) (n :: acc)
      in
      let steps = halve diff [] in
      List.map steps
        ~fn:(fun step ->
          if value > target then
            value - step
          else
            value + step)

let int32 = fun value ->
  if value = 0l then
    []
  else
    let rec halve n acc =
      if n = 0l then
        acc
      else
        halve (Int32.div n 2l) (n :: acc)
    in
    let steps = halve (Int32.abs value) [] in
    List.map steps
      ~fn:(fun step ->
        if value > 0l then
          Int32.sub value step
        else
          Int32.add value step)

let int64 = fun value ->
  if value = 0L then
    []
  else
    let rec halve n acc =
      if n = 0L then
        acc
      else
        halve (Int64.div n 2L) (n :: acc)
    in
    let steps = halve (Int64.abs value) [] in
    List.map steps
      ~fn:(fun step ->
        if value > 0L then
          Int64.sub value step
        else
          Int64.add value step)

let float = fun value ->
  if value = 0.0 then
    []
  else
    let rec halve n acc =
      if n = 0.0 || Float.abs n < 0.000_1 then
        acc
      else
        halve (n /. 2.0) (n :: acc)
    in
    let steps = halve (Float.abs value) [] in
    List.map steps
      ~fn:(fun step ->
        if value > 0.0 then
          value -. step
        else
          value +. step)

let bool = fun value ->
  if value then
    [ false ]
  else
    []

let char = fun value ->
  if value = 'a' then
    []
  else
    let code = Char.code value in
    let target_code = Char.code 'a' in
    if code <= target_code then
      []
    else
      let diff = code - target_code in
      let rec halve n acc =
        if n = 0 then
          acc
        else
          halve (n / 2) (n :: acc)
      in
      let steps = halve diff [] in
      List.map steps ~fn:(fun step -> Char.from_int_unchecked (code - step))

let rune = fun value ->
  let code = Unicode.Rune.to_int value in
  if code = 0 then
    []
  else
    let rec halve n acc =
      if n = 0 then
        acc
      else
        halve (n / 2) (n :: acc)
    in
    let steps = halve code [] in
    List.filter_map steps ~fn:(fun step -> Unicode.Rune.of_int (code - step))

let string = fun value ->
  let len = String.length value in
  if len = 0 then
    []
  else
    (* Strategy 1: Remove characters *)
    let rec remove_positions n acc =
      if n >= len then
        acc
      else
        let prefix = String.sub value ~offset:0 ~len:n in
        let suffix = String.sub value ~offset:(n + 1) ~len:(len - n - 1) in
        remove_positions (n + 1) ((prefix ^ suffix) :: acc)
    in
    let removed = remove_positions 0 [] in
    (* Strategy 2: Shrink to shorter lengths *)
    let rec shrink_length curr acc =
      if curr <= 0 then
        acc
      else
        let half = curr / 2 in
        let shorter = String.sub value ~offset:0 ~len:half in
        shrink_length half (shorter :: acc)
    in
    let shortened = shrink_length len [] in
    removed @ shortened

(* === COLLECTION SHRINKERS === *)

let list = fun elem_shrinker ->
  fun lst ->
    let len = List.length lst in
    if len = 0 then
      []
    else
      (* Strategy 1: Remove elements *)
      let rec remove_at n acc =
        if n >= len then
          acc
        else
          let rec remove_nth = fun i ->
            function
            | [] -> []
            | x :: xs ->
                if i = n then
                  xs
                else
                  x :: remove_nth (i + 1) xs
          in
          remove_at (n + 1) (remove_nth 0 lst :: acc)
      in
      let removed = remove_at 0 [] in
      (* Strategy 2: Shrink to shorter lengths *)
      let rec take n lst =
        match n, lst with
        | (0, _)
        | (_, []) -> []
        | n, x :: xs -> x :: take (n - 1) xs
      in
      let rec shrink_length curr acc =
        if curr <= 0 then
          acc
        else
          let half = curr / 2 in
          shrink_length half (take half lst :: acc)
      in
      let shortened = shrink_length len [] in
      removed @ shortened

let array = fun elem_shrinker ->
  fun arr ->
    let arr_to_list a =
      let rec build i acc =
        if i < 0 then
          acc
        else
          build (i - 1) (Array.get_unchecked a ~at:i :: acc)
      in
      build (Array.length a - 1) []
    in
    let list_to_arr lst =
      match lst with
      | [] -> [||]
      | hd :: _ ->
          let len = List.length lst in
          let a = Array.make ~count:len ~value:hd in
          let rec fill = fun i ->
            function
            | [] -> ()
            | x :: xs ->
                Array.set_unchecked a ~at:i ~value:x;
                fill (i + 1) xs
          in
          fill 0 lst;
          a
    in
    List.map (list elem_shrinker (arr_to_list arr)) ~fn:list_to_arr

let vector = fun elem_shrinker ->
  fun vec ->
    let lst = Vector.iter vec |> Iter.Iterator.to_list in
    List.map (list elem_shrinker lst) ~fn:Vector.from_list

let hashmap = fun key_shrinker value_shrinker ->
  fun hm ->
    let lst = HashMap.to_list hm in
    List.map (list nil lst) ~fn:HashMap.from_list

let hashset = fun elem_shrinker ->
  fun hs ->
    let lst = HashSet.to_list hs in
    List.map (list elem_shrinker lst) ~fn:HashSet.from_list

let queue = fun elem_shrinker ->
  fun q ->
    let lst = Queue.to_list q in
    List.map (list elem_shrinker lst) ~fn:Queue.from_list

let deque = fun elem_shrinker ->
  fun d ->
    let lst = Deque.iter d |> Iter.Iterator.to_list in
    List.map (list elem_shrinker lst)
      ~fn:(fun lst ->
        let d' = Deque.create () in
        List.for_each lst ~fn:(fun value -> Deque.push_back d' ~value);
        d')

let heap = fun elem_shrinker ->
  fun h ->
    let lst = Heap.iter h |> Iter.Iterator.to_list in
    List.map (list elem_shrinker lst)
      ~fn:(fun lst ->
        let h' = Heap.create () in
        List.for_each lst ~fn:(fun value -> Heap.push h' ~value);
        h')

(* === TUPLE SHRINKERS === *)

let pair = fun shrinker_a shrinker_b ->
  fun ((a, b)) ->
    let shrunk_a =
      List.map (shrinker_a a) ~fn:(fun a' -> (a', b))
    in
    let shrunk_b =
      List.map (shrinker_b b) ~fn:(fun b' -> (a, b'))
    in
    shrunk_a @ shrunk_b

let triple = fun shrinker_a shrinker_b shrinker_c ->
  fun ((a, b, c)) ->
    let shrunk_a =
      List.map (shrinker_a a) ~fn:(fun a' -> (a', b, c))
    in
    let shrunk_b =
      List.map (shrinker_b b) ~fn:(fun b' -> (a, b', c))
    in
    let shrunk_c =
      List.map (shrinker_c c) ~fn:(fun c' -> (a, b, c'))
    in
    shrunk_a @ shrunk_b @ shrunk_c

(* === OPTION & RESULT SHRINKERS === *)

let option = fun elem_shrinker ->
  fun opt ->
    match opt with
    | None -> []
    | Some x ->
        let none_candidate = [ None ] in
        let shrunk_some =
          List.map (elem_shrinker x) ~fn:(fun x' -> Some x')
        in
        none_candidate @ shrunk_some

let result = fun ok_shrinker err_shrinker ->
  fun res ->
    match res with
    | Ok x -> List.map (ok_shrinker x) ~fn:(fun x' -> Ok x')
    | Error e -> List.map (err_shrinker e) ~fn:(fun e' -> Error e')

(* === COMBINATORS === *)

let map = fun f f_inv shrinker ->
  fun b ->
    let a = f_inv b in
    List.map (shrinker a) ~fn:f

let filter = fun pred shrinker ->
  fun value ->
    List.filter (shrinker value) ~fn:pred

(* === LOW-LEVEL INTERFACE === *)

let shrink = fun shrinker value -> shrinker value
