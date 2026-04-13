open Std
open Std.Collections

module Array = Collections.Array

(* A shrinker is a function that takes a value and returns a list of smaller values *)

type 'value t = 'value -> 'value list

let contains = fun values candidate ->
  let rec loop remaining =
    match remaining with
    | [] -> false
    | value :: rest ->
        if value = candidate then
          true
        else
          loop rest
  in
  loop values

let dedupe = fun values ->
  let rec loop acc remaining =
    match remaining with
    | [] -> List.reverse acc
    | value :: rest ->
        if contains acc value then
          loop acc rest
        else
          loop (value :: acc) rest
  in
  loop [] values

let rec take = fun count values ->
  match count, values with
  | (0, _)
  | (_, []) -> []
  | count, value :: rest -> value :: take (count - 1) rest

let nth = fun values ~at ->
  let rec loop index remaining =
    match remaining with
    | [] -> None
    | value :: rest ->
        if index = at then
          Some value
        else
          loop (index + 1) rest
  in
  loop 0 values

let replace_nth = fun values ~at ~value ->
  let rec loop index remaining =
    match remaining with
    | [] -> []
    | head :: rest ->
        if index = at then
          value :: rest
        else
          head :: loop (index + 1) rest
  in
  loop 0 values

let string_remove_at = fun value at ->
  let prefix = String.sub value ~offset:0 ~len:at in
  let suffix = String.sub value ~offset:(at + 1) ~len:(String.length value - at - 1) in
  prefix ^ suffix

let string_replace_at = fun value ~at ~char ->
  let prefix = String.sub value ~offset:0 ~len:at in
  let suffix = String.sub value ~offset:(at + 1) ~len:(String.length value - at - 1) in
  prefix ^ String.make ~len:1 ~char ^ suffix

(* === BASIC SHRINKERS === *)

let nil = fun _value -> []

let towards = fun target ->
  fun value ->
    if value = target then
      []
    else
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
          halve (n / 2) (n :: acc)
      in
      let steps = halve abs_diff [] in
      let steps =
        match List.reverse steps with
        | _original :: rest -> List.reverse rest
        | [] -> []
      in
      let smaller_values =
        List.map steps
          ~fn:(fun step ->
            if diff > 0 then
              target + step
            else
              target - step)
      in
      target :: smaller_values

(* === PRIMITIVE SHRINKERS === *)

let int = towards 0

let int_towards = fun target ->
  towards target

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
    let steps = halve (Int32.abs value) [] |> List.reverse in
    let smaller_steps =
      match steps with
      | _original :: rest -> List.reverse rest
      | [] -> []
    in
    0l
    :: List.map smaller_steps
      ~fn:(fun step ->
        if value > 0l then
          step
        else
          Int32.neg step)

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
    let steps = halve (Int64.abs value) [] |> List.reverse in
    let smaller_steps =
      match steps with
      | _original :: rest -> List.reverse rest
      | [] -> []
    in
    0L
    :: List.map smaller_steps
      ~fn:(fun step ->
        if value > 0L then
          step
        else
          Int64.neg step)

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
    let steps = halve (Float.abs value) [] |> List.reverse in
    let smaller_steps =
      match steps with
      | _original :: rest -> List.reverse rest
      | [] -> []
    in
    0.0
    :: List.map smaller_steps
      ~fn:(fun step ->
        if value > 0.0 then
          step
        else
          -.step)

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
      let steps = halve diff [] |> List.reverse in
      let smaller_steps =
        match steps with
        | _original :: rest -> List.reverse rest
        | [] -> []
      in
      'a' :: List.map smaller_steps ~fn:(fun step -> Char.from_int_unchecked (target_code + step))

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
    let steps = halve code [] |> List.reverse in
    let smaller_steps =
      match steps with
      | _original :: rest -> List.reverse rest
      | [] -> []
    in
    let candidates =
      0 :: smaller_steps
    in
    List.filter_map candidates ~fn:Unicode.Rune.from_int

let string = fun value ->
  let len = String.length value in
  if len = 0 then
    []
  else
    let rec remove_positions index acc =
      if index >= len then
        acc
      else
        remove_positions (index + 1) (string_remove_at value index :: acc)
    in
    let removed = remove_positions 0 [] in
    let rec shrink_length current acc =
      if current <= 0 then
        acc
      else
        let half = current / 2 in
        shrink_length half (String.sub value ~offset:0 ~len:half :: acc)
    in
    let shortened = shrink_length len [] in
    let rec shrink_chars index acc =
      if index >= len then
        acc
      else
        let current_char = String.get_unchecked value ~at:index in
        let shrunk =
          List.map (char current_char) ~fn:(fun char' -> string_replace_at value ~at:index ~char:char')
        in
        shrink_chars (index + 1) (List.reverse_append shrunk acc)
    in
    dedupe (removed @ shortened @ shrink_chars 0 [])

(* === COLLECTION SHRINKERS === *)

let list = fun elem_shrinker ->
  fun lst ->
    let len = List.length lst in
    if len = 0 then
      []
    else
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
      let rec shrink_length curr acc =
        if curr <= 0 then
          acc
        else
          let half = curr / 2 in
          shrink_length half (take half lst :: acc)
      in
      let shortened = shrink_length len [] in
      let rec shrink_elements index acc =
        match nth lst ~at:index with
        | None -> acc
        | Some value ->
            let shrunk =
              List.map (elem_shrinker value)
                ~fn:(fun value' -> replace_nth lst ~at:index ~value:value')
            in
            shrink_elements (index + 1) (List.reverse_append shrunk acc)
      in
      dedupe (removed @ shortened @ shrink_elements 0 [])

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
    let entry_shrinker = fun ((key, value)) ->
      let shrunk_keys =
        List.map (key_shrinker key) ~fn:(fun key' -> (key', value))
      in
      let shrunk_values =
        List.map (value_shrinker value) ~fn:(fun value' -> (key, value'))
      in
      shrunk_keys @ shrunk_values
    in
    List.map (list entry_shrinker lst) ~fn:HashMap.from_list

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
