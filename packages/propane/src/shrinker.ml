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
        let steps = List.rev steps in
        (* Reverse to get decreasing order *)
        let steps =
          match steps with
          | _ :: rest -> rest
          | [] -> []
        in
        List.map
          (fun step ->
            if diff > 0 then
              target + step
            else
              target - step)
          steps

(* === PRIMITIVE SHRINKERS === *)

let int = towards 0

let int_towards = fun target ->
    fun value ->
      if value = target then
        []
      else
        let diff = abs (value - target) in
        let rec halve n acc =
          if n = 0 then
            acc
          else
            halve (n / 2) (n :: acc)
        in
        let steps = halve diff [] in
        List.map
          (fun step ->
            if value > target then
              value - step
            else
              value + step)
          steps

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
      List.map
        (fun step ->
          if value > 0l then
            Int32.sub value step
          else
            Int32.add value step)
        steps

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
      List.map
        (fun step ->
          if value > 0L then
            Int64.sub value step
          else
            Int64.add value step)
        steps

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
      List.map
        (fun step ->
          if value > 0.0 then
            value -. step
          else
            value +. step)
        steps

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
        List.map (fun step -> Char.chr (code - step)) steps

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
      List.filter_map (fun step -> Unicode.Rune.of_int (code - step)) steps

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
          let prefix = String.sub value 0 n in
          let suffix = String.sub value (n + 1) (len - n - 1) in
          remove_positions (n + 1) ((prefix ^ suffix) :: acc)
      in
      let removed = remove_positions 0 [] in
      (* Strategy 2: Shrink to shorter lengths *)
      let rec shrink_length curr acc =
        if curr <= 0 then
          acc
        else
          let half = curr / 2 in
          let shorter = String.sub value 0 half in
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
            build (i - 1) (a.(i) :: acc)
        in
        build (Array.length a - 1) []
      in
      let list_to_arr lst =
        match lst with
        | [] -> [||]
        | hd :: _ ->
            let len = List.length lst in
            let a = Array.make len hd in
            let rec fill = fun i ->
                function
                | [] -> ()
                | x :: xs ->
                    a.(i) <- x;
                    fill (i + 1) xs
            in
            fill 0 lst;
            a
      in
      List.map list_to_arr (list elem_shrinker (arr_to_list arr))

let vector = fun elem_shrinker ->
    fun vec ->
      let lst = Vector.into_iter vec |> Iter.Iterator.to_list in
      List.map Vector.of_list (list elem_shrinker lst)

let hashmap = fun key_shrinker value_shrinker ->
    fun hm ->
      let lst = HashMap.to_list hm in
      List.map HashMap.of_list (list nil lst)

let hashset = fun elem_shrinker ->
    fun hs ->
      let lst = HashSet.to_list hs in
      List.map HashSet.of_list (list elem_shrinker lst)

let queue = fun elem_shrinker ->
    fun q ->
      let lst = Queue.to_list q in
      List.map Queue.of_list (list elem_shrinker lst)

let deque = fun elem_shrinker ->
    fun d ->
      let lst = Deque.into_iter d |> Iter.Iterator.to_list in
      List.map
        (fun lst ->
          let d' = Deque.create () in
          List.iter (Deque.push_back d') lst;
          d')
        (list elem_shrinker lst)

let heap = fun elem_shrinker ->
    fun h ->
      let lst = Heap.into_iter h |> Iter.Iterator.to_list in
      List.map
        (fun lst ->
          let h' = Heap.create () in
          List.iter (Heap.push h') lst;
          h')
        (list elem_shrinker lst)

(* === TUPLE SHRINKERS === *)

let pair = fun shrinker_a shrinker_b ->
    fun ((a, b)) ->
      let shrunk_a =
        List.map (fun a' -> (a', b)) (shrinker_a a)
      in
      let shrunk_b =
        List.map (fun b' -> (a, b')) (shrinker_b b)
      in
      shrunk_a @ shrunk_b

let triple = fun shrinker_a shrinker_b shrinker_c ->
    fun ((a, b, c)) ->
      let shrunk_a =
        List.map (fun a' -> (a', b, c)) (shrinker_a a)
      in
      let shrunk_b =
        List.map (fun b' -> (a, b', c)) (shrinker_b b)
      in
      let shrunk_c =
        List.map (fun c' -> (a, b, c')) (shrinker_c c)
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
            List.map (fun x' -> Some x') (elem_shrinker x)
          in
          none_candidate @ shrunk_some

let result = fun ok_shrinker err_shrinker ->
    fun res ->
      match res with
      | Ok x -> List.map (fun x' -> Ok x') (ok_shrinker x)
      | Error e -> List.map (fun e' -> Error e') (err_shrinker e)

(* === COMBINATORS === *)

let map = fun f f_inv shrinker ->
    fun b ->
      let a = f_inv b in
      List.map f (shrinker a)

let filter = fun pred shrinker ->
    fun value ->
      List.filter pred (shrinker value)

(* === LOW-LEVEL INTERFACE === *)

let shrink = fun shrinker value -> shrinker value
