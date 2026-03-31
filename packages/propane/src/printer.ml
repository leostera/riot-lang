open Std

type 'value t = 'value -> string

(* === PRIMITIVE PRINTERS === *)

let int = Int.to_string

let int32 = Int32.to_string

let int64 = Int64.to_string

let float = fun ?precision -> Float.to_string ?precision

let bool = Bool.to_string

let char = fun c -> "'" ^ String.make 1 c ^ "'"

let rune = fun r ->
  let code = Unicode.Rune.to_int r in
  "U+" ^ Int.to_string code

let string = fun s -> "\"" ^ s ^ "\""

(* === COLLECTION PRINTERS === *)

let list = fun elem_printer lst ->
  let elements = List.map elem_printer lst in
  "[" ^ String.concat "; " elements ^ "]"

let array = fun elem_printer arr ->
  let elements = Collections.Array.into_iter arr |> Iter.Iterator.to_list |> List.map elem_printer in
  "[|" ^ String.concat "; " elements ^ "|]"

let vector = fun elem_printer vec ->
  let elements = Collections.Vector.into_iter vec |> Iter.Iterator.to_list |> List.map elem_printer in
  "vec[" ^ String.concat "; " elements ^ "]"

let hashmap = fun key_printer value_printer hm ->
  let pairs = Collections.HashMap.into_iter hm |> Iter.Iterator.to_list in
  let pair_strs =
    List.map (fun ((k, v)) -> key_printer k ^ " => " ^ value_printer v) pairs
  in
  "map{" ^ String.concat "; " pair_strs ^ "}"

let hashset = fun elem_printer hs ->
  let elements = Collections.HashSet.into_iter hs |> Iter.Iterator.to_list |> List.map elem_printer in
  "set{" ^ String.concat "; " elements ^ "}"

let queue = fun elem_printer q ->
  let elements = Collections.Queue.into_iter q |> Iter.Iterator.to_list |> List.map elem_printer in
  "queue[" ^ String.concat "; " elements ^ "]"

let deque = fun elem_printer d ->
  let elements = Collections.Deque.into_iter d |> Iter.Iterator.to_list |> List.map elem_printer in
  "deque[" ^ String.concat "; " elements ^ "]"

let heap = fun elem_printer h ->
  let elements = Collections.Heap.into_iter h |> Iter.Iterator.to_list |> List.map elem_printer in
  "heap[" ^ String.concat "; " elements ^ "]"

(* === TUPLE PRINTERS === *)

let pair = fun printer_a printer_b ((a, b)) -> "(" ^ printer_a a ^ ", " ^ printer_b b ^ ")"

let triple = fun printer_a printer_b printer_c ((a, b, c)) -> "("
^ printer_a a
^ ", "
^ printer_b b
^ ", "
^ printer_c c
^ ")"

(* === OPTION & RESULT PRINTERS === *)

let option = fun elem_printer ->
  function
  | None -> "None"
  | Some x -> "Some (" ^ elem_printer x ^ ")"

let result = fun ok_printer err_printer ->
  function
  | Ok x -> "Ok (" ^ ok_printer x ^ ")"
  | Error e -> "Error (" ^ err_printer e ^ ")"
