open Std

type t =
  | RGB of int * int * int
  | ANSI of int
  | ANSI256 of int
  | No_color

let to_string = fun t ->
  match t with
  | RGB (r, g, b) -> "RGB(" ^ Int.to_string r ^ "," ^ Int.to_string g ^ "," ^ Int.to_string b ^ ")"
  | ANSI i -> "ANSI(" ^ Int.to_string i ^ ")"
  | ANSI256 i -> "ANSI256(" ^ Int.to_string i ^ ")"
  | No_color -> "No_color"

exception Invalid_color of string

exception Invalid_color_param of string

exception Invalid_color_num of string * int

let to_255 = fun str ->
  match Int.parse ("0x" ^ str) with
  | Some value -> value
  | None -> raise (Invalid_color_param str)

let rgb = fun r g b -> RGB (to_255 r, to_255 g, to_255 b)

let rgb = fun str ->
  match str
  |> String.into_iter
  |> Iter.Iterator.map ~fn:Unicode.Rune.to_char
  |> Iter.Iterator.to_list
  |> List.map ~fn:(fun char -> String.make ~len:1 ~char) with
  | [ "#"; r1; r2; g1; g2; b1; b2 ] -> rgb (r1 ^ r2) (g1 ^ g2) (b1 ^ b2)
  | [ "#"; r1; g1; b1 ] -> rgb (r1 ^ r1) (g1 ^ g1) (b1 ^ b1)
  | _ -> raise (Invalid_color str)

let ansi = fun value ->
  if Int.(value < 0 || value > 15) then
    raise (Invalid_color_num ("ansi", value))
  else
    ANSI value

let ansi256 = fun value ->
  if Int.(value < 0 || value > 255) then
    raise (Invalid_color_num ("ansi256", value))
  else
    ANSI256 value

let no_color = No_color

let cap_rgb = fun x -> Int.(min (max 0 x) 255)

let from_rgb = fun (r, g, b) -> RGB (cap_rgb r, cap_rgb g, cap_rgb b)

let make = fun str ->
  if String.starts_with ~prefix:"#" str then
    rgb str
  else
    match Int.parse str with
    | Some i ->
        if Int.(i < 0 || i > 255) then
          raise (Invalid_color_num ("numeric", i))
        else if i < 16 then
          ansi i
        else
          ansi256 i
    | None -> raise (Invalid_color str)

let to_escape_seq: mode:[> `bg | `fg] -> t -> string = fun ~mode t ->
  match t with
  | RGB (r, g, b) ->
      let prefix =
        match mode with
        | `fg -> "38;2;"
        | `bg -> "48;2;"
      in
      prefix ^ Int.to_string r ^ ";" ^ Int.to_string g ^ ";" ^ Int.to_string b
  | ANSI c ->
      let bg_mod x =
        if mode = `bg then
          x + 10
        else
          x
      in
      let c =
        if c < 8 then
          bg_mod c + 30
        else
          bg_mod (c - 8) + 90
      in
      Int.to_string c
  | ANSI256 c ->
      let prefix =
        match mode with
        | `fg -> "38;5;"
        | `bg -> "48;5;"
      in
      prefix ^ Int.to_string c
  | No_color -> ""

let is_no_color = fun t -> t = No_color

let is_rgb = fun t ->
  match t with
  | RGB _ -> true
  | _ -> false

let is_ansi = fun t ->
  match t with
  | ANSI _ -> true
  | _ -> false

let is_ansi256 = fun t ->
  match t with
  | ANSI256 _ -> true
  | _ -> false
