open Std

type t = RGB of int * int * int | ANSI of int | ANSI256 of int | No_color

let to_string t =
  match t with
  | RGB (r, g, b) -> "RGB(" ^ Int.to_string r ^ "," ^ Int.to_string g ^ "," ^ Int.to_string b ^ ")"
  | ANSI i -> "ANSI(" ^ Int.to_string i ^ ")"
  | ANSI256 i -> "ANSI256(" ^ Int.to_string i ^ ")"
  | No_color -> "No_color"

exception Invalid_color of string
exception Invalid_color_param of string
exception Invalid_color_num of string * int

let to_255 str =
  try Int.of_string ("0x" ^ str)
  with Failure _ -> raise (Invalid_color_param str)

let rgb r g b = RGB (to_255 r, to_255 g, to_255 b)

let rgb str =
  match String.to_seq str |> List.of_seq |> List.map (String.make 1) with
  | [ "#"; r1; r2; g1; g2; b1; b2 ] -> rgb (r1 ^ r2) (g1 ^ g2) (b1 ^ b2)
  | [ "#"; r1; g1; b1 ] -> rgb r1 g1 b1
  | _ -> raise (Invalid_color str)

let ansi i = ANSI i
let ansi256 i = ANSI256 i
let no_color = No_color
let cap_rgb x = Int.(min (max 0 x) 255)
let of_rgb (r, g, b) = RGB (cap_rgb r, cap_rgb g, cap_rgb b)

let make str =
  if String.starts_with ~prefix:"#" str then rgb str
  else
    try
      let i = Int.of_string str in
      if i < 16 then ansi i else ansi256 i
    with Failure _ -> raise (Invalid_color str)

let to_escape_seq ~mode t =
  match t with
  | RGB (r, g, b) -> "2;" ^ Int.to_string r ^ ";" ^ Int.to_string g ^ ";" ^ Int.to_string b
  | ANSI c ->
      let bg_mod x = if mode = `bg then x + 10 else x in
      let c = if c < 8 then bg_mod c + 30 else bg_mod (c - 8) + 90 in
      Int.to_string c
  | ANSI256 c -> "5;" ^ Int.to_string c
  | No_color -> ""

let is_no_color t = t = No_color
let is_rgb t = match t with RGB _ -> true | _ -> false
let is_ansi t = match t with ANSI _ -> true | _ -> false
let is_ansi256 t = match t with ANSI256 _ -> true | _ -> false
