(** Rune - Unicode code points *)
open Global
open IO
module Uchar = Kernel.Uchar

type t = Uchar.t

(* Constants *)
let max = Uchar.unsafe_of_int 0x10FFFF
let replacement = Uchar.unsafe_of_int 0xFFFD
let max_ascii = Uchar.unsafe_of_int 0x7F
let max_latin1 = Uchar.unsafe_of_int 0xFF

(* Conversion *)
let of_int n = 
  if Uchar.is_valid n then Some (Uchar.unsafe_of_int n)
  else None
let to_int = Uchar.to_int
let of_char c = Uchar.of_char c
let unsafe_of_int n = Uchar.unsafe_of_int n

let to_string r =
  let buf = Bytes.create 4 in
  let len = Bytes.set_utf_8_uchar buf 0 r in
  Bytes.sub_string buf 0 len

(* Character classification - using full Unicode tables *)

let is_ascii r = to_int r <= 0x7F

let is_letter r =
  Unicode_tables.in_table Unicode_tables._l (to_int r)

let is_upper r =
  Unicode_tables.in_table Unicode_tables._lu (to_int r)

let is_lower r =
  Unicode_tables.in_table Unicode_tables._ll (to_int r)

let is_title r =
  Unicode_tables.in_table Unicode_tables._lt (to_int r)

let is_digit r =
  Unicode_tables.in_table Unicode_tables._nd (to_int r)

let is_number r =
  Unicode_tables.in_table Unicode_tables._n (to_int r)

let is_space r =
  Unicode_tables.in_table Unicode_tables._zs (to_int r)

let is_punct r =
  Unicode_tables.in_table Unicode_tables._p (to_int r)

let is_symbol r =
  Unicode_tables.in_table Unicode_tables._s (to_int r)

let is_mark r =
  Unicode_tables.in_table Unicode_tables._m (to_int r)

let is_control r =
  Unicode_tables.in_table Unicode_tables._cc (to_int r)

let is_print r = not (is_control r)

let is_graphic r =
  is_print r && not (is_space r)

(* Case conversion using full Unicode tables *)
let to_upper r =
  unsafe_of_int (Case_tables.to_upper (to_int r))

let to_lower r =
  unsafe_of_int (Case_tables.to_lower (to_int r))

let to_title r =
  unsafe_of_int (Case_tables.to_title (to_int r))

(* Display width calculation using complete width tables *)
let width r =
  let c = to_int r in
  let open Width_tables in
  
  (* Control characters have width 0 *)
  if is_control r then 0
  (* Combining marks have width 0 *)
  else if in_table combining c then 0
  (* Zero-width characters *)
  else if c = 0x200B || c = 0x200C || c = 0x200D || c = 0xFEFF then 0
  (* Double-width characters *)
  else if in_table doublewidth c then 2
  (* Ambiguous width - depends on locale setting *)
  else if in_table ambiguous c then
    if Config.get_east_asian_width () then 2 else 1
  (* Narrow width (explicitly width 1) *)
  else if in_table narrow c then 1
  (* Default to width 1 *)
  else 1

(* East Asian width properties *)
let is_wide r =
  let c = to_int r in
  let open Width_tables in
  in_table doublewidth c

let is_fullwidth r =
  let c = to_int r in
  c >= 0xFF00 && c <= 0xFFEF

let is_ambiguous r =
  let c = to_int r in
  let open Width_tables in
  in_table ambiguous c
