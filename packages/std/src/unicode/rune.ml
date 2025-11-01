(** Rune - Unicode code points *)

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

(* Character classification - basic implementation *)
(* Note: These are simplified. Full implementation would use Unicode tables *)

let is_ascii r = to_int r <= 0x7F

let is_letter r =
  let c = to_int r in
  (c >= 0x41 && c <= 0x5A) ||  (* A-Z *)
  (c >= 0x61 && c <= 0x7A) ||  (* a-z *)
  (c >= 0xC0 && c <= 0xFF && c <> 0xD7 && c <> 0xF7)  (* Latin-1 letters *)

let is_digit r =
  let c = to_int r in
  c >= 0x30 && c <= 0x39  (* 0-9 *)

let is_space r =
  match to_int r with
  | 0x20 | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D  (* Common ASCII whitespace *)
  | 0xA0  (* Non-breaking space *)
  | 0x1680 | 0x2000 | 0x2001 | 0x2002 | 0x2003 | 0x2004 
  | 0x2005 | 0x2006 | 0x2007 | 0x2008 | 0x2009 | 0x200A
  | 0x2028 | 0x2029 | 0x202F | 0x205F | 0x3000 -> true
  | _ -> false

let is_control r =
  let c = to_int r in
  (c <= 0x1F) || (c >= 0x7F && c <= 0x9F)

let is_print r = not (is_control r)

let is_graphic r =
  is_print r && not (is_space r)

let is_mark r =
  let c = to_int r in
  let open Width_tables in
  in_table combining c

let is_number r = is_digit r  (* Simplified *)

let is_punct r =
  let c = to_int r in
  (c >= 0x21 && c <= 0x2F) ||  (* Basic punctuation range 1 *)
  (c >= 0x3A && c <= 0x40) ||  (* Basic punctuation range 2 *)
  (c >= 0x5B && c <= 0x60) ||  (* Basic punctuation range 3 *)
  (c >= 0x7B && c <= 0x7E)     (* Basic punctuation range 4 *)

let is_symbol r =
  let c = to_int r in
  (c >= 0x2200 && c <= 0x22FF) ||  (* Mathematical symbols *)
  (c >= 0x2300 && c <= 0x23FF) ||  (* Miscellaneous Technical *)
  (c >= 0x2600 && c <= 0x26FF) ||  (* Miscellaneous Symbols *)
  (c >= 0x2700 && c <= 0x27BF)     (* Dingbats *)

(* Case operations *)
let is_upper r =
  let c = to_int r in
  (c >= 0x41 && c <= 0x5A) ||  (* A-Z *)
  (c >= 0xC0 && c <= 0xDE && c <> 0xD7)  (* Latin-1 uppercase *)

let is_lower r =
  let c = to_int r in
  (c >= 0x61 && c <= 0x7A) ||  (* a-z *)
  (c >= 0xDF && c <= 0xFF && c <> 0xF7)  (* Latin-1 lowercase *)

let is_title _ = false  (* Simplified: titlecase is rare *)

let to_upper r =
  let c = to_int r in
  if c >= 0x61 && c <= 0x7A then
    unsafe_of_int (c - 32)  (* a-z -> A-Z *)
  else if c >= 0xE0 && c <= 0xFE && c <> 0xF7 then
    unsafe_of_int (c - 32)  (* Latin-1 lowercase -> uppercase *)
  else r

let to_lower r =
  let c = to_int r in
  if c >= 0x41 && c <= 0x5A then
    unsafe_of_int (c + 32)  (* A-Z -> a-z *)
  else if c >= 0xC0 && c <= 0xDE && c <> 0xD7 then
    unsafe_of_int (c + 32)  (* Latin-1 uppercase -> lowercase *)
  else r

let to_title r = to_upper r  (* Simplified *)

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
