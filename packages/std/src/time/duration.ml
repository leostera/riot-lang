open Kernel

type timespec = { secs : int; nanos : int }
type t = timespec

(* Constants *)
let zero = { secs = 0; nanos = 0 }
let max = { secs = max_int; nanos = 999_999_999 }

(* Creation *)
let make ~secs ~nanos = { secs; nanos }
let from_days days = { secs = days * 86400; nanos = 0 }
let from_hours hours = { secs = hours * 3600; nanos = 0 }
let from_mins mins = { secs = mins * 60; nanos = 0 }
let from_secs secs = { secs; nanos = 0 }

let from_millis millis =
  let secs = millis / 1000 in
  let nanos = millis mod 1000 * 1_000_000 in
  { secs; nanos }

let from_micros micros =
  let secs = micros / 1_000_000 in
  let nanos = micros mod 1_000_000 * 1_000 in
  { secs; nanos }

let from_nanos nanos =
  let secs = nanos / 1_000_000_000 in
  let remaining_nanos = nanos mod 1_000_000_000 in
  { secs; nanos = remaining_nanos }

let from_secs_float f =
  let secs = int_of_float f in
  let nanos = int_of_float ((f -. float_of_int secs) *. 1_000_000_000.0) in
  { secs; nanos }

let from_weeks weeks = { secs = weeks * 604800; nanos = 0 }

(* Conversion *)
let to_secs t = t.secs

let to_secs_float t =
  float_of_int t.secs +. (float_of_int t.nanos /. 1_000_000_000.0)

let to_secs_string ?(precision = 2) t =
  let secs_f = to_secs_float t in
  let multiplier = 10.0 ** float_of_int precision in
  let rounded = Float.round (secs_f *. multiplier) /. multiplier in
  (* Format manually without Printf or String module *)
  let int_part = int_of_float rounded in
  let frac_part = rounded -. float_of_int int_part in
  if precision = 0 then
    Int.to_string int_part
  else
    let frac_scaled = int_of_float (frac_part *. multiplier) in
    let frac_str = Int.to_string frac_scaled in
    (* Pad with leading zeros if needed *)
    let padding = Kernel.String.make (precision - Kernel.String.length frac_str) '0' in
    (* Use Kernel.String.concat to avoid String module dependency *)
    Kernel.String.concat "" [Int.to_string int_part; "."; padding; frac_str]

let to_millis t = (t.secs * 1000) + (t.nanos / 1_000_000)
let to_micros t = (t.secs * 1_000_000) + (t.nanos / 1_000)

let to_nanos t =
  Int64.add
    (Int64.mul (Int64.of_int t.secs) 1_000_000_000L)
    (Int64.of_int t.nanos)

(* Subsecond components *)
let subsec_millis t = t.nanos / 1_000_000
let subsec_micros t = t.nanos / 1_000
let subsec_nanos t = t.nanos

(* Predicates *)
let is_zero t = t.secs = 0 && t.nanos = 0

(* Helper for normalizing timespec *)
let normalize t =
  if t.nanos >= 1_000_000_000 then
    let extra_secs = t.nanos / 1_000_000_000 in
    let remaining_nanos = t.nanos mod 1_000_000_000 in
    { secs = t.secs + extra_secs; nanos = remaining_nanos }
  else if t.nanos < 0 then
    let borrow_secs = (abs t.nanos + 999_999_999) / 1_000_000_000 in
    {
      secs = t.secs - borrow_secs;
      nanos = t.nanos + (borrow_secs * 1_000_000_000);
    }
  else t

(* Arithmetic operations *)
let add a b = normalize { secs = a.secs + b.secs; nanos = a.nanos + b.nanos }

let sub a b =
  let result = { secs = a.secs - b.secs; nanos = a.nanos - b.nanos } in
  if result.secs < 0 || (result.secs = 0 && result.nanos < 0) then zero
  else normalize result

let mul t factor =
  if factor <= 0 then zero
  else
    let total_nanos = Int64.of_int t.nanos in
    let total_secs = Int64.of_int t.secs in
    let factor_64 = Int64.of_int factor in
    let new_nanos = Int64.mul total_nanos factor_64 in
    let new_secs = Int64.mul total_secs factor_64 in
    let final_secs = Int64.add new_secs (Int64.div new_nanos 1_000_000_000L) in
    let final_nanos = Int64.rem new_nanos 1_000_000_000L in
    { secs = Int64.to_int final_secs; nanos = Int64.to_int final_nanos }

let div t divisor =
  if divisor <= 0 then panic "Division by zero or negative number"
  else
    let total_nanos =
      Int64.add
        (Int64.mul (Int64.of_int t.secs) 1_000_000_000L)
        (Int64.of_int t.nanos)
    in
    let result_nanos = Int64.div total_nanos (Int64.of_int divisor) in
    let secs = Int64.div result_nanos 1_000_000_000L in
    let nanos = Int64.rem result_nanos 1_000_000_000L in
    { secs = Int64.to_int secs; nanos = Int64.to_int nanos }

(* Checked operations *)
let checked_add a b =
  try
    let result = add a b in
    if result.secs >= 0 then Some result else None
  with _ -> None

let checked_sub a b =
  try
    let result = sub a b in
    if result.secs >= 0 && not (result.secs = 0 && result.nanos < 0) then
      Some result
    else None
  with _ -> None

let checked_mul t factor =
  try
    if factor < 0 then None
    else
      let result = mul t factor in
      if result.secs >= 0 then Some result else None
  with _ -> None

let checked_div t divisor =
  try if divisor <= 0 then None else Some (div t divisor) with _ -> None

(* Saturating operations *)
let saturating_add a b =
  match checked_add a b with Some result -> result | None -> max

let saturating_sub a b =
  match checked_sub a b with Some result -> result | None -> zero

let saturating_mul t factor =
  match checked_mul t factor with
  | Some result -> result
  | None -> if factor > 0 then max else zero

(* Floating point operations *)
let mul_f64 t factor =
  if factor <= 0.0 then zero
  else
    let total_nanos_f =
      (float_of_int t.secs *. 1_000_000_000.0) +. float_of_int t.nanos
    in
    let result_nanos_f = total_nanos_f *. factor in
    let secs = int_of_float (result_nanos_f /. 1_000_000_000.0) in
    let nanos = int_of_float (mod_float result_nanos_f 1_000_000_000.0) in
    { secs; nanos }

let div_f64 t divisor =
  if divisor <= 0.0 then panic "Division by zero or negative number"
  else
    let total_nanos_f =
      (float_of_int t.secs *. 1_000_000_000.0) +. float_of_int t.nanos
    in
    let result_nanos_f = total_nanos_f /. divisor in
    let secs = int_of_float (result_nanos_f /. 1_000_000_000.0) in
    let nanos = int_of_float (mod_float result_nanos_f 1_000_000_000.0) in
    { secs; nanos }

(* Utility *)
let abs_diff a b = if compare a b >= 0 then sub a b else sub b a

and compare a b =
  let secs_cmp = compare a.secs b.secs in
  if secs_cmp = 0 then compare a.nanos b.nanos else secs_cmp

let min a b = if compare a b <= 0 then a else b
let max a b = if compare a b >= 0 then a else b
let equal a b = compare a b = 0
