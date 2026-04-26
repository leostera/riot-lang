open Kernel

let panic = Kernel.SystemError.panic

type timespec = { secs: int; nanos: int }

type t = timespec

(* Constants *)

let zero = { secs = 0; nanos = 0 }

let max_duration = { secs = (-1) lsr 1; nanos = 999_999_999 }

(* Creation *)

let make = fun ~secs ~nanos -> { secs; nanos }

let from_days = fun days -> { secs = days * 86_400; nanos = 0 }

let from_hours = fun hours -> { secs = hours * 3_600; nanos = 0 }

let from_mins = fun mins -> { secs = mins * 60; nanos = 0 }

let from_secs = fun secs -> { secs; nanos = 0 }

let from_millis = fun millis ->
  let secs = millis / 1_000 in
  let nanos = millis mod 1_000 * 1_000_000 in
  { secs; nanos }

let from_micros = fun micros ->
  let secs = micros / 1_000_000 in
  let nanos = micros mod 1_000_000 * 1_000 in
  { secs; nanos }

let from_nanos = fun nanos ->
  let secs = nanos / 1_000_000_000 in
  let remaining_nanos = nanos mod 1_000_000_000 in
  { secs; nanos = remaining_nanos }

let from_secs_float = fun f ->
  let secs = Int.from_float f in
  let nanos = Int.from_float ((f -. Float.from_int secs) *. 1_000_000_000.0) in
  { secs; nanos }

let from_weeks = fun weeks -> { secs = weeks * 604_800; nanos = 0 }

(* Conversion *)

let to_secs = fun t -> t.secs

let to_secs_float = fun t -> Float.from_int t.secs +. (Float.from_int t.nanos /. 1_000_000_000.0)

let to_secs_string = fun ?(precision = 2) t ->
  let secs_f = to_secs_float t in
  let multiplier = 10.0 ** Float.from_int precision in
  let rounded = Float.round (secs_f *. multiplier) /. multiplier in
  (* Format manually without Printf or String module *)
  let int_part = Int.from_float rounded in
  let frac_part = rounded -. Float.from_int int_part in
  if precision = 0 then
    Int.to_string int_part
  else
    let frac_scaled = Int.from_float (frac_part *. multiplier) in
    let frac_str = Int.to_string frac_scaled in
    (* Pad with leading zeros if needed *)
    let padding = Kernel.String.make ~len:(precision - Kernel.String.length frac_str) ~char:'0' in
    (* Use Kernel.String.concat to avoid String module dependency *)
    Kernel.String.concat "" [ Int.to_string int_part; "."; padding; frac_str; ]

let to_millis = fun t -> (t.secs * 1_000) + (t.nanos / 1_000_000)

let to_micros = fun t -> (t.secs * 1_000_000) + (t.nanos / 1_000)

let to_nanos = fun t ->
  Int64.add (Int64.mul (Int64.from_int t.secs) 1_000_000_000L) (Int64.from_int t.nanos)

(* Subsecond components *)

let subsec_millis = fun t -> t.nanos / 1_000_000

let subsec_micros = fun t -> t.nanos / 1_000

let subsec_nanos = fun t -> t.nanos

(* Predicates *)

let is_zero = fun t -> t.secs = 0 && t.nanos = 0

(* Helper for normalizing timespec *)

let normalize = fun t ->
  if t.nanos >= 1_000_000_000 then
    let extra_secs = t.nanos / 1_000_000_000 in
    let remaining_nanos = t.nanos mod 1_000_000_000 in
    { secs = t.secs + extra_secs; nanos = remaining_nanos }
  else if t.nanos < 0 then
    let borrow_secs = (Int.abs t.nanos + 999_999_999) / 1_000_000_000 in
    { secs = t.secs - borrow_secs; nanos = t.nanos + (borrow_secs * 1_000_000_000) }
  else
    t

(* Arithmetic operations *)

let add = fun a b -> normalize { secs = a.secs + b.secs; nanos = a.nanos + b.nanos }

let sub = fun a b ->
  let result = { secs = a.secs - b.secs; nanos = a.nanos - b.nanos } in
  if result.secs < 0 || (result.secs = 0 && result.nanos < 0) then
    zero
  else
    normalize result

let mul = fun t factor ->
  if factor <= 0 then
    zero
  else
    let total_nanos = Int64.from_int t.nanos in
    let total_secs = Int64.from_int t.secs in
    let factor_64 = Int64.from_int factor in
    let new_nanos = Int64.mul total_nanos factor_64 in
    let new_secs = Int64.mul total_secs factor_64 in
    let final_secs = Int64.add new_secs (Int64.div new_nanos 1_000_000_000L) in
    let final_nanos = Int64.rem new_nanos 1_000_000_000L in
    { secs = Int64.to_int final_secs; nanos = Int64.to_int final_nanos }

let div = fun t divisor ->
  if divisor <= 0 then
    panic "Division by zero or negative number"
  else
    let total_nanos =
      Int64.add (Int64.mul (Int64.from_int t.secs) 1_000_000_000L) (Int64.from_int t.nanos)
    in
    let result_nanos = Int64.div total_nanos (Int64.from_int divisor) in
    let secs = Int64.div result_nanos 1_000_000_000L in
    let nanos = Int64.rem result_nanos 1_000_000_000L in
    { secs = Int64.to_int secs; nanos = Int64.to_int nanos }

(* Checked operations *)

let checked_add = fun a b ->
  try
    let result = add a b in
    if result.secs >= 0 then
      Some result
    else
      None
  with
  | _ -> None

let checked_sub = fun a b ->
  try
    let result = sub a b in
    if result.secs >= 0 && not (result.secs = 0 && result.nanos < 0) then
      Some result
    else
      None
  with
  | _ -> None

let checked_mul = fun t factor ->
  try
    if factor < 0 then
      None
    else
      let result = mul t factor in
      if result.secs >= 0 then
        Some result
      else
        None
  with
  | _ -> None

let checked_div = fun t divisor ->
  try
    if divisor <= 0 then
      None
    else
      Some (div t divisor)
  with
  | _ -> None

(* Saturating operations *)

let saturating_add = fun a b ->
  match checked_add a b with
  | Some result -> result
  | None -> max_duration

let saturating_sub = fun a b ->
  match checked_sub a b with
  | Some result -> result
  | None -> zero

let saturating_mul = fun t factor ->
  match checked_mul t factor with
  | Some result -> result
  | None ->
      if factor > 0 then
        max_duration
      else
        zero

(* Floating point operations *)

let mul_f64 = fun t factor ->
  if factor <= 0.0 then
    zero
  else
    let total_nanos_f = (Float.from_int t.secs *. 1_000_000_000.0) +. Float.from_int t.nanos in
    let result_nanos_f = total_nanos_f *. factor in
    let secs = Int.from_float (result_nanos_f /. 1_000_000_000.0) in
    let nanos = Int.from_float (Float.rem result_nanos_f 1_000_000_000.0) in
    { secs; nanos }

let div_f64 = fun t divisor ->
  if divisor <= 0.0 then
    panic "Division by zero or negative number"
  else
    let total_nanos_f = (Float.from_int t.secs *. 1_000_000_000.0) +. Float.from_int t.nanos in
    let result_nanos_f = total_nanos_f /. divisor in
    let secs = Int.from_float (result_nanos_f /. 1_000_000_000.0) in
    let nanos = Int.from_float (Float.rem result_nanos_f 1_000_000_000.0) in
    { secs; nanos }

(* Utility *)

let compare = fun a b ->
  let secs_cmp = Int.compare a.secs b.secs in
  match secs_cmp with
  | Order.EQ -> Int.compare a.nanos b.nanos
  | Order.LT
  | Order.GT -> secs_cmp

let abs_diff = fun a b ->
  match compare a b with
  | Order.LT -> sub b a
  | Order.EQ
  | Order.GT -> sub a b

let min = fun a b ->
  match compare a b with
  | Order.LT
  | Order.EQ -> a
  | Order.GT -> b

let max = fun a b ->
  match compare a b with
  | Order.LT -> b
  | Order.EQ
  | Order.GT -> a

let equal = fun a b ->
  match compare a b with
  | Order.EQ -> true
  | Order.LT
  | Order.GT -> false
