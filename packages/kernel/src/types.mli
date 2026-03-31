(** Core types from Stdlib *)
(* Floating point classification *)

type fpclass = Stdlib.fpclass =
  | FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan
type nonrec 'a option = 'a option =
  | None
  | Some of 'a
type ('a, 'e) result = ('a, 'e) Stdlib.result =
  | Ok of 'a
  | Error of 'e
type 'a cell = {
  mutable value : 'a;
}
type signal_behavior = Stdlib.Sys.signal_behavior =
  | Signal_default
  | Signal_ignore
  | Signal_handle of (int -> unit)
(** Behavior for signal handling *)
