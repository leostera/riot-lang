open Std

(** Human-readable byte and duration formatting. *)

(** Parse or conversion failure. *)
type error =
  | Empty
  | ExpectedNumber of int
  | InvalidNumber of string
  | MissingUnit of string
  | UnknownUnit of string
  | Overflow
  | PrecisionLoss of string

(** Render an error for diagnostics and test output. *)
val error_to_string: error -> string

(**
   Format a byte count using binary IEC units.

   Example:
   ```ocaml
   Human_units.bytes 563_200 = "550 KiB"
   ```
*)
val bytes: int -> string

(**
   Parse a human-readable byte count.

   Binary units such as `KiB`, `MiB`, and `GiB` use powers of 1024. Decimal
   units such as `KB`, `MB`, and `GB` use powers of 1000.
*)
val parse_bytes: string -> (int, error) result

(**
   Format a duration using compact human-readable units.

   Example:
   ```ocaml
   Human_units.duration (Time.Duration.from_nanos 12_202) = "12.2µs"
   ```
*)
val duration: Time.Duration.t -> string

(**
   Parse a human-readable duration.

   Examples:
   ```ocaml
   Human_units.parse_duration "2years 2mins 12us"
   Human_units.parse_duration "12.2µs"
   ```
*)
val parse_duration: string -> (Time.Duration.t, error) result
