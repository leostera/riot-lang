open Std

(** Decode a JSON string using the promoted top-level [Serde] API. *)
val of_string : 'value Serde.t -> string -> ('value, Serde.error) result
