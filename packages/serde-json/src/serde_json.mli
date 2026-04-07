open Std

val to_string : 'value Serde.Ser.t -> 'value -> (string, Serde.error) result

(** Decode a JSON string using the promoted top-level [Serde] API. *)
val of_string : 'value Serde.De.t -> string -> ('value, Serde.error) result
