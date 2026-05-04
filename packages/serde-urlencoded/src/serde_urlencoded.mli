open Std

val to_string: 'value Serde.Ser.t -> 'value -> (string, Serde.error) result

val to_writer: 'value Serde.Ser.t -> IO.Writer.t -> 'value -> (unit, Serde.error) result

val from_string: 'value Serde.De.t -> string -> ('value, Serde.error) result

val from_reader: 'value Serde.De.t -> IO.Reader.t -> ('value, Serde.error) result
