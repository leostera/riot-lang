open Global

(** A log event with timestamp, level, message, and metadata *)
type t = {
  timestamp: DateTime.t;
  level: Level.t;
  message: string;
  metadata: Metadata.t;
}

let make = fun ~level ~message ?(metadata = Metadata.empty) () ->
  { timestamp = DateTime.now (); level; message; metadata }
