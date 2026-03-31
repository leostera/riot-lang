open Global

(** A log event with timestamp, level, message, and metadata *)
type t = {
  timestamp: Datetime.t;
  level: Level.t;
  message: string;
  metadata: Metadata.t;
}

let make = fun ~level ~message ?(metadata = Metadata.empty) () -> {
  timestamp = Datetime.now ();
  level;
  message;
  metadata;

}
