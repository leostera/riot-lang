open Global

(** Log events *)
(** Create a log event *)
type t = {
  timestamp: Datetime.t;
  level: Level.t;
  message: string;
  metadata: Metadata.t;
}
val make: level:Level.t -> message:string -> ?metadata:Metadata.t -> unit -> t
