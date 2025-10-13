(** WebSocket Frame Serializer *)

open Std

val serialize : Frame.t -> string
(** Serialize a WebSocket frame to wire format *)
