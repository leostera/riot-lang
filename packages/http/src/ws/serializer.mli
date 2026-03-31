(** WebSocket Frame Serializer *)
open Std

(** Serialize a WebSocket frame to wire format *)
val serialize : Frame.t -> string
