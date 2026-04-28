(** WebSocket Frame Serializer *)
open Std

type error =
  | MaskGenerationFailed of Random.error
val error_to_string: error -> string

(** Serialize a WebSocket frame to wire format *)
val serialize: ?rng:Random.Rng.t -> Frame.t -> (string, error) result
