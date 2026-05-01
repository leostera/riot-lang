(** WebSocket Frame Serializer *)
open Std

type role =
  | Server
  | Client
type error =
  | MaskGenerationFailed of Random.error
  | ClientFrameNotMasked
  | ServerFrameMasked
  | ReservedBitsSet
  | FragmentedControlFrame of {
      opcode: Frame.opcode;
    }
  | ControlFramePayloadTooLarge of {
      opcode: Frame.opcode;
      payload_length: int;
    }
  | InvalidClosePayload of Frame.close_payload_error
  | InvalidTextPayloadUtf8 of { payload_length: int }

val error_to_string: error -> string

(** Serialize a WebSocket frame to wire format *)
val serialize: ?rng:Random.Rng.t -> ?role:role -> Frame.t -> (string, error) result
