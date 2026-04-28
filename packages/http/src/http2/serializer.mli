(** HTTP/2 Frame Serializer *)
open Std

type payload_error = {
  frame_type: Frame.frame_type;
  payload: Frame.payload;
}
type error =
  | PayloadMismatch of payload_error
  | SettingsAckWithPayload of { setting_count: int }
  | InvalidPingPayloadLength of { length: int }
  | InvalidWindowUpdateIncrement of { increment: int }
  | PayloadLengthTooLarge of { length: int; max_length: int }
  | InvalidUnknownFrameTypeCode of { code: int }
val error_to_string: error -> string

val serialize_frame: Frame.t -> (string, error) Result.t

val write_uint24_be: int -> string

val write_uint32_be: int -> string

val write_uint16_be: int -> string
