(** HTTP/2 Frame Serializer *)
open Std

type payload_error = {
  frame_type: Frame.frame_type;
  payload: Frame.payload;
}
type stream_id_rule =
  | MustBeZero
  | MustBeNonZero
type setting_id =
  | HeaderTableSize
  | MaxConcurrentStreams
  | InitialWindowSize
  | MaxFrameSize
  | MaxHeaderListSize
type setting_value_rule =
  | Unsigned32
  | InitialWindowSizeRange
  | MaxFrameSizeRange
type error =
  | PayloadMismatch of payload_error
  | SettingsAckWithPayload of { setting_count: int }
  | InvalidPingPayloadLength of { length: int }
  | InvalidWindowUpdateIncrement of { increment: int }
  | PayloadLengthTooLarge of { length: int; max_length: int }
  | InvalidUnknownFrameTypeCode of { code: int }
  | InvalidStreamId of {
      frame_type: Frame.frame_type;
      stream_id: int;
      expected: stream_id_rule;
    }
  | InvalidPaddingLength of {
      frame_type: Frame.frame_type;
      pad_length: int;
    }
  | MissingPriorityFields of {
      frame_type: Frame.frame_type;
    }
  | InvalidPriorityWeight of { weight: int }
  | InvalidStreamDependency of { stream_dependency: int }
  | InvalidPriorityDependency of { stream_id: int; stream_dependency: int }
  | InvalidStreamIdRange of { stream_id: int }
  | InvalidPromisedStreamId of { promised_stream_id: int }
  | InvalidLastStreamId of { last_stream_id: int }
  | InvalidSettingValue of {
      setting: setting_id;
      value: int;
      expected: setting_value_rule;
    }
  | InvalidErrorCode of { code: int }

val error_to_string: error -> string

val serialize_frame: Frame.t -> (string, error) Result.t

val write_uint24_be: int -> string

val write_uint32_be: int -> string

val write_uint16_be: int -> string
