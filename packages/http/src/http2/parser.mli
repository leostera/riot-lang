(** HTTP/2 Frame Parser *)
open Std

type config = { max_frame_size: int }
type 'a parse_result =
  | Done of { value: 'a; remaining: string }
  | Need_more
  | Error of error

and error =
  | FailedToRead of read_field
  | FrameSizeExceedsMaximum of { size: int; max_size: int }
  | InvalidStreamId of {
      frame_type: Frame.frame_type;
      stream_id: int;
      expected: stream_id_rule;
    }
  | InvalidPaddingLength of { length: int; pad_length: int }
  | InvalidHeadersFrameLength of { length: int; offset: int; pad_length: int }
  | InvalidPushPromiseFrameLength of { length: int; offset: int; pad_length: int }
  | InvalidPayloadLength of {
      frame_type: Frame.frame_type;
      expected: payload_length_rule;
      actual: int;
    }
  | MalformedPriorityPayload
  | SettingsAckWithPayload of { length: int }
  | SettingsLengthNotMultipleOfSix of { length: int }
  | InvalidSettingValue of {
      setting: setting_id;
      value: int;
      expected: setting_value_rule;
    }
  | WindowUpdateIncrementZero
  | InvalidPriorityDependency of { stream_id: int; stream_dependency: int }

and read_field =
  | FrameLength
  | FrameType
  | Flags
  | StreamId
  | Priority
  | ErrorCode
  | SettingId
  | SettingValue
  | PromisedStreamId
  | LastStreamId
  | WindowSizeIncrement

and stream_id_rule =
  | MustBeZero
  | MustBeNonZero

and payload_length_rule =
  | Exactly of int
  | AtLeast of int
  | MultipleOf of int

and setting_id =
  | EnablePush
  | InitialWindowSize
  | MaxFrameSize

and setting_value_rule =
  | ZeroOrOne
  | InitialWindowSizeRange
  | MaxFrameSizeRange
val error_to_string: error -> string

val frame_type_name: Frame.frame_type -> string

val parse_frame: ?config:config -> string -> Frame.t parse_result

val parse_frame_header:
  ?config:config ->
  string ->
  (int * Frame.frame_type * Frame.flags * Frame.stream_id) parse_result

val read_uint24_be: string -> int -> int option

val read_uint32_be: string -> int -> int option

val read_uint16_be: string -> int -> int option
