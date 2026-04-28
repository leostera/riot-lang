open Std
open Std.IO

module Buffer = IO.Buffer
module Cell = Sync.Cell

type config = { max_frame_size: int }

let default_config = { max_frame_size = 16_384 }

type parse_phase =
  | ReadingFrameHeader of {
      buffer: Buffer.t;
      bytes_read: int;
    }
  | ReadingFramePayload of {
      header_bytes: string;
      buffer: Buffer.t;
      bytes_read: int;
      total_length: int;
    }

type state = {
  config: config;
  phase: parse_phase Cell.t;
}

type parse_error =
  | ReadFailed of IO.error
  | FrameParseFailed of Parser.error

type parse_result =
  | Frame of Frame.t
  | Need_more
  | Error of parse_error

let parser_config = fun (config: config): Parser.config -> {
  Parser.max_frame_size = config.max_frame_size;
}

let parse_error_to_string = function
  | ReadFailed error -> "Read failed: " ^ IO.error_message error
  | FrameParseFailed error -> Parser.error_to_string error

let create = fun ?(config = default_config) () -> {
  config;
  phase = Cell.create (ReadingFrameHeader { buffer = Buffer.create ~size:9; bytes_read = 0 });
}

let reset = fun state ->
  Cell.set
    state.phase
    (ReadingFrameHeader { buffer = Buffer.create ~size:9; bytes_read = 0 })

let buffered_bytes = fun state ->
  match Cell.get state.phase with
  | ReadingFrameHeader { bytes_read; _ } -> bytes_read
  | ReadingFramePayload { bytes_read; _ } -> bytes_read + 9

let read_n_bytes = fun reader buffer n ->
  if n <= 0 then
    Ok 0
  else
    let reader = IO.Reader.take reader ~limit:n in
    match IO.Reader.read reader ~into:buffer with
    | Ok bytes_read -> Ok bytes_read
    | Error error -> Error (ReadFailed error)

let frame_payload_length = fun config header_bytes ->
  match Parser.parse_frame_header ~config:(parser_config config) header_bytes with
  | Parser.Done { value = (length, _, _, _); _ } -> Ok length
  | Parser.Need_more -> Error (FrameParseFailed (Parser.FailedToRead Parser.FrameLength))
  | Parser.Error error -> Error (FrameParseFailed error)

let parse_complete_frame = fun bytes ->
  match Parser.parse_frame bytes with
  | Parser.Done { value; _ } -> Frame value
  | Parser.Need_more -> Need_more
  | Parser.Error error -> Error (FrameParseFailed error)

let finish_complete_frame = fun state bytes ->
  match parse_complete_frame bytes with
  | Frame frame ->
      reset state;
      Frame frame
  | Need_more -> Need_more
  | Error error ->
      reset state;
      Error error

let rec parse = fun state reader ->
  match Cell.get state.phase with
  | ReadingFrameHeader { buffer; bytes_read } -> (
      let needed = 9 - bytes_read in
      match read_n_bytes reader buffer needed with
      | Error error -> Error error
      | Ok actual_read ->
          if actual_read = 0 && bytes_read = 0 then
            Need_more
          else if bytes_read + actual_read < 9 then (
            Cell.set
              state.phase
              (ReadingFrameHeader { buffer; bytes_read = bytes_read + actual_read });
            Need_more
          ) else
            let header_bytes = Buffer.contents buffer in
            match frame_payload_length state.config header_bytes with
            | Error error ->
                reset state;
                Error error
            | Ok 0 -> finish_complete_frame state header_bytes
            | Ok total_length ->
                Cell.set
                  state.phase
                  (
                    ReadingFramePayload {
                      header_bytes;
                      buffer = Buffer.create ~size:total_length;
                      bytes_read = 0;
                      total_length;
                    }
                  );
                parse state reader
    )
  | ReadingFramePayload {
    header_bytes;
    buffer;
    bytes_read;
    total_length
  } ->
      (
          let needed = total_length - bytes_read in
          match read_n_bytes reader buffer needed with
          | Error error -> Error error
          | Ok actual_read ->
              if actual_read = 0 && bytes_read = 0 then
                Need_more
              else if bytes_read + actual_read < total_length then (
                Cell.set
                  state.phase
                  (
                    ReadingFramePayload {
                      header_bytes;
                      buffer;
                      bytes_read = bytes_read + actual_read;
                      total_length;
                    }
                  );
                Need_more
              ) else
                finish_complete_frame state (header_bytes ^ Buffer.contents buffer)
        )
