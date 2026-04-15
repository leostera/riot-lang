open Std
open Std.IO

(* Use Buffer from Std.IO *)

module Buffer = IO.Buffer

(* Use Cell from Sync *)

module Cell = Sync.Cell

(** Reentrant HPACK decoder using IO.Reader *)
type decoder_phase =
  | WaitingForHeader
  (** Waiting for first byte of next header *)
  | ReadingIndexedName of {
      first_byte: int;
      prefix_bits: int;
      accumulated_value: int;
      multiplier: int
    }
  | ReadingLiteralName of { name_length: int; bytes_read: int; buffer: Buffer.t }
  | ReadingLiteralValue of {
      name: string;
      value_length: int;
      bytes_read: int;
      buffer: Buffer.t;
      should_index: bool
    }

type decoder = {
  hpack_decoder: Hpack.decoder;
  mutable phase: decoder_phase;
  accumulated_headers: Hpack.header list Cell.t;
}

type decode_error =
  | Invalid_header_index of int
  | Invalid_name_index of int
  | Unsupported_encoding
  | Invalid_decoder_state
  | Need_more_data

type decode_result =
  | Headers of Hpack.header list
  | Need_more
  | Error of decode_error

let create = fun ?(max_dynamic_table_size = 4_096) () ->
  {
    hpack_decoder = Hpack.create_decoder ~max_dynamic_table_size ();
    phase = WaitingForHeader;
    accumulated_headers = Cell.create []
  }

let update_max_table_size = fun decoder size ->
  Hpack.update_max_table_size decoder.hpack_decoder size

let reset = fun decoder ->
  decoder.phase <- WaitingForHeader;
  Cell.set decoder.accumulated_headers []

let dynamic_table_size = fun decoder ->
  (* Access internal dynamic table size - simplified for now *)
  4_096

(** Try to read one byte from reader *)
let read_byte = fun reader ->
  let buf = Bytes.create ~size:1 in
  match IO.Reader.read reader buf with
  | Ok 1 -> Some (Bytes.get_unchecked buf ~at:0 |> Char.to_int)
  | _ -> None

(** Try to read N bytes from reader *)
let read_n_bytes = fun reader n ->
  if n = 0 then
    Some (Bytes.create ~size:0)
  else
    let buf = Bytes.create ~size:n in
    match IO.Reader.read reader buf with
    | Ok bytes_read when bytes_read = n -> Some buf
    | _ -> None

(** Decode variable-length integer (reentrant) *)
let decode_varint_incremental = fun reader first_byte prefix_bits accumulated multiplier ->
  let prefix_mask = (1 lsl prefix_bits) - 1 in
  let first_value = first_byte land prefix_mask in
  if first_value < prefix_mask then
    Result.Ok (first_value, 0, 1)
  else
    (* Need continuation bytes *)
    let rec read_continuation acc mult =
      match read_byte reader with
      | None -> Result.Error Need_more_data
      | Some byte ->
          let value = byte land 0x7f in
          let acc = acc + (value * mult) in
          if byte land 0x80 = 0 then
            Result.Ok (acc, 0, 1)
          else
            read_continuation acc (mult * 128)
    in
    read_continuation prefix_mask multiplier

let handle_indexed_header = fun decoder reader first_byte decode_next ->
  match decode_varint_incremental reader first_byte 7 0 1 with
  | Result.Error Need_more_data ->
      Need_more
  | Result.Error e ->
      Error e
  | Result.Ok (index, _, _) ->
      match Hpack.static_table_lookup index with
      | Some header ->
          let headers = Cell.get decoder.accumulated_headers in
          Cell.set decoder.accumulated_headers (header :: headers);
          decoder.phase <- WaitingForHeader;
          decode_next ()
      | None -> Error (Invalid_header_index index)

let handle_literal_incremental = fun decoder reader first_byte decode_next ->
  match decode_varint_incremental reader first_byte 6 0 1 with
  | Result.Error Need_more_data -> Need_more
  | Result.Error e -> Error e
  | Result.Ok (name_index, _, _) ->
      if name_index = 0 then
        match read_byte reader with
        | None -> Need_more
        | Some len_byte -> (
            match decode_varint_incremental reader len_byte 7 0 1 with
            | Result.Error Need_more_data ->
                Need_more
            | Result.Error e ->
                Error e
            | Result.Ok (name_length, _, _) ->
                decoder.phase <- ReadingLiteralName {
                  name_length;
                  bytes_read = 0;
                  buffer = Buffer.create ~size:name_length
                };
                decode_next ()
          )
      else
        (* Indexed name *)
        match Hpack.static_table_lookup name_index with
        | Some header -> (
            match read_byte reader with
            | None -> Need_more
            | Some len_byte -> (
                match decode_varint_incremental reader len_byte 7 0 1 with
                | Result.Error Need_more_data ->
                    Need_more
                | Result.Error e ->
                    Error e
                | Result.Ok (value_length, _, _) ->
                    decoder.phase <- ReadingLiteralValue {
                      name = header.name;
                      value_length;
                      bytes_read = 0;
                      buffer = Buffer.create ~size:value_length;
                      should_index = true;
                    };
                    decode_next ()
              )
          )
        | None -> Error (Invalid_name_index name_index)

let decode = fun decoder reader ->
  let rec decode_next () =
    match decoder.phase with
    | WaitingForHeader -> (
        match read_byte reader with
        | None -> Need_more
        | Some first_byte ->
            if first_byte land 0x80 != 0 then
              handle_indexed_header decoder reader first_byte decode_next
            else if first_byte land 0x40 != 0 then
              handle_literal_incremental decoder reader first_byte decode_next
            else
              Error Unsupported_encoding
      )
    | ReadingLiteralName { name_length; bytes_read; buffer } ->
        let remaining = name_length - bytes_read in
        (
          match read_n_bytes reader remaining with
          | None -> Need_more
          | Some data ->
              Buffer.add_bytes buffer data;
              let name = Buffer.contents buffer in
              (* Now need to read value *)
              (
                match read_byte reader with
                | None -> Need_more
                | Some len_byte -> (
                    match decode_varint_incremental reader len_byte 7 0 1 with
                    | Result.Error Need_more_data ->
                        Need_more
                    | Result.Error e ->
                        Error e
                    | Result.Ok (value_length, _, _) ->
                        decoder.phase <- ReadingLiteralValue {
                          name;
                          value_length;
                          bytes_read = 0;
                          buffer = Buffer.create ~size:value_length;
                          should_index = true;
                        };
                        decode_next ()
                  )
              )
        )
    | ReadingLiteralValue {
      name;
      value_length;
      bytes_read;
      buffer;
      should_index
    } ->
        let remaining = value_length - bytes_read in
        (
          match read_n_bytes reader remaining with
          | None -> Need_more
          | Some data ->
              Buffer.add_bytes buffer data;
              let value = Buffer.contents buffer in
              let header = { Hpack.name; value } in
              (* TODO: Add to dynamic table if needed *)
              (* if should_index then ... *)
              (* Add to accumulated headers *)
              let headers = Cell.get decoder.accumulated_headers in
              Cell.set decoder.accumulated_headers (header :: headers);
              (* Reset to waiting for next header *)
              decoder.phase <- WaitingForHeader;
              (* Check if we have complete header block *)
              (* For simplicity, return headers when we've decoded at least one *)
              if List.length (Cell.get decoder.accumulated_headers) > 0 then
                let result = List.reverse (Cell.get decoder.accumulated_headers) in
                Cell.set decoder.accumulated_headers [];
                Headers result
              else
                decode_next ()
        )
    | _ ->
        Error Invalid_decoder_state
  in
  decode_next ()
