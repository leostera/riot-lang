open Std
open Std.IO

module Buffer = IO.Buffer
module Cell = Sync.Cell

(** Reentrant HPACK decoder using IO.Reader. *)
type decoder = {
  max_dynamic_table_size: int Cell.t;
  hpack_decoder: Hpack.decoder Cell.t;
  pending_input: string Cell.t;
}

type decode_error =
  | ReadFailed of IO.error
  | HpackDecodeFailed of Hpack.decode_error

type decode_result =
  | Headers of Hpack.header list
  | Need_more
  | Error of decode_error

let create = fun ?(max_dynamic_table_size = 4_096) () -> {
  max_dynamic_table_size = Cell.create max_dynamic_table_size;
  hpack_decoder = Cell.create (Hpack.create_decoder ~max_dynamic_table_size ());
  pending_input = Cell.create "";
}

let decode_error_to_string = function
  | ReadFailed error -> "HPACK reader failed to read input: " ^ IO.error_message error
  | HpackDecodeFailed error -> Hpack.decode_error_to_string error

let update_max_table_size = fun decoder size ->
  match Hpack.update_decoder_max_table_size (Cell.get decoder.hpack_decoder) size with
  | Ok () ->
      Cell.set decoder.max_dynamic_table_size size;
      Ok ()
  | Error error -> Error error

let reset = fun decoder ->
  Cell.set decoder.pending_input "";
  Cell.set
    decoder.hpack_decoder
    (Hpack.create_decoder ~max_dynamic_table_size:(Cell.get decoder.max_dynamic_table_size) ())

let dynamic_table_size = fun decoder ->
  Hpack.decoder_dynamic_table_size
    (Cell.get decoder.hpack_decoder)

let is_incomplete_decode = function
  | Hpack.IncompleteIntegerEncoding
  | Hpack.IncompleteStringEncoding
  | Hpack.StringDataTruncated _ -> true
  | Hpack.UnsupportedHuffmanStringEncoding
  | Hpack.IntegerEncodingOverflow _
  | Hpack.InvalidHeaderIndex _
  | Hpack.InvalidNameIndex _
  | Hpack.DynamicTableSizeUpdateFailed _
  | Hpack.DynamicTableSizeUpdateAfterHeaders -> false

let decode = fun decoder reader ->
  let buffer = Buffer.create ~size:4_096 in
  match IO.Reader.read_to_end reader ~into:buffer with
  | Error error -> Error (ReadFailed error)
  | Ok _ ->
      let input = Cell.get decoder.pending_input ^ Buffer.contents buffer in
      if String.length input = 0 then
        Need_more
      else
        match Hpack.decode (Cell.get decoder.hpack_decoder) (Bytes.from_string input) with
        | Ok headers ->
            Cell.set decoder.pending_input "";
            Headers headers
        | Error error when is_incomplete_decode error ->
            Cell.set decoder.pending_input input;
            Need_more
        | Error error ->
            Cell.set decoder.pending_input "";
            Error (HpackDecodeFailed error)
