open Std
open Std.IO

module Buffer = IO.Buffer
module Cell = Sync.Cell

(** Reentrant HPACK decoder using IO.Reader. *)
type decoder = {
  hpack_decoder: Hpack.decoder;
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
  hpack_decoder = Hpack.create_decoder ~max_dynamic_table_size ();
  pending_input = Cell.create "";
}

let decode_error_to_string = function
  | ReadFailed error -> "HPACK reader failed to read input: " ^ IO.error_message error
  | HpackDecodeFailed error -> Hpack.decode_error_to_string error

let update_max_table_size = fun decoder size ->
  Hpack.update_decoder_max_table_size decoder.hpack_decoder size

let reset = fun decoder -> Cell.set decoder.pending_input ""

let dynamic_table_size = fun _decoder ->
  (* Access internal dynamic table size - simplified for now. *)
  4_096

let is_incomplete_decode = function
  | Hpack.IncompleteIntegerEncoding
  | Hpack.IncompleteStringEncoding
  | Hpack.StringDataTruncated _ -> true
  | Hpack.UnsupportedHuffmanStringEncoding
  | Hpack.InvalidHeaderIndex _
  | Hpack.InvalidNameIndex _
  | Hpack.DynamicTableSizeUpdateFailed _ -> false

let decode = fun decoder reader ->
  let buffer = Buffer.create ~size:4_096 in
  match IO.Reader.read_to_end reader ~into:buffer with
  | Error error -> Error (ReadFailed error)
  | Ok _ ->
      let input = Cell.get decoder.pending_input ^ Buffer.contents buffer in
      if String.length input = 0 then
        Need_more
      else
        match Hpack.decode decoder.hpack_decoder (Bytes.from_string input) with
        | Ok headers ->
            Cell.set decoder.pending_input "";
            Headers headers
        | Error error when is_incomplete_decode error ->
            Cell.set decoder.pending_input input;
            Need_more
        | Error error ->
            Cell.set decoder.pending_input "";
            Error (HpackDecodeFailed error)
