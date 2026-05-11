open Std
open Std.IO

(* Use Array from Collections *)

module Array = Collections.Array
module Vector = Collections.Vector

(* Use Cell from Sync *)

module Cell = Sync.Cell

(** HPACK: Header Compression for HTTP/2 (RFC 7541) *)
type header = { name: string; value: string }

type table_size_error =
  | InvalidTableSize of { size: int }

type decode_error =
  | IncompleteIntegerEncoding
  | IntegerEncodingOverflow of { accumulator: int; multiplier: int; value: int }
  | IncompleteStringEncoding
  | StringDataTruncated of { length: int; available: int }
  | UnsupportedHuffmanStringEncoding
  | InvalidHeaderIndex of int
  | InvalidNameIndex of int
  | DynamicTableSizeUpdateFailed of table_size_error
  | DynamicTableSizeUpdateAfterHeaders

type encode_error =
  | HeaderNotIndexed of header

let table_size_error_to_string = fun (InvalidTableSize { size }) ->
  "Invalid HPACK dynamic table size: " ^ Int.to_string size

let decode_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | IncompleteIntegerEncoding -> "Incomplete HPACK integer encoding"
  | IntegerEncodingOverflow { accumulator; multiplier; value } ->
      "HPACK integer encoding overflowed with accumulator "
      ^ Int.to_string accumulator
      ^ ", multiplier "
      ^ Int.to_string multiplier
      ^ ", value "
      ^ Int.to_string value
  | IncompleteStringEncoding -> "Incomplete HPACK string encoding"
  | StringDataTruncated { length; available } ->
      "HPACK string data truncated: expected "
      ^ Int.to_string length
      ^ " bytes, got "
      ^ Int.to_string available
  | UnsupportedHuffmanStringEncoding -> "Unsupported HPACK Huffman string encoding"
  | InvalidHeaderIndex index -> "Invalid HPACK header index: " ^ Int.to_string index
  | InvalidNameIndex index -> "Invalid HPACK name index: " ^ Int.to_string index
  | DynamicTableSizeUpdateFailed error -> table_size_error_to_string error
  | DynamicTableSizeUpdateAfterHeaders -> "HPACK dynamic table size update appeared after a header field"

let encode_error_to_string = fun (HeaderNotIndexed header) ->
  "HPACK header is not present in the static or dynamic table: " ^ header.name ^ ": " ^ header.value

type encoding_type =
  | Indexed
  | LiteralWithIndexing
  | LiteralWithoutIndexing
  | LiteralNeverIndexed

(**
   {1 Static Table}

   RFC 7541 Appendix A: Static Table Definition
   The static table consists of 61 common HTTP header fields.
   Indexes are 1-based (index 0 is invalid).
*)

let static_table = [|
  { name = ":authority"; value = "" };
  { name = ":method"; value = "GET" };
  { name = ":method"; value = "POST" };
  { name = ":path"; value = "/" };
  { name = ":path"; value = "/index.html" };
  { name = ":scheme"; value = "http" };
  { name = ":scheme"; value = "https" };
  { name = ":status"; value = "200" };
  { name = ":status"; value = "204" };
  { name = ":status"; value = "206" };
  { name = ":status"; value = "304" };
  { name = ":status"; value = "400" };
  { name = ":status"; value = "404" };
  { name = ":status"; value = "500" };
  { name = "accept-charset"; value = "" };
  { name = "accept-encoding"; value = "gzip, deflate" };
  { name = "accept-language"; value = "" };
  { name = "accept-ranges"; value = "" };
  { name = "accept"; value = "" };
  { name = "access-control-allow-origin"; value = "" };
  { name = "age"; value = "" };
  { name = "allow"; value = "" };
  { name = "authorization"; value = "" };
  { name = "cache-control"; value = "" };
  { name = "content-disposition"; value = "" };
  { name = "content-encoding"; value = "" };
  { name = "content-language"; value = "" };
  { name = "content-length"; value = "" };
  { name = "content-location"; value = "" };
  { name = "content-range"; value = "" };
  { name = "content-type"; value = "" };
  { name = "cookie"; value = "" };
  { name = "date"; value = "" };
  { name = "etag"; value = "" };
  { name = "expect"; value = "" };
  { name = "expires"; value = "" };
  { name = "from"; value = "" };
  { name = "host"; value = "" };
  { name = "if-match"; value = "" };
  { name = "if-modified-since"; value = "" };
  { name = "if-none-match"; value = "" };
  { name = "if-range"; value = "" };
  { name = "if-unmodified-since"; value = "" };
  { name = "last-modified"; value = "" };
  { name = "link"; value = "" };
  { name = "location"; value = "" };
  { name = "max-forwards"; value = "" };
  { name = "proxy-authenticate"; value = "" };
  { name = "proxy-authorization"; value = "" };
  { name = "range"; value = "" };
  { name = "referer"; value = "" };
  { name = "refresh"; value = "" };
  { name = "retry-after"; value = "" };
  { name = "server"; value = "" };
  { name = "set-cookie"; value = "" };
  { name = "strict-transport-security"; value = "" };
  { name = "transfer-encoding"; value = "" };
  { name = "user-agent"; value = "" };
  { name = "vary"; value = "" };
  { name = "via"; value = "" };
  { name = "www-authenticate"; value = "" };
|]

let static_table_size = Array.length static_table

let static_table_lookup = fun index ->
  if index >= 1 && index <= static_table_size then
    Some (Array.get_unchecked static_table ~at:(index - 1))
  else
    None

let static_table_find = fun ~name ~value ->
  let rec loop i =
    if i >= static_table_size then
      None
    else
      let entry = Array.get_unchecked static_table ~at:i in
      if String.equal entry.name name && String.equal entry.value value then
        Some (i + 1)
      else
        loop (i + 1)
  in
  loop 0

let static_table_find_name = fun name ->
  let rec loop i =
    if i >= static_table_size then
      None
    else if String.equal (Array.get_unchecked static_table ~at:i).name name then
      Some (i + 1)
    else
      loop (i + 1)
  in
  loop 0

(**
   {1 Dynamic Table}

   The dynamic table is a FIFO queue of recently seen headers.
   Entries are evicted when the table exceeds its maximum size.

   Per RFC 7541 Section 4.1:
   - Table size = sum of entry sizes
   - Entry size = length(name) + length(value) + 32 bytes overhead
*)

let header_size = fun header -> String.length header.name + String.length header.value + 32

module DynamicTable = struct
  type t = {
    entries: header list Cell.t;
    current_size: int Cell.t;
    max_size: int Cell.t;
  }

  let create = fun max_size -> {
    entries = Cell.create [];
    current_size = Cell.create 0;
    max_size = Cell.create max_size;
  }

  let size = fun t -> Cell.get t.current_size

  let max_size = fun t -> Cell.get t.max_size

  let entries = fun t -> Cell.get t.entries

  let rec evict_to_fit = fun t new_entry_size ->
    let current = Cell.get t.current_size in
    let max = Cell.get t.max_size in
    if current + new_entry_size <= max then
      ()
    else
      (* Remove oldest entry (last in list) *)
      match List.reverse (Cell.get t.entries) with
      | [] -> ()
      | oldest :: rest ->
          let remaining = List.reverse rest in
          Cell.set t.entries remaining;
          Cell.set t.current_size (current - header_size oldest);
          evict_to_fit t new_entry_size

  let add = fun t header ->
    let entry_size = header_size header in
    (* RFC 7541 Section 4.4: If entry is larger than max size, empty the table *)
    if entry_size > Cell.get t.max_size then (
      Cell.set t.entries [];
      Cell.set t.current_size 0
    ) else (
      evict_to_fit t entry_size;
      let new_entries = header :: Cell.get t.entries in
      Cell.set t.entries new_entries;
      Cell.set t.current_size (Cell.get t.current_size + entry_size)
    )

  let lookup = fun t index ->
    let entries = Cell.get t.entries in
    if index >= 1 && index <= List.length entries then
      Some (List.get_unchecked entries ~at:(index - 1))
    else
      None

  let find = fun t ~name ~value ->
    let entries = Cell.get t.entries in
    let rec loop = fun i ->
      fun __tmp1 ->
        match __tmp1 with
        | [] -> None
        | hdr :: rest ->
            if String.equal hdr.name name && String.equal hdr.value value then
              Some i
            else
              loop (i + 1) rest
    in
    loop 1 entries

  let find_name = fun t name ->
    let entries = Cell.get t.entries in
    let rec loop = fun i ->
      fun __tmp1 ->
        match __tmp1 with
        | [] -> None
        | hdr :: rest ->
            if String.equal hdr.name name then
              Some i
            else
              loop (i + 1) rest
    in
    loop 1 entries

  let update_max_size = fun t new_max ->
    Cell.set t.max_size new_max;
    (* Evict entries if necessary *)
    evict_to_fit t 0
end

(**
   {1 Integer Encoding/Decoding}

   RFC 7541 Section 5.1: Integer representation with variable length encoding.

   Integers are encoded with a prefix of N bits:
   - If value < 2^N - 1: encode directly in N bits
   - Otherwise: encode 2^N - 1 in N bits, then encode (value - (2^N - 1)) as a series of bytes
*)

module Integer = struct
  let encode = fun prefix_bits value ->
    let max_prefix = (1 lsl prefix_bits) - 1 in
    if value < max_prefix then
      let result = Bytes.create ~size:1 in
      Bytes.set_unchecked result ~at:0 ~char:(Char.from_int_unchecked value);
      result
    else
      (* Doesn't fit, use continuation bytes *)
      let buf = Buffer.create ~size:8 in
      Buffer.add_char buf (Char.from_int_unchecked max_prefix);
    let remaining = Cell.create (value - max_prefix) in
    while Cell.get remaining >= 128 do
      let byte = (Cell.get remaining land 0b0111_1111) lor 0b1000_0000 in
      Buffer.add_char buf (Char.from_int_unchecked byte);
      Cell.set remaining (Cell.get remaining lsr 7)
    done;
    Buffer.add_char buf (Char.from_int_unchecked (Cell.get remaining));
    Bytes.from_string (Buffer.contents buf)

  let decode = fun prefix_bits first_byte data offset ->
    let prefix_mask = (1 lsl prefix_bits) - 1 in
    let first_value = Char.to_int first_byte land prefix_mask in
    if first_value < prefix_mask then
      Ok (first_value, offset)
    else
      (* Need to read continuation bytes *)
      let rec read_continuation acc multiplier pos =
        if pos >= Bytes.length data then
          Error IncompleteIntegerEncoding
        else
          let byte =
            Bytes.get_unchecked data ~at:pos
            |> Char.to_int
          in
          let value = byte land 0b0111_1111 in
          if Int.equal value 0 then
            if byte land 0b1000_0000 = 0 then
              Ok (acc, pos + 1)
            else if multiplier > Int.max_int / 128 then
              Error (IntegerEncodingOverflow { accumulator = acc; multiplier; value })
            else
              read_continuation acc (multiplier * 128) (pos + 1)
          else if multiplier > Int.max_int / value then
            Error (IntegerEncodingOverflow { accumulator = acc; multiplier; value })
          else
            let delta = value * multiplier in
            if acc > Int.max_int - delta then
              Error (IntegerEncodingOverflow { accumulator = acc; multiplier; value })
            else
              let acc = acc + delta in
              if byte land 0b1000_0000 = 0 then
                Ok (acc, pos + 1)
              else if multiplier > Int.max_int / 128 then
                Error (IntegerEncodingOverflow { accumulator = acc; multiplier; value })
              else
                read_continuation acc (multiplier * 128) (pos + 1)
      in
      match read_continuation prefix_mask 1 offset with
      | Ok (value, new_offset) -> Ok (value, new_offset)
      | Error e -> Error e
end

(**
   {1 String Encoding/Decoding}

   RFC 7541 Section 5.2: String literal representation.

   Strings can be encoded either:
   - As plain octets (Huffman bit = 0)
   - Using Huffman encoding (Huffman bit = 1)

   Format: [H bit | 7-bit length prefix] [length bytes] [string data]

   Note: Full Huffman implementation deferred to huffman.ml
*)

module String_ = struct
  let encode = fun ?(use_huffman = false) str ->
    let len = String.length str in
    (* TODO: Implement Huffman encoding *)
    (* For now, always use plain encoding *)
    let len_bytes = Integer.encode 7 len in
    let buf = Buffer.create ~size:(Bytes.length len_bytes + len) in
    Buffer.add_bytes buf len_bytes;
    Buffer.add_string buf str;
    Bytes.from_string (Buffer.contents buf)

  let decode = fun data offset ->
    if offset >= Bytes.length data then
      Error IncompleteStringEncoding
    else
      let first_byte = Bytes.get_unchecked data ~at:offset in
      let is_huffman = Char.to_int first_byte land 0b1000_0000 != 0 in
      match Integer.decode 7 first_byte data (offset + 1) with
      | Error e -> Error e
      | Ok (length, new_offset) ->
          if new_offset + length > Bytes.length data then
            Error (StringDataTruncated { length; available = Bytes.length data - new_offset })
          else if is_huffman then
            Error UnsupportedHuffmanStringEncoding
          else
            let str_data = Bytes.sub_unchecked data ~offset:new_offset ~len:length in
            Ok (Bytes.to_string str_data, new_offset + length)
end

(** {1 Encoder} *)

type encoder = {
  dynamic_table: DynamicTable.t;
  sensitive_headers: string list Cell.t;
}

let create_encoder = fun ?(max_dynamic_table_size = 4_096) () -> {
  dynamic_table = DynamicTable.create max_dynamic_table_size;
  sensitive_headers = Cell.create [ "authorization"; "cookie"; "set-cookie" ];
}

let update_encoder_max_table_size = fun encoder new_size ->
  if new_size < 0 then
    Error (InvalidTableSize { size = new_size })
  else (
    DynamicTable.update_max_size encoder.dynamic_table new_size;
    Ok ()
  )

let encoder_dynamic_table_size = fun encoder -> DynamicTable.size encoder.dynamic_table

let encoder_dynamic_table_max_size = fun encoder -> DynamicTable.max_size encoder.dynamic_table

let is_sensitive_header = fun name ->
  let name = String.lowercase_ascii name in
  List.contains [ "authorization"; "cookie"; "set-cookie"; "proxy-authorization"; ] ~value:name

let encode_indexed_header = fun index ->
  (* Indexed Header Field: 1xxxxxxx *)
  let prefix_byte = 0b1000_0000 in
  let index_bytes = Integer.encode 7 index in
  let result = Bytes.create ~size:(Bytes.length index_bytes) in
  Bytes.set_unchecked
    result
    ~at:0
    ~char:(Char.from_int_unchecked
      (prefix_byte lor Char.to_int (Bytes.get_unchecked index_bytes ~at:0)));
  Bytes.blit_unchecked
    index_bytes
    ~src_offset:1
    ~dst:result
    ~dst_offset:1
    ~len:(Bytes.length index_bytes - 1);
  result

let encode_literal_with_indexing = fun ~name_index ~name ~value ->
  (* Literal Header Field with Incremental Indexing: 01xxxxxx *)
  let buf = Buffer.create ~size:64 in
  match name_index with
  | Some index ->
      (* Name is indexed *)
      let prefix_byte = 0b0100_0000 in
      let index_bytes = Integer.encode 6 index in
      Buffer.add_char
        buf
        (Char.from_int_unchecked
          (prefix_byte lor Char.to_int (Bytes.get_unchecked index_bytes ~at:0)));
      Buffer.add_bytes
        buf
        (Bytes.sub_unchecked index_bytes ~offset:1 ~len:(Bytes.length index_bytes - 1));
      Buffer.add_bytes buf (String_.encode value);
      Bytes.from_string (Buffer.contents buf)
  | None ->
      (* Name is not indexed *)
      Buffer.add_char buf '\x40';
      (* Index 0 means literal name *)
      Buffer.add_bytes buf (String_.encode name);
      Buffer.add_bytes buf (String_.encode value);
      Bytes.from_string (Buffer.contents buf)

let encode_literal_without_indexing = fun ~name_index ~name ~value ->
  (* Literal Header Field without Indexing: 0000xxxx *)
  let buf = Buffer.create ~size:64 in
  match name_index with
  | Some index ->
      let index_bytes = Integer.encode 4 index in
      Buffer.add_bytes buf index_bytes;
      Buffer.add_bytes buf (String_.encode value);
      Bytes.from_string (Buffer.contents buf)
  | None ->
      Buffer.add_char buf '\x00';
      Buffer.add_bytes buf (String_.encode name);
      Buffer.add_bytes buf (String_.encode value);
      Bytes.from_string (Buffer.contents buf)

let encode_literal_never_indexed = fun ~name_index ~name ~value ->
  (* Literal Header Field Never Indexed: 0001xxxx *)
  let buf = Buffer.create ~size:64 in
  let prefix_byte = 0b0001_0000 in
  match name_index with
  | Some index ->
      let index_bytes = Integer.encode 4 index in
      Buffer.add_char
        buf
        (Char.from_int_unchecked
          (prefix_byte lor Char.to_int (Bytes.get_unchecked index_bytes ~at:0)));
      Buffer.add_bytes
        buf
        (Bytes.sub_unchecked index_bytes ~offset:1 ~len:(Bytes.length index_bytes - 1));
      Buffer.add_bytes buf (String_.encode value);
      Bytes.from_string (Buffer.contents buf)
  | None ->
      Buffer.add_char buf (Char.from_int_unchecked prefix_byte);
      Buffer.add_bytes buf (String_.encode name);
      Buffer.add_bytes buf (String_.encode value);
      Bytes.from_string (Buffer.contents buf)

let header_name_index = fun encoder name ->
  let static_name_match = static_table_find_name name in
  let dynamic_name_match = DynamicTable.find_name encoder.dynamic_table name in
  match (static_name_match, dynamic_name_match) with
  | (Some i, _) -> Some i
  | (None, Some i) -> Some (static_table_size + i)
  | (None, None) -> None

let encode_header = fun encoder header ~encoding_type ->
  let header = { header with name = String.lowercase_ascii header.name } in
  let { name; value } = header in
  let name_index = header_name_index encoder name in
  match encoding_type with
  | LiteralNeverIndexed -> Ok (encode_literal_never_indexed ~name_index ~name ~value)
  | LiteralWithoutIndexing -> Ok (encode_literal_without_indexing ~name_index ~name ~value)
  | Indexed
  | LiteralWithIndexing ->
      let static_match = static_table_find ~name ~value in
      let dynamic_match = DynamicTable.find encoder.dynamic_table ~name ~value in
      (
        match (static_match, dynamic_match) with
        | (Some index, _)
        | (_, Some index) ->
            let actual_index =
              match static_match with
              | Some i -> i
              | None -> static_table_size + Option.unwrap dynamic_match
            in
            Ok (encode_indexed_header actual_index)
        | (None, None) -> (
            match encoding_type with
            | Indexed -> Error (HeaderNotIndexed header)
            | LiteralWithIndexing ->
                let result = encode_literal_with_indexing ~name_index ~name ~value in
                DynamicTable.add encoder.dynamic_table header;
                Ok result
            | LiteralWithoutIndexing
            | LiteralNeverIndexed -> Ok (encode_literal_never_indexed ~name_index ~name ~value)
          )
      )

let encode = fun encoder ?(sensitive_headers = []) () ~headers ->
  let buf = Buffer.create ~size:256 in
  let sensitive_headers = List.map sensitive_headers ~fn:String.lowercase_ascii in
  let rec encode_all = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (Bytes.from_string (Buffer.contents buf))
    | header :: rest ->
        let encoding_type =
          if let name = String.lowercase_ascii header.name in
          is_sensitive_header name || List.contains sensitive_headers ~value:name then
            LiteralNeverIndexed
          else
            LiteralWithIndexing
        in
        match encode_header encoder header ~encoding_type with
        | Error error -> Error error
        | Ok encoded ->
            Buffer.add_bytes buf encoded;
            encode_all rest
  in
  encode_all headers

(** {1 Decoder} *)

type decoder = {
  dynamic_table: DynamicTable.t;
}

let create_decoder = fun ?(max_dynamic_table_size = 4_096) () -> {
  dynamic_table = DynamicTable.create max_dynamic_table_size;
}

let update_decoder_max_table_size = fun decoder new_size ->
  if new_size < 0 then
    Error (InvalidTableSize { size = new_size })
  else (
    DynamicTable.update_max_size decoder.dynamic_table new_size;
    Ok ()
  )

let update_max_table_size = update_decoder_max_table_size

let decoder_dynamic_table_size = fun decoder -> DynamicTable.size decoder.dynamic_table

let decoder_dynamic_table_max_size = fun decoder -> DynamicTable.max_size decoder.dynamic_table

let lookup_header = fun decoder index ->
  if index <= static_table_size then
    static_table_lookup index
  else
    DynamicTable.lookup decoder.dynamic_table (index - static_table_size)

let decode_header_block = fun decoder ~allow_table_size_update data offset ->
  if offset >= Bytes.length data then
    Ok ([], offset)
  else
    let first_byte = Bytes.get_unchecked data ~at:offset in
    let first_code = Char.to_int first_byte in
    if first_code land 0b1000_0000 != 0 then
      match Integer.decode 7 first_byte data (offset + 1) with
      | Error e -> Error e
      | Ok (index, new_offset) -> (
          match lookup_header decoder index with
          | None -> Error (InvalidHeaderIndex index)
          | Some header -> Ok ([ header ], new_offset)
        )
    else if first_code land 0b0100_0000 != 0 then
      match Integer.decode 6 first_byte data (offset + 1) with
      | Error e -> Error e
      | Ok (name_index, pos1) -> (
          match (
            if name_index = 0 then
              String_.decode data pos1
            else
              match lookup_header decoder name_index with
              | None -> Error (InvalidNameIndex name_index)
              | Some h -> Ok (h.name, pos1)
          ) with
          | Error e -> Error e
          | Ok (name, pos2) ->
              match String_.decode data pos2 with
              | Error e -> Error e
              | Ok (value, new_offset) ->
                  let header = { name; value } in
                  DynamicTable.add decoder.dynamic_table header;
                  Ok ([ header ], new_offset)
        )
    else if first_code land 0b0010_0000 != 0 then
      if not allow_table_size_update then
        Error DynamicTableSizeUpdateAfterHeaders
      else
        match Integer.decode 5 first_byte data (offset + 1) with
        | Error e -> Error e
        | Ok (new_size, new_offset) ->
            update_decoder_max_table_size decoder new_size
            |> Result.map_err ~fn:(fun error -> DynamicTableSizeUpdateFailed error)
            |> Result.map ~fn:(fun () -> ([], new_offset))
    else if first_code land 0b0001_0000 != 0 then
      match Integer.decode 4 first_byte data (offset + 1) with
      | Error e -> Error e
      | Ok (name_index, pos1) -> (
          match (
            if name_index = 0 then
              String_.decode data pos1
            else
              match lookup_header decoder name_index with
              | None -> Error (InvalidNameIndex name_index)
              | Some h -> Ok (h.name, pos1)
          ) with
          | Error e -> Error e
          | Ok (name, pos2) ->
              match String_.decode data pos2 with
              | Error e -> Error e
              | Ok (value, new_offset) -> Ok ([ { name; value } ], new_offset)
        )
    else
      (* Literal without Indexing: 0000xxxx *)
      match Integer.decode 4 first_byte data (offset + 1) with
      | Error e -> Error e
      | Ok (name_index, pos1) -> (
          match (
            if name_index = 0 then
              String_.decode data pos1
            else
              match lookup_header decoder name_index with
              | None -> Error (InvalidNameIndex name_index)
              | Some h -> Ok (h.name, pos1)
          ) with
          | Error e -> Error e
          | Ok (name, pos2) ->
              match String_.decode data pos2 with
              | Error e -> Error e
              | Ok (value, new_offset) -> Ok ([ { name; value } ], new_offset)
        )

let decode = fun decoder data ->
  let headers = Vector.with_capacity ~size:8 in
  let rec decode_all allow_table_size_update offset =
    if offset >= Bytes.length data then
      Ok (
        Vector.to_array headers
        |> Array.to_list
      )
    else
      match decode_header_block decoder ~allow_table_size_update data offset with
      | Error e -> Error e
      | Ok (decoded, new_offset) ->
          decoded
          |> List.for_each ~fn:(fun header -> Vector.push headers ~value:header);
          let allow_table_size_update = allow_table_size_update && decoded = [] in
          decode_all allow_table_size_update new_offset
  in
  decode_all true 0
