open Std
open Std.Result.Syntax

type error = [`Msg of string | `Io_error of IO.error]

module Error = struct
  type t = error

  let to_string = function
    | `Msg message -> message
    | `Io_error err -> IO.error_message err
end

type physical_type =
  | Boolean
  | Int32
  | Int64
  | Int96
  | Float
  | Double
  | Byte_array
  | Fixed_len_byte_array
  | Unknown_physical_type of int

type converted_type =
  | Utf8
  | Map
  | Map_key_value
  | List
  | Enum
  | Decimal
  | Date
  | Time_millis
  | Time_micros
  | Timestamp_millis
  | Timestamp_micros
  | UInt_8
  | UInt_16
  | UInt_32
  | UInt_64
  | Int_8
  | Int_16
  | Int_32
  | Int_64
  | Json
  | Bson
  | Interval
  | Unknown_converted_type of int

type field_repetition_type =
  | Required
  | Optional
  | Repeated
  | Unknown_repetition_type of int

type encoding =
  | Plain
  | Plain_dictionary
  | Rle
  | Bit_packed
  | Delta_binary_packed
  | Delta_length_byte_array
  | Delta_byte_array
  | Rle_dictionary
  | Byte_stream_split
  | Unknown_encoding of int

type compression_codec =
  | Uncompressed
  | Snappy
  | Gzip
  | Lzo
  | Brotli
  | Lz4
  | Zstd
  | Lz4_raw
  | Unknown_compression_codec of int

type page_type =
  | Data_page
  | Index_page
  | Dictionary_page
  | Data_page_v2
  | Unknown_page_type of int

type column_order =
  | Type_defined_order

type key_value = {
  key: string;
  value: string option;
}

type schema_element = {
  type_: physical_type option;
  type_length: int option;
  repetition_type: field_repetition_type option;
  name: string;
  num_children: int option;
  converted_type: converted_type option;
  scale: int option;
  precision: int option;
  field_id: int option;
}

type sorting_column = { column_idx: int; descending: bool; nulls_first: bool }

type page_encoding_stats = { page_type: page_type; encoding: encoding; count: int }

type column_metadata = {
  type_: physical_type;
  encodings: encoding list;
  path_in_schema: string list;
  codec: compression_codec;
  num_values: int64;
  total_uncompressed_size: int64;
  total_compressed_size: int64;
  key_value_metadata: key_value list option;
  data_page_offset: int64;
  index_page_offset: int64 option;
  dictionary_page_offset: int64 option;
  encoding_stats: page_encoding_stats list option;
  bloom_filter_offset: int64 option;
  bloom_filter_length: int option;
}

type column_chunk = {
  file_path: string option;
  file_offset: int64;
  meta_data: column_metadata option;
  offset_index_offset: int64 option;
  offset_index_length: int option;
  column_index_offset: int64 option;
  column_index_length: int option;
  encrypted_column_metadata: string option;
}

type row_group = {
  columns: column_chunk list;
  total_byte_size: int64;
  num_rows: int64;
  sorting_columns: sorting_column list option;
  file_offset: int64 option;
  total_compressed_size: int64 option;
  ordinal: int option;
}

type file_metadata = {
  version: int;
  schema: schema_element list;
  num_rows: int64;
  row_groups: row_group list;
  key_value_metadata: key_value list option;
  created_by: string option;
  column_orders: column_order list option;
}

type footer = { metadata_length: int; encrypted_footer: bool }

type t = { body: string; metadata: file_metadata }

let fail = fun message -> Error (`Msg message)

let io_error = fun err -> Error (`Io_error err)

let magic = "PAR1"

let encrypted_magic = "PARE"

let footer_size = 8

let int64_fits_int = fun value ->
  (
    match Int64.compare value (Int64.of_int Int.min_int) with
    | Order.LT -> false
    | Order.EQ
    | Order.GT -> true
  ) && (
    match Int64.compare value (Int64.of_int Int.max_int) with
    | Order.LT
    | Order.EQ -> true
    | Order.GT -> false
  )

let int_of_int64 = fun kind value ->
  if int64_fits_int value then
    Ok (Int64.to_int value)
  else
    fail ("parquet " ^ kind ^ " exceeds the OCaml int range")

let ensure_i16 = fun kind value ->
  if value < (-0x8000) || value > 0x7fff then
    fail ("parquet " ^ kind ^ " is outside the i16 range")
  else
    Ok ()

let ensure_i32 = fun kind value ->
  if value < (-0x8000_0000) || value > 0x7fff_ffff then
    fail ("parquet " ^ kind ^ " is outside the i32 range")
  else
    Ok ()

let ensure_u32 = fun kind value ->
  if value < 0 then
    fail ("parquet " ^ kind ^ " is negative")
  else if (
    match Int64.compare (Int64.of_int value) 0xffff_ffffL with
    | Order.GT -> true
    | Order.LT
    | Order.EQ -> false
  ) then
    fail ("parquet " ^ kind ^ " exceeds the u32 range")
  else
    Ok ()

let string_segment_equals = fun value ~offset segment ->
  let segment_length = String.length segment in
  offset >= 0
  && offset + segment_length <= String.length value
  && String.equal (String.sub value ~offset ~len:segment_length) segment

let get_byte = fun value at -> Char.code (String.get_unchecked value ~at)

let decode_u32_le = fun value ~offset ->
  get_byte value offset
  lor (get_byte value (offset + 1) lsl 8)
  lor (get_byte value (offset + 2) lsl 16)
  lor (get_byte value (offset + 3) lsl 24)

let add_u32_le = fun buffer value ->
  IO.Buffer.add_char buffer (Char.chr (value land 0xff));
  IO.Buffer.add_char buffer (Char.chr ((value lsr 8) land 0xff));
  IO.Buffer.add_char buffer (Char.chr ((value lsr 16) land 0xff));
  IO.Buffer.add_char buffer (Char.chr ((value lsr 24) land 0xff))

module Thrift = struct
  type field_type =
    | Stop
    | Boolean_true
    | Boolean_false
    | Byte
    | I16
    | I32
    | I64
    | Double
    | Binary
    | List
    | Set
    | Map
    | Struct

  type element_type =
    | Bool
    | Byte_element
    | I16_element
    | I32_element
    | I64_element
    | Double_element
    | Binary_element
    | List_element
    | Set_element
    | Map_element
    | Struct_element

  type input = {
    source: string;
    length: int;
    mutable pos: int;
  }

  type field_header = {
    id: int;
    field_type: field_type;
    bool_value: bool option;
  }

  type list_header = { element_type: element_type; size: int }

  let input_of_string = fun source -> { source; length = String.length source; pos = 0 }

  let at = fun input message ->
    fail
      ("parquet compact thrift " ^ message ^ " at byte " ^ Int.to_string input.pos)

  let ensure = fun input needed kind ->
    if input.pos + needed > input.length then
      at input ("unexpected end of input while reading " ^ kind)
    else
      Ok ()

  let read_byte = fun input ->
    let* () = ensure input 1 "byte" in
    let value = get_byte input.source input.pos in
    input.pos <- input.pos + 1;
    Ok value

  let read_string = fun input len kind ->
    let* () = ensure input len kind in
    let value = String.sub input.source ~offset:input.pos ~len in
    input.pos <- input.pos + len;
    Ok value

  let int_of_nonnegative_int64 = fun input kind value ->
    if (
      match Int64.compare value 0L with
      | Order.LT -> true
      | Order.EQ
      | Order.GT -> false
    ) then
      at input (kind ^ " is negative")
    else
      int_of_int64 kind value

  let decode_zig_zag = fun value ->
    Int64.logxor
      (Int64.shift_right_logical value 1)
      (Int64.neg (Int64.logand value 1L))

  let rec read_vlq = fun input ->
    let rec loop shift acc =
      let* byte = read_byte input in
      let acc = Int64.logor acc (Int64.shift_left (Int64.of_int (byte land 0x7f)) shift) in
      if Int.equal (byte land 0x80) 0 then
        Ok acc
      else if shift >= 63 then
        at input "encountered an oversized varint"
      else
        loop (shift + 7) acc
    in
    loop 0 0L

  let read_zig_zag = fun input -> let* value = read_vlq input in Ok (decode_zig_zag value)

  let read_i16 = fun input -> let* value = read_zig_zag input in let* value =
    int_of_int64 "i16" value in let* () = ensure_i16 "i16" value in Ok value

  let read_i32 = fun input -> let* value = read_zig_zag input in let* value =
    int_of_int64 "i32" value in let* () = ensure_i32 "i32" value in Ok value

  let read_i64 = fun input -> read_zig_zag input

  let read_double = fun input ->
    let* bytes = read_string input 8 "double" in
    let open Int64 in
    let byte index =
      let shift = Std.Int.(index * 8) in
      shift_left (of_int (get_byte bytes index)) shift
    in
    Ok (float_of_bits
      (logor
        (byte 0)
        (logor
          (byte 1)
          (logor
            (byte 2)
            (logor (byte 3) (logor (byte 4) (logor (byte 5) (logor (byte 6) (byte 7)))))))))

  let read_binary = fun input -> let* length = read_vlq input in let* length =
    int_of_nonnegative_int64 input "binary length" length in
  read_string input length "binary payload"

  let read_bool = fun input ->
    let* value = read_byte input in
    match value with
    | 0x01 -> Ok true
    | 0x00
    | 0x02 -> Ok false
    | _ -> at input ("encountered an invalid bool byte " ^ Int.to_string value)

  let field_type_of_code = fun code ->
    match code with
    | 0 -> Ok Stop
    | 1 -> Ok Boolean_true
    | 2 -> Ok Boolean_false
    | 3 -> Ok Byte
    | 4 -> Ok I16
    | 5 -> Ok I32
    | 6 -> Ok I64
    | 7 -> Ok Double
    | 8 -> Ok Binary
    | 9 -> Ok List
    | 10 -> Ok Set
    | 11 -> Ok Map
    | 12 -> Ok Struct
    | _ -> fail ("parquet compact thrift encountered unknown field type " ^ Int.to_string code)

  let element_type_of_code = fun code ->
    match code with
    | 1
    | 2 -> Ok Bool
    | 3 -> Ok Byte_element
    | 4 -> Ok I16_element
    | 5 -> Ok I32_element
    | 6 -> Ok I64_element
    | 7 -> Ok Double_element
    | 8 -> Ok Binary_element
    | 9 -> Ok List_element
    | 10 -> Ok Set_element
    | 11 -> Ok Map_element
    | 12 -> Ok Struct_element
    | _ ->
        fail ("parquet compact thrift encountered unknown list element type " ^ Int.to_string code)

  let field_type_of_element_type = fun value ->
    match value with
    | Bool -> Boolean_true
    | Byte_element -> Byte
    | I16_element -> I16
    | I32_element -> I32
    | I64_element -> I64
    | Double_element -> Double
    | Binary_element -> Binary
    | List_element -> List
    | Set_element -> Set
    | Map_element -> Map
    | Struct_element -> Struct

  let string_of_element_type = fun value ->
    match value with
    | Bool -> "bool"
    | Byte_element -> "byte"
    | I16_element -> "i16"
    | I32_element -> "i32"
    | I64_element -> "i64"
    | Double_element -> "double"
    | Binary_element -> "binary"
    | List_element -> "list"
    | Set_element -> "set"
    | Map_element -> "map"
    | Struct_element -> "struct"

  let read_list_begin = fun input ->
    let* header = read_byte input in
    if Int.equal header 0 then
      Ok { element_type = Byte_element; size = 0 }
    else
      let* element_type = element_type_of_code (header land 0x0f) in
      let size_hint = header lsr 4 in
      let* size =
        if Int.equal size_hint 15 then
          let* length = read_vlq input in int_of_nonnegative_int64 input "list length" length
        else
          Ok size_hint
      in
      Ok { element_type; size }

  let read_field_begin = fun input last_field_id ->
    let* header = read_byte input in
    let field_delta = (header land 0xf0) lsr 4 in
    let* field_type = field_type_of_code (header land 0x0f) in
    match field_type with
    | Stop -> Ok { id = 0; field_type = Stop; bool_value = None }
    | _ ->
        let* field_id =
          if Int.equal field_delta 0 then
            read_i16 input
          else
            Ok (last_field_id + field_delta)
        in
        let bool_value =
          match field_type with
          | Boolean_true -> Some true
          | Boolean_false -> Some false
          | _ -> None
        in
        Ok { id = field_id; field_type; bool_value }

  let rec skip = fun input field_type ->
    let rec skip_with_depth depth field_type =
      if Int.equal depth 0 then
        at input "exceeded the skip recursion limit"
      else
        match field_type with
        | Stop
        | Boolean_true
        | Boolean_false -> Ok ()
        | Byte -> let* _ = read_byte input in Ok ()
        | I16
        | I32
        | I64 -> let* _ = read_vlq input in Ok ()
        | Double -> let* _ = read_string input 8 "double" in Ok ()
        | Binary -> let* _ = read_binary input in Ok ()
        | Struct ->
            let rec loop last_field_id =
              let* field = read_field_begin input last_field_id in
              match field.field_type with
              | Stop -> Ok ()
              | _ -> let* () = skip_with_depth (depth - 1) field.field_type in loop field.id
            in
            loop 0
        | List
        | Set ->
            let* header = read_list_begin input in
            let element_field_type = field_type_of_element_type header.element_type in
            let rec loop remaining =
              if Int.equal remaining 0 then
                Ok ()
              else
                let* () = skip_with_depth (depth - 1) element_field_type in loop (remaining - 1)
            in
            loop header.size
        | Map -> at input "encountered a thrift map, which Parquet metadata does not use"
    in
    skip_with_depth 64 field_type

  let write_byte = fun buffer value ->
    IO.Buffer.add_char buffer (Char.chr value);
    Ok ()

  let rec write_vlq = fun buffer value ->
    let byte = Int64.to_int (Int64.logand value 0x7fL) in
    let rest = Int64.shift_right_logical value 7 in
    if Int64.equal rest 0L then
      write_byte buffer byte
    else
      let* () = write_byte buffer (byte lor 0x80) in write_vlq buffer rest

  let write_zig_zag = fun buffer value ->
    write_vlq
      buffer
      (Int64.logxor (Int64.shift_left value 1) (Int64.shift_right value 63))

  let write_i16 = fun buffer value -> let* () = ensure_i16 "i16" value in
  write_zig_zag buffer (Int64.of_int value)

  let write_i32 = fun buffer value -> let* () = ensure_i32 "i32" value in
  write_zig_zag buffer (Int64.of_int value)

  let write_i64 = fun buffer value -> write_zig_zag buffer value

  let write_double = fun buffer value ->
    let bits = Int64.bits_of_float value in
    let open Int64 in
    let add shift = write_byte buffer (to_int (logand (shift_right_logical bits shift) 0xffL)) in
    let* () = add 0 in let* () = add 8 in let* () = add 16 in let* () = add 24 in let* () = add 32 in let* () =
      add 40 in let* () = add 48 in add 56

  let write_binary = fun buffer value ->
    let* () = write_vlq buffer (Int64.of_int (String.length value)) in
    IO.Buffer.add_string buffer value;
    Ok ()

  let code_of_field_type = fun value ->
    match value with
    | Stop -> 0
    | Boolean_true -> 1
    | Boolean_false -> 2
    | Byte -> 3
    | I16 -> 4
    | I32 -> 5
    | I64 -> 6
    | Double -> 7
    | Binary -> 8
    | List -> 9
    | Set -> 10
    | Map -> 11
    | Struct -> 12

  let code_of_element_type = fun value ->
    match value with
    | Bool -> 2
    | Byte_element -> 3
    | I16_element -> 4
    | I32_element -> 5
    | I64_element -> 6
    | Double_element -> 7
    | Binary_element -> 8
    | List_element -> 9
    | Set_element -> 10
    | Map_element -> 11
    | Struct_element -> 12

  let write_field_begin = fun buffer field_type field_id last_field_id ->
    let delta = field_id - last_field_id in
    if delta > 0 && delta <= 0x0f then
      let* () = write_byte buffer ((delta lsl 4) lor code_of_field_type field_type) in Ok field_id
    else
      let* () = write_byte buffer (code_of_field_type field_type) in let* () =
        write_i16 buffer field_id in Ok field_id

  let write_bool_field_begin = fun buffer field_id last_field_id value ->
    write_field_begin
      buffer
      (
        if value then
          Boolean_true
        else
          Boolean_false
      )
      field_id
      last_field_id

  let write_list_begin = fun buffer element_type length ->
    if length < 15 then
      write_byte buffer ((length lsl 4) lor code_of_element_type element_type)
    else
      let* () = write_byte buffer (0xf0 lor code_of_element_type element_type) in
      write_vlq buffer (Int64.of_int length)

  let write_struct_end = fun buffer -> write_byte buffer 0
end

let physical_type_of_int = fun value ->
  match value with
  | 0 -> Boolean
  | 1 -> Int32
  | 2 -> Int64
  | 3 -> Int96
  | 4 -> Float
  | 5 -> Double
  | 6 -> Byte_array
  | 7 -> Fixed_len_byte_array
  | _ -> Unknown_physical_type value

let int_of_physical_type = fun value ->
  match value with
  | Boolean -> 0
  | Int32 -> 1
  | Int64 -> 2
  | Int96 -> 3
  | Float -> 4
  | Double -> 5
  | Byte_array -> 6
  | Fixed_len_byte_array -> 7
  | Unknown_physical_type value -> value

let converted_type_of_int = fun value ->
  match value with
  | 0 -> Utf8
  | 1 -> Map
  | 2 -> Map_key_value
  | 3 -> List
  | 4 -> Enum
  | 5 -> Decimal
  | 6 -> Date
  | 7 -> Time_millis
  | 8 -> Time_micros
  | 9 -> Timestamp_millis
  | 10 -> Timestamp_micros
  | 11 -> UInt_8
  | 12 -> UInt_16
  | 13 -> UInt_32
  | 14 -> UInt_64
  | 15 -> Int_8
  | 16 -> Int_16
  | 17 -> Int_32
  | 18 -> Int_64
  | 19 -> Json
  | 20 -> Bson
  | 21 -> Interval
  | _ -> Unknown_converted_type value

let int_of_converted_type = fun value ->
  match value with
  | Utf8 -> 0
  | Map -> 1
  | Map_key_value -> 2
  | List -> 3
  | Enum -> 4
  | Decimal -> 5
  | Date -> 6
  | Time_millis -> 7
  | Time_micros -> 8
  | Timestamp_millis -> 9
  | Timestamp_micros -> 10
  | UInt_8 -> 11
  | UInt_16 -> 12
  | UInt_32 -> 13
  | UInt_64 -> 14
  | Int_8 -> 15
  | Int_16 -> 16
  | Int_32 -> 17
  | Int_64 -> 18
  | Json -> 19
  | Bson -> 20
  | Interval -> 21
  | Unknown_converted_type value -> value

let field_repetition_type_of_int = fun value ->
  match value with
  | 0 -> Required
  | 1 -> Optional
  | 2 -> Repeated
  | _ -> Unknown_repetition_type value

let int_of_field_repetition_type = fun value ->
  match value with
  | Required -> 0
  | Optional -> 1
  | Repeated -> 2
  | Unknown_repetition_type value -> value

let encoding_of_int = fun value ->
  match value with
  | 0 -> Plain
  | 2 -> Plain_dictionary
  | 3 -> Rle
  | 4 -> Bit_packed
  | 5 -> Delta_binary_packed
  | 6 -> Delta_length_byte_array
  | 7 -> Delta_byte_array
  | 8 -> Rle_dictionary
  | 9 -> Byte_stream_split
  | _ -> Unknown_encoding value

let int_of_encoding = fun value ->
  match value with
  | Plain -> 0
  | Plain_dictionary -> 2
  | Rle -> 3
  | Bit_packed -> 4
  | Delta_binary_packed -> 5
  | Delta_length_byte_array -> 6
  | Delta_byte_array -> 7
  | Rle_dictionary -> 8
  | Byte_stream_split -> 9
  | Unknown_encoding value -> value

let compression_codec_of_int = fun value ->
  match value with
  | 0 -> Uncompressed
  | 1 -> Snappy
  | 2 -> Gzip
  | 3 -> Lzo
  | 4 -> Brotli
  | 5 -> Lz4
  | 6 -> Zstd
  | 7 -> Lz4_raw
  | _ -> Unknown_compression_codec value

let int_of_compression_codec = fun value ->
  match value with
  | Uncompressed -> 0
  | Snappy -> 1
  | Gzip -> 2
  | Lzo -> 3
  | Brotli -> 4
  | Lz4 -> 5
  | Zstd -> 6
  | Lz4_raw -> 7
  | Unknown_compression_codec value -> value

let page_type_of_int = fun value ->
  match value with
  | 0 -> Data_page
  | 1 -> Index_page
  | 2 -> Dictionary_page
  | 3 -> Data_page_v2
  | _ -> Unknown_page_type value

let int_of_page_type = fun value ->
  match value with
  | Data_page -> 0
  | Index_page -> 1
  | Dictionary_page -> 2
  | Data_page_v2 -> 3
  | Unknown_page_type value -> value

let require_field = fun name value ->
  match value with
  | Some value -> Ok value
  | None -> fail ("parquet metadata is missing required field " ^ name)

let expect_field_type = fun field_type expected name ->
  if field_type = expected then
    Ok ()
  else
    fail ("parquet metadata field " ^ name ^ " has the wrong thrift type")

let expect_bool_field = fun field name ->
  match field.Thrift.bool_value with
  | Some _ -> Ok ()
  | None -> fail ("parquet metadata field " ^ name ^ " has the wrong thrift type")

let decode_list = fun input ~name ~element_type decode_element ->
  let* header = Thrift.read_list_begin input in
  let* () =
    if Int.equal header.Thrift.size 0 || header.Thrift.element_type = element_type then
      Ok ()
    else
      fail
        ("parquet metadata list "
        ^ name
        ^ " expected "
        ^ Thrift.string_of_element_type element_type
        ^ " elements")
  in
  let rec loop remaining acc =
    if Int.equal remaining 0 then
      Ok (List.rev acc)
    else
      let* value = decode_element input in loop (remaining - 1) (value :: acc)
  in
  loop header.size []

let write_i32_field = fun buffer field_id last_field_id value -> let* last_field_id =
  Thrift.write_field_begin buffer Thrift.I32 field_id last_field_id in let* () =
  Thrift.write_i32 buffer value in Ok last_field_id

let write_i16_field = fun buffer field_id last_field_id value -> let* last_field_id =
  Thrift.write_field_begin buffer Thrift.I16 field_id last_field_id in let* () =
  Thrift.write_i16 buffer value in Ok last_field_id

let write_i64_field = fun buffer field_id last_field_id value -> let* last_field_id =
  Thrift.write_field_begin buffer Thrift.I64 field_id last_field_id in let* () =
  Thrift.write_i64 buffer value in Ok last_field_id

let write_string_field = fun buffer field_id last_field_id value -> let* last_field_id =
  Thrift.write_field_begin buffer Thrift.Binary field_id last_field_id in let* () =
  Thrift.write_binary buffer value in Ok last_field_id

let write_bool_field = fun buffer field_id last_field_id value ->
  Thrift.write_bool_field_begin
    buffer
    field_id
    last_field_id
    value

let write_struct_field = fun buffer field_id last_field_id encode value -> let* last_field_id =
  Thrift.write_field_begin buffer Thrift.Struct field_id last_field_id in let* () =
  encode buffer value in Ok last_field_id

let write_list_field = fun buffer field_id last_field_id element_type encode values ->
  let* last_field_id = Thrift.write_field_begin buffer Thrift.List field_id last_field_id in
  let* () = Thrift.write_list_begin buffer element_type (List.length values) in
  let rec loop values =
    match values with
    | [] -> Ok ()
    | value :: rest -> let* () = encode buffer value in loop rest
  in
  let* () = loop values in Ok last_field_id

let rec decode_key_value = fun input ->
  let key = ref None in
  let value = ref None in
  let rec loop last_field_id =
    let* field = Thrift.read_field_begin input last_field_id in
    match field.field_type with
    | Thrift.Stop -> let* key = require_field "KeyValue.key" !key in Ok { key; value = !value }
    | _ ->
        let* () =
          match field.id with
          | 1 ->
              let* () = expect_field_type field.field_type Thrift.Binary "KeyValue.key" in
              let* read = Thrift.read_binary input in
              key := Some read;
              Ok ()
          | 2 ->
              let* () = expect_field_type field.field_type Thrift.Binary "KeyValue.value" in
              let* read = Thrift.read_binary input in
              value := Some read;
              Ok ()
          | _ -> Thrift.skip input field.field_type
        in
        loop field.id
  in
  loop 0

and encode_key_value = fun buffer (value: key_value) ->
  let* last_field_id = write_string_field buffer 1 0 value.key in
  let* _last_field_id =
    match value.value with
    | None -> Ok last_field_id
    | Some field_value -> write_string_field buffer 2 last_field_id field_value
  in
  Thrift.write_struct_end buffer

and decode_sorting_column = fun input ->
  let column_idx = ref None in
  let descending = ref None in
  let nulls_first = ref None in
  let rec loop last_field_id =
    let* field = Thrift.read_field_begin input last_field_id in
    match field.field_type with
    | Thrift.Stop ->
        let* column_idx = require_field "SortingColumn.column_idx" !column_idx in let* descending =
          require_field "SortingColumn.descending" !descending in let* nulls_first =
          require_field "SortingColumn.nulls_first" !nulls_first in
        Ok { column_idx; descending; nulls_first }
    | _ ->
        let* () =
          match field.id with
          | 1 ->
              let* () = expect_field_type field.field_type Thrift.I32 "SortingColumn.column_idx" in
              let* read = Thrift.read_i32 input in
              column_idx := Some read;
              Ok ()
          | 2 ->
              let* () = expect_bool_field field "SortingColumn.descending" in
              descending := field.bool_value;
              Ok ()
          | 3 ->
              let* () = expect_bool_field field "SortingColumn.nulls_first" in
              nulls_first := field.bool_value;
              Ok ()
          | _ -> Thrift.skip input field.field_type
        in
        loop field.id
  in
  loop 0

and encode_sorting_column = fun buffer (value: sorting_column) -> let* last_field_id =
  write_i32_field buffer 1 0 value.column_idx in let* last_field_id =
  write_bool_field buffer 2 last_field_id value.descending in let* _last_field_id =
  write_bool_field buffer 3 last_field_id value.nulls_first in Thrift.write_struct_end buffer

and decode_page_encoding_stats = fun input ->
  let page_type = ref None in
  let encoding = ref None in
  let count = ref None in
  let rec loop last_field_id =
    let* field = Thrift.read_field_begin input last_field_id in
    match field.field_type with
    | Thrift.Stop ->
        let* page_type = require_field "PageEncodingStats.page_type" !page_type in let* encoding =
          require_field "PageEncodingStats.encoding" !encoding in let* count =
          require_field "PageEncodingStats.count" !count in Ok { page_type; encoding; count }
    | _ ->
        let* () =
          match field.id with
          | 1 ->
              let* () = expect_field_type field.field_type Thrift.I32 "PageEncodingStats.page_type" in
              let* read = Thrift.read_i32 input in
              page_type := Some (page_type_of_int read);
              Ok ()
          | 2 ->
              let* () = expect_field_type field.field_type Thrift.I32 "PageEncodingStats.encoding" in
              let* read = Thrift.read_i32 input in
              encoding := Some (encoding_of_int read);
              Ok ()
          | 3 ->
              let* () = expect_field_type field.field_type Thrift.I32 "PageEncodingStats.count" in
              let* read = Thrift.read_i32 input in
              count := Some read;
              Ok ()
          | _ -> Thrift.skip input field.field_type
        in
        loop field.id
  in
  loop 0

and encode_page_encoding_stats = fun buffer (value: page_encoding_stats) -> let* last_field_id =
  write_i32_field buffer 1 0 (int_of_page_type value.page_type) in let* last_field_id =
  write_i32_field buffer 2 last_field_id (int_of_encoding value.encoding) in let* _last_field_id =
  write_i32_field buffer 3 last_field_id value.count in Thrift.write_struct_end buffer

and decode_schema_element = fun input ->
  let type_ = ref None in
  let type_length = ref None in
  let repetition_type = ref None in
  let name = ref None in
  let num_children = ref None in
  let converted_type = ref None in
  let scale = ref None in
  let precision = ref None in
  let field_id = ref None in
  let rec loop last_field_id =
    let* field = Thrift.read_field_begin input last_field_id in
    match field.field_type with
    | Thrift.Stop ->
        let* name = require_field "SchemaElement.name" !name in
        Ok {
          type_ = !type_;
          type_length = !type_length;
          repetition_type = !repetition_type;
          name;
          num_children = !num_children;
          converted_type = !converted_type;
          scale = !scale;
          precision = !precision;
          field_id = !field_id;
        }
    | _ ->
        let* () =
          match field.id with
          | 1 ->
              let* () = expect_field_type field.field_type Thrift.I32 "SchemaElement.type" in
              let* read = Thrift.read_i32 input in
              type_ := Some (physical_type_of_int read);
              Ok ()
          | 2 ->
              let* () = expect_field_type field.field_type Thrift.I32 "SchemaElement.type_length" in
              let* read = Thrift.read_i32 input in
              type_length := Some read;
              Ok ()
          | 3 ->
              let* () = expect_field_type
                field.field_type
                Thrift.I32
                "SchemaElement.repetition_type" in
              let* read = Thrift.read_i32 input in
              repetition_type := Some (field_repetition_type_of_int read);
              Ok ()
          | 4 ->
              let* () = expect_field_type field.field_type Thrift.Binary "SchemaElement.name" in
              let* read = Thrift.read_binary input in
              name := Some read;
              Ok ()
          | 5 ->
              let* () = expect_field_type field.field_type Thrift.I32 "SchemaElement.num_children" in
              let* read = Thrift.read_i32 input in
              num_children := Some read;
              Ok ()
          | 6 ->
              let* () = expect_field_type field.field_type Thrift.I32 "SchemaElement.converted_type" in
              let* read = Thrift.read_i32 input in
              converted_type := Some (converted_type_of_int read);
              Ok ()
          | 7 ->
              let* () = expect_field_type field.field_type Thrift.I32 "SchemaElement.scale" in
              let* read = Thrift.read_i32 input in
              scale := Some read;
              Ok ()
          | 8 ->
              let* () = expect_field_type field.field_type Thrift.I32 "SchemaElement.precision" in
              let* read = Thrift.read_i32 input in
              precision := Some read;
              Ok ()
          | 9 ->
              let* () = expect_field_type field.field_type Thrift.I32 "SchemaElement.field_id" in
              let* read = Thrift.read_i32 input in
              field_id := Some read;
              Ok ()
          | _ -> Thrift.skip input field.field_type
        in
        loop field.id
  in
  loop 0

and encode_schema_element = fun buffer (value: schema_element) ->
  let* last_field_id =
    match value.type_ with
    | None -> Ok 0
    | Some type_ -> write_i32_field buffer 1 0 (int_of_physical_type type_)
  in
  let* last_field_id =
    match value.type_length with
    | None -> Ok last_field_id
    | Some type_length -> write_i32_field buffer 2 last_field_id type_length
  in
  let* last_field_id =
    match value.repetition_type with
    | None -> Ok last_field_id
    | Some repetition_type ->
        write_i32_field buffer 3 last_field_id (int_of_field_repetition_type repetition_type)
  in
  let* last_field_id = write_string_field buffer 4 last_field_id value.name in
  let* last_field_id =
    match value.num_children with
    | None -> Ok last_field_id
    | Some num_children -> write_i32_field buffer 5 last_field_id num_children
  in
  let* last_field_id =
    match value.converted_type with
    | None -> Ok last_field_id
    | Some converted_type ->
        write_i32_field buffer 6 last_field_id (int_of_converted_type converted_type)
  in
  let* last_field_id =
    match value.scale with
    | None -> Ok last_field_id
    | Some scale -> write_i32_field buffer 7 last_field_id scale
  in
  let* last_field_id =
    match value.precision with
    | None -> Ok last_field_id
    | Some precision -> write_i32_field buffer 8 last_field_id precision
  in
  let* _last_field_id =
    match value.field_id with
    | None -> Ok last_field_id
    | Some field_id -> write_i32_field buffer 9 last_field_id field_id
  in
  Thrift.write_struct_end buffer

and decode_column_order = fun input ->
  let value = ref None in
  let rec loop last_field_id =
    let* field = Thrift.read_field_begin input last_field_id in
    match field.field_type with
    | Thrift.Stop -> require_field "ColumnOrder.TYPE_ORDER" !value
    | _ ->
        let* () =
          match field.id with
          | 1 ->
              let* () = expect_field_type field.field_type Thrift.Struct "ColumnOrder.TYPE_ORDER" in
              let* () = Thrift.skip input Thrift.Struct in
              value := Some Type_defined_order;
              Ok ()
          | _ -> Thrift.skip input field.field_type
        in
        loop field.id
  in
  loop 0

and encode_column_order = fun buffer (value: column_order) ->
  match value with
  | Type_defined_order ->
      let* _field_id = Thrift.write_field_begin buffer Thrift.Struct 1 0 in let* () =
        Thrift.write_struct_end buffer in Thrift.write_struct_end buffer

and decode_column_metadata = fun input ->
  let type_ = ref None in
  let encodings = ref None in
  let path_in_schema = ref None in
  let codec = ref None in
  let num_values = ref None in
  let total_uncompressed_size = ref None in
  let total_compressed_size = ref None in
  let key_value_metadata = ref None in
  let data_page_offset = ref None in
  let index_page_offset = ref None in
  let dictionary_page_offset = ref None in
  let encoding_stats = ref None in
  let bloom_filter_offset = ref None in
  let bloom_filter_length = ref None in
  let rec loop last_field_id =
    let* field = Thrift.read_field_begin input last_field_id in
    match field.field_type with
    | Thrift.Stop ->
        let* type_ = require_field "ColumnMetaData.type" !type_ in
        let* encodings = require_field "ColumnMetaData.encodings" !encodings in
        let* path_in_schema = require_field "ColumnMetaData.path_in_schema" !path_in_schema in
        let* codec = require_field "ColumnMetaData.codec" !codec in
        let* num_values = require_field "ColumnMetaData.num_values" !num_values in
        let* total_uncompressed_size =
          require_field "ColumnMetaData.total_uncompressed_size" !total_uncompressed_size in
        let* total_compressed_size =
          require_field "ColumnMetaData.total_compressed_size" !total_compressed_size in
        let* data_page_offset = require_field "ColumnMetaData.data_page_offset" !data_page_offset in
        Ok {
          type_;
          encodings;
          path_in_schema;
          codec;
          num_values;
          total_uncompressed_size;
          total_compressed_size;
          key_value_metadata = !key_value_metadata;
          data_page_offset;
          index_page_offset = !index_page_offset;
          dictionary_page_offset = !dictionary_page_offset;
          encoding_stats = !encoding_stats;
          bloom_filter_offset = !bloom_filter_offset;
          bloom_filter_length = !bloom_filter_length;
        }
    | _ ->
        let* () =
          match field.id with
          | 1 ->
              let* () = expect_field_type field.field_type Thrift.I32 "ColumnMetaData.type" in
              let* read = Thrift.read_i32 input in
              type_ := Some (physical_type_of_int read);
              Ok ()
          | 2 ->
              let* () = expect_field_type field.field_type Thrift.List "ColumnMetaData.encodings" in
              let* read =
                decode_list
                  input
                  ~name:"ColumnMetaData.encodings"
                  ~element_type:Thrift.I32_element
                  (fun input -> let* value = Thrift.read_i32 input in Ok (encoding_of_int value)) in
              encodings := Some read;
              Ok ()
          | 3 ->
              let* () =
                expect_field_type field.field_type Thrift.List "ColumnMetaData.path_in_schema" in
              let* read =
                decode_list
                  input
                  ~name:"ColumnMetaData.path_in_schema"
                  ~element_type:Thrift.Binary_element
                  Thrift.read_binary in
              path_in_schema := Some read;
              Ok ()
          | 4 ->
              let* () = expect_field_type field.field_type Thrift.I32 "ColumnMetaData.codec" in
              let* read = Thrift.read_i32 input in
              codec := Some (compression_codec_of_int read);
              Ok ()
          | 5 ->
              let* () = expect_field_type field.field_type Thrift.I64 "ColumnMetaData.num_values" in
              let* read = Thrift.read_i64 input in
              num_values := Some read;
              Ok ()
          | 6 ->
              let* () =
                expect_field_type
                  field.field_type
                  Thrift.I64
                  "ColumnMetaData.total_uncompressed_size" in
              let* read = Thrift.read_i64 input in
              total_uncompressed_size := Some read;
              Ok ()
          | 7 ->
              let* () =
                expect_field_type field.field_type Thrift.I64 "ColumnMetaData.total_compressed_size" in
              let* read = Thrift.read_i64 input in
              total_compressed_size := Some read;
              Ok ()
          | 8 ->
              let* () =
                expect_field_type field.field_type Thrift.List "ColumnMetaData.key_value_metadata" in
              let* read =
                decode_list
                  input
                  ~name:"ColumnMetaData.key_value_metadata"
                  ~element_type:Thrift.Struct_element
                  decode_key_value in
              key_value_metadata := Some read;
              Ok ()
          | 9 ->
              let* () =
                expect_field_type field.field_type Thrift.I64 "ColumnMetaData.data_page_offset" in
              let* read = Thrift.read_i64 input in
              data_page_offset := Some read;
              Ok ()
          | 10 ->
              let* () =
                expect_field_type field.field_type Thrift.I64 "ColumnMetaData.index_page_offset" in
              let* read = Thrift.read_i64 input in
              index_page_offset := Some read;
              Ok ()
          | 11 ->
              let* () =
                expect_field_type
                  field.field_type
                  Thrift.I64
                  "ColumnMetaData.dictionary_page_offset" in
              let* read = Thrift.read_i64 input in
              dictionary_page_offset := Some read;
              Ok ()
          | 13 ->
              let* () =
                expect_field_type field.field_type Thrift.List "ColumnMetaData.encoding_stats" in
              let* read =
                decode_list
                  input
                  ~name:"ColumnMetaData.encoding_stats"
                  ~element_type:Thrift.Struct_element
                  decode_page_encoding_stats in
              encoding_stats := Some read;
              Ok ()
          | 14 ->
              let* () =
                expect_field_type field.field_type Thrift.I64 "ColumnMetaData.bloom_filter_offset" in
              let* read = Thrift.read_i64 input in
              bloom_filter_offset := Some read;
              Ok ()
          | 15 ->
              let* () =
                expect_field_type field.field_type Thrift.I32 "ColumnMetaData.bloom_filter_length" in
              let* read = Thrift.read_i32 input in
              bloom_filter_length := Some read;
              Ok ()
          | _ -> Thrift.skip input field.field_type
        in
        loop field.id
  in
  loop 0

and encode_column_metadata = fun buffer (value: column_metadata) ->
  let* last_field_id = write_i32_field buffer 1 0 (int_of_physical_type value.type_) in
  let* last_field_id =
    write_list_field
      buffer
      2
      last_field_id
      Thrift.I32_element
      (fun buffer value -> Thrift.write_i32 buffer (int_of_encoding value))
      value.encodings in
  let* last_field_id =
    write_list_field
      buffer
      3
      last_field_id
      Thrift.Binary_element
      Thrift.write_binary
      value.path_in_schema in
  let* last_field_id = write_i32_field buffer 4 last_field_id (int_of_compression_codec value.codec) in
  let* last_field_id = write_i64_field buffer 5 last_field_id value.num_values in
  let* last_field_id = write_i64_field buffer 6 last_field_id value.total_uncompressed_size in
  let* last_field_id = write_i64_field buffer 7 last_field_id value.total_compressed_size in
  let* last_field_id =
    match value.key_value_metadata with
    | None -> Ok last_field_id
    | Some key_value_metadata ->
        write_list_field
          buffer
          8
          last_field_id
          Thrift.Struct_element
          encode_key_value
          key_value_metadata
  in
  let* last_field_id = write_i64_field buffer 9 last_field_id value.data_page_offset in
  let* last_field_id =
    match value.index_page_offset with
    | None -> Ok last_field_id
    | Some index_page_offset -> write_i64_field buffer 10 last_field_id index_page_offset
  in
  let* last_field_id =
    match value.dictionary_page_offset with
    | None -> Ok last_field_id
    | Some dictionary_page_offset -> write_i64_field buffer 11 last_field_id dictionary_page_offset
  in
  let* last_field_id =
    match value.encoding_stats with
    | None -> Ok last_field_id
    | Some encoding_stats ->
        write_list_field
          buffer
          13
          last_field_id
          Thrift.Struct_element
          encode_page_encoding_stats
          encoding_stats
  in
  let* last_field_id =
    match value.bloom_filter_offset with
    | None -> Ok last_field_id
    | Some bloom_filter_offset -> write_i64_field buffer 14 last_field_id bloom_filter_offset
  in
  let* _last_field_id =
    match value.bloom_filter_length with
    | None -> Ok last_field_id
    | Some bloom_filter_length -> write_i32_field buffer 15 last_field_id bloom_filter_length
  in
  Thrift.write_struct_end buffer

and decode_column_chunk = fun input ->
  let file_path = ref None in
  let file_offset = ref None in
  let meta_data = ref None in
  let offset_index_offset = ref None in
  let offset_index_length = ref None in
  let column_index_offset = ref None in
  let column_index_length = ref None in
  let encrypted_column_metadata = ref None in
  let rec loop last_field_id =
    let* field = Thrift.read_field_begin input last_field_id in
    match field.field_type with
    | Thrift.Stop ->
        let* file_offset = require_field "ColumnChunk.file_offset" !file_offset in
        Ok {
          file_path = !file_path;
          file_offset;
          meta_data = !meta_data;
          offset_index_offset = !offset_index_offset;
          offset_index_length = !offset_index_length;
          column_index_offset = !column_index_offset;
          column_index_length = !column_index_length;
          encrypted_column_metadata = !encrypted_column_metadata;
        }
    | _ ->
        let* () =
          match field.id with
          | 1 ->
              let* () = expect_field_type field.field_type Thrift.Binary "ColumnChunk.file_path" in
              let* read = Thrift.read_binary input in
              file_path := Some read;
              Ok ()
          | 2 ->
              let* () = expect_field_type field.field_type Thrift.I64 "ColumnChunk.file_offset" in
              let* read = Thrift.read_i64 input in
              file_offset := Some read;
              Ok ()
          | 3 ->
              let* () = expect_field_type field.field_type Thrift.Struct "ColumnChunk.meta_data" in
              let* read = decode_column_metadata input in
              meta_data := Some read;
              Ok ()
          | 4 ->
              let* () =
                expect_field_type field.field_type Thrift.I64 "ColumnChunk.offset_index_offset" in
              let* read = Thrift.read_i64 input in
              offset_index_offset := Some read;
              Ok ()
          | 5 ->
              let* () =
                expect_field_type field.field_type Thrift.I32 "ColumnChunk.offset_index_length" in
              let* read = Thrift.read_i32 input in
              offset_index_length := Some read;
              Ok ()
          | 6 ->
              let* () =
                expect_field_type field.field_type Thrift.I64 "ColumnChunk.column_index_offset" in
              let* read = Thrift.read_i64 input in
              column_index_offset := Some read;
              Ok ()
          | 7 ->
              let* () =
                expect_field_type field.field_type Thrift.I32 "ColumnChunk.column_index_length" in
              let* read = Thrift.read_i32 input in
              column_index_length := Some read;
              Ok ()
          | 9 ->
              let* () =
                expect_field_type
                  field.field_type
                  Thrift.Binary
                  "ColumnChunk.encrypted_column_metadata" in
              let* read = Thrift.read_binary input in
              encrypted_column_metadata := Some read;
              Ok ()
          | _ -> Thrift.skip input field.field_type
        in
        loop field.id
  in
  loop 0

and encode_column_chunk = fun buffer (value: column_chunk) ->
  let* last_field_id =
    match value.file_path with
    | None -> Ok 0
    | Some file_path -> write_string_field buffer 1 0 file_path
  in
  let* last_field_id = write_i64_field buffer 2 last_field_id value.file_offset in
  let* last_field_id =
    match value.meta_data with
    | None -> Ok last_field_id
    | Some meta_data -> write_struct_field buffer 3 last_field_id encode_column_metadata meta_data
  in
  let* last_field_id =
    match value.offset_index_offset with
    | None -> Ok last_field_id
    | Some offset_index_offset -> write_i64_field buffer 4 last_field_id offset_index_offset
  in
  let* last_field_id =
    match value.offset_index_length with
    | None -> Ok last_field_id
    | Some offset_index_length -> write_i32_field buffer 5 last_field_id offset_index_length
  in
  let* last_field_id =
    match value.column_index_offset with
    | None -> Ok last_field_id
    | Some column_index_offset -> write_i64_field buffer 6 last_field_id column_index_offset
  in
  let* last_field_id =
    match value.column_index_length with
    | None -> Ok last_field_id
    | Some column_index_length -> write_i32_field buffer 7 last_field_id column_index_length
  in
  let* _last_field_id =
    match value.encrypted_column_metadata with
    | None -> Ok last_field_id
    | Some encrypted_column_metadata ->
        write_string_field buffer 9 last_field_id encrypted_column_metadata
  in
  Thrift.write_struct_end buffer

and decode_row_group = fun input ->
  let columns = ref None in
  let total_byte_size = ref None in
  let num_rows = ref None in
  let sorting_columns = ref None in
  let file_offset = ref None in
  let total_compressed_size = ref None in
  let ordinal = ref None in
  let rec loop last_field_id =
    let* field = Thrift.read_field_begin input last_field_id in
    match field.field_type with
    | Thrift.Stop ->
        let* columns = require_field "RowGroup.columns" !columns in
        let* total_byte_size = require_field "RowGroup.total_byte_size" !total_byte_size in
        let* num_rows = require_field "RowGroup.num_rows" !num_rows in
        Ok {
          columns;
          total_byte_size;
          num_rows;
          sorting_columns = !sorting_columns;
          file_offset = !file_offset;
          total_compressed_size = !total_compressed_size;
          ordinal = !ordinal;
        }
    | _ ->
        let* () =
          match field.id with
          | 1 ->
              let* () = expect_field_type field.field_type Thrift.List "RowGroup.columns" in
              let* read =
                decode_list
                  input
                  ~name:"RowGroup.columns"
                  ~element_type:Thrift.Struct_element
                  decode_column_chunk in
              columns := Some read;
              Ok ()
          | 2 ->
              let* () = expect_field_type field.field_type Thrift.I64 "RowGroup.total_byte_size" in
              let* read = Thrift.read_i64 input in
              total_byte_size := Some read;
              Ok ()
          | 3 ->
              let* () = expect_field_type field.field_type Thrift.I64 "RowGroup.num_rows" in
              let* read = Thrift.read_i64 input in
              num_rows := Some read;
              Ok ()
          | 4 ->
              let* () = expect_field_type field.field_type Thrift.List "RowGroup.sorting_columns" in
              let* read =
                decode_list
                  input
                  ~name:"RowGroup.sorting_columns"
                  ~element_type:Thrift.Struct_element
                  decode_sorting_column in
              sorting_columns := Some read;
              Ok ()
          | 5 ->
              let* () = expect_field_type field.field_type Thrift.I64 "RowGroup.file_offset" in
              let* read = Thrift.read_i64 input in
              file_offset := Some read;
              Ok ()
          | 6 ->
              let* () =
                expect_field_type field.field_type Thrift.I64 "RowGroup.total_compressed_size" in
              let* read = Thrift.read_i64 input in
              total_compressed_size := Some read;
              Ok ()
          | 7 ->
              let* () = expect_field_type field.field_type Thrift.I16 "RowGroup.ordinal" in
              let* read = Thrift.read_i16 input in
              ordinal := Some read;
              Ok ()
          | _ -> Thrift.skip input field.field_type
        in
        loop field.id
  in
  loop 0

and encode_row_group = fun buffer (value: row_group) ->
  let* last_field_id =
    write_list_field buffer 1 0 Thrift.Struct_element encode_column_chunk value.columns in
  let* last_field_id = write_i64_field buffer 2 last_field_id value.total_byte_size in
  let* last_field_id = write_i64_field buffer 3 last_field_id value.num_rows in
  let* last_field_id =
    match value.sorting_columns with
    | None -> Ok last_field_id
    | Some sorting_columns ->
        write_list_field
          buffer
          4
          last_field_id
          Thrift.Struct_element
          encode_sorting_column
          sorting_columns
  in
  let* last_field_id =
    match value.file_offset with
    | None -> Ok last_field_id
    | Some file_offset -> write_i64_field buffer 5 last_field_id file_offset
  in
  let* last_field_id =
    match value.total_compressed_size with
    | None -> Ok last_field_id
    | Some total_compressed_size -> write_i64_field buffer 6 last_field_id total_compressed_size
  in
  let* _last_field_id =
    match value.ordinal with
    | None -> Ok last_field_id
    | Some ordinal -> write_i16_field buffer 7 last_field_id ordinal
  in
  Thrift.write_struct_end buffer

and decode_file_metadata_input = fun input ->
  let version = ref None in
  let schema = ref None in
  let num_rows = ref None in
  let row_groups = ref None in
  let key_value_metadata = ref None in
  let created_by = ref None in
  let column_orders = ref None in
  let rec loop last_field_id =
    let* field = Thrift.read_field_begin input last_field_id in
    match field.field_type with
    | Thrift.Stop ->
        let* version = require_field "FileMetaData.version" !version in
        let* schema = require_field "FileMetaData.schema" !schema in
        let* num_rows = require_field "FileMetaData.num_rows" !num_rows in
        let* row_groups = require_field "FileMetaData.row_groups" !row_groups in
        Ok {
          version;
          schema;
          num_rows;
          row_groups;
          key_value_metadata = !key_value_metadata;
          created_by = !created_by;
          column_orders = !column_orders;
        }
    | _ ->
        let* () =
          match field.id with
          | 1 ->
              let* () = expect_field_type field.field_type Thrift.I32 "FileMetaData.version" in
              let* read = Thrift.read_i32 input in
              version := Some read;
              Ok ()
          | 2 ->
              let* () = expect_field_type field.field_type Thrift.List "FileMetaData.schema" in
              let* read =
                decode_list
                  input
                  ~name:"FileMetaData.schema"
                  ~element_type:Thrift.Struct_element
                  decode_schema_element in
              schema := Some read;
              Ok ()
          | 3 ->
              let* () = expect_field_type field.field_type Thrift.I64 "FileMetaData.num_rows" in
              let* read = Thrift.read_i64 input in
              num_rows := Some read;
              Ok ()
          | 4 ->
              let* () = expect_field_type field.field_type Thrift.List "FileMetaData.row_groups" in
              let* read =
                decode_list
                  input
                  ~name:"FileMetaData.row_groups"
                  ~element_type:Thrift.Struct_element
                  decode_row_group in
              row_groups := Some read;
              Ok ()
          | 5 ->
              let* () =
                expect_field_type field.field_type Thrift.List "FileMetaData.key_value_metadata" in
              let* read =
                decode_list
                  input
                  ~name:"FileMetaData.key_value_metadata"
                  ~element_type:Thrift.Struct_element
                  decode_key_value in
              key_value_metadata := Some read;
              Ok ()
          | 6 ->
              let* () = expect_field_type field.field_type Thrift.Binary "FileMetaData.created_by" in
              let* read = Thrift.read_binary input in
              created_by := Some read;
              Ok ()
          | 7 ->
              let* () = expect_field_type field.field_type Thrift.List "FileMetaData.column_orders" in
              let* read =
                decode_list
                  input
                  ~name:"FileMetaData.column_orders"
                  ~element_type:Thrift.Struct_element
                  decode_column_order in
              column_orders := Some read;
              Ok ()
          | _ -> Thrift.skip input field.field_type
        in
        loop field.id
  in
  loop 0

and encode_file_metadata = fun buffer (value: file_metadata) ->
  let* last_field_id = write_i32_field buffer 1 0 value.version in
  let* last_field_id =
    write_list_field buffer 2 last_field_id Thrift.Struct_element encode_schema_element value.schema in
  let* last_field_id = write_i64_field buffer 3 last_field_id value.num_rows in
  let* last_field_id =
    write_list_field buffer 4 last_field_id Thrift.Struct_element encode_row_group value.row_groups in
  let* last_field_id =
    match value.key_value_metadata with
    | None -> Ok last_field_id
    | Some key_value_metadata ->
        write_list_field
          buffer
          5
          last_field_id
          Thrift.Struct_element
          encode_key_value
          key_value_metadata
  in
  let* last_field_id =
    match value.created_by with
    | None -> Ok last_field_id
    | Some created_by -> write_string_field buffer 6 last_field_id created_by
  in
  let* _last_field_id =
    match value.column_orders with
    | None -> Ok last_field_id
    | Some column_orders ->
        write_list_field
          buffer
          7
          last_field_id
          Thrift.Struct_element
          encode_column_order
          column_orders
  in
  Thrift.write_struct_end buffer

let decode_metadata = fun input ->
  let thrift_input = Thrift.input_of_string input in
  let* metadata = decode_file_metadata_input thrift_input in
  if Int.equal thrift_input.pos thrift_input.length then
    Ok metadata
  else
    fail "parquet metadata has trailing thrift bytes"

let encode_metadata = fun metadata ->
  let buffer = IO.Buffer.create ~size:256 in
  let* () = encode_file_metadata buffer metadata in Ok (IO.Buffer.contents buffer)

let decode_footer_tail = fun input ->
  if not (Int.equal (String.length input) footer_size) then
    fail ("parquet footer tail must be exactly " ^ Int.to_string footer_size ^ " bytes")
  else if string_segment_equals input ~offset:4 magic then
    Ok { metadata_length = decode_u32_le input ~offset:0; encrypted_footer = false }
  else if string_segment_equals input ~offset:4 encrypted_magic then
    Ok { metadata_length = decode_u32_le input ~offset:0; encrypted_footer = true }
  else
    fail "parquet footer tail has invalid magic bytes"

let encode_footer_tail = fun metadata_length ->
  let* () = ensure_u32 "metadata length" metadata_length in
  let buffer = IO.Buffer.create ~size:footer_size in
  add_u32_le buffer metadata_length;
  IO.Buffer.add_string buffer magic;
  Ok (IO.Buffer.contents buffer)

let from_string = fun input ->
  if String.length input < String.length magic + footer_size then
    fail "parquet file is too small"
  else if not (string_segment_equals input ~offset:0 magic) then
    fail "parquet file is missing the leading magic bytes"
  else
    let footer_offset = String.length input - footer_size in
    let footer_input = String.sub input ~offset:footer_offset ~len:footer_size in
    let* footer = decode_footer_tail footer_input in
    if footer.encrypted_footer then
      fail "encrypted Parquet footers are not supported yet"
    else
      let metadata_offset = footer_offset - footer.metadata_length in
      if metadata_offset < String.length magic then
        fail "parquet metadata length points outside the file"
      else
        let metadata_bytes = String.sub input ~offset:metadata_offset ~len:footer.metadata_length in
        let body =
          String.sub
            input
            ~offset:(String.length magic)
            ~len:(metadata_offset - String.length magic)
        in
        let* metadata = decode_metadata metadata_bytes in Ok { body; metadata }

let from_reader = fun reader ->
  let buffer = IO.Buffer.create ~size:4_096 in
  match IO.read_to_end reader ~into:buffer with
  | Ok _ -> from_string (IO.Buffer.contents buffer)
  | Error err -> io_error err

let to_string = fun (value: t) ->
  let* metadata_bytes = encode_metadata value.metadata in
  let* footer = encode_footer_tail (String.length metadata_bytes) in
  let buffer =
    IO.Buffer.create
      ~size:(String.length magic
      + String.length value.body
      + String.length metadata_bytes
      + footer_size)
  in
  IO.Buffer.add_string buffer magic;
  IO.Buffer.add_string buffer value.body;
  IO.Buffer.add_string buffer metadata_bytes;
  IO.Buffer.add_string buffer footer;
  Ok (IO.Buffer.contents buffer)

let to_writer = fun writer (value: t) ->
  let* metadata_bytes = encode_metadata value.metadata in
  let* footer = encode_footer_tail (String.length metadata_bytes) in
  let write_string value =
    let buffer = IO.Buffer.from_string value in
    match IO.write_all writer ~from:buffer with
    | Ok () -> Ok ()
    | Error err -> io_error err
  in
  let* () = write_string magic in let* () = write_string value.body in let* () =
    write_string metadata_bytes in write_string footer

module Reader = struct
  let from_string = from_string

  let from_reader = from_reader
end

module Writer = struct
  let to_string = to_string

  let to_writer = to_writer
end
