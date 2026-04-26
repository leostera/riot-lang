open Std
open Std.Result.Syntax
module Test = Std.Test
module Json = Data.Json

let byte_to_hex = fun value ->
  let digit value =
    if value < 10 then
      Char.chr (Char.code '0' + value)
    else
      Char.chr (Char.code 'a' + value - 10)
  in
  String.make ~len:1 ~char:(digit ((value lsr 4) land 0x0f))
  ^ String.make ~len:1 ~char:(digit (value land 0x0f))

let hex_of_string = fun value ->
  let parts =
    List.init
      ~count:(String.length value)
      ~fn:(fun index -> byte_to_hex (Char.code (String.get_unchecked value ~at:index)))
  in
  String.concat " " parts

let json_of_option = fun encode value ->
  match value with
  | None -> Json.Null
  | Some value -> encode value

let json_of_int64 = fun value -> Json.String (Int64.to_string value)

let string_of_physical_type = function
  | Parquet.Boolean -> "Boolean"
  | Parquet.Int32 -> "Int32"
  | Parquet.Int64 -> "Int64"
  | Parquet.Int96 -> "Int96"
  | Parquet.Float -> "Float"
  | Parquet.Double -> "Double"
  | Parquet.Byte_array -> "Byte_array"
  | Parquet.Fixed_len_byte_array -> "Fixed_len_byte_array"
  | Parquet.Unknown_physical_type value -> "Unknown_physical_type(" ^ Int.to_string value ^ ")"

let string_of_converted_type = function
  | Parquet.Utf8 -> "Utf8"
  | Parquet.Map -> "Map"
  | Parquet.Map_key_value -> "Map_key_value"
  | Parquet.List -> "List"
  | Parquet.Enum -> "Enum"
  | Parquet.Decimal -> "Decimal"
  | Parquet.Date -> "Date"
  | Parquet.Time_millis -> "Time_millis"
  | Parquet.Time_micros -> "Time_micros"
  | Parquet.Timestamp_millis -> "Timestamp_millis"
  | Parquet.Timestamp_micros -> "Timestamp_micros"
  | Parquet.UInt_8 -> "UInt_8"
  | Parquet.UInt_16 -> "UInt_16"
  | Parquet.UInt_32 -> "UInt_32"
  | Parquet.UInt_64 -> "UInt_64"
  | Parquet.Int_8 -> "Int_8"
  | Parquet.Int_16 -> "Int_16"
  | Parquet.Int_32 -> "Int_32"
  | Parquet.Int_64 -> "Int_64"
  | Parquet.Json -> "Json"
  | Parquet.Bson -> "Bson"
  | Parquet.Interval -> "Interval"
  | Parquet.Unknown_converted_type value -> "Unknown_converted_type(" ^ Int.to_string value ^ ")"

let string_of_repetition_type = function
  | Parquet.Required -> "Required"
  | Parquet.Optional -> "Optional"
  | Parquet.Repeated -> "Repeated"
  | Parquet.Unknown_repetition_type value -> "Unknown_repetition_type(" ^ Int.to_string value ^ ")"

let string_of_encoding = function
  | Parquet.Plain -> "Plain"
  | Parquet.Plain_dictionary -> "Plain_dictionary"
  | Parquet.Rle -> "Rle"
  | Parquet.Bit_packed -> "Bit_packed"
  | Parquet.Delta_binary_packed -> "Delta_binary_packed"
  | Parquet.Delta_length_byte_array -> "Delta_length_byte_array"
  | Parquet.Delta_byte_array -> "Delta_byte_array"
  | Parquet.Rle_dictionary -> "Rle_dictionary"
  | Parquet.Byte_stream_split -> "Byte_stream_split"
  | Parquet.Unknown_encoding value -> "Unknown_encoding(" ^ Int.to_string value ^ ")"

let string_of_compression_codec = function
  | Parquet.Uncompressed -> "Uncompressed"
  | Parquet.Snappy -> "Snappy"
  | Parquet.Gzip -> "Gzip"
  | Parquet.Lzo -> "Lzo"
  | Parquet.Brotli -> "Brotli"
  | Parquet.Lz4 -> "Lz4"
  | Parquet.Zstd -> "Zstd"
  | Parquet.Lz4_raw -> "Lz4_raw"
  | Parquet.Unknown_compression_codec value -> "Unknown_compression_codec(" ^ Int.to_string value ^ ")"

let string_of_page_type = function
  | Parquet.Data_page -> "Data_page"
  | Parquet.Index_page -> "Index_page"
  | Parquet.Dictionary_page -> "Dictionary_page"
  | Parquet.Data_page_v2 -> "Data_page_v2"
  | Parquet.Unknown_page_type value -> "Unknown_page_type(" ^ Int.to_string value ^ ")"

let json_of_key_value = fun (value: Parquet.key_value) ->
  Json.Object [
    ("key", Json.String value.key);
    ("value", json_of_option (fun value -> Json.String value) value.value);
  ]

let json_of_schema_element = fun (value: Parquet.schema_element) ->
  Json.Object [
    ("type", json_of_option (fun value -> Json.String (string_of_physical_type value)) value.type_);
    ("type_length", json_of_option (fun value -> Json.Int value) value.type_length);
    (
      "repetition_type",
      json_of_option (fun value -> Json.String (string_of_repetition_type value)) value.repetition_type
    );
    ("name", Json.String value.name);
    ("num_children", json_of_option (fun value -> Json.Int value) value.num_children);
    (
      "converted_type",
      json_of_option (fun value -> Json.String (string_of_converted_type value)) value.converted_type
    );
    ("scale", json_of_option (fun value -> Json.Int value) value.scale);
    ("precision", json_of_option (fun value -> Json.Int value) value.precision);
    ("field_id", json_of_option (fun value -> Json.Int value) value.field_id);
  ]

let json_of_sorting_column = fun (value: Parquet.sorting_column) ->
  Json.Object [
    ("column_idx", Json.Int value.column_idx);
    ("descending", Json.Bool value.descending);
    ("nulls_first", Json.Bool value.nulls_first);
  ]

let json_of_page_encoding_stats = fun (value: Parquet.page_encoding_stats) ->
  Json.Object [
    ("page_type", Json.String (string_of_page_type value.page_type));
    ("encoding", Json.String (string_of_encoding value.encoding));
    ("count", Json.Int value.count);
  ]

let json_of_column_metadata = fun (value: Parquet.column_metadata) ->
  Json.Object [
    ("type", Json.String (string_of_physical_type value.type_));
    (
      "encodings",
      Json.Array (List.map value.encodings ~fn:(fun value -> Json.String (string_of_encoding value)))
    );
    (
      "path_in_schema",
      Json.Array (List.map value.path_in_schema ~fn:(fun value -> Json.String value))
    );
    ("codec", Json.String (string_of_compression_codec value.codec));
    ("num_values", json_of_int64 value.num_values);
    ("total_uncompressed_size", json_of_int64 value.total_uncompressed_size);
    ("total_compressed_size", json_of_int64 value.total_compressed_size);
    (
      "key_value_metadata",
      json_of_option (fun value -> Json.Array (List.map value ~fn:json_of_key_value)) value.key_value_metadata
    );
    ("data_page_offset", json_of_int64 value.data_page_offset);
    ("index_page_offset", json_of_option json_of_int64 value.index_page_offset);
    ("dictionary_page_offset", json_of_option json_of_int64 value.dictionary_page_offset);
    (
      "encoding_stats",
      json_of_option
        (fun value -> Json.Array (List.map value ~fn:json_of_page_encoding_stats))
        value.encoding_stats
    );
    ("bloom_filter_offset", json_of_option json_of_int64 value.bloom_filter_offset);
    ("bloom_filter_length", json_of_option (fun value -> Json.Int value) value.bloom_filter_length);
  ]

let json_of_column_chunk = fun (value: Parquet.column_chunk) ->
  Json.Object [
    ("file_path", json_of_option (fun value -> Json.String value) value.file_path);
    ("file_offset", json_of_int64 value.file_offset);
    ("meta_data", json_of_option json_of_column_metadata value.meta_data);
    ("offset_index_offset", json_of_option json_of_int64 value.offset_index_offset);
    ("offset_index_length", json_of_option (fun value -> Json.Int value) value.offset_index_length);
    ("column_index_offset", json_of_option json_of_int64 value.column_index_offset);
    ("column_index_length", json_of_option (fun value -> Json.Int value) value.column_index_length);
    (
      "encrypted_column_metadata",
      json_of_option (fun value -> Json.String value) value.encrypted_column_metadata
    );
  ]

let json_of_row_group = fun (value: Parquet.row_group) ->
  Json.Object [
    ("columns", Json.Array (List.map value.columns ~fn:json_of_column_chunk));
    ("total_byte_size", json_of_int64 value.total_byte_size);
    ("num_rows", json_of_int64 value.num_rows);
    (
      "sorting_columns",
      json_of_option (fun value -> Json.Array (List.map value ~fn:json_of_sorting_column)) value.sorting_columns
    );
    ("file_offset", json_of_option json_of_int64 value.file_offset);
    ("total_compressed_size", json_of_option json_of_int64 value.total_compressed_size);
    ("ordinal", json_of_option (fun value -> Json.Int value) value.ordinal);
  ]

let json_of_column_order = function
  | Parquet.Type_defined_order -> Json.String "Type_defined_order"

let json_of_file_metadata = fun (value: Parquet.file_metadata) ->
  Json.Object [
    ("version", Json.Int value.version);
    ("schema", Json.Array (List.map value.schema ~fn:json_of_schema_element));
    ("num_rows", json_of_int64 value.num_rows);
    ("row_groups", Json.Array (List.map value.row_groups ~fn:json_of_row_group));
    (
      "key_value_metadata",
      json_of_option (fun value -> Json.Array (List.map value ~fn:json_of_key_value)) value.key_value_metadata
    );
    ("created_by", json_of_option (fun value -> Json.String value) value.created_by);
    (
      "column_orders",
      json_of_option (fun value -> Json.Array (List.map value ~fn:json_of_column_order)) value.column_orders
    );
  ]

let snapshot_json_of_file = fun (value: Parquet.t) ->
  let encoded = Parquet.to_string value |> Result.expect ~msg:"snapshot file should encode" in
  let footer = String.sub encoded ~offset:(String.length encoded - 8) ~len:8
  |> Parquet.decode_footer_tail
  |> Result.expect ~msg:"snapshot file footer should decode" in
  let metadata_bytes = Parquet.encode_metadata value.metadata |> Result.expect ~msg:"snapshot metadata should encode" in
  Json.Object [
    ("file_size", Json.Int (String.length encoded));
    ("body_size", Json.Int (String.length value.body));
    ("body_hex", Json.String (hex_of_string value.body));
    ("metadata_length", Json.Int footer.metadata_length);
    ("footer_encrypted", Json.Bool footer.encrypted_footer);
    ("metadata_hex", Json.String (hex_of_string metadata_bytes));
    ("encoded_hex", Json.String (hex_of_string encoded));
    ("metadata", json_of_file_metadata value.metadata);
  ]

let empty_file: Parquet.t = {
  body = "";
  metadata =
    {
      version = 1;
      schema =
        [ {
            type_ = None;
            type_length = None;
            repetition_type = None;
            name = "schema";
            num_children = Some 1;
            converted_type = None;
            scale = None;
            precision = None;
            field_id = None;
          }; {
            type_ = Some Parquet.Int32;
            type_length = None;
            repetition_type = Some Parquet.Required;
            name = "pirate_count";
            num_children = None;
            converted_type = None;
            scale = None;
            precision = None;
            field_id = Some 7;
          }; ];
      num_rows = 0L;
      row_groups = [];
      key_value_metadata = Some [ { key = "series"; value = Some "One Piece" } ];
      created_by = Some "riot/parquet";
      column_orders = Some [ Parquet.Type_defined_order ];
    };
}

let nested_file: Parquet.t = {
  body = "RIOT";
  metadata =
    {
      version = 2;
      schema =
        [ {
            type_ = None;
            type_length = None;
            repetition_type = None;
            name = "schema";
            num_children = Some 1;
            converted_type = None;
            scale = None;
            precision = None;
            field_id = None;
          }; {
            type_ = Some Parquet.Byte_array;
            type_length = None;
            repetition_type = Some Parquet.Optional;
            name = "captain";
            num_children = None;
            converted_type = Some Parquet.Utf8;
            scale = None;
            precision = None;
            field_id = Some 3;
          }; ];
      num_rows = 42L;
      row_groups =
        [ {
            columns =
              [ {
                  file_path = None;
                  file_offset = 4L;
                  meta_data =
                    Some {
                      type_ = Parquet.Byte_array;
                      encodings = [ Parquet.Plain; Parquet.Rle_dictionary ];
                      path_in_schema = [ "captain" ];
                      codec = Parquet.Snappy;
                      num_values = 42L;
                      total_uncompressed_size = 256L;
                      total_compressed_size = 128L;
                      key_value_metadata = Some [ { key = "crew"; value = Some "Straw Hats" } ];
                      data_page_offset = 4L;
                      index_page_offset = Some 64L;
                      dictionary_page_offset = Some 32L;
                      encoding_stats = Some [
                        { page_type = Parquet.Data_page; encoding = Parquet.Plain; count = 1 };
                      ];
                      bloom_filter_offset = Some 96L;
                      bloom_filter_length = Some 12;
                    };
                  offset_index_offset = Some 80L;
                  offset_index_length = Some 8;
                  column_index_offset = Some 88L;
                  column_index_length = Some 8;
                  encrypted_column_metadata = None;
                }; ];
            total_byte_size = 256L;
            num_rows = 42L;
            sorting_columns = Some [ { column_idx = 0; descending = false; nulls_first = true } ];
            file_offset = Some 4L;
            total_compressed_size = Some 128L;
            ordinal = Some 0;
          }; ];
      key_value_metadata = Some [ { key = "arc"; value = Some "Wano" } ];
      created_by = Some "riot/parquet";
      column_orders = Some [ Parquet.Type_defined_order ];
    };
}

let test_snapshot_empty_file_layout = fun ctx ->
  Test.Snapshot.assert_json ~ctx ~actual:(snapshot_json_of_file empty_file)

let test_snapshot_nested_file_layout = fun ctx ->
  Test.Snapshot.assert_json ~ctx ~actual:(snapshot_json_of_file nested_file)

let tests = [
  Test.case "parquet snapshot empty file layout" test_snapshot_empty_file_layout;
  Test.case "parquet snapshot nested file layout" test_snapshot_nested_file_layout;
]

let main ~args = Test.Cli.main ~name:"parquet_snapshot_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
