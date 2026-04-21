open Std
open Propane
module Test = Std.Test

let primitive_examples = 5_000

let composite_examples = 1_000

let io_chunk_size = 7

let io_writer_of_buffer =
  let module Write = struct
    type t = IO.Buffer.t

    let write = fun buffer ~from ->
      let written = IO.Buffer.readable_bytes from in
      IO.Buffer.append_slice buffer (IO.Buffer.readable from) |> Result.expect ~msg:"parquet property writer should append buffer contents";
      Ok written

    let write_vectored = fun buffer ~from ->
      let written = ref 0 in
      IO.IoVec.for_each from
        ~fn:(fun chunk ->
          IO.Buffer.append_slice buffer chunk |> Result.expect ~msg:"parquet property writer should append slices";
          written := !written + IO.IoSlice.length chunk);
      Ok !written

    let flush = fun _buffer -> Ok ()
  end in
  fun buffer ->
    IO.Writer.from_sink (module Write) buffer

let small_size_gen = Generator.int_range 0 2

let small_string_gen = Generator.string_size (Generator.int_range 0 12) Generator.char_printable

let body_string_gen = Generator.string_size (Generator.int_range 0 16) Generator.char

let small_int32_gen = Generator.int_range (-256) 256

let small_i16_gen = Generator.int_range (-128) 128

let small_int64_gen = Generator.int64_range (-10_000L) 10_000L

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

let print_column_order = function
  | Parquet.Type_defined_order -> "Type_defined_order"

let print_key_value = fun (value: Parquet.key_value) ->
  String.concat
    ""
    [
      "{ key = ";
      Printer.string value.key;
      "; value = ";
      Printer.option Printer.string value.value;
      " }";
    ]

let print_schema_element = fun (value: Parquet.schema_element) ->
  String.concat ""
    [
      "{ type_ = ";
      Printer.option (fun value -> string_of_physical_type value) value.type_;
      "; type_length = ";
      Printer.option Printer.int value.type_length;
      "; repetition_type = ";
      Printer.option (fun value -> string_of_repetition_type value) value.repetition_type;
      "; name = ";
      Printer.string value.name;
      "; num_children = ";
      Printer.option Printer.int value.num_children;
      "; converted_type = ";
      Printer.option (fun value -> string_of_converted_type value) value.converted_type;
      "; scale = ";
      Printer.option Printer.int value.scale;
      "; precision = ";
      Printer.option Printer.int value.precision;
      "; field_id = ";
      Printer.option Printer.int value.field_id;
      " }";
    ]

let print_sorting_column = fun (value: Parquet.sorting_column) ->
  String.concat
    ""
    [
      "{ column_idx = ";
      Printer.int value.column_idx;
      "; descending = ";
      Printer.bool value.descending;
      "; nulls_first = ";
      Printer.bool value.nulls_first;
      " }";
    ]

let print_page_encoding_stats = fun (value: Parquet.page_encoding_stats) ->
  String.concat
    ""
    [
      "{ page_type = ";
      string_of_page_type value.page_type;
      "; encoding = ";
      string_of_encoding value.encoding;
      "; count = ";
      Printer.int value.count;
      " }";
    ]

let print_column_metadata = fun (value: Parquet.column_metadata) ->
  String.concat ""
    [
      "{ type_ = ";
      string_of_physical_type value.type_;
      "; encodings = ";
      Printer.list string_of_encoding value.encodings;
      "; path_in_schema = ";
      Printer.list Printer.string value.path_in_schema;
      "; codec = ";
      string_of_compression_codec value.codec;
      "; num_values = ";
      Printer.int64 value.num_values;
      "; total_uncompressed_size = ";
      Printer.int64 value.total_uncompressed_size;
      "; total_compressed_size = ";
      Printer.int64 value.total_compressed_size;
      "; key_value_metadata = ";
      Printer.option (Printer.list print_key_value) value.key_value_metadata;
      "; data_page_offset = ";
      Printer.int64 value.data_page_offset;
      "; index_page_offset = ";
      Printer.option Printer.int64 value.index_page_offset;
      "; dictionary_page_offset = ";
      Printer.option Printer.int64 value.dictionary_page_offset;
      "; encoding_stats = ";
      Printer.option (Printer.list print_page_encoding_stats) value.encoding_stats;
      "; bloom_filter_offset = ";
      Printer.option Printer.int64 value.bloom_filter_offset;
      "; bloom_filter_length = ";
      Printer.option Printer.int value.bloom_filter_length;
      " }";
    ]

let print_column_chunk = fun (value: Parquet.column_chunk) ->
  String.concat ""
    [
      "{ file_path = ";
      Printer.option Printer.string value.file_path;
      "; file_offset = ";
      Printer.int64 value.file_offset;
      "; meta_data = ";
      Printer.option print_column_metadata value.meta_data;
      "; offset_index_offset = ";
      Printer.option Printer.int64 value.offset_index_offset;
      "; offset_index_length = ";
      Printer.option Printer.int value.offset_index_length;
      "; column_index_offset = ";
      Printer.option Printer.int64 value.column_index_offset;
      "; column_index_length = ";
      Printer.option Printer.int value.column_index_length;
      "; encrypted_column_metadata = ";
      Printer.option Printer.string value.encrypted_column_metadata;
      " }";
    ]

let print_row_group = fun (value: Parquet.row_group) ->
  String.concat ""
    [
      "{ columns = ";
      Printer.list print_column_chunk value.columns;
      "; total_byte_size = ";
      Printer.int64 value.total_byte_size;
      "; num_rows = ";
      Printer.int64 value.num_rows;
      "; sorting_columns = ";
      Printer.option (Printer.list print_sorting_column) value.sorting_columns;
      "; file_offset = ";
      Printer.option Printer.int64 value.file_offset;
      "; total_compressed_size = ";
      Printer.option Printer.int64 value.total_compressed_size;
      "; ordinal = ";
      Printer.option Printer.int value.ordinal;
      " }";
    ]

let print_file_metadata = fun (value: Parquet.file_metadata) ->
  String.concat ""
    [
      "{ version = ";
      Printer.int value.version;
      "; schema = ";
      Printer.list print_schema_element value.schema;
      "; num_rows = ";
      Printer.int64 value.num_rows;
      "; row_groups = ";
      Printer.list print_row_group value.row_groups;
      "; key_value_metadata = ";
      Printer.option (Printer.list print_key_value) value.key_value_metadata;
      "; created_by = ";
      Printer.option Printer.string value.created_by;
      "; column_orders = ";
      Printer.option (Printer.list print_column_order) value.column_orders;
      " }";
    ]

let print_file = fun (value: Parquet.t) ->
  String.concat
    ""
    [
      "{ body = ";
      Printer.string value.body;
      "; metadata = ";
      print_file_metadata value.metadata;
      " }";
    ]

let physical_type_gen = Generator.one_of
  [
    Generator.return Parquet.Boolean;
    Generator.return Parquet.Int32;
    Generator.return Parquet.Int64;
    Generator.return Parquet.Int96;
    Generator.return Parquet.Float;
    Generator.return Parquet.Double;
    Generator.return Parquet.Byte_array;
    Generator.return Parquet.Fixed_len_byte_array;
  ]

let converted_type_gen = Generator.one_of
  [
    Generator.return Parquet.Utf8;
    Generator.return Parquet.Map;
    Generator.return Parquet.Map_key_value;
    Generator.return Parquet.List;
    Generator.return Parquet.Enum;
    Generator.return Parquet.Decimal;
    Generator.return Parquet.Date;
    Generator.return Parquet.Time_millis;
    Generator.return Parquet.Time_micros;
    Generator.return Parquet.Timestamp_millis;
    Generator.return Parquet.Timestamp_micros;
    Generator.return Parquet.UInt_8;
    Generator.return Parquet.UInt_16;
    Generator.return Parquet.UInt_32;
    Generator.return Parquet.UInt_64;
    Generator.return Parquet.Int_8;
    Generator.return Parquet.Int_16;
    Generator.return Parquet.Int_32;
    Generator.return Parquet.Int_64;
    Generator.return Parquet.Json;
    Generator.return Parquet.Bson;
    Generator.return Parquet.Interval;
  ]

let repetition_type_gen = Generator.one_of
  [
    Generator.return Parquet.Required;
    Generator.return Parquet.Optional;
    Generator.return Parquet.Repeated;
  ]

let encoding_gen = Generator.one_of
  [
    Generator.return Parquet.Plain;
    Generator.return Parquet.Plain_dictionary;
    Generator.return Parquet.Rle;
    Generator.return Parquet.Bit_packed;
    Generator.return Parquet.Delta_binary_packed;
    Generator.return Parquet.Delta_length_byte_array;
    Generator.return Parquet.Delta_byte_array;
    Generator.return Parquet.Rle_dictionary;
    Generator.return Parquet.Byte_stream_split;
  ]

let compression_codec_gen = Generator.one_of
  [
    Generator.return Parquet.Uncompressed;
    Generator.return Parquet.Snappy;
    Generator.return Parquet.Gzip;
    Generator.return Parquet.Lzo;
    Generator.return Parquet.Brotli;
    Generator.return Parquet.Lz4;
    Generator.return Parquet.Zstd;
    Generator.return Parquet.Lz4_raw;
  ]

let page_type_gen = Generator.one_of
  [
    Generator.return Parquet.Data_page;
    Generator.return Parquet.Index_page;
    Generator.return Parquet.Dictionary_page;
    Generator.return Parquet.Data_page_v2;
  ]

let column_order_gen = Generator.return Parquet.Type_defined_order

let key_value_gen =
  Generator.map2
    (fun key value -> ({ key; value }: Parquet.key_value))
    small_string_gen
    (Generator.option small_string_gen)

let key_value_arb = Arbitrary.make ~print:print_key_value key_value_gen

let schema_element_gen =
  Generator.map3
    (fun (type_, type_length, repetition_type) (name, num_children, converted_type) (scale, precision, field_id) ->
      ({
          type_;
          type_length;
          repetition_type;
          name;
          num_children;
          converted_type;
          scale;
          precision;
          field_id;
        }: Parquet.schema_element))
    (Generator.triple
      (Generator.option physical_type_gen)
      (Generator.option small_int32_gen)
      (Generator.option repetition_type_gen))
    (Generator.triple
      small_string_gen
      (Generator.option small_int32_gen)
      (Generator.option converted_type_gen))
    (Generator.triple
      (Generator.option small_int32_gen)
      (Generator.option small_int32_gen)
      (Generator.option small_int32_gen))

let schema_element_arb = Arbitrary.make ~print:print_schema_element schema_element_gen

let sorting_column_gen =
  Generator.map3
    (fun column_idx descending nulls_first ->
      ({ column_idx; descending; nulls_first }: Parquet.sorting_column))
    small_int32_gen
    Generator.bool
    Generator.bool

let sorting_column_arb = Arbitrary.make ~print:print_sorting_column sorting_column_gen

let page_encoding_stats_gen =
  Generator.map3
    (fun page_type encoding count -> ({ page_type; encoding; count }: Parquet.page_encoding_stats))
    page_type_gen
    encoding_gen
    small_int32_gen

let page_encoding_stats_arb = Arbitrary.make ~print:print_page_encoding_stats page_encoding_stats_gen

let key_value_list_gen = Generator.list_size small_size_gen key_value_gen

let string_list_gen = Generator.list_size small_size_gen small_string_gen

let encoding_list_gen = Generator.list_size small_size_gen encoding_gen

let sorting_column_list_gen = Generator.list_size small_size_gen sorting_column_gen

let page_encoding_stats_list_gen = Generator.list_size small_size_gen page_encoding_stats_gen

let column_order_list_gen = Generator.list_size small_size_gen column_order_gen

let column_metadata_gen =
  Generator.map3
    (fun (type_, encodings, path_in_schema, codec) (num_values, total_uncompressed_size, total_compressed_size, data_page_offset) (key_value_metadata, index_page_offset, dictionary_page_offset, encoding_stats, bloom_filter_offset, bloom_filter_length) ->
      ({
          type_;
          encodings;
          path_in_schema;
          codec;
          num_values;
          total_uncompressed_size;
          total_compressed_size;
          key_value_metadata;
          data_page_offset;
          index_page_offset;
          dictionary_page_offset;
          encoding_stats;
          bloom_filter_offset;
          bloom_filter_length;
        }: Parquet.column_metadata))
    (Generator.map2
      (fun (type_, encodings) (path_in_schema, codec) -> (type_, encodings, path_in_schema, codec))
      (Generator.pair physical_type_gen encoding_list_gen)
      (Generator.pair string_list_gen compression_codec_gen))
    (Generator.map2
      (fun (num_values, total_uncompressed_size) (total_compressed_size, data_page_offset) ->
        (num_values, total_uncompressed_size, total_compressed_size, data_page_offset))
      (Generator.pair small_int64_gen small_int64_gen)
      (Generator.pair small_int64_gen small_int64_gen))
    (Generator.map3
      (fun (key_value_metadata, index_page_offset) (dictionary_page_offset, encoding_stats) (bloom_filter_offset, bloom_filter_length) ->
        (
          key_value_metadata,
          index_page_offset,
          dictionary_page_offset,
          encoding_stats,
          bloom_filter_offset,
          bloom_filter_length
        ))
      (Generator.pair (Generator.option key_value_list_gen) (Generator.option small_int64_gen))
      (Generator.pair
        (Generator.option small_int64_gen)
        (Generator.option page_encoding_stats_list_gen))
      (Generator.pair (Generator.option small_int64_gen) (Generator.option small_int32_gen)))

let column_metadata_arb = Arbitrary.make ~print:print_column_metadata column_metadata_gen

let column_chunk_gen =
  Generator.map3
    (fun (file_path, file_offset, meta_data) (offset_index_offset, offset_index_length) (column_index_offset, column_index_length, encrypted_column_metadata) ->
      ({
          file_path;
          file_offset;
          meta_data;
          offset_index_offset;
          offset_index_length;
          column_index_offset;
          column_index_length;
          encrypted_column_metadata;
        }: Parquet.column_chunk))
    (Generator.triple
      (Generator.option small_string_gen)
      small_int64_gen
      (Generator.option column_metadata_gen))
    (Generator.pair (Generator.option small_int64_gen) (Generator.option small_int32_gen))
    (Generator.triple
      (Generator.option small_int64_gen)
      (Generator.option small_int32_gen)
      (Generator.option small_string_gen))

let column_chunk_arb = Arbitrary.make ~print:print_column_chunk column_chunk_gen

let column_chunk_list_gen = Generator.list_size small_size_gen column_chunk_gen

let row_group_gen =
  Generator.map3
    (fun (columns, total_byte_size, num_rows) (sorting_columns, file_offset) (total_compressed_size, ordinal) ->
      ({
          columns;
          total_byte_size;
          num_rows;
          sorting_columns;
          file_offset;
          total_compressed_size;
          ordinal;
        }: Parquet.row_group))
    (Generator.triple column_chunk_list_gen small_int64_gen small_int64_gen)
    (Generator.pair (Generator.option sorting_column_list_gen) (Generator.option small_int64_gen))
    (Generator.pair (Generator.option small_int64_gen) (Generator.option small_i16_gen))

let row_group_arb = Arbitrary.make ~print:print_row_group row_group_gen

let row_group_list_gen = Generator.list_size small_size_gen row_group_gen

let schema_element_list_gen = Generator.list_size (Generator.int_range 1 3) schema_element_gen

let file_metadata_gen =
  Generator.map3
    (fun (version, schema, num_rows) (row_groups, key_value_metadata) (created_by, column_orders) ->
      ({
          version;
          schema;
          num_rows;
          row_groups;
          key_value_metadata;
          created_by;
          column_orders;
        }: Parquet.file_metadata))
    (Generator.triple (Generator.int_range 0 5) schema_element_list_gen small_int64_gen)
    (Generator.pair row_group_list_gen (Generator.option key_value_list_gen))
    (Generator.pair (Generator.option small_string_gen) (Generator.option column_order_list_gen))

let file_metadata_arb = Arbitrary.make ~print:print_file_metadata file_metadata_gen

let file_gen =
  Generator.map2 (fun body metadata -> ({ body; metadata }: Parquet.t)) body_string_gen file_metadata_gen

let file_arb = Arbitrary.make ~print:print_file file_gen

let base_schema_root: Parquet.schema_element = {
  type_ = None;
  type_length = None;
  repetition_type = None;
  name = "schema";
  num_children = Some 1;
  converted_type = None;
  scale = None;
  precision = None;
  field_id = None;
}

let base_schema_leaf: Parquet.schema_element = {
  type_ = Some Parquet.Int32;
  type_length = None;
  repetition_type = Some Parquet.Required;
  name = "field";
  num_children = None;
  converted_type = None;
  scale = None;
  precision = None;
  field_id = Some 1;
}

let base_column_metadata: Parquet.column_metadata = {
  type_ = Parquet.Int32;
  encodings = [ Parquet.Plain ];
  path_in_schema = [ "field" ];
  codec = Parquet.Uncompressed;
  num_values = 0L;
  total_uncompressed_size = 0L;
  total_compressed_size = 0L;
  key_value_metadata = None;
  data_page_offset = 0L;
  index_page_offset = None;
  dictionary_page_offset = None;
  encoding_stats = None;
  bloom_filter_offset = None;
  bloom_filter_length = None;
}

let base_column_chunk: Parquet.column_chunk = {
  file_path = None;
  file_offset = 0L;
  meta_data = Some base_column_metadata;
  offset_index_offset = None;
  offset_index_length = None;
  column_index_offset = None;
  column_index_length = None;
  encrypted_column_metadata = None;
}

let base_row_group: Parquet.row_group = {
  columns = [ base_column_chunk ];
  total_byte_size = 0L;
  num_rows = 0L;
  sorting_columns = None;
  file_offset = None;
  total_compressed_size = None;
  ordinal = None;
}

let base_metadata: Parquet.file_metadata = {
  version = 1;
  schema = [ base_schema_root; base_schema_leaf ];
  num_rows = 0L;
  row_groups = [];
  key_value_metadata = None;
  created_by = Some "riot/parquet";
  column_orders = Some [ Parquet.Type_defined_order ];
}

let file_metadata_with_key_value = fun value ->
  { base_metadata with key_value_metadata = Some [ value ] }

let file_metadata_with_schema_element = fun value ->
  { base_metadata with schema = [ base_schema_root; value ] }

let file_metadata_with_page_encoding_stats = fun value ->
  let column_metadata = { base_column_metadata with encoding_stats = Some [ value ] } in
  let column_chunk = { base_column_chunk with meta_data = Some column_metadata } in
  let row_group = { base_row_group with columns = [ column_chunk ] } in
  { base_metadata with row_groups = [ row_group ] }

let file_metadata_with_column_metadata = fun value ->
  let column_chunk = { base_column_chunk with meta_data = Some value } in
  let row_group = { base_row_group with columns = [ column_chunk ] } in
  { base_metadata with row_groups = [ row_group ] }

let file_metadata_with_column_chunk = fun value ->
  let row_group = { base_row_group with columns = [ value ] } in
  { base_metadata with row_groups = [ row_group ] }

let file_metadata_with_row_group = fun value -> { base_metadata with row_groups = [ value ] }

let file_of_metadata = fun metadata : Parquet.t -> { body = ""; metadata }

let run_property = fun ?(examples = primitive_examples) name arb predicate ->
  let config = { Property.default_config with test_count = examples } in
  let prop = Property.for_all arb predicate in
  Test.property ~size:Test.Large name ~examples
    (fun _ctx ->
      match Property.check ~config ~on_progress:(Test.Context.emit_progress _ctx) prop with
      | Property.Success -> Ok ()
      | Property.Failure { counter_example; shrink_steps } -> Error (String.concat
        "\n"
        [
          "Property failed";
          "Counter-example (after " ^ Int.to_string shrink_steps ^ " shrink steps):";
          counter_example;
        ])
      | Property.Error { exception_; backtrace } -> Error (String.concat
        "\n"
        [ "Exception raised:"; Kernel.Exception.to_string exception_; backtrace; ])
      | Property.Assumption_violated -> Error "Too many test cases violated assumptions (>10x test count)")

let metadata_roundtrip = fun wrap extract value ->
  match Parquet.encode_metadata (wrap value) with
  | Ok encoded -> (
      match Parquet.decode_metadata encoded with
      | Ok decoded -> extract decoded = value
      | Error err -> Property.fail ("metadata decode failed: " ^ Parquet.Error.to_string err)
    )
  | Error err -> Property.fail ("metadata encode failed: " ^ Parquet.Error.to_string err)

let file_roundtrip = fun value ->
  match Parquet.to_string value with
  | Ok encoded -> (
      match Parquet.from_string encoded with
      | Ok decoded -> decoded = value
      | Error err -> Property.fail ("file decode failed: " ^ Parquet.Error.to_string err)
    )
  | Error err -> Property.fail ("file encode failed: " ^ Parquet.Error.to_string err)

let file_io_roundtrip = fun value ->
  let buffer = IO.Buffer.create ~size:256 in
  match Parquet.to_writer (io_writer_of_buffer buffer) value with
  | Ok () -> (
      match Parquet.from_reader
        (String.to_reader ~chunk_size:io_chunk_size (IO.Buffer.contents buffer)) with
      | Ok decoded -> decoded = value
      | Error err -> Property.fail ("reader decode failed: " ^ Parquet.Error.to_string err)
    )
  | Error err -> Property.fail ("writer encode failed: " ^ Parquet.Error.to_string err)

let extract_only_key_value = fun (metadata: Parquet.file_metadata) ->
  match metadata.key_value_metadata with
  | Some [ value ] -> value
  | _ -> Property.fail "expected wrapped metadata to contain exactly one key/value pair"

let extract_only_schema_element = fun (metadata: Parquet.file_metadata) ->
  match metadata.schema with
  | _root :: [ value ] -> value
  | _ -> Property.fail "expected wrapped metadata to contain the schema root plus one element"

let extract_only_page_encoding_stats = fun (metadata: Parquet.file_metadata) ->
  match metadata.row_groups with
  | [ row_group ] -> (
      match row_group.columns with
      | [ column_chunk ] -> (
          match column_chunk.meta_data with
          | Some column_metadata -> (
              match column_metadata.encoding_stats with
              | Some [ value ] -> value
              | _ -> Property.fail "expected wrapped metadata to contain one page encoding stats entry"
            )
          | None -> Property.fail "expected wrapped column chunk to contain metadata"
        )
      | _ -> Property.fail "expected wrapped row group to contain one column chunk"
    )
  | _ -> Property.fail "expected wrapped metadata to contain one row group"

let extract_only_column_metadata = fun (metadata: Parquet.file_metadata) ->
  match metadata.row_groups with
  | [ row_group ] -> (
      match row_group.columns with
      | [ column_chunk ] -> (
          match column_chunk.meta_data with
          | Some value -> value
          | None -> Property.fail "expected wrapped column chunk to contain metadata"
        )
      | _ -> Property.fail "expected wrapped row group to contain one column chunk"
    )
  | _ -> Property.fail "expected wrapped metadata to contain one row group"

let extract_only_column_chunk = fun (metadata: Parquet.file_metadata) ->
  match metadata.row_groups with
  | [ row_group ] -> (
      match row_group.columns with
      | [ value ] -> value
      | _ -> Property.fail "expected wrapped row group to contain one column chunk"
    )
  | _ -> Property.fail "expected wrapped metadata to contain one row group"

let extract_only_row_group = fun (metadata: Parquet.file_metadata) ->
  match metadata.row_groups with
  | [ value ] -> value
  | _ -> Property.fail "expected wrapped metadata to contain one row group"

let key_value_roundtrip_prop = run_property
  "parquet property key_value roundtrips through metadata"
  key_value_arb
  (metadata_roundtrip file_metadata_with_key_value extract_only_key_value)

let schema_element_roundtrip_prop = run_property
  "parquet property schema_element roundtrips through metadata"
  schema_element_arb
  (metadata_roundtrip file_metadata_with_schema_element extract_only_schema_element)

let page_encoding_stats_roundtrip_prop = run_property
  "parquet property page_encoding_stats roundtrip through metadata"
  page_encoding_stats_arb
  (metadata_roundtrip file_metadata_with_page_encoding_stats extract_only_page_encoding_stats)

let column_metadata_roundtrip_prop = run_property
  ~examples:composite_examples
  "parquet property column_metadata roundtrips through metadata"
  column_metadata_arb
  (metadata_roundtrip file_metadata_with_column_metadata extract_only_column_metadata)

let column_chunk_roundtrip_prop = run_property
  ~examples:composite_examples
  "parquet property column_chunk roundtrips through metadata"
  column_chunk_arb
  (metadata_roundtrip file_metadata_with_column_chunk extract_only_column_chunk)

let row_group_roundtrip_prop = run_property
  ~examples:composite_examples
  "parquet property row_group roundtrips through metadata"
  row_group_arb
  (metadata_roundtrip file_metadata_with_row_group extract_only_row_group)

let file_metadata_roundtrip_prop =
  run_property ~examples:composite_examples "parquet property file_metadata roundtrips" file_metadata_arb
    (fun value ->
      match Parquet.encode_metadata value with
      | Ok encoded -> (
          match Parquet.decode_metadata encoded with
          | Ok decoded -> decoded = value
          | Error err -> Property.fail ("metadata decode failed: " ^ Parquet.Error.to_string err)
        )
      | Error err -> Property.fail ("metadata encode failed: " ^ Parquet.Error.to_string err))

let file_roundtrip_prop = run_property
  ~examples:composite_examples
  "parquet property files roundtrip"
  file_arb
  file_roundtrip

let file_io_roundtrip_prop = run_property
  ~examples:composite_examples
  "parquet property files roundtrip over io"
  file_arb
  file_io_roundtrip

let tests = [
  key_value_roundtrip_prop;
  schema_element_roundtrip_prop;
  page_encoding_stats_roundtrip_prop;
  column_metadata_roundtrip_prop;
  column_chunk_roundtrip_prop;
  row_group_roundtrip_prop;
  file_metadata_roundtrip_prop;
  file_roundtrip_prop;
  file_io_roundtrip_prop;
]

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"parquet_property_tests" ~tests ~args ())
    ~args:Env.args
    ()
