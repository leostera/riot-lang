open Std

type error =
[
  `Msg of string
  | `Io_error of IO.error
]
module Error: sig
  type t = error
  val to_string: t -> string
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
type sorting_column = {
  column_idx: int;
  descending: bool;
  nulls_first: bool;
}
type page_encoding_stats = {
  page_type: page_type;
  encoding: encoding;
  count: int;
}
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
type footer = {
  metadata_length: int;
  encrypted_footer: bool;
}
type t = {
  body: string;
  metadata: file_metadata;
}
val decode_footer_tail: string -> (footer, error) result

val decode_metadata: string -> (file_metadata, error) result

val encode_metadata: file_metadata -> (string, error) result

module Reader: sig
  val from_string: string -> (t, error) result

  val from_reader: ('src, IO.error) IO.Reader.t -> (t, error) result
end

module Writer: sig
  val to_string: t -> (string, error) result

  val to_writer: ('dst, IO.error) IO.Writer.t -> t -> (unit, error) result
end

val from_string: string -> (t, error) result

val from_reader: ('src, IO.error) IO.Reader.t -> (t, error) result

val to_string: t -> (string, error) result

val to_writer: ('dst, IO.error) IO.Writer.t -> t -> (unit, error) result
