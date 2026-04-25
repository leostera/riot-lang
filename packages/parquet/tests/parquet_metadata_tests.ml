open Std
open Std.Result.Syntax

module Test = Std.Test

let sample_metadata: Parquet.file_metadata = {
  version = 1;
  schema = [
    {
      type_ = None;
      type_length = None;
      repetition_type = None;
      name = "schema";
      num_children = Some 1;
      converted_type = None;
      scale = None;
      precision = None;
      field_id = None
    };
    {
      type_ = Some Parquet.Int32;
      type_length = None;
      repetition_type = Some Parquet.Required;
      name = "pirate_count";
      num_children = None;
      converted_type = None;
      scale = None;
      precision = None;
      field_id = Some 7
    };
  ];
  num_rows = 0L;
  row_groups = [];
  key_value_metadata = Some [ { key = "series"; value = Some "One Piece" } ];
  created_by = Some "riot/parquet";
  column_orders = Some [ Parquet.Type_defined_order ]
}

let metadata_with_unknown_enums: Parquet.file_metadata = {
  version = 2;
  schema = [
    {
      type_ = None;
      type_length = None;
      repetition_type = None;
      name = "schema";
      num_children = Some 1;
      converted_type = None;
      scale = None;
      precision = None;
      field_id = None
    };
    {
      type_ = Some (Parquet.Unknown_physical_type 42);
      type_length = Some 4;
      repetition_type = Some (Parquet.Unknown_repetition_type 17);
      name = "mystery";
      num_children = None;
      converted_type = Some (Parquet.Unknown_converted_type 99);
      scale = Some 2;
      precision = Some 8;
      field_id = Some 9
    };
  ];
  num_rows = 3L;
  row_groups = [];
  key_value_metadata = Some [ { key = "arc"; value = Some "Water 7" } ];
  created_by = Some "riot/parquet";
  column_orders = None
}

let test_roundtrips_metadata = fun _ctx ->
  let* encoded =
    match Parquet.encode_metadata sample_metadata with
    | Ok encoded -> Ok encoded
    | Error err -> Error ("metadata encode failed: " ^ Parquet.Error.to_string err)
  in
  match Parquet.decode_metadata encoded with
  | Ok decoded when decoded = sample_metadata -> Ok ()
  | Ok _ -> Error "expected parquet metadata roundtrip to preserve the value"
  | Error err -> Error ("metadata decode failed: " ^ Parquet.Error.to_string err)

let test_preserves_unknown_enum_values = fun _ctx ->
  let* encoded =
    match Parquet.encode_metadata metadata_with_unknown_enums with
    | Ok encoded -> Ok encoded
    | Error err -> Error ("metadata encode failed: " ^ Parquet.Error.to_string err)
  in
  match Parquet.decode_metadata encoded with
  | Ok decoded when decoded = metadata_with_unknown_enums -> Ok ()
  | Ok _ -> Error "expected parquet metadata roundtrip to preserve unknown enum codes"
  | Error err -> Error ("metadata decode failed: " ^ Parquet.Error.to_string err)

let test_rejects_trailing_metadata_bytes = fun _ctx ->
  let* encoded =
    match Parquet.encode_metadata sample_metadata with
    | Ok encoded -> Ok encoded
    | Error err -> Error ("metadata encode failed: " ^ Parquet.Error.to_string err)
  in
  match Parquet.decode_metadata (encoded ^ "\000") with
  | Ok _ -> Error "expected trailing thrift metadata bytes to be rejected"
  | Error _ -> Ok ()

let tests = [ Test.case "parquet metadata roundtrips" test_roundtrips_metadata; Test.case "parquet metadata preserves unknown enum values" test_preserves_unknown_enum_values; Test.case "parquet metadata rejects trailing bytes" test_rejects_trailing_metadata_bytes ]

let main ~args = Test.Cli.main ~name:"parquet_metadata_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
