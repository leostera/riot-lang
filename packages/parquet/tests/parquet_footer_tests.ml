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

let sample_file: Parquet.t = { body = ""; metadata = sample_metadata }

let test_decodes_footer_tail_from_encoded_file = fun _ctx ->
  let* encoded =
    match Parquet.to_string sample_file with
    | Ok encoded -> Ok encoded
    | Error err -> Error ("parquet encode failed: " ^ Parquet.Error.to_string err)
  in
  let footer = String.sub encoded ~offset:(String.length encoded - 8) ~len:8 in
  match Parquet.decode_footer_tail footer with
  | Ok decoded when Int.equal decoded.metadata_length (String.length encoded - 12) && Bool.equal decoded.encrypted_footer false -> Ok ()
  | Ok _ -> Error "expected parquet footer tail to report the metadata length"
  | Error err -> Error ("parquet footer decode failed: " ^ Parquet.Error.to_string err)

let test_rejects_invalid_footer_magic = fun _ctx ->
  match Parquet.decode_footer_tail "\000\000\000\000WRNG" with
  | Ok _ -> Error "expected parquet footer parsing to reject invalid magic bytes"
  | Error _ -> Ok ()

let test_rejects_wrong_footer_size = fun _ctx ->
  match Parquet.decode_footer_tail "PAR1" with
  | Ok _ -> Error "expected parquet footer parsing to reject short inputs"
  | Error _ -> Ok ()

let tests = [ Test.case "parquet decodes footer tails from encoded files" test_decodes_footer_tail_from_encoded_file; Test.case "parquet rejects invalid footer magic" test_rejects_invalid_footer_magic; Test.case "parquet rejects wrong footer sizes" test_rejects_wrong_footer_size ]

let main ~args = Test.Cli.main ~name:"parquet_footer_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
