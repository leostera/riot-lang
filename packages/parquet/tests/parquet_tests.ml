open Std
open Std.Result.Syntax

module Test = Std.Test

let io_writer_of_buffer =
  let module Write = struct
    type t = IO.Buffer.t

    type err = IO.error

    let write = fun buffer ~buf ->
      IO.Buffer.add_string buffer buf;
      Ok (String.length buf)

    let write_owned_vectored = fun buffer ~bufs ->
      let written = ref 0 in
      IO.Iovec.for_each bufs ~fn:(fun { IO.Iovec.buffer = chunk; offset; length } ->
          IO.Buffer.add_subbytes buffer chunk offset length;
          written := !written + length);
      Ok !written

    let flush = fun _buffer -> Ok ()
  end in
  fun buffer ->
    IO.Writer.of_write_src (module Write) buffer

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
      field_id = None;
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
      field_id = Some 7;
    };
  ];
  num_rows = 0L;
  row_groups = [];
  key_value_metadata = Some [ { key = "series"; value = Some "One Piece" } ];
  created_by = Some "riot/parquet";
  column_orders = Some [ Parquet.Type_defined_order ];
}

let sample_file: Parquet.t = {
  body = "";
  metadata = sample_metadata;
}

let buffered_file: Parquet.t = {
  body = "\001\002\003\004";
  metadata = sample_metadata;
}

let test_roundtrips_empty_file = fun _ctx ->
  let* encoded =
    match Parquet.to_string sample_file with
    | Ok encoded -> Ok encoded
    | Error err -> Error ("parquet encode failed: " ^ Parquet.Error.to_string err)
  in
  match Parquet.from_string encoded with
  | Ok decoded when decoded = sample_file -> Ok ()
  | Ok _ -> Error "expected parquet roundtrip to preserve the file"
  | Error err -> Error ("parquet decode failed: " ^ Parquet.Error.to_string err)

let test_preserves_body_bytes = fun _ctx ->
  let* encoded =
    match Parquet.to_string buffered_file with
    | Ok encoded -> Ok encoded
    | Error err -> Error ("parquet encode failed: " ^ Parquet.Error.to_string err)
  in
  match Parquet.from_string encoded with
  | Ok decoded when decoded = buffered_file -> Ok ()
  | Ok _ -> Error "expected parquet reader to preserve body bytes between header and footer"
  | Error err -> Error ("parquet decode failed: " ^ Parquet.Error.to_string err)

let test_decodes_from_reader = fun _ctx ->
  let* encoded =
    match Parquet.to_string buffered_file with
    | Ok encoded -> Ok encoded
    | Error err -> Error ("parquet encode failed: " ^ Parquet.Error.to_string err)
  in
  match Parquet.from_reader (String.to_reader ~chunk_size:3 encoded) with
  | Ok decoded when decoded = buffered_file -> Ok ()
  | Ok _ -> Error "expected parquet reader to decode from IO.Reader"
  | Error err -> Error ("parquet reader decode failed: " ^ Parquet.Error.to_string err)

let test_writes_to_writer = fun _ctx ->
  let buffer = IO.Buffer.create ~size:128 in
  let* () =
    match Parquet.to_writer (io_writer_of_buffer buffer) sample_file with
    | Ok () -> Ok ()
    | Error err -> Error ("parquet writer encode failed: " ^ Parquet.Error.to_string err)
  in
  match Parquet.from_string (IO.Buffer.contents buffer) with
  | Ok decoded when decoded = sample_file -> Ok ()
  | Ok _ -> Error "expected parquet writer output to decode back into the original file"
  | Error err -> Error ("parquet writer output decode failed: " ^ Parquet.Error.to_string err)

let test_decodes_footer_tail = fun _ctx ->
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

let test_rejects_trailing_metadata_bytes = fun _ctx ->
  let* encoded =
    match Parquet.encode_metadata sample_metadata with
    | Ok encoded -> Ok encoded
    | Error err -> Error ("parquet metadata encode failed: " ^ Parquet.Error.to_string err)
  in
  match Parquet.decode_metadata (encoded ^ "\000") with
  | Ok _ -> Error "expected trailing thrift metadata bytes to be rejected"
  | Error _ -> Ok ()

let tests = [
  Test.case "parquet roundtrips empty files" test_roundtrips_empty_file;
  Test.case "parquet preserves body bytes" test_preserves_body_bytes;
  Test.case "parquet decodes from readers" test_decodes_from_reader;
  Test.case "parquet writes to writers" test_writes_to_writer;
  Test.case "parquet decodes footer tails" test_decodes_footer_tail;
  Test.case "parquet rejects trailing metadata bytes" test_rejects_trailing_metadata_bytes;
]

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"parquet_tests" ~tests ~args)
    ~args:Env.args
    ()
