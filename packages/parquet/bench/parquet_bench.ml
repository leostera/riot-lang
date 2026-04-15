open Std

let small_bench_config: Bench.bench_config = { iterations = 100; warmup = 5 }

let large_bench_config: Bench.bench_config = { iterations = 20; warmup = 2 }

let io_chunk_size = 4_096

let io_writer_of_buffer =
  let module Write = struct
    type t = IO.Buffer.t

    type err = IO.error

    let write = fun buffer ~buf ->
      IO.Buffer.add_string buffer buf;
      Ok (String.length buf)

    let write_owned_vectored = fun buffer ~bufs ->
      let written = ref 0 in
      IO.Iovec.for_each bufs
        ~fn:(fun { IO.Iovec.buffer=chunk; offset; length } ->
          IO.Buffer.add_subbytes buffer chunk offset length;
          written := !written + length);
      Ok !written

    let flush = fun _buffer -> Ok ()
  end in
  fun buffer ->
    IO.Writer.of_write_src (module Write) buffer

type fixture_spec = {
  label: string;
  row_group_count: int;
  column_count: int;
  body_size: int;
}

type fixture = {
  label: string;
  value: Parquet.t;
  metadata_bytes: string;
  encoded: string;
}

let human_size = fun bytes ->
  if bytes >= 1_000_000 then
    Int.to_string (bytes / 1_000_000) ^ "MB"
  else if bytes >= 1_000 then
    Int.to_string (bytes / 1_000) ^ "KB"
  else
    Int.to_string bytes ^ "B"

let column_name = fun index -> "field_" ^ Int.to_string index

let schema_for_columns = fun column_count ->
  let root: Parquet.schema_element = {
    type_ = None;
    type_length = None;
    repetition_type = None;
    name = "schema";
    num_children = Some column_count;
    converted_type = None;
    scale = None;
    precision = None;
    field_id = None;
  }
  in
  let leaves =
    List.init ~count:column_count
      ~fn:(fun index ->
        ({
            type_ =
              Some (
                if Int.rem index 2 = 0 then
                  Parquet.Int32
                else
                  Parquet.Byte_array
              );
            type_length = None;
            repetition_type = Some Parquet.Optional;
            name = column_name index;
            num_children = None;
            converted_type =
              Some (
                if Int.rem index 2 = 0 then
                  Parquet.Int_32
                else
                  Parquet.Utf8
              );
            scale = None;
            precision = None;
            field_id = Some (index + 1);
          }: Parquet.schema_element))
  in
  root :: leaves

let column_metadata = fun index row_group_index ->
  let name = column_name index in
  let offset = Int64.of_int ((row_group_index * 10_000) + (index * 128)) in
  ({
      type_ =
        if Int.rem index 2 = 0 then
          Parquet.Int32
        else
          Parquet.Byte_array;
      encodings = [ Parquet.Plain; Parquet.Rle_dictionary ];
      path_in_schema = [ name ];
      codec =
        if Int.rem index 2 = 0 then
          Parquet.Uncompressed
        else
          Parquet.Snappy;
      num_values = 1_024L;
      total_uncompressed_size = 4_096L;
      total_compressed_size = 2_048L;
      key_value_metadata = Some [ { key = "column"; value = Some name } ];
      data_page_offset = offset;
      index_page_offset = Some Int64.(add offset 32L);
      dictionary_page_offset = Some Int64.(add offset 16L);
      encoding_stats = Some [
        { page_type = Parquet.Data_page; encoding = Parquet.Plain; count = 1 };
      ];
      bloom_filter_offset = Some Int64.(add offset 64L);
      bloom_filter_length = Some 16;
    }: Parquet.column_metadata)

let column_chunk = fun index row_group_index ->
  let metadata = column_metadata index row_group_index in
  ({
      file_path = None;
      file_offset = metadata.data_page_offset;
      meta_data = Some metadata;
      offset_index_offset = Some Int64.(add metadata.data_page_offset 80L);
      offset_index_length = Some 8;
      column_index_offset = Some Int64.(add metadata.data_page_offset 88L);
      column_index_length = Some 8;
      encrypted_column_metadata = None;
    }: Parquet.column_chunk)

let row_group = fun column_count row_group_index ->
  let columns =
    List.init ~count:column_count ~fn:(fun index -> column_chunk index row_group_index)
  in
  ({
      columns;
      total_byte_size = Int64.of_int (column_count * 4_096);
      num_rows = 1_024L;
      sorting_columns = Some [ { column_idx = 0; descending = false; nulls_first = true } ];
      file_offset = Some (Int64.of_int (row_group_index * 10_000));
      total_compressed_size = Some (Int64.of_int (column_count * 2_048));
      ordinal = Some row_group_index;
    }: Parquet.row_group)

let build_fixture = fun (spec: fixture_spec) ->
  let metadata: Parquet.file_metadata = {
    version = 2;
    schema = schema_for_columns spec.column_count;
    num_rows = Int64.of_int (spec.row_group_count * 1_024);
    row_groups = List.init ~count:spec.row_group_count ~fn:(row_group spec.column_count);
    key_value_metadata = Some [
      { key = "label"; value = Some spec.label };
      { key = "package"; value = Some "parquet" };
    ];
    created_by = Some "riot/parquet bench";
    column_orders = Some [ Parquet.Type_defined_order ];
  }
  in
  let value: Parquet.t = { body = String.make ~len:spec.body_size ~char:'x'; metadata } in
  let metadata_bytes = Parquet.encode_metadata metadata |> Result.expect ~msg:"bench fixture metadata should encode" in
  let encoded = Parquet.to_string value |> Result.expect ~msg:"bench fixture file should encode" in
  let decoded = Parquet.from_string encoded |> Result.expect ~msg:"bench fixture file should decode" in
  if not (decoded = value) then
    panic ("parquet_bench: fixture roundtrip failed for " ^ spec.label);
  { label = spec.label; value; metadata_bytes; encoded }

let small_fixture_spec = { label = "small"; row_group_count = 2; column_count = 4; body_size = 256 }

let large_fixture_spec = {
  label = "large";
  row_group_count = 32;
  column_count = 8;
  body_size = 1_000_000
}

let bench_encode_metadata = fun fixture () -> ignore (Parquet.encode_metadata fixture.value.metadata)

let bench_decode_metadata = fun fixture () -> ignore (Parquet.decode_metadata fixture.metadata_bytes)

let bench_encode_file = fun fixture () -> ignore (Parquet.to_string fixture.value)

let bench_encode_writer = fun fixture () ->
  let buffer = IO.Buffer.create ~size:(String.length fixture.encoded) in
  ignore (Parquet.to_writer (io_writer_of_buffer buffer) fixture.value)

let bench_decode_file = fun fixture () -> ignore (Parquet.from_string fixture.encoded)

let bench_decode_reader = fun fixture () ->
  ignore (Parquet.from_reader (String.to_reader ~chunk_size:io_chunk_size fixture.encoded))

let benchmark_suite = fun fixture ->
  let config =
    if String.equal fixture.label "small" then
      small_bench_config
    else
      large_bench_config
  in
  let file_size = human_size (String.length fixture.encoded) in
  let metadata_size = human_size (String.length fixture.metadata_bytes) in
  Bench.[
    with_config
      ~config
      ("parquet encode metadata " ^ fixture.label ^ " fixture (" ^ metadata_size ^ ")")
      (bench_encode_metadata fixture);
    with_config
      ~config
      ("parquet decode metadata " ^ fixture.label ^ " fixture (" ^ metadata_size ^ ")")
      (bench_decode_metadata fixture);
    with_config
      ~config
      ("parquet encode file " ^ fixture.label ^ " fixture (" ^ file_size ^ ")")
      (bench_encode_file fixture);
    with_config
      ~config
      ("parquet encode writer " ^ fixture.label ^ " fixture (" ^ file_size ^ ")")
      (bench_encode_writer fixture);
    with_config
      ~config
      ("parquet decode file " ^ fixture.label ^ " fixture (" ^ file_size ^ ")")
      (bench_decode_file fixture);
    with_config
      ~config
      ("parquet decode reader " ^ fixture.label ^ " fixture (" ^ file_size ^ ")")
      (bench_decode_reader fixture);
  ]

let () =
  Actors.run
    ~main:(fun ~args ->
      let small_fixture = build_fixture small_fixture_spec in
      let large_fixture = build_fixture large_fixture_spec in
      let benchmarks = benchmark_suite small_fixture @ benchmark_suite large_fixture in
      Bench.Cli.main ~name:"parquet benchmarks" ~benchmarks ~args)
    ~args:Env.args
    ()
