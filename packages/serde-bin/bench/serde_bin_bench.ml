open Std
module Array = Collections.Array
module Vector = Collections.Vector
module Marshal = Stdlib.Marshal
module De = Serde.De
module Ser = Serde.Ser

let io_chunk_size = 4_096

type mode =
  | Idle
  | Named of string
  | Counted of int
  | Sampled of float

type primitive_record = {
  ready: bool;
  count: int;
  small: int32;
  big: int64;
  ratio: float;
  label: string;
  alias: string option;
  mode: mode;
  unit_value: unit;
}

type batch = {
  batch_id: int;
  name: string;
  items: primitive_record vec;
  mirrors: primitive_record array;
  featured: primitive_record;
  status: mode;
}

type dataset = {
  version: int;
  source: string;
  batches: batch vec;
  mirrors: batch array;
  flags: mode vec;
  primary: mode;
}

type primitive_field =
  | Primitive_ready
  | Primitive_count
  | Primitive_small
  | Primitive_big
  | Primitive_ratio
  | Primitive_label
  | Primitive_alias
  | Primitive_mode
  | Primitive_unit_value

type batch_field =
  | Batch_id
  | Batch_name
  | Batch_items
  | Batch_mirrors
  | Batch_featured
  | Batch_status

type dataset_field =
  | Dataset_version
  | Dataset_source
  | Dataset_batches
  | Dataset_mirrors
  | Dataset_flags
  | Dataset_primary

type primitive_builder = {
  mutable ready: bool option;
  mutable count: int option;
  mutable small: int32 option;
  mutable big: int64 option;
  mutable ratio: float option;
  mutable label: string option;
  mutable alias: string option option;
  mutable mode: mode option;
  mutable unit_value: unit option;
}

type batch_builder = {
  mutable batch_id: int option;
  mutable name: string option;
  mutable items: primitive_record vec option;
  mutable mirrors: primitive_record array option;
  mutable featured: primitive_record option;
  mutable status: mode option;
}

type dataset_builder = {
  mutable version: int option;
  mutable source: string option;
  mutable batches: batch vec option;
  mutable mirrors: batch array option;
  mutable flags: mode vec option;
  mutable primary: mode option;
}

let bench_config: Bench.bench_config = { iterations = 50; warmup = 3 }

let io_reader_of_string = fun ?(chunk_size = io_chunk_size) value ->
  let offset = ref 0 in
  let module Read = struct
    type t = string

    type err = IO.error

    let read = fun source ?timeout:_ buf ->
      let remaining = String.length source - !offset in
      if Int.equal remaining 0 then
        Ok 0
      else
        let to_read = min chunk_size (min remaining (IO.Bytes.length buf)) in
        IO.Bytes.blit_string source !offset buf 0 to_read;
        offset := !offset + to_read;
        Ok to_read

    let read_vectored = fun source bufs ->
      let total = ref 0 in
      let continue = ref true in
      IO.Iovec.iter bufs
        (fun { ba; off; len } ->
          if !continue then
            let remaining = String.length source - !offset in
            if Int.equal remaining 0 then
              continue := false
            else
              let to_read = min chunk_size (min remaining len) in
              IO.Bytes.blit_string source !offset ba off to_read;
              offset := !offset + to_read;
              total := !total + to_read;
              if Int.compare to_read len < 0 then
                continue := false);
      Ok !total
  end in
  IO.Reader.of_read_src (module Read) value

let io_writer_of_buffer =
  let module Write = struct
    type t = IO.Buffer.t

    type err = IO.error

    let write = fun buffer ~buf ->
      IO.Buffer.add_string buffer buf;
      Ok (String.length buf)

    let write_owned_vectored = fun buffer ~bufs ->
      let written = ref 0 in
      IO.Iovec.iter bufs
        (fun { ba; off; len } ->
          IO.Buffer.add_string buffer (IO.Bytes.sub_string ba off len);
          written := !written + len);
      Ok !written

    let flush = fun _buffer -> Ok ()
  end in
  fun buffer ->
    IO.Writer.of_write_src (module Write) buffer

let human_size = fun bytes ->
  if bytes >= 1_000_000 then
    Int.to_string (bytes / 1_000_000) ^ "MB"
  else if bytes >= 1_000 then
    Int.to_string (bytes / 1_000) ^ "KB"
  else
    Int.to_string bytes ^ "B"

let array_encode = Ser.array

let array_decode = De.array

let unit_encode = Ser.null

let unit_decode = De.const ()

let primitive_fields = De.fields
  [
    De.field "ready" Primitive_ready;
    De.field "count" Primitive_count;
    De.field "small" Primitive_small;
    De.field "big" Primitive_big;
    De.field "ratio" Primitive_ratio;
    De.field "label" Primitive_label;
    De.field "alias" Primitive_alias;
    De.field "mode" Primitive_mode;
    De.field "unit_value" Primitive_unit_value;
  ]

let batch_fields = De.fields
  [
    De.field "batch_id" Batch_id;
    De.field "name" Batch_name;
    De.field "items" Batch_items;
    De.field "mirrors" Batch_mirrors;
    De.field "featured" Batch_featured;
    De.field "status" Batch_status;
  ]

let dataset_fields = De.fields
  [
    De.field "version" Dataset_version;
    De.field "source" Dataset_source;
    De.field "batches" Dataset_batches;
    De.field "mirrors" Dataset_mirrors;
    De.field "flags" Dataset_flags;
    De.field "primary" Dataset_primary;
  ]

let mode_decode =
  De.variant
    [
      De.Variant.unit "Idle" Idle;
      De.Variant.newtype "Named" De.string (fun value -> Named value);
      De.Variant.newtype "Counted" De.int (fun value -> Counted value);
      De.Variant.newtype "Sampled" De.float (fun value -> Sampled value);
    ]

let mode_encode =
  Ser.variant
    [
      Ser.Variant.unit "Idle"
        (function
        | Idle -> true
        | _ -> false);
      Ser.Variant.newtype "Named" Ser.string
        (function
        | Named value -> Some value
        | _ -> None);
      Ser.Variant.newtype "Counted" Ser.int
        (function
        | Counted value -> Some value
        | _ -> None);
      Ser.Variant.newtype "Sampled" Ser.float
        (function
        | Sampled value -> Some value
        | _ -> None);
    ]

let primitive_decode =
  De.record_mut ~fields:primitive_fields
    ~create:(fun () : primitive_builder ->
      {
        ready = None;
        count = None;
        small = None;
        big = None;
        ratio = None;
        label = None;
        alias = None;
        mode = None;
        unit_value = None;
      })
    ~step:(fun reader builder field ->
      match field with
      | Some Primitive_ready -> builder.ready <- Some (De.read reader De.bool)
      | Some Primitive_count -> builder.count <- Some (De.read reader De.int)
      | Some Primitive_small -> builder.small <- Some (De.read reader De.int32)
      | Some Primitive_big -> builder.big <- Some (De.read reader De.int64)
      | Some Primitive_ratio -> builder.ratio <- Some (De.read reader De.float)
      | Some Primitive_label -> builder.label <- Some (De.read reader De.string)
      | Some Primitive_alias -> builder.alias <- Some (De.read reader (De.option De.string))
      | Some Primitive_mode -> builder.mode <- Some (De.read reader mode_decode)
      | Some Primitive_unit_value -> builder.unit_value <- Some (De.read reader unit_decode)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun (builder: primitive_builder) ->
      match (builder.ready, builder.count, builder.small, builder.big, builder.ratio, builder.label, builder.alias, builder.mode, builder.unit_value) with
      | (Some ready, Some count, Some small, Some big, Some ratio, Some label, Some alias, Some mode, Some unit_value) ->
          ({ ready; count; small; big; ratio; label; alias; mode; unit_value }: primitive_record)
      | _ -> De.missing_field ())

let primitive_encode =
  Ser.record
    (Ser.fields
      [
        Ser.field "ready" Ser.bool (fun (value: primitive_record) -> value.ready);
        Ser.field "count" Ser.int (fun (value: primitive_record) -> value.count);
        Ser.field "small" Ser.int32 (fun (value: primitive_record) -> value.small);
        Ser.field "big" Ser.int64 (fun (value: primitive_record) -> value.big);
        Ser.field "ratio" Ser.float (fun (value: primitive_record) -> value.ratio);
        Ser.field "label" Ser.string (fun (value: primitive_record) -> value.label);
        Ser.field "alias" (Ser.option Ser.string) (fun (value: primitive_record) -> value.alias);
        Ser.field "mode" mode_encode (fun (value: primitive_record) -> value.mode);
        Ser.field "unit_value" unit_encode (fun (value: primitive_record) -> value.unit_value);
      ])

let batch_decode =
  De.record_mut ~fields:batch_fields
    ~create:(fun () : batch_builder ->
      { batch_id = None; name = None; items = None; mirrors = None; featured = None; status = None })
    ~step:(fun reader builder field ->
      match field with
      | Some Batch_id -> builder.batch_id <- Some (De.read reader De.int)
      | Some Batch_name -> builder.name <- Some (De.read reader De.string)
      | Some Batch_items -> builder.items <- Some (De.read reader (De.list primitive_decode))
      | Some Batch_mirrors -> builder.mirrors <- Some (De.read reader (array_decode primitive_decode))
      | Some Batch_featured -> builder.featured <- Some (De.read reader primitive_decode)
      | Some Batch_status -> builder.status <- Some (De.read reader mode_decode)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun (builder: batch_builder) ->
      match (builder.batch_id, builder.name, builder.items, builder.mirrors, builder.featured, builder.status) with
      | (Some batch_id, Some name, Some items, Some mirrors, Some featured, Some status) ->
          ({ batch_id; name; items; mirrors; featured; status }: batch)
      | _ -> De.missing_field ())

let batch_encode =
  Ser.record
    (Ser.fields
      [
        Ser.field "batch_id" Ser.int (fun (value: batch) -> value.batch_id);
        Ser.field "name" Ser.string (fun (value: batch) -> value.name);
        Ser.field "items" (Ser.list primitive_encode) (fun (value: batch) -> value.items);
        Ser.field "mirrors" (array_encode primitive_encode) (fun (value: batch) -> value.mirrors);
        Ser.field "featured" primitive_encode (fun (value: batch) -> value.featured);
        Ser.field "status" mode_encode (fun (value: batch) -> value.status);
      ])

let dataset_decode =
  De.record_mut ~fields:dataset_fields
    ~create:(fun () : dataset_builder ->
      { version = None; source = None; batches = None; mirrors = None; flags = None; primary = None })
    ~step:(fun reader builder field ->
      match field with
      | Some Dataset_version -> builder.version <- Some (De.read reader De.int)
      | Some Dataset_source -> builder.source <- Some (De.read reader De.string)
      | Some Dataset_batches -> builder.batches <- Some (De.read reader (De.list batch_decode))
      | Some Dataset_mirrors -> builder.mirrors <- Some (De.read reader (array_decode batch_decode))
      | Some Dataset_flags -> builder.flags <- Some (De.read reader (De.list mode_decode))
      | Some Dataset_primary -> builder.primary <- Some (De.read reader mode_decode)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun (builder: dataset_builder) ->
      match (builder.version, builder.source, builder.batches, builder.mirrors, builder.flags, builder.primary) with
      | (Some version, Some source, Some batches, Some mirrors, Some flags, Some primary) ->
          ({ version; source; batches; mirrors; flags; primary }: dataset)
      | _ -> De.missing_field ())

let dataset_encode =
  Ser.record
    (Ser.fields
      [
        Ser.field "version" Ser.int (fun (value: dataset) -> value.version);
        Ser.field "source" Ser.string (fun (value: dataset) -> value.source);
        Ser.field "batches" (Ser.list batch_encode) (fun (value: dataset) -> value.batches);
        Ser.field "mirrors" (array_encode batch_encode) (fun (value: dataset) -> value.mirrors);
        Ser.field "flags" (Ser.list mode_encode) (fun (value: dataset) -> value.flags);
        Ser.field "primary" mode_encode (fun (value: dataset) -> value.primary);
      ])

let build_mode = fun seed ->
  match seed mod 4 with
  | 0 -> Idle
  | 1 -> Named ("mode-" ^ Int.to_string seed)
  | 2 -> Counted (seed * 17)
  | _ -> Sampled ((float seed /. 7.0) +. 0.125)

let build_primitive_record = fun batch_index item_index ->
  let seed = (batch_index * 97) + item_index in
  ({
      ready = seed land 1 = 0;
      count = seed;
      small = Int32.of_int ((batch_index lsl 8) lxor item_index);
      big = Int64.of_int ((seed * 65_537) + batch_index);
      ratio = (float seed /. 9.0) +. 0.375;
      label = "record-" ^ Int.to_string batch_index ^ "-" ^ Int.to_string item_index;
      alias =
        if seed mod 3 = 0 then
          Some ("alias-" ^ Int.to_string seed)
        else
          None;
      mode = build_mode seed;
      unit_value = ();
    }: primitive_record)

let build_batch = fun batch_index ->
  let items = Vector.with_capacity 12 in
  for item_index = 0 to 11 do
    Vector.push items (build_primitive_record batch_index item_index)
  done;
  let mirrors = Array.init 6 (fun mirror_index -> build_primitive_record batch_index (mirror_index + 32)) in
  ({
      batch_id = batch_index;
      name = "batch-" ^ Int.to_string batch_index;
      items;
      mirrors;
      featured = build_primitive_record batch_index 99;
      status = build_mode (batch_index * 11);
    }: batch)

type fixture_spec = {
  label: string;
  source: string;
  batch_count: int;
  mirror_count: int;
}

let small_fixture_spec = {
  label = "small";
  source = "serde-bin primitive benchmark";
  batch_count = 128;
  mirror_count = 24;
}

let large_fixture_spec = {
  label = "large";
  source = "serde-bin large primitive benchmark";
  batch_count = 7_424;
  mirror_count = 1_392;
}

let build_dataset = fun spec ->
  let batches = Vector.with_capacity spec.batch_count in
  for batch_index = 0 to spec.batch_count - 1 do
    Vector.push batches (build_batch batch_index)
  done;
  let mirrors = Array.init spec.mirror_count (fun index -> build_batch (index + 512)) in
  let flags = Vector.with_capacity 64 in
  for index = 0 to 63 do
    Vector.push flags (build_mode (index + 2_000))
  done;
  ({ version = 2; source = spec.source; batches; mirrors; flags; primary = Sampled 3.1415926535 }: dataset)

type fixture = {
  label: string;
  dataset: dataset;
  serde_bytes: string;
  marshal_bytes: string;
  writer_buffer: IO.Buffer.t;
  writer: (IO.Buffer.t, IO.error) IO.Writer.t;
}

let build_fixture = fun spec ->
  let dataset = build_dataset spec in
  let serde_bytes =
    Serde_bin.to_string dataset_encode dataset
    |> Result.expect ~msg:"expected serde-bin fixture encoding to succeed"
  in
  let marshal_bytes = Marshal.to_string dataset [] in
  let decoded: dataset =
    Serde_bin.of_string dataset_decode serde_bytes
    |> Result.expect ~msg:"expected serde-bin fixture decoding to succeed"
  in
  let _marshal_roundtrip: dataset = Marshal.from_string marshal_bytes 0 in
  ignore decoded;
  let writer_buffer = IO.Buffer.create (String.length serde_bytes) in
  let writer = io_writer_of_buffer writer_buffer in
  { label = spec.label; dataset; serde_bytes; marshal_bytes; writer_buffer; writer }

let bench_encode_serde_in_memory = fun fixture () ->
  ignore (Serde_bin.to_string dataset_encode fixture.dataset)

let bench_encode_serde_writer = fun fixture () ->
  IO.Buffer.clear fixture.writer_buffer;
  ignore (Serde_bin.to_writer dataset_encode fixture.writer fixture.dataset)

let bench_encode_marshal = fun fixture () ->
  ignore (Marshal.to_string fixture.dataset [])

let bench_decode_serde_in_memory = fun fixture () ->
  ignore (Serde_bin.of_string dataset_decode fixture.serde_bytes)

let bench_decode_serde_reader = fun fixture () ->
  ignore (Serde_bin.of_reader dataset_decode (io_reader_of_string fixture.serde_bytes))

let bench_decode_marshal = fun fixture () ->
  let _decoded: dataset = Marshal.from_string fixture.marshal_bytes 0 in
  ()

let benchmark_suite = fun fixture ->
  let serde_size = human_size (String.length fixture.serde_bytes) in
  let marshal_size = human_size (String.length fixture.marshal_bytes) in
  Bench.[
    with_config
      ~config:bench_config
      ("serde-bin encode in-memory " ^ fixture.label ^ " dataset (" ^ serde_size ^ ")")
      (bench_encode_serde_in_memory fixture);
    with_config
      ~config:bench_config
      ("serde-bin encode writer " ^ fixture.label ^ " dataset (" ^ serde_size ^ ")")
      (bench_encode_serde_writer fixture);
    with_config
      ~config:bench_config
      ("Stdlib.Marshal encode " ^ fixture.label ^ " dataset (" ^ marshal_size ^ ")")
      (bench_encode_marshal fixture);
    with_config
      ~config:bench_config
      ("serde-bin decode in-memory " ^ fixture.label ^ " dataset (" ^ serde_size ^ ")")
      (bench_decode_serde_in_memory fixture);
    with_config
      ~config:bench_config
      ("serde-bin decode reader " ^ fixture.label ^ " dataset (" ^ serde_size ^ ")")
      (bench_decode_serde_reader fixture);
    with_config
      ~config:bench_config
      ("Stdlib.Marshal decode " ^ fixture.label ^ " dataset (" ^ marshal_size ^ ")")
      (bench_decode_marshal fixture);
  ]

let () =
  Actors.run
    ~main:(fun ~args ->
      let small_fixture = build_fixture small_fixture_spec in
      let large_fixture = build_fixture large_fixture_spec in
      let benchmarks = benchmark_suite small_fixture @ benchmark_suite large_fixture in
      Bench.Cli.main ~name:"serde-bin benchmarks" ~benchmarks ~args)
    ~args:Env.args
    ()
