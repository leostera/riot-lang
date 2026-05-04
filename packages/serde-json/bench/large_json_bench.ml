open Std

module Vector = Collections.Vector

let ( let* ) value fn = Result.and_then value ~fn

module Json = Data.Json
module De = Serde.De
module Ser = Serde.Ser

type child = {
  owner: string;
  score: float;
  flags: bool vec;
}

type item = {
  id: int;
  name: string;
  active: bool;
  tags: string vec;
  metrics: int vec;
  child: child;
  note: string option;
}

type dataset = {
  version: int;
  source: string;
  items: item vec;
}

type child_field =
  | Child_owner
  | Child_score
  | Child_flags
  | Child_unknown

type item_field =
  | Item_id
  | Item_name
  | Item_active
  | Item_tags
  | Item_metrics
  | Item_child
  | Item_note
  | Item_unknown

type dataset_field =
  | Dataset_version
  | Dataset_source
  | Dataset_items
  | Dataset_unknown

type child_builder = {
  mutable owner: string option;
  mutable score: float option;
  mutable flags: bool vec option;
}

type item_builder = {
  mutable id: int option;
  mutable name: string option;
  mutable active: bool option;
  mutable tags: string vec option;
  mutable metrics: int vec option;
  mutable child: child option;
  mutable note: string option option;
}

type dataset_builder = {
  mutable version: int option;
  mutable source: string option;
  mutable items: item vec option;
}

type fixture = {
  json: Json.t;
  dataset: dataset;
  text: string;
  bytes: int;
}

let bench_config: Bench.bench_config = { iterations = 20; warmup = 1 }

let large_bench_config: Bench.bench_config = { iterations = 3; warmup = 0 }

let fixture_path = Path.v "packages/serde-json/bench/fixtures/large_payload.json"

let large_fixture_path = Path.v "_build/bench/serde-json/fixtures/large_payload_100mb.json"

let large_fixture_target_bytes = 100_000_000

let human_size = fun bytes ->
  if bytes >= 1_000_000 then
    Int.to_string (bytes / 1_000_000) ^ "MB"
  else if bytes >= 1_000 then
    Int.to_string (bytes / 1_000) ^ "KB"
  else
    Int.to_string bytes ^ "B"

let normalize_json = fun __tmp1 ->
  match __tmp1 with
  | Json.Embed value -> value
  | value -> value

let child_fields =
  De.fields
    [ De.field "owner" Child_owner; De.field "score" Child_score; De.field "flags" Child_flags ]

let item_fields =
  De.fields
    [
      De.field "id" Item_id;
      De.field "name" Item_name;
      De.field "active" Item_active;
      De.field "tags" Item_tags;
      De.field "metrics" Item_metrics;
      De.field "child" Item_child;
      De.field "note" Item_note;
    ]

let dataset_fields =
  De.fields
    [
      De.field "version" Dataset_version;
      De.field "source" Dataset_source;
      De.field "items" Dataset_items;
    ]

let expect_field = fun name json ->
  match Json.get_field name (normalize_json json) with
  | Some value -> Ok value
  | None -> Error ("missing field " ^ name)

let expect_string = fun __tmp1 ->
  match __tmp1 with
  | Json.String value -> Ok value
  | _ -> Error "expected string"

let expect_int = fun __tmp1 ->
  match __tmp1 with
  | Json.Int value -> Ok value
  | _ -> Error "expected int"

let expect_float = fun __tmp1 ->
  match __tmp1 with
  | Json.Float value -> Ok value
  | Json.Int value -> Ok (Float.from_int value)
  | _ -> Error "expected float"

let expect_bool = fun __tmp1 ->
  match __tmp1 with
  | Json.Bool value -> Ok value
  | _ -> Error "expected bool"

let expect_array = fun __tmp1 ->
  match __tmp1 with
  | Json.Array values -> Ok values
  | _ -> Error "expected array"

let expect_object = fun __tmp1 ->
  match __tmp1 with
  | Json.Object _ as value -> Ok value
  | _ -> Error "expected object"

let expect_field_as = fun name json decode ->
  let* value = expect_field name json in
  decode (normalize_json value)

let vec_to_list = fun values ->
  let items = ref [] in
  Vector.for_each values ~fn:(fun value -> items := value :: !items);
  List.rev !items

let vec_map_to_list = fun map values ->
  let items = ref [] in
  Vector.for_each values ~fn:(fun value -> items := map value :: !items);
  List.rev !items

let vec_get_exn = fun values index ->
  match Vector.get values ~at:index with
  | Some value -> value
  | None -> panic "large_json_bench.vec_get_exn: index out of bounds"

let decode_vec = fun values decode ->
  let decoded = Vector.with_capacity ~size:(List.length values) in
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok decoded
    | value :: rest ->
        let* value = decode value in
        Vector.push decoded ~value;
        loop rest
  in
  loop values

let child_decode =
  De.record_mut
    ~fields:child_fields
    ~create:(fun (): child_builder -> { owner = None; score = None; flags = None })
    ~step:(fun reader builder next ->
      match next with
      | Some Child_owner -> builder.owner <- Some (De.read reader De.string)
      | Some Child_score -> builder.score <- Some (De.read reader De.float)
      | Some Child_flags -> builder.flags <- Some (De.read reader (De.list De.bool))
      | Some Child_unknown
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun (builder: child_builder) ->
      match (builder.owner, builder.score, builder.flags) with
      | (Some owner, Some score, Some flags) -> ({ owner; score; flags }: child)
      | _ -> De.missing_field ())

let item_decode =
  De.record_mut
    ~fields:item_fields
    ~create:(fun (): item_builder ->
      {
        id = None;
        name = None;
        active = None;
        tags = None;
        metrics = None;
        child = None;
        note = None;
      })
    ~step:(fun reader builder next ->
      match next with
      | Some Item_id -> builder.id <- Some (De.read reader De.int)
      | Some Item_name -> builder.name <- Some (De.read reader De.string)
      | Some Item_active -> builder.active <- Some (De.read reader De.bool)
      | Some Item_tags -> builder.tags <- Some (De.read reader (De.list De.string))
      | Some Item_metrics -> builder.metrics <- Some (De.read reader (De.list De.int))
      | Some Item_child -> builder.child <- Some (De.read reader child_decode)
      | Some Item_note -> builder.note <- Some (De.read reader (De.option De.string))
      | Some Item_unknown
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun (builder: item_builder) ->
      match (
        builder.id,
        builder.name,
        builder.active,
        builder.tags,
        builder.metrics,
        builder.child,
        builder.note
      ) with
      | (Some id, Some name, Some active, Some tags, Some metrics, Some child, Some note) ->
          ({
            id;
            name;
            active;
            tags;
            metrics;
            child;
            note;
          }: item)
      | _ -> De.missing_field ())

let dataset_decode =
  De.record_mut
    ~fields:dataset_fields
    ~create:(fun (): dataset_builder -> { version = None; source = None; items = None })
    ~step:(fun reader builder next ->
      match next with
      | Some Dataset_version -> builder.version <- Some (De.read reader De.int)
      | Some Dataset_source -> builder.source <- Some (De.read reader De.string)
      | Some Dataset_items -> builder.items <- Some (De.read reader (De.list item_decode))
      | Some Dataset_unknown
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun (builder: dataset_builder) ->
      match (builder.version, builder.source, builder.items) with
      | (Some version, Some source, Some items) -> ({ version; source; items }: dataset)
      | _ -> De.missing_field ())

let child_encode =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "owner" Ser.string (fun (value: child) -> value.owner);
          Ser.field "score" Ser.float (fun (value: child) -> value.score);
          Ser.field "flags" (Ser.list Ser.bool) (fun (value: child) -> value.flags);
        ]
    )

let item_encode =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "id" Ser.int (fun (value: item) -> value.id);
          Ser.field "name" Ser.string (fun (value: item) -> value.name);
          Ser.field "active" Ser.bool (fun (value: item) -> value.active);
          Ser.field "tags" (Ser.list Ser.string) (fun (value: item) -> value.tags);
          Ser.field "metrics" (Ser.list Ser.int) (fun (value: item) -> value.metrics);
          Ser.field "child" child_encode (fun (value: item) -> value.child);
          Ser.field "note" (Ser.option Ser.string) (fun (value: item) -> value.note);
        ]
    )

let dataset_encode =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "version" Ser.int (fun (value: dataset) -> value.version);
          Ser.field "source" Ser.string (fun (value: dataset) -> value.source);
          Ser.field "items" (Ser.list item_encode) (fun (value: dataset) -> value.items);
        ]
    )

let manual_bool_vec = fun values ->
  decode_vec
    values
    (fun value -> expect_bool (normalize_json value))

let manual_int_vec = fun values ->
  decode_vec
    values
    (fun value -> expect_int (normalize_json value))

let manual_string_vec = fun values ->
  decode_vec
    values
    (fun value -> expect_string (normalize_json value))

let rec manual_items_of_json = fun values -> decode_vec values manual_item_of_json

and manual_child_of_json = fun json ->
  let* json = expect_object (normalize_json json) in
  let* owner = expect_field_as "owner" json expect_string in
  let* score = expect_field_as "score" json expect_float in
  let* flags =
    expect_field_as
      "flags"
      json
      (fun value ->
        let* values = expect_array value in
        manual_bool_vec values)
  in
  Ok ({ owner; score; flags }: child)

and manual_item_of_json = fun json ->
  let* json = expect_object (normalize_json json) in
  let* id = expect_field_as "id" json expect_int in
  let* name = expect_field_as "name" json expect_string in
  let* active = expect_field_as "active" json expect_bool in
  let* tags =
    expect_field_as
      "tags"
      json
      (fun value ->
        let* values = expect_array value in
        manual_string_vec values)
  in
  let* metrics =
    expect_field_as
      "metrics"
      json
      (fun value ->
        let* values = expect_array value in
        manual_int_vec values)
  in
  let* child = expect_field_as "child" json manual_child_of_json in
  let* note =
    expect_field_as
      "note"
      json
      (fun value ->
        match normalize_json value with
        | Json.Null -> Ok None
        | value ->
            let* note = expect_string value in
            Ok (Some note))
  in
  Ok ({
    id;
    name;
    active;
    tags;
    metrics;
    child;
    note;
  }: item)

let manual_dataset_of_json = fun json ->
  let* json = expect_object (normalize_json json) in
  let* version = expect_field_as "version" json expect_int in
  let* source = expect_field_as "source" json expect_string in
  let* items =
    expect_field_as
      "items"
      json
      (fun value ->
        let* values = expect_array value in
        manual_items_of_json values)
  in
  Ok ({ version; source; items }: dataset)

let rec child_to_json = fun (value: child) ->
  Json.Object [
    ("owner", Json.String value.owner);
    ("score", Json.Float value.score);
    ("flags", Json.Array (vec_map_to_list (fun flag -> Json.Bool flag) value.flags));
  ]

and item_to_json = fun (value: item) ->
  Json.Object [
    ("id", Json.Int value.id);
    ("name", Json.String value.name);
    ("active", Json.Bool value.active);
    ("tags", Json.Array (vec_map_to_list (fun tag -> Json.String tag) value.tags));
    ("metrics", Json.Array (vec_map_to_list (fun metric -> Json.Int metric) value.metrics));
    ("child", child_to_json value.child);
    ("note", match value.note with
    | None -> Json.Null
    | Some note -> Json.String note);
  ]

let dataset_to_json = fun (value: dataset) ->
  Json.Object [
    ("version", Json.Int value.version);
    ("source", Json.String value.source);
    ("items", Json.Array (vec_map_to_list item_to_json value.items));
  ]

let parse_json = fun text ->
  Json.from_string text
  |> Result.expect ~msg:"expected benchmark payload to parse as JSON"

let read_fixture_text = fun () ->
  Fs.read_to_string fixture_path
  |> Result.expect ~msg:("expected benchmark fixture at " ^ Path.to_string fixture_path)

let load_fixture = fun () ->
  let text = read_fixture_text () in
  let bytes = String.length text in
  let json = parse_json text in
  let dataset =
    manual_dataset_of_json json
    |> Result.expect ~msg:"expected benchmark payload to decode into the typed dataset"
  in
  {
    json;
    dataset;
    text;
    bytes;
  }

let repeat_items = fun (items: item vec) ~count ->
  let base_count = Vector.len items in
  if Int.equal base_count 0 then
    Vector.create ()
  else
    let repeated = Vector.with_capacity ~size:count in
    for index = 0 to count - 1 do
      let template: item = vec_get_exn items (index mod base_count) in
      let item: item = {
        template with
        id = template.id + index;
        name = template.name ^ "-" ^ Int.to_string index;
        note =
          match template.note with
          | None -> None
          | Some note ->
              Some (note ^ "-" ^ Int.to_string index);
      }
      in
      Vector.push repeated ~value:item
    done;
  repeated

let dataset_with_target_size = fun (base_dataset: dataset) ~target_bytes ->
  let base_count = Vector.len base_dataset.items in
  if Int.equal base_count 0 then
    panic "expected benchmark fixture to contain at least one item"
  else
    let rec grow current_count =
      let dataset: dataset = {
        version = base_dataset.version;
        source = base_dataset.source ^ "-100mb";
        items = repeat_items base_dataset.items ~count:current_count;
      }
      in
      let text =
        Serde_json.to_string dataset_encode dataset
        |> Result.expect ~msg:"expected generated 100MB benchmark fixture to serialize"
      in
      if String.length text >= target_bytes then
        (dataset, text)
      else
        let scaled_count =
          max
            (current_count + base_count)
            ((current_count * target_bytes) / max 1 (String.length text))
        in
        grow scaled_count
    in
    grow base_count

let ensure_fixture_parent_dir = fun path ->
  match Path.parent path with
  | None -> ()
  | Some parent ->
      Fs.create_dir_all parent
      |> Result.expect
        ~msg:("expected benchmark fixture directory to be creatable: " ^ Path.to_string parent)

let ensure_large_fixture_text = fun base_fixture ->
  match Fs.exists large_fixture_path with
  | Ok true ->
      let text =
        Fs.read_to_string large_fixture_path
        |> Result.expect
          ~msg:("expected large benchmark fixture at " ^ Path.to_string large_fixture_path)
      in
      if String.length text >= large_fixture_target_bytes then
        text
      else
        (
          let (dataset, text) =
            dataset_with_target_size base_fixture.dataset ~target_bytes:large_fixture_target_bytes
          in
          ignore dataset;
          ensure_fixture_parent_dir large_fixture_path;
          Fs.write text large_fixture_path
          |> Result.expect
            ~msg:("expected large benchmark fixture to be writable at "
            ^ Path.to_string large_fixture_path);
          text
        )
  | Ok false ->
      let (dataset, text) =
        dataset_with_target_size base_fixture.dataset ~target_bytes:large_fixture_target_bytes
      in
      ignore dataset;
      ensure_fixture_parent_dir large_fixture_path;
      Fs.write text large_fixture_path
      |> Result.expect
        ~msg:("expected large benchmark fixture to be writable at "
        ^ Path.to_string large_fixture_path);
      text
  | Error err -> panic ("expected benchmark fixture path to be accessible: " ^ IO.error_message err)

let decode_serde = fun text ->
  Serde_json.from_string dataset_decode text
  |> Result.expect ~msg:"expected fast serde benchmark decode to succeed"

let decode_serde_reader = fun text ->
  match Serde_json.from_reader dataset_decode (String.to_reader text) with
  | Ok value -> value
  | Error err ->
      panic ("expected serde reader benchmark decode to succeed: " ^ Serde.Error.to_string err)

let decode_serde_reader_buffered = fun ~chunk_size text ->
  match Serde_json.from_reader dataset_decode (String.to_reader ~chunk_size text) with
  | Ok value -> value
  | Error err ->
      panic
        ("expected buffered serde reader benchmark decode to succeed: " ^ Serde.Error.to_string err)

let decode_serde_reader_chunked = fun text ->
  match Serde_json.from_reader dataset_decode (String.to_reader ~chunk_size:1 text) with
  | Ok value -> value
  | Error err ->
      panic
        ("expected chunked serde reader benchmark decode to succeed: " ^ Serde.Error.to_string err)

let load_large_fixture = fun base_fixture ->
  let text = ensure_large_fixture_text base_fixture in
  let bytes = String.length text in
  let dataset = decode_serde text in
  {
    json = Json.Null;
    dataset;
    text;
    bytes;
  }

let encode_manual = fun dataset -> Json.to_string (dataset_to_json dataset)

let encode_serde = fun dataset ->
  Serde_json.to_string dataset_encode dataset
  |> Result.expect ~msg:"expected fast serde benchmark encode to succeed"

let decode_manual = fun text ->
  let json = parse_json text in
  manual_dataset_of_json json
  |> Result.expect ~msg:"expected manual benchmark decode to succeed"

let decode_manual_from_json = fun json ->
  manual_dataset_of_json json
  |> Result.expect ~msg:"expected manual benchmark decode from parsed tree to succeed"

let bench_parse_only = fun fixture () -> ignore (parse_json fixture.text)

let bench_decode_manual_tree = fun fixture () -> ignore (decode_manual_from_json fixture.json)

let bench_decode_manual = fun fixture () -> ignore (decode_manual fixture.text)

let bench_decode_serde = fun fixture () -> ignore (decode_serde fixture.text)

let bench_decode_serde_reader = fun fixture () -> ignore (decode_serde_reader fixture.text)

let bench_decode_serde_reader_buffered = fun fixture () ->
  ignore
    (decode_serde_reader_buffered ~chunk_size:4_096 fixture.text)

let bench_decode_serde_reader_chunked = fun fixture () ->
  ignore
    (decode_serde_reader_chunked fixture.text)

let bench_encode_manual = fun fixture () -> ignore (encode_manual fixture.dataset)

let bench_encode_serde = fun fixture () -> ignore (encode_serde fixture.dataset)

let benchmark_suite = fun fixture ->
  let size = human_size fixture.bytes in
  Bench.[
    with_config ~config:bench_config ("parse json tree (" ^ size ^ ")") (bench_parse_only fixture);
    with_config
      ~config:bench_config
      ("manual decode from parsed tree (" ^ size ^ ")")
      (bench_decode_manual_tree fixture);
    with_config
      ~config:bench_config
      ("manual decode total (" ^ size ^ ")")
      (bench_decode_manual fixture);
    with_config
      ~config:bench_config
      ("serde decode total (" ^ size ^ ")")
      (bench_decode_serde fixture);
    with_config
      ~config:bench_config
      ("serde decode via direct-string reader total (" ^ size ^ ")")
      (bench_decode_serde_reader fixture);
    with_config
      ~config:bench_config
      ("serde decode via 4KiB reader total (" ^ size ^ ")")
      (bench_decode_serde_reader_buffered fixture);
    with_config
      ~config:bench_config
      ("serde decode via 1-byte reader total (" ^ size ^ ")")
      (bench_decode_serde_reader_chunked fixture);
    with_config
      ~config:bench_config
      ("manual encode total (" ^ size ^ ")")
      (bench_encode_manual fixture);
    with_config
      ~config:bench_config
      ("serde encode total (" ^ size ^ ")")
      (bench_encode_serde fixture);
  ]

let large_benchmark_suite = fun fixture ->
  let size = human_size fixture.bytes in
  Bench.[
    with_config
      ~config:large_bench_config
      ("manual decode total (" ^ size ^ ")")
      (bench_decode_manual fixture);
    with_config
      ~config:large_bench_config
      ("serde decode total (" ^ size ^ ")")
      (bench_decode_serde fixture);
    with_config
      ~config:large_bench_config
      ("serde decode via 4KiB reader total (" ^ size ^ ")")
      (bench_decode_serde_reader_buffered fixture);
    with_config
      ~config:large_bench_config
      ("manual encode total (" ^ size ^ ")")
      (bench_encode_manual fixture);
    with_config
      ~config:large_bench_config
      ("serde encode total (" ^ size ^ ")")
      (bench_encode_serde fixture);
  ]

let main ~args =
  let fixture = load_fixture () in
  let large_fixture = load_large_fixture fixture in
  Bench.Cli.main
    ~name:"serde-json large payload benchmarks"
    ~benchmarks:(benchmark_suite fixture @ large_benchmark_suite large_fixture)
    ~args

let () = Runtime.run ~main ~args:Env.args ()
