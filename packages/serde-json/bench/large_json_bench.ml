open Std
open Serde

let ( let* ) = Result.and_then

module Json = Data.Json

type child = {
  owner: string;
  score: float;
  flags: bool list;
}

type item = {
  id: int;
  name: string;
  active: bool;
  tags: string list;
  metrics: int list;
  child: child;
  note: string option;
}

type dataset = {
  version: int;
  source: string;
  items: item list;
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

type fixture = {
  json: Json.t;
  text: string;
  bytes: int;
}

let bench_config: Bench.bench_config = { iterations = 20; warmup = 1 }
let fixture_path = Path.v "packages/serde-json/bench/fixtures/large_payload.json"

let human_size = fun bytes ->
  if bytes >= 1_000_000 then
    Int.to_string (bytes / 1_000_000) ^ "MB"
  else if bytes >= 1_000 then
    Int.to_string (bytes / 1_000) ^ "KB"
  else
    Int.to_string bytes ^ "B"

let normalize_json = function
  | Json.Embed value -> value
  | value -> value

let child_fields =
  fields [
    field "owner" Child_owner;
    field "score" Child_score;
    field "flags" Child_flags;
  ]

let item_fields =
  fields [
    field "id" Item_id;
    field "name" Item_name;
    field "active" Item_active;
    field "tags" Item_tags;
    field "metrics" Item_metrics;
    field "child" Item_child;
    field "note" Item_note;
  ]

let dataset_fields =
  fields [
    field "version" Dataset_version;
    field "source" Dataset_source;
    field "items" Dataset_items;
  ]

let expect_field = fun name json ->
  match Json.get_field name (normalize_json json) with
  | Some value -> Ok value
  | None -> Error ("missing field " ^ name)

let expect_string = function
  | Json.String value -> Ok value
  | _ -> Error "expected string"

let expect_int = function
  | Json.Int value -> Ok value
  | _ -> Error "expected int"

let expect_float = function
  | Json.Float value -> Ok value
  | Json.Int value -> Ok (Float.of_int value)
  | _ -> Error "expected float"

let expect_bool = function
  | Json.Bool value -> Ok value
  | _ -> Error "expected bool"

let expect_array = function
  | Json.Array values -> Ok values
  | _ -> Error "expected array"

let expect_object = function
  | Json.Object _ as value -> Ok value
  | _ -> Error "expected object"

let expect_field_as = fun name json decode ->
  let* value = expect_field name json in
  decode (normalize_json value)

let child_decode =
  record
    ~fields:child_fields
    ~init:(None, None, None)
    ~step:(fun reader (owner, score, flags) next ->
      match next with
      | Some Child_owner ->
          (Some (read reader string), score, flags)
      | Some Child_score ->
          (owner, Some (read reader float), flags)
      | Some Child_flags ->
          (owner, score, Some (read reader (list bool)))
      | Some Child_unknown
      | None ->
          let () = read reader skip_any in
          (owner, score, flags))
    ~finish:(fun (owner, score, flags) ->
      match (owner, score, flags) with
      | (Some owner, Some score, Some flags) ->
          { owner; score; flags }
      | _ ->
          missing_field ())

let item_decode =
  record
    ~fields:item_fields
    ~init:(None, None, None, None, None, None, None)
    ~step:(fun reader (id, name, active, tags, metrics, child, note) next ->
      match next with
      | Some Item_id ->
          (Some (read reader int), name, active, tags, metrics, child, note)
      | Some Item_name ->
          (id, Some (read reader string), active, tags, metrics, child, note)
      | Some Item_active ->
          (id, name, Some (read reader bool), tags, metrics, child, note)
      | Some Item_tags ->
          (id, name, active, Some (read reader (list string)), metrics, child, note)
      | Some Item_metrics ->
          (id, name, active, tags, Some (read reader (list int)), child, note)
      | Some Item_child ->
          (id, name, active, tags, metrics, Some (read reader child_decode), note)
      | Some Item_note ->
          (id, name, active, tags, metrics, child, Some (read reader (option string)))
      | Some Item_unknown
      | None ->
          let () = read reader skip_any in
          (id, name, active, tags, metrics, child, note))
    ~finish:(fun (id, name, active, tags, metrics, child, note) ->
      match (id, name, active, tags, metrics, child, note) with
      | (Some id, Some name, Some active, Some tags, Some metrics, Some child, Some note) ->
          { id; name; active; tags; metrics; child; note }
      | _ ->
          missing_field ())

let dataset_decode =
  record
    ~fields:dataset_fields
    ~init:(None, None, None)
    ~step:(fun reader (version, source, items) next ->
      match next with
      | Some Dataset_version ->
          (Some (read reader int), source, items)
      | Some Dataset_source ->
          (version, Some (read reader string), items)
      | Some Dataset_items ->
          (version, source, Some (read reader (list item_decode)))
      | Some Dataset_unknown
      | None ->
          let () = read reader skip_any in
          (version, source, items))
    ~finish:(fun (version, source, items) ->
      match (version, source, items) with
      | (Some version, Some source, Some items) ->
          { version; source; items }
      | _ ->
          missing_field ())

let rec manual_bool_list = function
  | [] -> Ok []
  | value :: rest ->
      let* value = expect_bool (normalize_json value) in
      let* rest = manual_bool_list rest in
      Ok (value :: rest)

let rec manual_int_list = function
  | [] -> Ok []
  | value :: rest ->
      let* value = expect_int (normalize_json value) in
      let* rest = manual_int_list rest in
      Ok (value :: rest)

let rec manual_string_list = function
  | [] -> Ok []
  | value :: rest ->
      let* value = expect_string (normalize_json value) in
      let* rest = manual_string_list rest in
      Ok (value :: rest)

let rec manual_items_of_json = function
  | [] -> Ok []
  | value :: rest ->
      let* value = manual_item_of_json value in
      let* rest = manual_items_of_json rest in
      Ok (value :: rest)

and manual_child_of_json = fun json ->
  let* json = expect_object (normalize_json json) in
  let* owner = expect_field_as "owner" json expect_string in
  let* score = expect_field_as "score" json expect_float in
  let* flags = expect_field_as "flags" json (fun value ->
    let* values = expect_array value in
    manual_bool_list values)
  in
  Ok { owner; score; flags }

and manual_item_of_json = fun json ->
  let* json = expect_object (normalize_json json) in
  let* id = expect_field_as "id" json expect_int in
  let* name = expect_field_as "name" json expect_string in
  let* active = expect_field_as "active" json expect_bool in
  let* tags = expect_field_as "tags" json (fun value ->
    let* values = expect_array value in
    manual_string_list values)
  in
  let* metrics = expect_field_as "metrics" json (fun value ->
    let* values = expect_array value in
    manual_int_list values)
  in
  let* child = expect_field_as "child" json manual_child_of_json in
  let* note =
    expect_field_as "note" json (fun value ->
      match normalize_json value with
      | Json.Null -> Ok None
      | value ->
          let* note = expect_string value in
          Ok (Some note))
  in
  Ok { id; name; active; tags; metrics; child; note }

let manual_dataset_of_json = fun json ->
  let* json = expect_object (normalize_json json) in
  let* version = expect_field_as "version" json expect_int in
  let* source = expect_field_as "source" json expect_string in
  let* items = expect_field_as "items" json (fun value ->
    let* values = expect_array value in
    manual_items_of_json values)
  in
  Ok { version; source; items }

let parse_json = fun text ->
  Json.of_string text |> Result.expect ~msg:"expected benchmark payload to parse as JSON"

let read_fixture_text = fun () ->
  Fs.read_to_string fixture_path
  |> Result.expect
       ~msg:("expected benchmark fixture at " ^ Path.to_string fixture_path)

let load_fixture = fun () ->
  let text = read_fixture_text () in
  let bytes = String.length text in
  let json = parse_json text in
  { json; text; bytes }

let decode_serde = fun text ->
  Serde_json.of_string dataset_decode text
  |> Result.expect ~msg:"expected fast serde benchmark decode to succeed"

let decode_manual = fun text ->
  let json = parse_json text in
  manual_dataset_of_json json |> Result.expect ~msg:"expected manual benchmark decode to succeed"

let decode_manual_from_json = fun json ->
  manual_dataset_of_json json
  |> Result.expect ~msg:"expected manual benchmark decode from parsed tree to succeed"

let bench_parse_only = fun fixture () ->
  ignore (parse_json fixture.text)

let bench_decode_manual_tree = fun fixture () ->
  ignore (decode_manual_from_json fixture.json)

let bench_decode_manual = fun fixture () ->
  ignore (decode_manual fixture.text)

let bench_decode_serde = fun fixture () ->
  ignore (decode_serde fixture.text)

let benchmark_suite = fun fixture ->
  let size = human_size fixture.bytes in
  Bench.[
    with_config ~config:bench_config ("parse json tree (" ^ size ^ ")") (bench_parse_only fixture);
    with_config ~config:bench_config
      ("manual decode from parsed tree (" ^ size ^ ")")
      (bench_decode_manual_tree fixture);
    with_config ~config:bench_config ("manual decode total (" ^ size ^ ")") (bench_decode_manual fixture);
    with_config ~config:bench_config ("serde decode total (" ^ size ^ ")") (bench_decode_serde fixture);
  ]

let () =
  Actors.run
    ~main:(fun ~args ->
      let fixture = load_fixture () in
      Bench.Cli.main
        ~name:("serde-json large payload benchmarks (" ^ human_size fixture.bytes ^ ")")
        ~benchmarks:(benchmark_suite fixture)
        ~args)
    ~args:Env.args
    ()
