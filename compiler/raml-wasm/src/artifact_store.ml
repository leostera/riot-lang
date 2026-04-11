open Std
open Std.Data
module Compiler_config = Raml_core.Config
module Compiler_target = Raml_core.Target
module Artifacts = Wir.Artifacts

let ( let* ) = Result.and_then

type t = {
  store: Contentstore.t;
  target: Compiler_target.t;
}

type error =
  | Save_failed of { namespace: string; key: string; message: string }
  | Decode_failed of { namespace: string; key: string; message: string }

module Module_summary = struct
  type t = Artifacts.Module_summary.t = {
    unit_name: string;
    imports: string list;
    exports: string list;
    global_count: int;
    function_count: int;
    init_item_count: int;
    function_table_element_count: int;
    has_indirect_calls: bool;
    needs_closure_runtime: bool;
  }

  let to_json = Artifacts.Module_summary.to_json

  let of_json = fun json ->
    let json_string_list name =
      match Json.get_field name json |> Option.and_then Json.get_array with
      | None -> Error ("missing or invalid '" ^ name ^ "'")
      | Some values ->
          values |> List.fold_right
            (fun value acc ->
              match (Json.get_string value, acc) with
              | Some value, Ok values -> Ok (value :: values)
              | None, _ -> Error ("invalid string entry in '" ^ name ^ "'")
              | _, Error _ as error -> error)
            (Ok [])
    in
    let json_int name =
      match Json.get_field name json |> Option.and_then Json.get_int with
      | Some value -> Ok value
      | None -> Error ("missing or invalid '" ^ name ^ "'")
    in
    let json_bool name =
      match Json.get_field name json |> Option.and_then Json.get_bool with
      | Some value -> Ok value
      | None -> Error ("missing or invalid '" ^ name ^ "'")
    in
    let ( let* ) = Result.and_then in
    let* unit_name =
      match Json.get_field "unit_name" json |> Option.and_then Json.get_string with
      | Some value -> Ok value
      | None -> Error "missing or invalid 'unit_name'"
    in
    let* imports = json_string_list "imports" in
    let* exports = json_string_list "exports" in
    let* global_count = json_int "global_count" in
    let* function_count = json_int "function_count" in
    let* init_item_count = json_int "init_item_count" in
    let* function_table_element_count = json_int "function_table_element_count" in
    let* has_indirect_calls = json_bool "has_indirect_calls" in
    let* needs_closure_runtime = json_bool "needs_closure_runtime" in
    Ok {
      unit_name;
      imports;
      exports;
      global_count;
      function_count;
      init_item_count;
      function_table_element_count;
      has_indirect_calls;
      needs_closure_runtime;
    }
end

module Object_artifact = struct
  type t = {
    id: string;
    unit_name: string;
    summary: Module_summary.t;
    payload: Json.t;
  }

  let of_object = fun (object_: Artifacts.Object.t) ->
    let payload = Artifacts.Object.to_json object_ in
    let id = payload |> Json.to_string |> Crypto.hash_string |> Crypto.Digest.hex in
    { id; unit_name = object_.unit_name; summary = object_.summary; payload }

  let to_json = fun artifact ->
    Json.obj
      [
        ("schema", Json.string "raml-wasm/object-v1");
        ("id", Json.string artifact.id);
        ("unit_name", Json.string artifact.unit_name);
        ("summary", Module_summary.to_json artifact.summary);
        ("payload", artifact.payload);
      ]

  let of_json = fun json ->
    let ( let* ) = Result.and_then in
    let* id =
      match Json.get_field "id" json |> Option.and_then Json.get_string with
      | Some value -> Ok value
      | None -> Error "missing or invalid 'id'"
    in
    let* unit_name =
      match Json.get_field "unit_name" json |> Option.and_then Json.get_string with
      | Some value -> Ok value
      | None -> Error "missing or invalid 'unit_name'"
    in
    let* summary =
      match Json.get_field "summary" json with
      | Some value -> Module_summary.of_json value
      | None -> Error "missing 'summary'"
    in
    let* payload =
      match Json.get_field "payload" json with
      | Some value -> Ok value
      | None -> Error "missing 'payload'"
    in
    Ok { id; unit_name; summary; payload }
end

module Linked_program_artifact = struct
  type t = {
    id: string;
    unit_names: string list;
    imports: string list;
    exports: string list;
    needs_closure_runtime: bool;
    payload: Json.t;
  }

  let of_linked_program = fun (linked_program: Artifacts.Linked_program.t) ->
    let payload = Artifacts.Linked_program.to_json linked_program in
    let id = payload |> Json.to_string |> Crypto.hash_string |> Crypto.Digest.hex in
    {
      id;
      unit_names = List.map (fun (object_: Artifacts.Object.t) -> object_.unit_name) linked_program.objects;
      imports = List.map Wir.Types.Import.key linked_program.imports;
      exports = List.map (fun (export: Raml_core.Core_ir.Export.t) -> export.name) linked_program.exports;
      needs_closure_runtime = linked_program.needs_closure_runtime;
      payload;
    }

  let to_json = fun artifact ->
    Json.obj
      [
        ("schema", Json.string "raml-wasm/linked-program-v1");
        ("id", Json.string artifact.id);
        ("unit_names", Json.array (List.map Json.string artifact.unit_names));
        ("imports", Json.array (List.map Json.string artifact.imports));
        ("exports", Json.array (List.map Json.string artifact.exports));
        ("needs_closure_runtime", Json.bool artifact.needs_closure_runtime);
        ("payload", artifact.payload);
      ]

  let of_json = fun json ->
    let json_string_list name =
      match Json.get_field name json |> Option.and_then Json.get_array with
      | None -> Error ("missing or invalid '" ^ name ^ "'")
      | Some values ->
          values |> List.fold_right
            (fun value acc ->
              match (Json.get_string value, acc) with
              | Some value, Ok values -> Ok (value :: values)
              | None, _ -> Error ("invalid string entry in '" ^ name ^ "'")
              | _, Error _ as error -> error)
            (Ok [])
    in
    let ( let* ) = Result.and_then in
    let* id =
      match Json.get_field "id" json |> Option.and_then Json.get_string with
      | Some value -> Ok value
      | None -> Error "missing or invalid 'id'"
    in
    let* unit_names = json_string_list "unit_names" in
    let* imports = json_string_list "imports" in
    let* exports = json_string_list "exports" in
    let* needs_closure_runtime =
      match Json.get_field "needs_closure_runtime" json |> Option.and_then Json.get_bool with
      | Some value -> Ok value
      | None -> Error "missing or invalid 'needs_closure_runtime'"
    in
    let* payload =
      match Json.get_field "payload" json with
      | Some value -> Ok value
      | None -> Error "missing 'payload'"
    in
    Ok {
      id;
      unit_names;
      imports;
      exports;
      needs_closure_runtime;
      payload;
    }
end

module Module_artifact = struct
  type t = {
    id: string;
    unit_name: string option;
    size_bytes: int;
    memory_pages: int;
    wasm_base64: string;
    node_runner: string;
    payload: Json.t;
  }

  let of_codegen_artifact = fun ?unit_name (artifact: Codegen.artifact) ->
    let payload = Codegen.artifact_to_json artifact in
    let id = payload |> Json.to_string |> Crypto.hash_string |> Crypto.Digest.hex in
    {
      id;
      unit_name;
      size_bytes = artifact.size_bytes;
      memory_pages = artifact.memory_pages;
      wasm_base64 = artifact.wasm_base64;
      node_runner = artifact.node_runner;
      payload;
    }

  let to_json = fun artifact ->
    Json.obj
      [
        ("schema", Json.string "raml-wasm/module-v1");
        ("id", Json.string artifact.id);
        (
          "unit_name",
          Option.map Json.string artifact.unit_name |> Option.unwrap_or ~default:Json.null
        );
        ("size_bytes", Json.int artifact.size_bytes);
        ("memory_pages", Json.int artifact.memory_pages);
        ("wasm_base64", Json.string artifact.wasm_base64);
        ("node_runner", Json.string artifact.node_runner);
        ("payload", artifact.payload);
      ]

  let of_json = fun json ->
    let ( let* ) = Result.and_then in
    let* id =
      match Json.get_field "id" json |> Option.and_then Json.get_string with
      | Some value -> Ok value
      | None -> Error "missing or invalid 'id'"
    in
    let unit_name = Json.get_field "unit_name" json |> Option.and_then Json.get_string in
    let* size_bytes =
      match Json.get_field "size_bytes" json |> Option.and_then Json.get_int with
      | Some value -> Ok value
      | None -> Error "missing or invalid 'size_bytes'"
    in
    let* memory_pages =
      match Json.get_field "memory_pages" json |> Option.and_then Json.get_int with
      | Some value -> Ok value
      | None -> Error "missing or invalid 'memory_pages'"
    in
    let* wasm_base64 =
      match Json.get_field "wasm_base64" json |> Option.and_then Json.get_string with
      | Some value -> Ok value
      | None -> Error "missing or invalid 'wasm_base64'"
    in
    let* node_runner =
      match Json.get_field "node_runner" json |> Option.and_then Json.get_string with
      | Some value -> Ok value
      | None -> Error "missing or invalid 'node_runner'"
    in
    let* payload =
      match Json.get_field "payload" json with
      | Some value -> Ok value
      | None -> Error "missing 'payload'"
    in
    Ok {
      id;
      unit_name;
      size_bytes;
      memory_pages;
      wasm_base64;
      node_runner;
      payload;
    }
end

let create = fun store ~target () -> { store; target }

let of_config = fun config ->
  match Compiler_config.content_store config with
  | None -> None
  | Some store -> Some (create store ~target:(Compiler_config.target config) ())

let target = fun store -> store.target

let target_namespace = fun store -> "raml-wasm/v1/" ^ Compiler_target.to_string store.target

let objects_namespace = fun store -> target_namespace store ^ "/objects"

let object_index_namespace = fun store -> target_namespace store ^ "/objects-by-unit"

let linked_programs_namespace = fun store -> target_namespace store ^ "/linked-programs"

let linked_program_index_namespace = fun store -> target_namespace store ^ "/linked-programs-by-unit"

let modules_namespace = fun store -> target_namespace store ^ "/modules"

let module_index_namespace = fun store -> target_namespace store ^ "/modules-by-unit"

let error_to_json = fun error ->
  match error with
  | Save_failed { namespace; key; message } -> Json.obj
    [
      ("kind", Json.string "save_failed");
      ("namespace", Json.string namespace);
      ("key", Json.string key);
      ("message", Json.string message);
    ]
  | Decode_failed { namespace; key; message } -> Json.obj
    [
      ("kind", Json.string "decode_failed");
      ("namespace", Json.string namespace);
      ("key", Json.string key);
      ("message", Json.string message);
    ]

let save_named_json = fun store ~namespace ~key ~json ->
  match Contentstore.Store.save_named_json_bundle store.store ~namespace ~key ~json with
  | Ok () -> Ok ()
  | Error message -> Error (Save_failed { namespace; key; message })

let load_named_json = fun store ~namespace ~key ->
  Contentstore.Store.load_named_json_bundle store.store ~namespace ~key

let save_object = fun store ~(object_:Artifacts.Object.t) ->
  let artifact = Object_artifact.of_object object_ in
  let* () = save_named_json
    store
    ~namespace:(objects_namespace store)
    ~key:artifact.id
    ~json:(Object_artifact.to_json artifact) in
  let* () = save_named_json
    store
    ~namespace:(object_index_namespace store)
    ~key:artifact.unit_name
    ~json:(Json.obj [ ("id", Json.string artifact.id) ]) in
  Ok artifact

let decode = fun decode json ->
  match decode json with
  | Ok value -> Some value
  | Error _ -> None

let load_object = fun store ~id ->
  load_named_json store ~namespace:(objects_namespace store) ~key:id
  |> Option.and_then (decode Object_artifact.of_json)

let find_object_by_unit_name = fun store ~unit_name ->
  match load_named_json store ~namespace:(object_index_namespace store) ~key:unit_name with
  | Some index_json -> (
      match Json.get_field "id" index_json |> Option.and_then Json.get_string with
      | Some id -> load_object store ~id
      | None -> None
    )
  | None -> None

let save_linked_program = fun store ~(linked_program:Artifacts.Linked_program.t) ->
  let artifact = Linked_program_artifact.of_linked_program linked_program in
  let* () = save_named_json
    store
    ~namespace:(linked_programs_namespace store)
    ~key:artifact.id
    ~json:(Linked_program_artifact.to_json artifact) in
  let* () =
    artifact.unit_names |> List.fold_left
      (fun result unit_name ->
        let* () = result in
        save_named_json
          store
          ~namespace:(linked_program_index_namespace store)
          ~key:unit_name
          ~json:(Json.obj [ ("id", Json.string artifact.id) ]))
      (Ok ())
  in
  Ok artifact

let load_linked_program = fun store ~id ->
  load_named_json store ~namespace:(linked_programs_namespace store) ~key:id
  |> Option.and_then (decode Linked_program_artifact.of_json)

let find_linked_program_by_unit_name = fun store ~unit_name ->
  match load_named_json store ~namespace:(linked_program_index_namespace store) ~key:unit_name with
  | Some index_json -> (
      match Json.get_field "id" index_json |> Option.and_then Json.get_string with
      | Some id -> load_linked_program store ~id
      | None -> None
    )
  | None -> None

let save_module = fun store ?unit_name (artifact: Codegen.artifact) ->
  let artifact = Module_artifact.of_codegen_artifact ?unit_name artifact in
  let* () = save_named_json
    store
    ~namespace:(modules_namespace store)
    ~key:artifact.id
    ~json:(Module_artifact.to_json artifact) in
  let* () =
    match artifact.unit_name with
    | None -> Ok ()
    | Some unit_name -> save_named_json
      store
      ~namespace:(module_index_namespace store)
      ~key:unit_name
      ~json:(Json.obj [ ("id", Json.string artifact.id) ])
  in
  Ok artifact

let load_module = fun store ~id ->
  load_named_json store ~namespace:(modules_namespace store) ~key:id
  |> Option.and_then (decode Module_artifact.of_json)

let find_module_by_unit_name = fun store ~unit_name ->
  match load_named_json store ~namespace:(module_index_namespace store) ~key:unit_name with
  | Some index_json -> (
      match Json.get_field "id" index_json |> Option.and_then Json.get_string with
      | Some id -> load_module store ~id
      | None -> None
    )
  | None -> None
