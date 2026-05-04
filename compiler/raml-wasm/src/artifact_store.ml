open Std
open Std.Data
module Compiler_config = Raml_core.Config
module Compiler_target = Raml_core.Target
module Artifacts = Wir.Artifacts

let ( let* ) value fn = Result.and_then value ~fn

type t = {
  store: Contentstore.t;
  target: Compiler_target.t;
}

type error =
  | Save_failed of { namespace: string; key: string; message: string }
  | Decode_failed of { namespace: string; key: string; message: string }

let json_string_field = fun name json ->
  match Json.get_field name json with
  | Some value -> Json.get_string value
  | None -> None

let json_int_field = fun name json ->
  match Json.get_field name json with
  | Some value -> Json.get_int value
  | None -> None

let json_bool_field = fun name json ->
  match Json.get_field name json with
  | Some value -> Json.get_bool value
  | None -> None

let json_array_field = fun name json ->
  match Json.get_field name json with
  | Some value -> Json.get_array value
  | None -> None

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

  let from_json = fun json ->
    let json_string_list name =
      match json_array_field name json with
      | None -> Error ("missing or invalid '" ^ name ^ "'")
      | Some values ->
          let rec loop values acc =
            match values with
            | [] -> Ok (List.rev acc)
            | value :: rest -> (
                match Json.get_string value with
                | Some value -> loop rest (value :: acc)
                | None -> Error ("invalid string entry in '" ^ name ^ "'")
              )
          in
          loop values []
    in
    let json_int name =
      match json_int_field name json with
      | Some value -> Ok value
      | None -> Error ("missing or invalid '" ^ name ^ "'")
    in
    let json_bool name =
      match json_bool_field name json with
      | Some value -> Ok value
      | None -> Error ("missing or invalid '" ^ name ^ "'")
    in
    let ( let* ) value fn = Result.and_then value ~fn in
    let* unit_name =
      match json_string_field "unit_name" json with
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

  let from_object = fun (object_: Artifacts.Object.t) ->
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

  let from_json = fun json ->
    let ( let* ) value fn = Result.and_then value ~fn in
    let* id =
      match json_string_field "id" json with
      | Some value -> Ok value
      | None -> Error "missing or invalid 'id'"
    in
    let* unit_name =
      match json_string_field "unit_name" json with
      | Some value -> Ok value
      | None -> Error "missing or invalid 'unit_name'"
    in
    let* summary =
      match Json.get_field "summary" json with
      | Some value -> Module_summary.from_json value
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

  let from_linked_program = fun (linked_program: Artifacts.Linked_program.t) ->
    let payload = Artifacts.Linked_program.to_json linked_program in
    let id = payload |> Json.to_string |> Crypto.hash_string |> Crypto.Digest.hex in
    {
      id;
      unit_names = List.map
        linked_program.objects
        ~fn:(fun (object_: Artifacts.Object.t) -> object_.unit_name);
      imports = List.map linked_program.imports ~fn:Wir.Types.Import.key;
      exports = List.map
        linked_program.exports
        ~fn:(fun (export: Raml_core.Core_ir.Export.t) -> export.name);
      needs_closure_runtime = linked_program.needs_closure_runtime;
      payload;
    }

  let to_json = fun artifact ->
    Json.obj
      [
        ("schema", Json.string "raml-wasm/linked-program-v1");
        ("id", Json.string artifact.id);
        ("unit_names", Json.array (List.map artifact.unit_names ~fn:Json.string));
        ("imports", Json.array (List.map artifact.imports ~fn:Json.string));
        ("exports", Json.array (List.map artifact.exports ~fn:Json.string));
        ("needs_closure_runtime", Json.bool artifact.needs_closure_runtime);
        ("payload", artifact.payload);
      ]

  let from_json = fun json ->
    let json_string_list name =
      match json_array_field name json with
      | None -> Error ("missing or invalid '" ^ name ^ "'")
      | Some values ->
          let rec loop values acc =
            match values with
            | [] -> Ok (List.rev acc)
            | value :: rest -> (
                match Json.get_string value with
                | Some value -> loop rest (value :: acc)
                | None -> Error ("invalid string entry in '" ^ name ^ "'")
              )
          in
          loop values []
    in
    let ( let* ) value fn = Result.and_then value ~fn in
    let* id =
      match json_string_field "id" json with
      | Some value -> Ok value
      | None -> Error "missing or invalid 'id'"
    in
    let* unit_names = json_string_list "unit_names" in
    let* imports = json_string_list "imports" in
    let* exports = json_string_list "exports" in
    let* needs_closure_runtime =
      match json_bool_field "needs_closure_runtime" json with
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

  let from_codegen_artifact = fun ?unit_name (artifact: Codegen.artifact) ->
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
          Option.map artifact.unit_name ~fn:Json.string |> Option.unwrap_or ~default:Json.null
        );
        ("size_bytes", Json.int artifact.size_bytes);
        ("memory_pages", Json.int artifact.memory_pages);
        ("wasm_base64", Json.string artifact.wasm_base64);
        ("node_runner", Json.string artifact.node_runner);
        ("payload", artifact.payload);
      ]

  let from_json = fun json ->
    let ( let* ) value fn = Result.and_then value ~fn in
    let* id =
      match json_string_field "id" json with
      | Some value -> Ok value
      | None -> Error "missing or invalid 'id'"
    in
    let unit_name = json_string_field "unit_name" json in
    let* size_bytes =
      match json_int_field "size_bytes" json with
      | Some value -> Ok value
      | None -> Error "missing or invalid 'size_bytes'"
    in
    let* memory_pages =
      match json_int_field "memory_pages" json with
      | Some value -> Ok value
      | None -> Error "missing or invalid 'memory_pages'"
    in
    let* wasm_base64 =
      match json_string_field "wasm_base64" json with
      | Some value -> Ok value
      | None -> Error "missing or invalid 'wasm_base64'"
    in
    let* node_runner =
      match json_string_field "node_runner" json with
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

let from_config = fun config ->
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
  match Contentstore.Store.save_named_object
    store.store
    ~key:(namespace ^ "/" ^ key)
    ~content:(Json.to_string json) with
  | Ok () -> Ok ()
  | Error err -> Error (Save_failed {
    namespace;
    key;
    message = Contentstore.Store.error_message err
  })

let load_named_json = fun store ~namespace ~key ->
  match Contentstore.Store.open_named_object store.store ~key:(namespace ^ "/" ^ key) with
  | Error _ -> None
  | Ok file -> (
      match Fs.File.read_to_end file with
      | Error _ -> None
      | Ok content -> Data.Json.from_string content |> Result.to_option
    )

let save_object = fun store ~(object_:Artifacts.Object.t) ->
  let artifact = Object_artifact.from_object object_ in
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
  match load_named_json store ~namespace:(objects_namespace store) ~key:id with
  | Some json -> decode Object_artifact.from_json json
  | None -> None

let find_object_by_unit_name = fun store ~unit_name ->
  match load_named_json store ~namespace:(object_index_namespace store) ~key:unit_name with
  | Some index_json -> (
      match json_string_field "id" index_json with
      | Some id -> load_object store ~id
      | None -> None
    )
  | None -> None

let save_linked_program = fun store ~(linked_program:Artifacts.Linked_program.t) ->
  let artifact = Linked_program_artifact.from_linked_program linked_program in
  let* () = save_named_json
    store
    ~namespace:(linked_programs_namespace store)
    ~key:artifact.id
    ~json:(Linked_program_artifact.to_json artifact) in
  let* () =
    artifact.unit_names |> List.fold_left ~init:(Ok ())
      ~fn:(fun result unit_name ->
        let* () = result in
        save_named_json
          store
          ~namespace:(linked_program_index_namespace store)
          ~key:unit_name
          ~json:(Json.obj [ ("id", Json.string artifact.id) ]))
  in
  Ok artifact

let load_linked_program = fun store ~id ->
  match load_named_json store ~namespace:(linked_programs_namespace store) ~key:id with
  | Some json -> decode Linked_program_artifact.from_json json
  | None -> None

let find_linked_program_by_unit_name = fun store ~unit_name ->
  match load_named_json store ~namespace:(linked_program_index_namespace store) ~key:unit_name with
  | Some index_json -> (
      match json_string_field "id" index_json with
      | Some id -> load_linked_program store ~id
      | None -> None
    )
  | None -> None

let save_module = fun store ?unit_name (artifact: Codegen.artifact) ->
  let artifact = Module_artifact.from_codegen_artifact ?unit_name artifact in
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
  match load_named_json store ~namespace:(modules_namespace store) ~key:id with
  | Some json -> decode Module_artifact.from_json json
  | None -> None

let find_module_by_unit_name = fun store ~unit_name ->
  match load_named_json store ~namespace:(module_index_namespace store) ~key:unit_name with
  | Some index_json -> (
      match json_string_field "id" index_json with
      | Some id -> load_module store ~id
      | None -> None
    )
  | None -> None
