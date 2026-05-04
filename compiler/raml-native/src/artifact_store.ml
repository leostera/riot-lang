open Std
open Std.Data
module Compiler_config = Raml_core.Config
module Compiler_target = Raml_core.Target

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

module Assembly_artifact = struct
  type t = {
    id: string;
    unit_name: string;
    target: string;
    assembly: string;
    payload: Json.t;
  }

  let create = fun ~target ~unit_name ~assembly ->
    let payload = Json.obj
      [
        ("target", Json.string target);
        ("unit_name", Json.string unit_name);
        ("assembly", Json.string assembly);
      ] in
    let id = payload |> Json.to_string |> Crypto.hash_string |> Crypto.Digest.hex in
    {
      id;
      unit_name;
      target;
      assembly;
      payload;
    }

  let to_json = fun artifact ->
    Json.obj
      [
        ("schema", Json.string "raml-native/assembly-v1");
        ("id", Json.string artifact.id);
        ("unit_name", Json.string artifact.unit_name);
        ("target", Json.string artifact.target);
        ("assembly", Json.string artifact.assembly);
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
    let* target =
      match json_string_field "target" json with
      | Some value -> Ok value
      | None -> Error "missing or invalid 'target'"
    in
    let* assembly =
      match json_string_field "assembly" json with
      | Some value -> Ok value
      | None -> Error "missing or invalid 'assembly'"
    in
    let* payload =
      match Json.get_field "payload" json with
      | Some value -> Ok value
      | None -> Error "missing 'payload'"
    in
    Ok {
      id;
      unit_name;
      target;
      assembly;
      payload;
    }
end

module Link_plan_artifact = struct
  type t = {
    id: string;
    artifact: string;
    command: string;
    payload: Json.t;
  }

  let create = fun ~(artifact:Linker.artifact) ~command ->
    let artifact = Linker.artifact_to_string artifact in
    let payload = Json.obj [ ("artifact", Json.string artifact); ("command", Json.string command); ] in
    let id = payload |> Json.to_string |> Crypto.hash_string |> Crypto.Digest.hex in
    { id; artifact; command; payload }

  let to_json = fun artifact ->
    Json.obj
      [
        ("schema", Json.string "raml-native/link-plan-v1");
        ("id", Json.string artifact.id);
        ("artifact", Json.string artifact.artifact);
        ("command", Json.string artifact.command);
        ("payload", artifact.payload);
      ]

  let from_json = fun json ->
    let ( let* ) value fn = Result.and_then value ~fn in
    let* id =
      match json_string_field "id" json with
      | Some value -> Ok value
      | None -> Error "missing or invalid 'id'"
    in
    let* artifact =
      match json_string_field "artifact" json with
      | Some value -> Ok value
      | None -> Error "missing or invalid 'artifact'"
    in
    let* command =
      match json_string_field "command" json with
      | Some value -> Ok value
      | None -> Error "missing or invalid 'command'"
    in
    let* payload =
      match Json.get_field "payload" json with
      | Some value -> Ok value
      | None -> Error "missing 'payload'"
    in
    Ok { id; artifact; command; payload }
end

let create = fun store ~target () -> { store; target }

let from_config = fun config ->
  match Compiler_config.content_store config with
  | None -> None
  | Some store -> Some (create store ~target:(Compiler_config.target config) ())

let target = fun store -> store.target

let target_namespace = fun store -> "raml-native/v1/" ^ Compiler_target.to_string store.target

let assembly_namespace = fun store -> target_namespace store ^ "/assembly"

let assembly_index_namespace = fun store -> target_namespace store ^ "/assembly-by-unit"

let link_plan_namespace = fun store -> target_namespace store ^ "/link-plans"

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
  match Contentstore.Store.save_named_object store.store ~key:(namespace ^ "/" ^ key) ~content:(Json.to_string json) with
  | Ok () -> Ok ()
  | Error err ->
      Error (Save_failed {
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
      | Ok content ->
          Data.Json.from_string content |> Result.to_option
    )

let save_assembly = fun store ~unit_name ~assembly ->
  let artifact = Assembly_artifact.create
    ~target:(Compiler_target.to_string store.target)
    ~unit_name
    ~assembly in
  let* () = save_named_json
    store
    ~namespace:(assembly_namespace store)
    ~key:artifact.id
    ~json:(Assembly_artifact.to_json artifact) in
  let* () = save_named_json
    store
    ~namespace:(assembly_index_namespace store)
    ~key:unit_name
    ~json:(Json.obj [ ("id", Json.string artifact.id) ]) in
  Ok artifact

let load_assembly = fun store ~id ->
  match load_named_json store ~namespace:(assembly_namespace store) ~key:id with
  | None -> None
  | Some json -> (
      match Assembly_artifact.from_json json with
      | Ok artifact -> Some artifact
      | Error _ -> None
    )

let find_assembly_by_unit_name = fun store ~unit_name ->
  match load_named_json store ~namespace:(assembly_index_namespace store) ~key:unit_name with
  | Some index_json -> (
      match json_string_field "id" index_json with
      | Some id -> load_assembly store ~id
      | None -> None
    )
  | None -> None

let save_link_plan = fun store ~(artifact:Linker.artifact) ~command ->
  let artifact = Link_plan_artifact.create ~artifact ~command in
  let* () = save_named_json
    store
    ~namespace:(link_plan_namespace store)
    ~key:artifact.id
    ~json:(Link_plan_artifact.to_json artifact) in
  Ok artifact

let load_link_plan = fun store ~id ->
  match load_named_json store ~namespace:(link_plan_namespace store) ~key:id with
  | None -> None
  | Some json -> (
      match Link_plan_artifact.from_json json with
      | Ok artifact -> Some artifact
      | Error _ -> None
    )
