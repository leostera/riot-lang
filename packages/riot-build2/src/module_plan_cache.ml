open Std

module De = Serde.De
module Ser = Serde.Ser

type payload = {
  version: int;
  package: string;
  action_graph_json: string;
  action_count: int;
}

type field =
  | Version
  | Package
  | Action_graph_json
  | Action_count

type builder = {
  mutable version: int option;
  mutable package: string option;
  mutable action_graph_json: string option;
  mutable action_count: int option;
}

let fields =
  De.fields [
    De.field "version" Version;
    De.field "package" Package;
    De.field "action_graph_json" Action_graph_json;
    De.field "action_count" Action_count;
  ]

let deserialize =
  De.record_mut
    ~fields
    ~create:(fun () ->
      {
        version = None;
        package = None;
        action_graph_json = None;
        action_count = None;
      })
    ~step:(fun reader builder field ->
      match field with
      | Some Version -> builder.version <- Some (De.read reader De.int)
      | Some Package -> builder.package <- Some (De.read reader De.string)
      | Some Action_graph_json -> builder.action_graph_json <- Some (De.read reader De.string)
      | Some Action_count -> builder.action_count <- Some (De.read reader De.int)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (
        builder.version,
        builder.package,
        builder.action_graph_json,
        builder.action_count
      ) with
      | (Some version, Some package, Some action_graph_json, Some action_count) ->
          ({ version; package; action_graph_json; action_count }: payload)
      | _ -> De.missing_field ())

let serialize =
  Ser.record
    (
      Ser.fields [
        Ser.field "version" Ser.int (fun (value: payload) -> value.version);
        Ser.field "package" Ser.string (fun (value: payload) -> value.package);
        Ser.field "action_graph_json" Ser.string (fun (value: payload) -> value.action_graph_json);
        Ser.field "action_count" Ser.int (fun (value: payload) -> value.action_count);
      ]
    )

let create_cache = fun ~store ->
  Graph_cache.create
    ~store
    ~namespace:Riot_store.Store.ModulePlans
    ~serialize
    ~deserialize

let payload_of_plan = fun (plan: Module_plan.t) ->
  ({
    version = 1;
    package = Riot_model.Package_name.to_string plan.package.name;
    action_graph_json =
      plan.action_graph
      |> Riot_planner.Action_graph.to_json
      |> Data.Json.to_string;
    action_count = List.length plan.action_nodes;
  }: payload)

let decode_error = fun reason ->
  Error.GraphCacheDecodeFailed {
    namespace = Riot_store.Store.ModulePlans;
    reason;
  }

let action_graph = fun ~package (payload: payload) ->
  let expected = Riot_model.Package_name.to_string package in
  if not (Int.equal payload.version 1) then
    Error (decode_error "unsupported module plan cache payload version")
  else if not (String.equal payload.package expected) then
    Error (decode_error "module plan cache package does not match requested package")
  else
    match Data.Json.from_string payload.action_graph_json with
    | Error error -> Error (decode_error (Data.Json.error_to_string error))
    | Ok json -> (
        match Riot_planner.Action_graph.from_json json with
        | Ok graph ->
            let actual_count = List.length (Riot_planner.Action_graph.nodes graph) in
            if Int.equal actual_count payload.action_count then
              Ok graph
            else
              Error (decode_error "module plan cache action count mismatch")
        | Error reason -> Error (decode_error reason)
      )
