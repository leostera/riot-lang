open Std

module De = Serde.De
module Ser = Serde.Ser
module Vector = Collections.Vector

type payload = {
  version: int;
  package: string;
  path: string;
  module_path: string list;
  source_hash: string;
  summary: string;
}

type field =
  | Version
  | Package
  | Path
  | Module_path
  | Source_hash
  | Summary

type builder = {
  mutable version: int option;
  mutable package: string option;
  mutable path: string option;
  mutable module_path: string list option;
  mutable source_hash: string option;
  mutable summary: string option;
}

let vector_to_list = fun values ->
  let rec loop index items =
    if index < 0 then
      items
    else
      loop (Int.sub index 1) (Vector.get_unchecked values ~at:index :: items)
  in
  loop (Int.sub (Vector.length values) 1) []

let de_list = fun decode -> De.map (De.list decode) vector_to_list

let ser_list = fun encode -> Ser.contramap Vector.from_list (Ser.list encode)

let fields =
  De.fields [
    De.field "version" Version;
    De.field "package" Package;
    De.field "path" Path;
    De.field "module_path" Module_path;
    De.field "source_hash" Source_hash;
    De.field "summary" Summary;
  ]

let deserialize =
  De.record_mut
    ~fields
    ~create:(fun () ->
      {
        version = None;
        package = None;
        path = None;
        module_path = Some [];
        source_hash = None;
        summary = None;
      })
    ~step:(fun reader builder field ->
      match field with
      | Some Version -> builder.version <- Some (De.read reader De.int)
      | Some Package -> builder.package <- Some (De.read reader De.string)
      | Some Path -> builder.path <- Some (De.read reader De.string)
      | Some Module_path -> builder.module_path <- Some (De.read reader (de_list De.string))
      | Some Source_hash -> builder.source_hash <- Some (De.read reader De.string)
      | Some Summary -> builder.summary <- Some (De.read reader De.string)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (
        builder.version,
        builder.package,
        builder.path,
        builder.module_path,
        builder.source_hash,
        builder.summary
      ) with
      | (Some version, Some package, Some path, Some module_path, Some source_hash, Some summary) ->
          ({ version; package; path; module_path; source_hash; summary }: payload)
      | _ -> De.missing_field ())

let serialize =
  Ser.record
    (
      Ser.fields [
        Ser.field "version" Ser.int (fun (value: payload) -> value.version);
        Ser.field "package" Ser.string (fun (value: payload) -> value.package);
        Ser.field "path" Ser.string (fun (value: payload) -> value.path);
        Ser.field "module_path" (ser_list Ser.string) (fun (value: payload) -> value.module_path);
        Ser.field "source_hash" Ser.string (fun (value: payload) -> value.source_hash);
        Ser.field "summary" Ser.string (fun (value: payload) -> value.summary);
      ]
    )

let create_cache = fun ~store ->
  Graph_cache.create
    ~store
    ~namespace:Riot_store.Store.SourceAnalysis
    ~serialize
    ~deserialize

let write_list = fun hasher items ->
  List.for_each
    items
    ~fn:(fun item ->
      Crypto.Sha256.write hasher item;
      Crypto.Sha256.write hasher "\x1f")

let write_list_list = fun hasher items ->
  List.for_each
    items
    ~fn:(fun item ->
      write_list hasher item;
      Crypto.Sha256.write hasher "\x1e")

let input_hash = fun ~package (analysis: Riot_planner.Module_graph.source_analysis) ->
  let source = analysis.analysis_task in
  let hasher = Crypto.Sha256.create () in
  Crypto.Sha256.write hasher "riot-build2-source-analysis:v2";
  Crypto.Sha256.write hasher (Riot_model.Package_name.to_string package);
  Crypto.Sha256.write hasher "\x1f";
  Crypto.Sha256.write hasher (Path.to_string source.task_path);
  Crypto.Sha256.write hasher "\x1f";
  (
    match source.task_module_path with
    | None -> ()
    | Some module_path -> write_list hasher module_path
  );
  write_list hasher source.task_implicit_opens;
  write_list_list hasher source.task_implicit_open_paths;
  Crypto.Sha256.write_hash hasher analysis.analysis_source_hash;
  Crypto.Sha256.finish hasher

let payload = fun ~package (analysis: Riot_planner.Module_graph.source_analysis) ->
  let summary =
    match analysis.analysis_summary with
    | Ok _ -> "ok"
    | Error _ -> "error"
  in
  ({
    version = 1;
    package = Riot_model.Package_name.to_string package;
    path = Path.to_string analysis.analysis_task.task_path;
    module_path = Option.unwrap_or ~default:[] analysis.analysis_task.task_module_path;
    source_hash = Crypto.Digest.hex analysis.analysis_source_hash;
    summary;
  }: payload)
