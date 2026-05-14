open Std

module De = Serde.De
module Ser = Serde.Ser
module Vector = Collections.Vector

type payload = {
  version: int;
  package: string;
  path: string;
  module_path: string list option;
  source_hash: string;
  source_summary: Riot_planner.Dep_analyzer.source_summary;
}

type field =
  | Version
  | Package
  | Path
  | Module_path
  | Source_hash
  | Source_summary

type builder = {
  mutable version: int option;
  mutable package: string option;
  mutable path: string option;
  mutable module_path: string list option option;
  mutable source_hash: string option;
  mutable source_summary: Riot_planner.Dep_analyzer.source_summary option;
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
  De.fields
    [
      De.field "version" Version;
      De.field "package" Package;
      De.field "path" Path;
      De.field "module_path" Module_path;
      De.field "source_hash" Source_hash;
      De.field "source_summary" Source_summary;
    ]

let deserialize =
  De.record_mut
    ~fields
    ~create:(fun () ->
      {
        version = None;
        package = None;
        path = None;
        module_path = Some None;
        source_hash = None;
        source_summary = None;
      })
    ~step:(fun reader builder field ->
      match field with
      | Some Version -> builder.version <- Some (De.read reader De.int)
      | Some Package -> builder.package <- Some (De.read reader De.string)
      | Some Path -> builder.path <- Some (De.read reader De.string)
      | Some Module_path ->
          builder.module_path <- Some (De.read reader (De.option (de_list De.string)))
      | Some Source_hash -> builder.source_hash <- Some (De.read reader De.string)
      | Some Source_summary ->
          builder.source_summary <- Some (De.read
            reader
            Riot_planner.Dep_analyzer.source_summary_deserializer)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (
        builder.version,
        builder.package,
        builder.path,
        builder.module_path,
        builder.source_hash,
        builder.source_summary
      ) with
      | (
          Some version,
          Some package,
          Some path,
          Some module_path,
          Some source_hash,
          Some source_summary
        ) -> ({
        version;
        package;
        path;
        module_path;
        source_hash;
        source_summary;
      }: payload)
      | _ -> De.missing_field ())

let serialize =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "version" Ser.int (fun (value: payload) -> value.version);
          Ser.field "package" Ser.string (fun (value: payload) -> value.package);
          Ser.field "path" Ser.string (fun (value: payload) -> value.path);
          Ser.field
            "module_path"
            (Ser.option (ser_list Ser.string))
            (fun (value: payload) -> value.module_path);
          Ser.field "source_hash" Ser.string (fun (value: payload) -> value.source_hash);
          Ser.field
            "source_summary"
            Riot_planner.Dep_analyzer.source_summary_serializer
            (fun (value: payload) -> value.source_summary);
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

let input_hash_for_task = fun
  ~package ~(task:Riot_planner.Module_graph.source_analysis_task) ~source_hash ->
  let hasher = Crypto.Sha256.create () in
  Crypto.Sha256.write hasher "riot-build2-source-analysis:v3";
  Crypto.Sha256.write hasher (Riot_model.Package_name.to_string package);
  Crypto.Sha256.write hasher "\x1f";
  Crypto.Sha256.write hasher (Path.to_string task.task_path);
  Crypto.Sha256.write hasher "\x1f";
  (
    match task.task_module_path with
    | None -> ()
    | Some module_path -> write_list hasher module_path
  );
  write_list hasher task.task_implicit_opens;
  write_list_list hasher task.task_implicit_open_paths;
  Crypto.Sha256.write_hash hasher source_hash;
  Crypto.Sha256.finish hasher

let input_hash = fun ~package (analysis: Riot_planner.Module_graph.source_analysis) ->
  input_hash_for_task
    ~package
    ~task:analysis.analysis_task
    ~source_hash:analysis.analysis_source_hash

let payload = fun ~package (analysis: Riot_planner.Module_graph.source_analysis) ->
  match analysis.analysis_summary with
  | Error _ -> None
  | Ok source_summary ->
      Some ({
        version = 2;
        package = Riot_model.Package_name.to_string package;
        path = Path.to_string analysis.analysis_task.task_path;
        module_path = analysis.analysis_task.task_module_path;
        source_hash = Crypto.Digest.hex analysis.analysis_source_hash;
        source_summary;
      }: payload)

let planning_error = fun reason -> Riot_planner.Planning_error.DependencyAnalysisFailed { reason }

let analysis = fun ~(task:Riot_planner.Module_graph.source_analysis_task) (payload: payload) ->
  if payload.version != 2 then
    Error (planning_error "unsupported source analysis cache payload version")
  else if not (String.equal payload.path (Path.to_string task.task_path)) then
    Error (planning_error "source analysis cache payload path does not match requested task")
  else if not (payload.module_path = task.task_module_path) then
    Error (planning_error "source analysis cache payload module path does not match requested task")
  else
    Riot_planner.Module_graph.source_analysis_of_summary task payload.source_summary

let source_hash = fun (payload: payload) ->
  payload.source_summary.Riot_planner.Dep_analyzer.source_hash

let cache_error = fun reason ->
  Error.GraphCacheEncodeFailed { namespace = Riot_store.Store.SourceAnalysis; reason }

let summary_hash = fun summary ->
  let stable_summary =
    Riot_planner.Dep_analyzer.{
      summary with
      source_hash = Crypto.hash_string "riot-build2-source-summary-hash-elided";
    }
  in
  match Serde_json.to_string Riot_planner.Dep_analyzer.source_summary_serializer stable_summary with
  | Error error -> Error (cache_error (Serde.Error.to_string error))
  | Ok encoded -> Ok (Crypto.hash_string encoded)

let summary_hash_of_analysis = fun (analysis: Riot_planner.Module_graph.source_analysis) ->
  match analysis.analysis_summary with
  | Ok summary -> summary_hash summary
  | Error _ ->
      Error (Error.ExecutorInvariantViolated {
        message = "source analysis summary hash requested before successful dependency analysis";
      })
