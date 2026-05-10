open Std

module ConcurrentHashMap = Collections.ConcurrentHashMap

type t = {
  source_analysis_cache: Source_analysis_cache.payload Graph_cache.t;
  analyses: (Source_analysis.key, Riot_planner.Module_graph.source_analysis) ConcurrentHashMap.t;
}

let create = fun ~store () -> {
  source_analysis_cache = Source_analysis_cache.create_cache ~store;
  analyses = ConcurrentHashMap.with_capacity ~size:512;
}

let find = fun t key -> ConcurrentHashMap.get t.analyses ~key

let missing = fun t ~package tasks ->
  tasks
  |> List.filter_map
    ~fn:(fun task ->
      let key = Source_analysis.key_from_task ~package task in
      match find t key with
      | Some _ -> None
      | None -> Some (Source_analysis.make ~package ~task))

let planning_error = fun source error ->
  Error.SourceAnalysisFailed {
    source;
    reason = Riot_planner.Planning_error.to_string error;
  }

let store_analysis = fun t source analysis ->
  ignore (ConcurrentHashMap.insert t.analyses ~key:source.Source_analysis.key ~value:analysis)

let cached_analysis_has_hash = fun analysis source_hash ->
  Crypto.Hash.equal analysis.Riot_planner.Module_graph.analysis_source_hash source_hash

let load_persisted = fun t source source_hash ->
  let input_hash =
    Source_analysis_cache.input_hash_for_task
      ~package:source.Source_analysis.key.package
      ~task:source.task
      ~source_hash
  in
  match Graph_cache.get t.source_analysis_cache input_hash with
  | None -> Ok false
  | Some (Error error) -> Error error
  | Some (Ok payload) ->
      let open Std.Result.Syntax in
      let* analysis =
        Source_analysis_cache.analysis ~task:source.task payload
        |> Result.map_err ~fn:(planning_error source.task.task_display_path)
      in
      store_analysis t source analysis;
      Ok true

let execute = fun t (source: Source_analysis.t) ->
  let open Std.Result.Syntax in
  let* source_hash =
    Riot_planner.Module_graph.source_hash_for_task source.task
    |> Result.map_err ~fn:(planning_error source.task.task_display_path)
  in
  match find t source.key with
  | Some cached when cached_analysis_has_hash cached source_hash -> Ok ()
  | Some _
  | None ->
      let* loaded = load_persisted t source source_hash in
      if loaded then
        Ok ()
      else (
        match Riot_planner.Module_graph.analyze_source source.task with
        | Ok analysis ->
            let input_hash =
              Source_analysis_cache.input_hash ~package:source.key.package analysis
            in
            let* () =
              match Source_analysis_cache.payload ~package:source.key.package analysis with
              | None -> Ok ()
              | Some payload -> Graph_cache.put t.source_analysis_cache input_hash payload
            in
            store_analysis t source analysis;
            Ok ()
        | Error error -> Error (planning_error source.task.task_display_path error)
      )

let analyze_from_cache = fun t package ~on_source_analyzed tasks ->
  let source_count = List.length tasks in
  tasks
  |> List.enumerate
  |> List.map
    ~fn:(fun (index, task) ->
      let key = Source_analysis.key_from_task ~package:package.Riot_model.Package.name task in
      let analysis =
        match find t key with
        | Some cached -> Ok { cached with Riot_planner.Module_graph.analysis_task = task }
        | None -> Riot_planner.Module_graph.analyze_source task
      in
      match analysis with
      | Ok analysis ->
          on_source_analyzed
            Riot_planner.Module_graph.{
              source = task.task_display_path;
              source_index = Int.succ index;
              source_count;
            };
          Ok analysis
      | Error error -> Error error)
