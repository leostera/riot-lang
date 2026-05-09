open Std

module ConcurrentHashMap = Collections.ConcurrentHashMap

type t = {
  analyses: (Source_analysis.key, Riot_planner.Module_graph.source_analysis) ConcurrentHashMap.t;
}

let create = fun () -> { analyses = ConcurrentHashMap.with_capacity ~size:512 }

let find = fun t key -> ConcurrentHashMap.get t.analyses ~key

let missing = fun t ~package tasks ->
  tasks
  |> List.filter_map
    ~fn:(fun task ->
      let key = Source_analysis.key_from_task ~package task in
      match find t key with
      | Some _ -> None
      | None -> Some (Source_analysis.make ~package ~task))

let execute = fun t (source: Source_analysis.t) ->
  match Riot_planner.Module_graph.analyze_source source.task with
  | Ok analysis ->
      ignore (ConcurrentHashMap.insert t.analyses ~key:source.key ~value:analysis);
      Ok ()
  | Error error ->
      Error (Error.SourceAnalysisFailed {
        source = source.task.task_display_path;
        reason = Riot_planner.Planning_error.to_string error;
      })

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
