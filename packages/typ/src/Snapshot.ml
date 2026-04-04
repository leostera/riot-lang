open Std

type analysis_slot = {
  source_id: SourceId.t;
  source: Source.t;
  config: TypConfig.t;
  mutable base_analysis: SourceAnalysis.t option;
  mutable analysis: SourceAnalysis.t option;
}

type t = {
  revision: int;
  analyses: analysis_slot list;
  mutable qualified_exports: (SourceId.t * TypConfig.env) list option;
}

let make = fun ~revision ~config ~sources ->
  let analyses = sources
  |> List.map
    (fun (source: Source.t) ->
      { source_id = source.source_id; source; config; base_analysis = None; analysis = None }) in
  { revision; analyses; qualified_exports = None }

let force_base_analysis = fun (slot: analysis_slot) ->
  match slot.base_analysis with
  | Some analysis -> analysis
  | None ->
      let analysis = SourceAnalysis.analyze ~config:slot.config slot.source in
      let () =
        slot.base_analysis <- Some analysis
      in
      analysis

let sanitize_module_name = fun name ->
  String.map (fun ch -> if ch = '-' then '_' else ch) name

let implicit_module_name_of_source = fun (source: Source.t) ->
  let raw_name =
    match source.origin with
    | Source.Path path -> Path.remove_extension path |> Path.basename
    | Source.Label label ->
        label
        |> Path.v
        |> Path.remove_extension
        |> Path.basename
  in
  sanitize_module_name raw_name |> String.capitalize_ascii

let qualify_exports = fun module_name exports ->
  List.map (fun (name, scheme) -> (module_name ^ "." ^ name, scheme)) exports

let qualified_exports = fun (snapshot: t) ->
  match snapshot.qualified_exports with
  | Some exports -> exports
  | None ->
      let exports =
        snapshot.analyses
        |> List.map
          (fun (slot: analysis_slot) ->
            let analysis = force_base_analysis slot in
            let module_name = implicit_module_name_of_source slot.source in
            (slot.source_id, SourceAnalysis.exports analysis |> qualify_exports module_name))
      in
      let () =
        snapshot.qualified_exports <- Some exports
      in
      exports

let ambient_env_for = fun (snapshot: t) source_id ->
  qualified_exports snapshot
  |> List.filter (fun (candidate_source_id, _) -> not (SourceId.equal candidate_source_id source_id))
  |> List.map snd
  |> List.flatten

let force_analysis = fun (snapshot: t) (slot: analysis_slot) ->
  match slot.analysis with
  | Some analysis -> analysis
  | None ->
      let config =
        TypConfig.with_ambient slot.config ~ambient:(ambient_env_for snapshot slot.source_id)
      in
      let analysis = SourceAnalysis.analyze ~config slot.source in
      let () =
        slot.analysis <- Some analysis
      in
      analysis

let revision = fun snapshot -> snapshot.revision

let analyses = fun snapshot -> snapshot.analyses |> List.map (force_analysis snapshot)

let find_analysis = fun snapshot source_id ->
  List.find_opt
    (fun (slot: analysis_slot) ->
      SourceId.equal slot.source_id source_id)
  snapshot.analyses |> function
  | Some slot -> Some (force_analysis snapshot slot)
  | None -> None
