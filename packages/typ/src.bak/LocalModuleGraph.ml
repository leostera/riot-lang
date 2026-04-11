open Std
open Model
module Array = Collections.Array

type visible_module_name =
  | InternalName of LocalModules.InternalName.t
  | AmbientName of LocalModules.AmbientName.t

type 'a input_source = {
  payload: 'a;
  source_id: SourceId.t;
  internal_name: LocalModules.InternalName.t;
  visible_names: visible_module_name list;
  required_names: LocalModules.RequiredName.t list;
}

type group_id = int

type dependency_set_id = int

type 'a graph_source = {
  input: 'a input_source;
  required_names: LocalModules.RequiredName.t array;
  dependency_set_id: dependency_set_id;
  unresolved_local_names: LocalModules.RequiredName.t array;
}

type 'a group = {
  id: group_id;
  internal_name: LocalModules.InternalName.t;
  visible_names: visible_module_name array;
  sources: 'a graph_source list;
  dependency_ids: group_id array;
}

type 'a t = {
  groups: 'a group array;
  candidate_ids_by_required_name:
    (LocalModules.RequiredName.t, group_id array) Collections.HashMap.t;
  dependency_local_ids_by_set_id: group_id array array;
  group_id_by_source_id: (int, group_id) Collections.HashMap.t;
}

type cycle = {
  module_ids: group_id list;
  module_names: string list;
  source_ids: SourceId.t list;
}

let dedupe_by_key_preserving_order = fun ~key items ->
  let seen = Collections.HashSet.with_capacity (List.length items + 1) in
  items |> List.filter
    (fun item ->
      let item_key = key item in
      if Collections.HashSet.contains seen item_key then
        false
      else
        let _ = Collections.HashSet.insert seen item_key in
        true)

let dedupe_module_ids_preserving_order = fun module_ids ->
  dedupe_by_key_preserving_order ~key:(fun module_id -> module_id) module_ids

let visible_module_name_to_string = fun visible_name ->
  match visible_name with
  | InternalName internal_name -> LocalModules.InternalName.to_string internal_name
  | AmbientName ambient_name -> LocalModules.AmbientName.to_string ambient_name

let required_name_of_visible_module_name = fun visible_name ->
  match visible_name with
  | InternalName internal_name -> LocalModules.RequiredName.of_internal_name internal_name
  | AmbientName ambient_name -> LocalModules.RequiredName.of_ambient_name ambient_name

let required_names_of_parse_result = fun ~current_module_name ~parse_result ~implicit_opens ->
  let explicit_dependencies =
    match Syn.Deps.of_parse_result parse_result with
    | Ok deps -> Syn.Deps.modules deps
    | Error _ -> []
  in
  let implicit_opens = implicit_opens
  |> List.map SurfacePath.to_string
  |> List.filter
    (fun module_name ->
      LocalModules.should_include_implicit_open ~current_module_name ~module_name) in
  dedupe_by_key_preserving_order ~key:(fun required_name -> required_name)
    ((explicit_dependencies @ implicit_opens) |> List.map LocalModules.RequiredName.of_string)

let grouped_sources_by_internal_module = fun ordered_sources ->
  let module_order_rev = ref [] in
  let sources_by_module_name = Collections.HashMap.with_capacity 64 in
  ordered_sources |> List.iter
    (fun (source: 'a input_source) ->
      let module_name = source.internal_name in
      let existing_sources_rev =
        match Collections.HashMap.get sources_by_module_name module_name with
        | Some existing_sources_rev -> existing_sources_rev
        | None ->
            module_order_rev := module_name :: !module_order_rev;
            []
      in
      let _ = Collections.HashMap.insert
        sources_by_module_name
        module_name
        (source :: existing_sources_rev) in
      ());
  !module_order_rev
  |> List.rev
  |> List.filter_map
    (fun module_name ->
      Collections.HashMap.get sources_by_module_name module_name
      |> Option.map (fun sources_rev -> (module_name, List.rev sources_rev)))

let visible_name_arrays_for_group = fun internal_name sources ->
  let visible_names = sources
  |> List.concat_map (fun (source: 'a input_source) -> source.visible_names)
  |> dedupe_by_key_preserving_order ~key:(fun visible_name -> visible_name) in
  let visible_names =
    if List.exists
      (function
        | InternalName current -> current = internal_name
        | AmbientName _ -> false)
      visible_names
    then
      visible_names
    else
      InternalName internal_name :: visible_names
  in
  Array.of_list visible_names

let candidate_ids_by_required_name = fun groups ->
  let by_name_rev = Collections.HashMap.with_capacity (Array.length groups * 4) in
  groups |> Array.iter
    (fun (group: 'a group) ->
      group.visible_names |> Array.iter
        (fun visible_name ->
          let required_name = required_name_of_visible_module_name visible_name in
          let existing_rev =
            match Collections.HashMap.get by_name_rev required_name with
            | Some existing_rev -> existing_rev
            | None -> []
          in
          let _ = Collections.HashMap.insert by_name_rev required_name (group.id :: existing_rev) in
          ()));
  let by_name = Collections.HashMap.with_capacity (Array.length groups * 4) in
  Collections.HashMap.iter
    (fun required_name module_ids_rev ->
      let _ = Collections.HashMap.insert
        by_name
        required_name
        (Array.of_list (List.rev module_ids_rev)) in
      ())
    by_name_rev;
  by_name

let dependency_local_ids = fun graph dependency_set_id ->
  graph.dependency_local_ids_by_set_id.(dependency_set_id)

let best_matching_local_module_ids = fun graph (group: 'a group) ~required_module_name ->
  let best_depth = ref None in
  let matches_rev = ref [] in
  let candidate_ids =
    match Collections.HashMap.get graph.candidate_ids_by_required_name required_module_name with
    | Some candidate_ids -> candidate_ids
    | None -> [||]
  in
  candidate_ids |> Array.iter
    (fun candidate_id ->
      let candidate_group = graph.groups.(candidate_id) in
      if not (Int.equal candidate_id group.id) then
        match LocalModules.contextual_match_depth
          ~current_module_name:group.internal_name
          ~required_module_name
          ~candidate_module_name:candidate_group.internal_name with
        | None -> ()
        | Some depth ->
            let current_best = Option.unwrap_or ~default:depth !best_depth in
            if Option.is_none !best_depth || depth > current_best then
              (
                best_depth := Some depth;
                matches_rev := [ candidate_group.id ]
              )
            else if Int.equal depth current_best then
              matches_rev := candidate_group.id :: !matches_rev);
  List.rev !matches_rev |> Array.of_list

let resolution_ids_by_required_name = fun graph (group: 'a group) required_names ->
  let by_name = Collections.HashMap.with_capacity (List.length required_names + 1) in
  required_names
  |> dedupe_by_key_preserving_order ~key:(fun required_name -> required_name)
  |> List.iter
    (fun required_name ->
      let local_ids = best_matching_local_module_ids graph group ~required_module_name:required_name in
      let _ = Collections.HashMap.insert by_name required_name local_ids in
      ());
  by_name

let create = fun ~ordered_sources ->
  let grouped_sources = grouped_sources_by_internal_module ordered_sources in
  let grouped_sources_array = grouped_sources |> Array.of_list in
  let groups =
    grouped_sources_array
    |> Array.mapi
      (fun module_id (internal_name, sources) ->
        {
          id = module_id;
          internal_name;
          visible_names = visible_name_arrays_for_group internal_name sources;
          sources = [];
          dependency_ids = [||];
        })
  in
  let graph = {
    groups;
    candidate_ids_by_required_name = candidate_ids_by_required_name groups;
    dependency_local_ids_by_set_id = [||];
    group_id_by_source_id = Collections.HashMap.with_capacity (List.length ordered_sources + 1);
  } in
  let dependency_set_id_by_local_ids = Collections.HashMap.with_capacity
    (List.length ordered_sources + 1) in
  let next_dependency_set_id = ref 0 in
  let dependency_local_ids_rev = ref [] in
  let intern_dependency_set local_ids =
    let local_ids_key = Array.to_list local_ids in
    match Collections.HashMap.get dependency_set_id_by_local_ids local_ids_key with
    | Some dependency_set_id -> dependency_set_id
    | None ->
        let dependency_set_id = !next_dependency_set_id in
        next_dependency_set_id := dependency_set_id + 1;
        dependency_local_ids_rev := Array.copy local_ids :: !dependency_local_ids_rev;
        let _ = Collections.HashMap.insert dependency_set_id_by_local_ids local_ids_key dependency_set_id in
        dependency_set_id
  in
  let groups =
    Array.mapi
      (fun module_id (group: 'a group) ->
        let (_internal_name, sources) = grouped_sources_array.(module_id) in
        let source_requirements = sources
        |> List.map (fun (source: 'a input_source) -> (source, source.required_names)) in
        let resolution_ids_by_required_name = source_requirements
        |> List.concat_map snd
        |> resolution_ids_by_required_name graph group in
        let graph_sources_rev = ref [] in
        let dependency_ids_rev = ref [] in
        source_requirements |> List.iter
          (fun ((source: 'a input_source), required_names) ->
            let required_local_ids_rev = ref [] in
            let unresolved_local_names_rev = ref [] in
            required_names |> List.iter
              (fun required_module_name ->
                let local_ids =
                  match Collections.HashMap.get resolution_ids_by_required_name required_module_name with
                  | Some local_ids -> local_ids
                  | None -> [||]
                in
                if Array.length local_ids = 0 then
                  unresolved_local_names_rev := required_module_name :: !unresolved_local_names_rev
                else
                  required_local_ids_rev :=
                    List.rev_append (Array.to_list local_ids) !required_local_ids_rev);
            let required_local_ids =
              !required_local_ids_rev |> List.rev |> dedupe_module_ids_preserving_order |> Array.of_list in
            dependency_ids_rev := List.rev_append (Array.to_list required_local_ids) !dependency_ids_rev;
            let dependency_set_id = intern_dependency_set required_local_ids in
            graph_sources_rev := {
              input = source;
              required_names = Array.of_list required_names;
              dependency_set_id;
              unresolved_local_names = !unresolved_local_names_rev |> List.rev |> Array.of_list;
            }
            :: !graph_sources_rev;
            let _ = Collections.HashMap.insert
              graph.group_id_by_source_id
              (SourceId.to_int source.source_id)
              module_id in
            ())
        ;
        let dependency_ids = !dependency_ids_rev
        |> List.rev
        |> List.filter (fun dependency_id -> not (Int.equal dependency_id module_id))
        |> dedupe_module_ids_preserving_order
        |> Array.of_list in
        { group with sources = List.rev !graph_sources_rev; dependency_ids })
      groups
  in
  {
    graph with
    groups;
    dependency_local_ids_by_set_id = !dependency_local_ids_rev |> List.rev |> Array.of_list
  }

let cycle_module_ids = fun path repeated_id ->
  let rec loop seen = function
    | [] -> List.rev (repeated_id :: seen)
    | head :: tail ->
        let seen = head :: seen in
        if Int.equal head repeated_id then
          List.rev seen
        else
          loop seen tail
  in
  loop [] path

let cycle_of_module_ids = fun graph module_ids ->
  let module_names = module_ids
  |> List.map
    (fun module_id -> LocalModules.InternalName.to_string graph.groups.(module_id).internal_name) in
  let source_ids = module_ids
  |> List.concat_map
    (fun module_id ->
      graph.groups.(module_id).sources
      |> List.map (fun (source: 'a graph_source) -> source.input.source_id))
  |> List.sort_uniq SourceId.compare in
  { module_ids; module_names; source_ids }

let reachable_group_ids = fun graph ~roots ->
  let root_group_ids = roots
  |> List.filter_map
    (fun source_id ->
      Collections.HashMap.get graph.group_id_by_source_id (SourceId.to_int source_id))
  |> dedupe_module_ids_preserving_order in
  let seen = Collections.HashSet.with_capacity (List.length root_group_ids + 1) in
  let rec discover ordered_rev = function
    | [] -> List.rev ordered_rev
    | group_id :: rest ->
        if Collections.HashSet.contains seen group_id then
          discover ordered_rev rest
        else
          let _ = Collections.HashSet.insert seen group_id in
          let dependencies = graph.groups.(group_id).dependency_ids |> Array.to_list in
          discover (group_id :: ordered_rev) (dependencies @ rest)
  in
  discover [] root_group_ids

let ordered_group_ids_from = fun graph group_ids ->
  let relevant = Collections.HashSet.of_list group_ids in
  let state = Array.make (Array.length graph.groups) 0 in
  let rec visit path ordered module_id =
    if not (Collections.HashSet.contains relevant module_id) then
      Ok ordered
    else
      match state.(module_id) with
      | 2 ->
          Ok ordered
      | 1 ->
          Error (cycle_module_ids path module_id |> cycle_of_module_ids graph)
      | _ ->
          state.(module_id) <- 1;
          let result =
            graph.groups.(module_id).dependency_ids
            |> Array.fold_left
              (fun result dependency_id ->
                match result with
                | Error _ as err -> err
                | Ok ordered -> visit (module_id :: path) ordered dependency_id)
              (Ok ordered)
          in
          (
            match result with
            | Error _ as err -> err
            | Ok ordered ->
                state.(module_id) <- 2;
                Ok (module_id :: ordered)
          )
  in
  group_ids
  |> List.fold_left
    (fun result module_id ->
      match result with
      | Error _ as err -> err
      | Ok ordered -> visit [] ordered module_id)
    (Ok [])
  |> Result.map List.rev

let ordered_group_ids = fun graph ->
  graph.groups
  |> Array.to_list
  |> List.map (fun (group: 'a group) -> group.id)
  |> ordered_group_ids_from graph

let ordered_subset_group_ids = fun graph ~group_ids ->
  ordered_group_ids_from graph group_ids

let closure_group_ids = fun graph ~roots ->
  reachable_group_ids graph ~roots

let ordered_closure_group_ids = fun graph ~roots ->
  closure_group_ids graph ~roots |> ordered_group_ids_from graph

let closure_source_ids = fun graph ~roots ->
  closure_group_ids graph ~roots
  |> List.concat_map
    (fun group_id ->
      graph.groups.(group_id).sources
      |> List.map (fun (source: 'a graph_source) -> source.input.source_id))
  |> List.sort_uniq SourceId.compare
