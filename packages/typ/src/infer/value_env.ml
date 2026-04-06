open Std
open Analysis
open Model

type t = Check_result.env

type scope_entries = (string * t) list

type scope_opens = (string * string list) list

let unique = fun env ->
  let seen = Collections.HashSet.with_capacity (List.length env) in
  let rec loop acc = function
    | [] -> List.rev acc
    | (name, scheme) :: rest ->
        if Collections.HashSet.contains seen name then
          loop acc rest
        else
          let () = Collections.HashSet.insert seen name |> ignore in
          loop ((name, scheme) :: acc) rest
  in
  loop [] env

let render = fun env ->
  env |> unique |> List.sort
    (fun (left, _) (right, _) ->
      String.compare left right)

let visible_entries = fun env ->
  let seen = Collections.HashSet.with_capacity (List.length env) in
  let rec loop acc = function
    | [] -> List.rev acc
    | ((name, _) as entry) :: rest ->
        if Collections.HashSet.contains seen name then
          loop acc rest
        else
          let () = Collections.HashSet.insert seen name |> ignore in
          loop (entry :: acc) rest
  in
  loop [] env

let lookup = fun env name ->
  List.find_opt
    (fun (candidate, _) ->
      String.equal candidate name)
    env

let lookup_all = fun env name ->
  env |> List.filter_map
    (fun (candidate, scheme) ->
      if String.equal candidate name then
        Some scheme
      else
        None)

let names = fun env -> render env |> List.map fst

let introduced_names = fun before after ->
  let before_name_set =
    visible_entries before
    |> List.fold_left
      (fun seen (name, _) ->
        let () = Collections.HashSet.insert seen name |> ignore in
        seen)
      (Collections.HashSet.with_capacity (List.length before))
  in
  visible_entries after |> List.filter_map
    (fun (name, _) ->
      if Collections.HashSet.contains before_name_set name then
        None
      else
        Some name)

let bind = fun env bindings -> bindings @ env

let has_prefix = fun ~prefix text ->
  let prefix_length = String.length prefix in
  if String.length text < prefix_length then
    false
  else
    String.sub text 0 prefix_length = prefix

let aliases_for_local_open = fun env module_path ->
  let prefix = module_path ^ "." in
  env |> List.filter_map
    (fun (name, scheme) ->
      if has_prefix ~prefix name then
        let suffix = String.sub
          name
          (String.length prefix)
          (String.length name - String.length prefix) in
        Some (suffix, scheme)
      else
        None)

let with_local_open = fun env module_path ->
  let aliases = aliases_for_local_open env module_path in
  bind env aliases

let entries_for_include = fun env module_path ->
  aliases_for_local_open env module_path |> unique |> render

let prefix_entries = fun prefix entries ->
  entries |> List.map (fun (name, scheme) -> (prefix ^ "." ^ name, scheme))

let export_names_for_module_alias = fun env ~alias_name ~module_path ->
  aliases_for_local_open env module_path
  |> unique
  |> render
  |> prefix_entries alias_name
  |> List.map fst

let entries_for_module_alias = fun env ~alias_name ~module_path ->
  if String.equal alias_name module_path then
    []
  else
    aliases_for_local_open env module_path |> unique |> render |> prefix_entries alias_name

let prelude_names = fun (config: TypConfig.t) -> config.prelude |> List.map fst

let ambient_names = fun (config: TypConfig.t) -> config.ambient |> List.map fst

let export = fun config env ->
  let hidden_names = prelude_names config @ ambient_names config in
  let hidden_name_set = Collections.HashSet.of_list hidden_names in
  render env
  |> List.filter (fun (name, _) -> not (Collections.HashSet.contains hidden_name_set name))

let export_with_forced_names = fun (state: State.t) env ->
  let hidden_names = prelude_names state.config @ ambient_names state.config in
  let hidden_name_set = Collections.HashSet.of_list hidden_names in
  let forced_name_set = Collections.HashSet.of_list state.forced_export_names in
  render env
  |> List.filter
    (fun (name, _) ->
      not (Collections.HashSet.contains hidden_name_set name)
      || Collections.HashSet.contains forced_name_set name)

let introduced_entries = fun before after ->
  let before_name_set =
    visible_entries before
    |> List.fold_left
      (fun seen (name, _) ->
        let () = Collections.HashSet.insert seen name |> ignore in
        seen)
      (Collections.HashSet.with_capacity (List.length before))
  in
  visible_entries after
  |> List.filter (fun (name, _) -> not (Collections.HashSet.contains before_name_set name))

let qualify_entries = fun scope_path entries ->
  List.map (fun (name, scheme) -> (State.qualify_name scope_path name, scheme)) entries

let scope_key = fun scope_path ->
  String.concat "." scope_path

let scope_prefix_keys = fun scope_path ->
  let rec loop acc current = function
    | [] -> List.rev acc
    | segment :: rest ->
        let current = current @ [ segment ] in
        loop (scope_key current :: acc) current rest
  in
  loop [ scope_key [] ] [] scope_path

let scope_locals_for = fun scope_entries scope_path ->
  scope_prefix_keys scope_path |> List.fold_left
    (fun acc key ->
      match List.assoc_opt key scope_entries with
      | Some entries -> bind acc entries
      | None -> acc)
    []

let update_scope_entries = fun scope_entries scope_path entries ->
  let key = scope_key scope_path in
  let existing =
    match List.assoc_opt key scope_entries with
    | Some entries -> entries
    | None -> []
  in
  let updated = bind existing entries in
  (key, updated) :: List.remove_assoc key scope_entries

let scope_opens_for = fun scope_opens scope_path ->
  scope_prefix_keys scope_path |> List.fold_left
    (fun acc key ->
      match List.assoc_opt key scope_opens with
      | Some modules -> acc @ modules
      | None -> acc)
    []

let update_scope_opens = fun scope_opens scope_path module_path ->
  let key = scope_key scope_path in
  let existing =
    match List.assoc_opt key scope_opens with
    | Some modules -> modules
    | None -> []
  in
  let updated = existing @ [ module_path ] in
  (key, updated) :: List.remove_assoc key scope_opens

let for_item_scope = fun export_env scope_entries scope_opens scope_path ->
  let locals = scope_locals_for scope_entries scope_path in
  let base_env = bind export_env locals in
  scope_opens_for scope_opens scope_path |> List.fold_left with_local_open base_env
