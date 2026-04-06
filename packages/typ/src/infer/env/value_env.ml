open Std
open Analysis
open Model

type t = Binding.t list

type scope_entries = (IdentPath.t * t) list

type scope_opens = (IdentPath.t * IdentPath.t list) list

let of_entries = fun ~provenance entries ->
  entries |> List.map
    (fun (path, scheme) -> Binding.make ~path ~scheme ~provenance)

let singleton = fun ~name ~scheme ~provenance ->
  [ Binding.make ~path:(IdentPath.of_name name) ~scheme ~provenance ]

let unique = fun env ->
  let seen = Collections.HashSet.with_capacity (List.length env) in
  let rec loop acc = function
    | [] -> List.rev acc
    | binding :: rest ->
        if Collections.HashSet.contains seen (Binding.path binding) then
          loop acc rest
        else
          let () = Collections.HashSet.insert seen (Binding.path binding) |> ignore in
          loop (binding :: acc) rest
  in
  loop [] env

let render_bindings = fun env ->
  env |> unique |> List.sort
    (fun left right ->
      String.compare
        (Binding.path left |> IdentPath.to_string)
        (Binding.path right |> IdentPath.to_string))

let render = fun env ->
  render_bindings env |> List.map Binding.render

let visible_entries = fun env ->
  let seen = Collections.HashSet.with_capacity (List.length env) in
  let rec loop acc = function
    | [] -> List.rev acc
    | binding :: rest ->
        if Collections.HashSet.contains seen (Binding.path binding) then
          loop acc rest
        else
          let () = Collections.HashSet.insert seen (Binding.path binding) |> ignore in
          loop (binding :: acc) rest
  in
  loop [] env

let lookup = fun env path ->
  List.find_opt
    (fun binding ->
      IdentPath.equal (Binding.path binding) path)
    env

let lookup_all = fun env path ->
  env |> List.filter
    (fun binding ->
      IdentPath.equal (Binding.path binding) path)

let names = fun env -> render_bindings env |> List.map (fun binding -> Binding.path binding |> IdentPath.to_string)

let introduced_names = fun before after ->
  let before_name_set =
    visible_entries before
    |> List.fold_left
      (fun seen binding ->
        let () = Collections.HashSet.insert seen (Binding.path binding) |> ignore in
        seen)
      (Collections.HashSet.with_capacity (List.length before))
  in
  visible_entries after |> List.filter_map
    (fun binding ->
      let path = Binding.path binding in
      if Collections.HashSet.contains before_name_set path then
        None
      else
        Some (IdentPath.to_string path))

let bind = fun env bindings -> bindings @ env

let aliases_for_local_open = fun env module_path ->
  env |> List.filter_map
    (fun binding ->
      match IdentPath.strip_prefix ~prefix:module_path (Binding.path binding) with
      | Some suffix when not (IdentPath.is_empty suffix) -> Some (Binding.with_path suffix binding)
      | _ -> None)

let with_local_open = fun env module_path ->
  let aliases = aliases_for_local_open env module_path in
  bind env aliases

let entries_for_include = fun env module_path ->
  aliases_for_local_open env module_path |> render_bindings

let prefix_entries = fun prefix entries ->
  entries |> List.map
    (fun binding -> Binding.with_path (IdentPath.prepend_name prefix (Binding.path binding)) binding)

let export_names_for_module_alias = fun env ~alias_name ~module_path ->
  aliases_for_local_open env module_path
  |> render_bindings
  |> prefix_entries alias_name
  |> List.map (fun binding -> Binding.path binding |> IdentPath.to_string)

let entries_for_module_alias = fun env ~alias_name ~module_path ->
  aliases_for_local_open env module_path
  |> render_bindings
  |> List.map
    (fun binding ->
      Binding.make
        ~path:(IdentPath.prepend_name alias_name (Binding.path binding))
        ~scheme:(Binding.scheme binding)
        ~provenance:(Binding.Module_alias { alias_name; module_path }))

let prelude_names = fun (config: TypConfig.t) -> config.prelude |> List.map fst

let ambient_names = fun (config: TypConfig.t) -> config.ambient |> List.map fst

let export = fun config env ->
  let hidden_names = prelude_names config @ ambient_names config in
  let hidden_name_set = Collections.HashSet.of_list hidden_names in
  render_bindings env
  |> List.filter
    (fun binding ->
      not (Collections.HashSet.contains hidden_name_set (Binding.path binding)))

let export_with_forced_names = fun (state: State.t) env ->
  let hidden_names = prelude_names state.config @ ambient_names state.config in
  let hidden_name_set = Collections.HashSet.of_list hidden_names in
  let forced_name_set = Collections.HashSet.of_list state.forced_export_names in
  render_bindings env
  |> List.filter
    (fun binding ->
      let path = Binding.path binding in
      let name = IdentPath.to_string path in
      not (Collections.HashSet.contains hidden_name_set path)
      || Collections.HashSet.contains forced_name_set name)

let introduced_entries = fun before after ->
  let before_name_set =
    visible_entries before
    |> List.fold_left
      (fun seen binding ->
        let () = Collections.HashSet.insert seen (Binding.path binding) |> ignore in
        seen)
      (Collections.HashSet.with_capacity (List.length before))
  in
  visible_entries after
  |> List.filter (fun binding -> not (Collections.HashSet.contains before_name_set (Binding.path binding)))

let qualify_entries = fun scope_path entries ->
  List.map
    (fun binding ->
      Binding.with_path (IdentPath.append_path scope_path (Binding.path binding)) binding)
    entries

let scope_locals_for = fun scope_entries scope_path ->
  IdentPath.prefixes scope_path |> List.fold_left
    (fun acc key ->
      match List.assoc_opt key scope_entries with
      | Some entries -> bind acc entries
      | None -> acc)
    []

let update_scope_entries = fun scope_entries scope_path entries ->
  let existing =
    match List.assoc_opt scope_path scope_entries with
    | Some entries -> entries
    | None -> []
  in
  let updated = bind existing entries in
  (scope_path, updated) :: List.remove_assoc scope_path scope_entries

let scope_opens_for = fun scope_opens scope_path ->
  IdentPath.prefixes scope_path |> List.fold_left
    (fun acc key ->
      match List.assoc_opt key scope_opens with
      | Some modules -> acc @ modules
      | None -> acc)
    []

let update_scope_opens = fun scope_opens scope_path module_path ->
  let existing =
    match List.assoc_opt scope_path scope_opens with
    | Some modules -> modules
    | None -> []
  in
  let updated = existing @ [ module_path ] in
  (scope_path, updated) :: List.remove_assoc scope_path scope_opens

let for_item_scope = fun export_env scope_entries scope_opens scope_path ->
  let locals = scope_locals_for scope_entries scope_path in
  let base_env = bind export_env locals in
  scope_opens_for scope_opens scope_path |> List.fold_left with_local_open base_env
