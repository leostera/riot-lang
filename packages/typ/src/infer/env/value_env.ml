open Std
open Analysis
open Model

module Name_map = Collections.Map.Make (String)

let visible_name_for_lookup = function
  | path -> (
      match IdentPath.to_segments path with
      | [ name ] when not (String.equal name "") -> Some name
      | _ -> None
    )

type t = {
  bindings: Binding.t list;
  by_name: Binding.t list Name_map.t;
}

let empty = { bindings = []; by_name = Name_map.empty }

let ident_name_of_path = fun path ->
  match IdentPath.last_name path with
  | Some name -> name
  | None -> ""

let prepend_binding = fun index binding ->
  match visible_name_for_lookup (Binding.path binding) with
  | Some name ->
      let existing = Name_map.find_opt name index |> Option.unwrap_or ~default:[] in
      Name_map.add name (binding :: existing) index
  | None -> index

let index_of_bindings = fun bindings ->
  bindings |> List.rev |> List.fold_left prepend_binding Name_map.empty

let of_bindings = fun bindings -> { bindings; by_name = index_of_bindings bindings }

let of_entries = fun ~make_ident ~provenance entries ->
  entries |> List.map
    (fun (path, scheme) ->
      let name = ident_name_of_path path in
      Binding.make ~ident:(make_ident name) ~path ~scheme ~provenance) |> of_bindings

let singleton = fun ~make_ident ~name ~scheme ~provenance ->
  [ Binding.make ~ident:(make_ident name) ~path:(IdentPath.of_name name) ~scheme ~provenance ] |> of_bindings

let bindings = fun env -> env.bindings

let unique = fun env ->
  let seen = Collections.HashSet.with_capacity (List.length env.bindings) in
  let rec loop acc = function
    | [] -> List.rev acc
    | binding :: rest ->
        if Collections.HashSet.contains seen (Binding.path binding) then
          loop acc rest
        else
          let () = Collections.HashSet.insert seen (Binding.path binding) |> ignore in
          loop (binding :: acc) rest
  in
  loop [] env.bindings |> of_bindings

let canonicalize = fun env ->
  env |> unique |> bindings |> List.sort
    (fun left right ->
      IdentPath.compare (Binding.path left) (Binding.path right)) |> of_bindings

let render = fun env -> canonicalize env |> bindings |> List.map Binding.render

let visible_entries = fun env ->
  let seen = Collections.HashSet.with_capacity (List.length env.bindings) in
  let rec loop acc = function
    | [] -> List.rev acc
    | binding :: rest ->
        if Collections.HashSet.contains seen (Binding.path binding) then
          loop acc rest
        else
          let () = Collections.HashSet.insert seen (Binding.path binding) |> ignore in
          loop (binding :: acc) rest
  in
  loop [] env.bindings |> of_bindings

let lookup = fun env path ->
  match visible_name_for_lookup path with
  | Some name -> (
      match Name_map.find_opt name env.by_name with
      | Some (binding :: _) -> Some binding
      | _ -> None
    )
  | None -> None

let lookup_all = fun env path ->
  match visible_name_for_lookup path with
  | Some name -> Name_map.find_opt name env.by_name |> Option.unwrap_or ~default:[]
  | None -> []

let names = fun env ->
  canonicalize env
  |> bindings
  |> List.map (fun binding -> Binding.path binding |> IdentPath.to_string)

let introduced_names = fun before after ->
  let before_name_set =
    visible_entries before
    |> bindings
    |> List.fold_left
      (fun seen binding ->
        let () = Collections.HashSet.insert seen (Binding.path binding) |> ignore in
        seen)
      (Collections.HashSet.with_capacity (List.length (bindings before)))
  in
  visible_entries after |> bindings |> List.filter_map
    (fun binding ->
      let path = Binding.path binding in
      if Collections.HashSet.contains before_name_set path then
        None
      else
        Some (IdentPath.to_string path))

let bind = fun env introduced ->
  let by_name =
    Name_map.fold
      (fun name introduced_bindings acc ->
        let existing = Name_map.find_opt name acc |> Option.unwrap_or ~default:[] in
        Name_map.add name (introduced_bindings @ existing) acc)
      introduced.by_name
      env.by_name
  in
  { bindings = introduced.bindings @ env.bindings; by_name }

let aliases_for_local_open = fun env module_path ->
  env.bindings |> List.filter_map
    (fun binding ->
      match IdentPath.strip_prefix ~prefix:module_path (Binding.path binding) with
      | Some suffix when not (IdentPath.is_empty suffix) -> Some (Binding.with_path suffix binding)
      | _ -> None) |> of_bindings

let with_local_open = fun env module_path ->
  let aliases = aliases_for_local_open env module_path in
  bind env aliases

let entries_for_include = fun env module_path -> aliases_for_local_open env module_path |> canonicalize

let prefix_entries = fun prefix entries ->
  entries |> bindings |> List.map
    (fun binding ->
      Binding.with_path (IdentPath.prepend_name prefix (Binding.path binding)) binding) |> of_bindings

let export_names_for_module_alias = fun env ~alias_name ~module_path ->
  aliases_for_local_open env module_path
  |> canonicalize
  |> prefix_entries alias_name
  |> bindings
  |> List.map (fun binding -> Binding.path binding |> IdentPath.to_string)

let entries_for_module_alias = fun env ~alias_name ~module_path ->
  aliases_for_local_open env module_path
  |> canonicalize
  |> bindings
  |> List.map
    (fun binding ->
      binding
      |> Binding.with_path (IdentPath.prepend_name alias_name (Binding.path binding))
      |> Binding.with_provenance (Binding.Module_alias { alias_name; module_path }))
  |> of_bindings

let prelude_names = fun (config: TypConfig.t) -> config.prelude |> List.map fst

let ambient_names = fun (config: TypConfig.t) -> config.ambient |> List.map fst

let export = fun config env ->
  let hidden_names = prelude_names config @ ambient_names config in
  let hidden_name_set = Collections.HashSet.of_list hidden_names in
  canonicalize env
  |> bindings
  |> List.filter
    (fun binding -> not (Collections.HashSet.contains hidden_name_set (Binding.path binding)))
  |> of_bindings

let export_with_forced_names = fun (state: State.t) env ->
  let hidden_names = prelude_names state.config @ ambient_names state.config in
  let hidden_name_set = Collections.HashSet.of_list hidden_names in
  let forced_name_set = Collections.HashSet.of_list state.forced_export_names in
  canonicalize env |> bindings |> List.filter
    (fun binding ->
      let path = Binding.path binding in
      let name = IdentPath.to_string path in
      not (Collections.HashSet.contains hidden_name_set path)
      || Collections.HashSet.contains forced_name_set name) |> of_bindings

let introduced_entries = fun before after ->
  let before_name_set =
    visible_entries before
    |> bindings
    |> List.fold_left
      (fun seen binding ->
        let () = Collections.HashSet.insert seen (Binding.path binding) |> ignore in
        seen)
      (Collections.HashSet.with_capacity (List.length (bindings before)))
  in
  visible_entries after
  |> bindings
  |> List.filter
    (fun binding -> not (Collections.HashSet.contains before_name_set (Binding.path binding)))
  |> of_bindings

let qualify_entries = fun scope_path entries ->
  entries |> bindings |> List.map
    (fun binding ->
      Binding.with_path (IdentPath.append_path scope_path (Binding.path binding)) binding) |> of_bindings
