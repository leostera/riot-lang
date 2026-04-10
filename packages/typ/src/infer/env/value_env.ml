open Std
open Analysis
open Model

module Name_map = Collections.Map.Make (String)

module Id_map = Collections.Map.Make (struct
  type t = int

  let compare = Int.compare
end)

module Path_map = Collections.Map.Make (struct
  type t = IdentPath.t

  let compare = IdentPath.compare
end)

type current = Binding.t list Name_map.t

type same = Binding.t Id_map.t

type components = Binding.t Name_map.t

type layer =
  | Nothing
  | Open of { root: IdentPath.t; components: components; next: t }
  | Map of { map_binding: Binding.t -> Binding.t; next: t }

and t = {
  current: current;
  same: same;
  layer: layer;
}

let empty = { current = Name_map.empty; same = Id_map.empty; layer = Nothing }

let is_empty = fun env ->
  Name_map.is_empty env.current && Id_map.is_empty env.same && match env.layer with
  | Nothing -> true
  | Open _
  | Map _ -> false

let visible_name_for_lookup = IdentPath.bare_name

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

let add_same = fun acc binding ->
  let local_id = Binding.ident binding |> Binding.ident_local_id in
  Id_map.add local_id binding acc

let current_of_bindings = fun bindings ->
  bindings |> List.rev |> List.fold_left prepend_binding Name_map.empty

let same_of_bindings = fun bindings -> bindings |> List.fold_left add_same Id_map.empty

let current_visible_components = fun current ->
  Name_map.fold
    (fun name bindings acc ->
      match bindings with
      | binding :: _ -> Name_map.add name binding acc
      | [] -> acc)
    current
    Name_map.empty

let merge_visible_components = fun dominant rest ->
  Name_map.fold
    (fun name binding acc ->
      if Name_map.mem name acc then
        acc
      else
        Name_map.add name binding acc)
    rest
    dominant

let of_bindings = fun bindings ->
  { current = current_of_bindings bindings; same = same_of_bindings bindings; layer = Nothing }

let of_entries = fun ~make_ident ~provenance entries ->
  entries |> List.map
    (fun (path, scheme) ->
      let name = ident_name_of_path path in
      Binding.make ~ident:(make_ident name) ~path ~scheme ~provenance) |> of_bindings

let singleton = fun ~make_ident ~name ~scheme ~provenance ->
  of_bindings
    [ Binding.make ~ident:(make_ident name) ~path:(IdentPath.of_name name) ~scheme ~provenance ]

let current_bindings = fun current -> Name_map.bindings current |> List.concat_map snd

let rec visible_components = fun env ->
  let current = current_visible_components env.current in
  match env.layer with
  | Nothing -> current
  | Open { components; next; _ } -> current
  |> merge_visible_components components
  |> merge_visible_components (visible_components next)
  | Map { map_binding; next } ->
      current |> merge_visible_components
        (visible_components next |> Name_map.map map_binding)

let bindings =
  let rec loop acc env =
    let acc = List.rev_append (current_bindings env.current) acc in
    match env.layer with
    | Nothing -> acc
    | Open { next; _ } -> loop acc next
    | Map { map_binding; next } -> loop acc next |> List.map map_binding
  in
  fun env -> loop [] env |> List.rev

let local_only = fun env -> env |> bindings |> of_bindings

let map = fun map_binding env ->
  if is_empty env then
    env
  else
    { current = Name_map.empty; same = Id_map.empty; layer = Map { map_binding; next = env } }

let qualify_binding = fun root binding ->
  Binding.with_path (IdentPath.append_path root (Binding.path binding)) binding

let add_open = fun ~root opened env ->
  {
    current = Name_map.empty;
    same = Id_map.empty;
    layer = Open { root; components = visible_components opened; next = env }
  }

let merge_current = fun introduced existing ->
  Name_map.fold
    (fun name introduced_bindings acc ->
      let current = Name_map.find_opt name acc |> Option.unwrap_or ~default:[] in
      Name_map.add name (introduced_bindings @ current) acc)
    introduced
    existing

let bind = fun env introduced ->
  if is_empty introduced then
    env
  else if is_empty env then
    introduced
  else
    {
      current = merge_current introduced.current env.current;
      same = Id_map.fold Id_map.add introduced.same env.same;
      layer = env.layer
    }

let rec find_same = fun env ident ->
  let local_id = Binding.ident_local_id ident in
  match Id_map.find_opt local_id env.same with
  | Some binding -> Some binding
  | None -> (
      match env.layer with
      | Nothing -> None
      | Open { next; _ } -> find_same next ident
      | Map { map_binding; next } -> find_same next ident |> Option.map map_binding
    )

let rec lookup_name = fun env name ->
  match Name_map.find_opt name env.current with
  | Some (binding :: _) -> Some binding
  | _ -> (
      match env.layer with
      | Nothing ->
          None
      | Open { root; components; next } -> (
          match Name_map.find_opt name components with
          | Some binding -> Some (qualify_binding root binding)
          | None -> lookup_name next name
        )
      | Map { map_binding; next } ->
          lookup_name next name |> Option.map map_binding
    )

let lookup = fun env path ->
  match visible_name_for_lookup path with
  | Some name -> lookup_name env name
  | None -> None

let rec lookup_all_name = fun env name ->
  let current = Name_map.find_opt name env.current |> Option.unwrap_or ~default:[] in
  match env.layer with
  | Nothing ->
      current
  | Open { root; components; next } ->
      let opened =
        match Name_map.find_opt name components with
        | Some binding -> [ qualify_binding root binding ]
        | None -> []
      in
      current @ opened @ lookup_all_name next name
  | Map { map_binding; next } ->
      current @ (lookup_all_name next name |> List.map map_binding)

let lookup_all = fun env path ->
  match visible_name_for_lookup path with
  | Some name -> lookup_all_name env name
  | None -> []

let unique = fun env ->
  let seen = Collections.HashSet.with_capacity 32 in
  let rec loop acc = function
    | [] -> List.rev acc
    | binding :: rest ->
        if Collections.HashSet.contains seen (Binding.path binding) then
          loop acc rest
        else (
          Collections.HashSet.insert seen (Binding.path binding) |> ignore;
          loop (binding :: acc) rest
        )
  in
  loop [] (bindings env) |> of_bindings

let canonicalize = fun env ->
  env |> unique |> bindings |> List.sort
    (fun left right ->
      IdentPath.compare (Binding.path left) (Binding.path right)) |> of_bindings

let render = fun env -> canonicalize env |> bindings |> List.map Binding.render

let visible_entries = fun env ->
  let seen = Collections.HashSet.with_capacity 32 in
  let rec loop acc = function
    | [] -> List.rev acc
    | binding :: rest ->
        if Collections.HashSet.contains seen (Binding.path binding) then
          loop acc rest
        else (
          Collections.HashSet.insert seen (Binding.path binding) |> ignore;
          loop (binding :: acc) rest
        )
  in
  loop [] (bindings env) |> of_bindings

let visible_binding_map = fun env ->
  visible_entries env |> bindings |> List.fold_left
    (fun acc binding ->
      Path_map.add (Binding.path binding) binding acc)
    Path_map.empty

let names = fun env ->
  canonicalize env
  |> bindings
  |> List.map (fun binding -> Binding.path binding |> IdentPath.to_string)

let introduced_names = fun before after ->
  let before_bindings = visible_binding_map before in
  visible_entries after |> bindings |> List.filter_map
    (fun binding ->
      let path = Binding.path binding in
      match Path_map.find_opt path before_bindings with
      | Some previous when Binding.same previous binding -> None
      | _ -> Some (IdentPath.to_string path))

let aliases_for_local_open = fun env module_path ->
  bindings env |> List.filter_map
    (fun binding ->
      match IdentPath.strip_prefix ~prefix:module_path (Binding.path binding) with
      | Some suffix when not (IdentPath.is_empty suffix) -> Some (Binding.with_path suffix binding)
      | _ -> None) |> of_bindings

let with_local_open = fun env module_path ->
  let aliases = aliases_for_local_open env module_path in
  bind env aliases

let entries_for_include = fun env module_path -> aliases_for_local_open env module_path |> canonicalize

let prefix_entries = fun prefix entries ->
  map
    (fun binding ->
      Binding.with_path (IdentPath.prepend_name prefix (Binding.path binding)) binding)
    entries

let export_names_for_module_alias = fun env ~alias_name ~module_path ->
  aliases_for_local_open env module_path
  |> canonicalize
  |> prefix_entries alias_name
  |> bindings
  |> List.map (fun binding -> Binding.path binding |> IdentPath.to_string)

let entries_for_module_alias = fun env ~alias_name ~module_path ->
  aliases_for_local_open env module_path
  |> canonicalize
  |> map
    (fun binding ->
      binding
      |> Binding.with_path (IdentPath.prepend_name alias_name (Binding.path binding))
      |> Binding.with_provenance (Binding.ModuleAlias { alias_name; module_path }))

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

let export_with_forced_names = fun ~config ~forced_export_names env ->
  let hidden_names = prelude_names config @ ambient_names config in
  let hidden_name_set = Collections.HashSet.of_list hidden_names in
  let forced_name_set = Collections.HashSet.of_list forced_export_names in
  canonicalize env |> bindings |> List.filter
    (fun binding ->
      let path = Binding.path binding in
      let name = IdentPath.to_string path in
      not (Collections.HashSet.contains hidden_name_set path)
      || Collections.HashSet.contains forced_name_set name) |> of_bindings

let introduced_entries = fun before after ->
  let before_bindings = visible_binding_map before in
  visible_entries after |> bindings |> List.filter
    (fun binding ->
      match Path_map.find_opt (Binding.path binding) before_bindings with
      | Some previous -> not (Binding.same previous binding)
      | None -> true) |> of_bindings

let qualify_entries = fun scope_path entries ->
  map
    (fun binding ->
      Binding.with_path (IdentPath.append_path scope_path (Binding.path binding)) binding)
    entries
