open Std
open Model

module Name_map = Collections.Map.Make (String)

module Id_map = Collections.Map.Make (struct
  type t = TypeConstructorId.t

  let compare = TypeConstructorId.compare
end)

type current = FileSummary.type_decl list Name_map.t

type components = {
  by_name: FileSummary.type_decl Name_map.t;
  by_id: FileSummary.type_decl Id_map.t;
}

type layer =
  | Nothing
  | Open of { root: IdentPath.t; components: components; next: t }
  | Map of { map_decl: FileSummary.type_decl -> FileSummary.type_decl; next: t }

and t = {
  current: current;
  by_id: FileSummary.type_decl Id_map.t;
  layer: layer;
}

let empty = { current = Name_map.empty; by_id = Id_map.empty; layer = Nothing }

let is_empty = fun env ->
  Name_map.is_empty env.current && Id_map.is_empty env.by_id && match env.layer with
  | Nothing -> true
  | Open _
  | Map _ -> false

let decl_name = fun (type_decl: FileSummary.type_decl) -> type_decl.declaration.type_name

let prepend_decl = fun index type_decl ->
  let name = decl_name type_decl in
  let existing = Name_map.find_opt name index |> Option.unwrap_or ~default:[] in
  Name_map.add name (type_decl :: existing) index

let current_of_type_decls = fun type_decls ->
  type_decls |> List.rev |> List.fold_left prepend_decl Name_map.empty

let id_index_of_type_decls = fun type_decls ->
  type_decls |> List.fold_left
    (fun acc (type_decl: FileSummary.type_decl) ->
      Id_map.add type_decl.declaration.type_constructor_id type_decl acc)
    Id_map.empty

let components_of_type_decls = fun type_decls ->
  let by_name =
    type_decls
    |> List.rev
    |> List.fold_left
      (fun acc (type_decl: FileSummary.type_decl) ->
        Name_map.add (decl_name type_decl) type_decl acc)
      Name_map.empty
  in
  let by_id = id_index_of_type_decls type_decls in
  { by_name; by_id }

let current_visible_components = fun env ->
  {
    by_name =
      Name_map.fold
        (fun name type_decls acc ->
          match type_decls with
          | type_decl :: _ -> Name_map.add name type_decl acc
          | [] -> acc)
        env.current
        Name_map.empty;
    by_id = env.by_id;
  }

let merge_visible_by_name = fun dominant rest ->
  Name_map.fold
    (fun name type_decl acc ->
      if Name_map.mem name acc then
        acc
      else
        Name_map.add name type_decl acc)
    rest
    dominant

let merge_visible_by_id = fun dominant rest ->
  Id_map.fold
    (fun id type_decl acc ->
      if Id_map.mem id acc then
        acc
      else
        Id_map.add id type_decl acc)
    rest
    dominant

let merge_visible_components = fun dominant rest ->
  {
    by_name = merge_visible_by_name dominant.by_name rest.by_name;
    by_id = merge_visible_by_id dominant.by_id rest.by_id
  }

let of_type_decls = fun type_decls ->
  {
    current = current_of_type_decls type_decls;
    by_id = id_index_of_type_decls type_decls;
    layer = Nothing
  }

let current_type_decls = fun current -> Name_map.bindings current |> List.concat_map snd

let rec visible_components = fun env ->
  let current = current_visible_components env in
  match env.layer with
  | Nothing ->
      current
  | Open { components; next; _ } ->
      current
      |> merge_visible_components components
      |> merge_visible_components (visible_components next)
  | Map { map_decl; next } ->
      let next_visible = visible_components next in
      current
      |> merge_visible_components
        {
          by_name = Name_map.map map_decl next_visible.by_name;
          by_id = Id_map.map map_decl next_visible.by_id
        }

let type_decls =
  let rec loop acc env =
    let acc = List.rev_append (current_type_decls env.current) acc in
    match env.layer with
    | Nothing -> acc
    | Open { next; _ } -> loop acc next
    | Map { map_decl; next } -> loop acc next |> List.map map_decl
  in
  fun env -> loop [] env |> List.rev

let local_only = fun env -> env |> type_decls |> of_type_decls

let qualify_type_decl = fun prefix (type_decl: FileSummary.type_decl) ->
  { type_decl with scope_path = IdentPath.append_path prefix type_decl.scope_path }

let visible_type_decls =
  let rec collect env =
    let current = current_visible_components env |> fun components -> components.by_name in
    match env.layer with
    | Nothing ->
        current
    | Open { root; components; next } ->
        let opened = Name_map.map (qualify_type_decl root) components.by_name in
        current
        |> merge_visible_by_name opened
        |> merge_visible_by_name (collect next)
    | Map { map_decl; next } ->
        let next_visible = Name_map.map map_decl (collect next) in
        merge_visible_by_name current next_visible
  in
  fun env ->
    collect env
    |> Name_map.bindings
    |> List.map snd

let map = fun map_decl env ->
  if is_empty env then
    env
  else
    { current = Name_map.empty; by_id = Id_map.empty; layer = Map { map_decl; next = env } }

let add_open = fun ~root opened env ->
  {
    current = Name_map.empty;
    by_id = Id_map.empty;
    layer = Open { root; components = visible_components opened; next = env }
  }

let merge_current = fun introduced existing ->
  Name_map.fold
    (fun name introduced_decls acc ->
      let current = Name_map.find_opt name acc |> Option.unwrap_or ~default:[] in
      Name_map.add name (introduced_decls @ current) acc)
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
      by_id = Id_map.fold Id_map.add introduced.by_id env.by_id;
      layer = env.layer
    }

let rec lookup_name = fun env name ->
  match Name_map.find_opt name env.current with
  | Some (type_decl :: _) -> Some type_decl
  | _ -> (
      match env.layer with
      | Nothing ->
          None
      | Open { root; components; next } -> (
          match Name_map.find_opt name components.by_name with
          | Some type_decl -> Some (qualify_type_decl root type_decl)
          | None -> lookup_name next name
        )
      | Map { map_decl; next } ->
          lookup_name next name |> Option.map map_decl
    )

let lookup = fun env path ->
  match IdentPath.bare_name path with
  | Some name -> lookup_name env name
  | None -> None

let rec lookup_by_id = fun env type_constructor_id ->
  match Id_map.find_opt type_constructor_id env.by_id with
  | Some type_decl -> Some type_decl
  | None -> (
      match env.layer with
      | Nothing ->
          None
      | Open { root; components; next } -> (
          match Id_map.find_opt type_constructor_id components.by_id with
          | Some type_decl -> Some (qualify_type_decl root type_decl)
          | None -> lookup_by_id next type_constructor_id
        )
      | Map { map_decl; next } ->
          lookup_by_id next type_constructor_id |> Option.map map_decl
    )

let qualify_entries = fun prefix env -> map (qualify_type_decl prefix) env

let entries_for_include = fun env module_path ->
  type_decls env |> List.filter_map
    (fun (type_decl: FileSummary.type_decl) ->
      match IdentPath.strip_prefix ~prefix:module_path type_decl.scope_path with
      | Some scope_path -> Some { type_decl with scope_path }
      | None -> None) |> of_type_decls

let entries_for_module_alias = fun env ~alias_name ~module_path ->
  entries_for_include env module_path |> qualify_entries (IdentPath.of_name alias_name)
