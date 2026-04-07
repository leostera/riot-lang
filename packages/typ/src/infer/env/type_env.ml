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
  | Open of {
      root: IdentPath.t;
      components: components;
      next: t;
    }

and t = {
  type_decls: FileSummary.type_decl list;
  current: current;
  by_id: FileSummary.type_decl Id_map.t;
  layer: layer;
}

let empty = { type_decls = []; current = Name_map.empty; by_id = Id_map.empty; layer = Nothing }

let decl_name = fun (type_decl: FileSummary.type_decl) -> type_decl.declaration.type_name

let prepend_decl = fun index type_decl ->
  let name = decl_name type_decl in
  let existing = Name_map.find_opt name index |> Option.unwrap_or ~default:[] in
  Name_map.add name (type_decl :: existing) index

let index_of_type_decls = fun type_decls ->
  type_decls |> List.rev |> List.fold_left prepend_decl Name_map.empty

let id_index_of_type_decls = fun type_decls ->
  type_decls
  |> List.fold_left
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

let of_type_decls = fun type_decls ->
  {
    type_decls;
    current = index_of_type_decls type_decls;
    by_id = id_index_of_type_decls type_decls;
    layer = Nothing;
  }

let type_decls = fun env -> env.type_decls

let local_only = fun env -> of_type_decls env.type_decls

let qualify_type_decl = fun prefix (type_decl: FileSummary.type_decl) ->
  { type_decl with scope_path = IdentPath.append_path prefix type_decl.scope_path }

let add_open = fun ~root opened env ->
  {
    type_decls = env.type_decls;
    current = Name_map.empty;
    by_id = env.by_id;
    layer = Open {
      root;
      components = components_of_type_decls opened.type_decls;
      next = env;
    };
  }

let merge_current = fun introduced existing ->
  Name_map.fold
    (fun name introduced_decls acc ->
      let current = Name_map.find_opt name acc |> Option.unwrap_or ~default:[] in
      Name_map.add name (introduced_decls @ current) acc)
    introduced
    existing

let bind = fun env introduced ->
  if List.is_empty introduced.type_decls then
    env
  else if List.is_empty env.type_decls && env.layer = Nothing then
    introduced
  else
    {
      type_decls = introduced.type_decls @ env.type_decls;
      current = merge_current introduced.current env.current;
      by_id = Id_map.fold Id_map.add introduced.by_id env.by_id;
      layer = env.layer;
    }

let rec lookup_name = fun env name ->
  match Name_map.find_opt name env.current with
  | Some (type_decl :: _) ->
      Some type_decl
  | _ -> (
      match env.layer with
      | Nothing ->
          None
      | Open { root; components; next } -> (
          match Name_map.find_opt name components.by_name with
          | Some type_decl ->
              Some (qualify_type_decl root type_decl)
          | None ->
              lookup_name next name
        )
    )

let lookup = fun env path ->
  match IdentPath.bare_name path with
  | Some name ->
      lookup_name env name
  | None ->
      None

let rec lookup_by_id = fun env type_constructor_id ->
  match Id_map.find_opt type_constructor_id env.by_id with
  | Some type_decl ->
      Some type_decl
  | None -> (
      match env.layer with
      | Nothing ->
          None
      | Open { root; components; next } -> (
          match Id_map.find_opt type_constructor_id components.by_id with
          | Some type_decl ->
              Some (qualify_type_decl root type_decl)
          | None ->
              lookup_by_id next type_constructor_id
        )
    )

let qualify_entries = fun prefix env ->
  env.type_decls
  |> List.map (qualify_type_decl prefix)
  |> of_type_decls

let entries_for_include = fun env module_path ->
  env.type_decls
  |> List.filter_map
    (fun (type_decl: FileSummary.type_decl) ->
      match IdentPath.strip_prefix ~prefix:module_path type_decl.scope_path with
      | Some scope_path -> Some { type_decl with scope_path }
      | None -> None)
  |> of_type_decls

let entries_for_module_alias = fun env ~alias_name ~module_path ->
  entries_for_include env module_path
  |> qualify_entries (IdentPath.of_name alias_name)
