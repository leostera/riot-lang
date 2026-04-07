open Std
open Model

module Path_map = Collections.Map.Make (struct
  type t = IdentPath.t

  let compare = IdentPath.compare
end)

module Id_map = Collections.Map.Make (struct
  type t = TypeConstructorId.t

  let compare = TypeConstructorId.compare
end)

type t = {
  type_decls: FileSummary.type_decl list;
  by_path: FileSummary.type_decl Path_map.t;
  by_id: FileSummary.type_decl Id_map.t;
}

let empty = { type_decls = []; by_path = Path_map.empty; by_id = Id_map.empty }

let decl_path = fun (type_decl: FileSummary.type_decl) ->
  IdentPath.append_name type_decl.scope_path type_decl.declaration.type_name

let of_type_decls = fun type_decls ->
  let by_path =
    type_decls
    |> List.fold_left
      (fun acc (type_decl: FileSummary.type_decl) ->
        Path_map.add (decl_path type_decl) type_decl acc)
      Path_map.empty
  in
  let by_id =
    type_decls
    |> List.fold_left
      (fun acc (type_decl: FileSummary.type_decl) ->
        Id_map.add type_decl.declaration.type_constructor_id type_decl acc)
      Id_map.empty
  in
  { type_decls; by_path; by_id }

let type_decls = fun env -> env.type_decls

let bind = fun env introduced ->
  if List.is_empty introduced.type_decls then
    env
  else if List.is_empty env.type_decls then
    introduced
  else
    let by_path = Path_map.fold Path_map.add introduced.by_path env.by_path in
    let by_id = Id_map.fold Id_map.add introduced.by_id env.by_id in
    let shadowed_paths =
      introduced.type_decls
      |> List.fold_left
        (fun seen type_decl ->
          Collections.HashSet.insert seen (decl_path type_decl) |> ignore;
          seen)
        (Collections.HashSet.with_capacity (List.length introduced.type_decls))
    in
    let type_decls = introduced.type_decls
    @ List.filter
      (fun type_decl -> not (Collections.HashSet.contains shadowed_paths (decl_path type_decl)))
      env.type_decls in
    { type_decls; by_path; by_id }

let lookup = fun env path ->
  Path_map.find_opt path env.by_path

let lookup_by_id = fun env type_constructor_id ->
  Id_map.find_opt type_constructor_id env.by_id

let qualify_entries = fun prefix env ->
  env.type_decls
  |> List.map
    (fun (type_decl: FileSummary.type_decl) ->
      { type_decl with scope_path = IdentPath.append_path prefix type_decl.scope_path })
  |> of_type_decls

let entries_for_include = fun env module_path ->
  env.type_decls |> List.filter_map
    (fun (type_decl: FileSummary.type_decl) ->
      match IdentPath.strip_prefix ~prefix:module_path type_decl.scope_path with
      | Some scope_path -> Some { type_decl with scope_path }
      | None -> None) |> of_type_decls

let entries_for_module_alias = fun env ~alias_name ~module_path ->
  entries_for_include env module_path |> qualify_entries (IdentPath.of_name alias_name)
