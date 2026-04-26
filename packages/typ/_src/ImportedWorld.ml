open Std
open Model

type opened_module = {
  visible_path: SurfacePath.t;
  module_id: PackageEnv.ModuleId.t;
}

type resolved_module = {
  visible_path: SurfacePath.t;
  module_id: PackageEnv.ModuleId.t;
  suffix: SurfacePath.t;
}

type t = {
  package_env: PackageEnv.t;
  scope_view: ScopeView.t;
}

let empty = fun () -> { package_env = PackageEnv.empty (); scope_view = ScopeView.empty () }

let create = fun ~package_env ~scope_view -> { package_env; scope_view }

let package_env = fun imported_world -> imported_world.package_env

let scope_view = fun imported_world -> imported_world.scope_view

let qualify_type_decl = fun ~visible_path (type_decl: FileSummary.type_decl) ->
  if SurfacePath.is_empty visible_path then
    type_decl
  else
    { type_decl with scope_path = SurfacePath.append_path visible_path type_decl.scope_path }

let resolve_visible_module_prefix = fun imported_world path ->
  ScopeView.resolve_visible_module_prefix imported_world.scope_view path
  |> Option.map (fun (visible_path, module_id, suffix) -> { visible_path; module_id; suffix })

let implicit_open_modules = fun imported_world ->
  ScopeView.implicit_open_modules imported_world.scope_view
  |> List.map (fun (visible_path, module_id) -> { visible_path; module_id })

let visible_modules = fun imported_world -> ScopeView.visible_modules imported_world.scope_view

let visible_type_decls_for_module = fun imported_world ~visible_path ~module_id ->
  PackageEnv.visible_type_decls imported_world.package_env [ (visible_path, module_id); ]

let visible_type_decls = fun imported_world ->
  visible_modules imported_world
  |> PackageEnv.visible_type_decls imported_world.package_env

let visible_type_decl_by_id = fun imported_world type_constructor_id ->
  visible_modules imported_world
  |> List.find_map
    (fun (visible_path, module_id) ->
      PackageEnv.lookup_type_decl_by_id imported_world.package_env module_id type_constructor_id
      |> Option.map (qualify_type_decl ~visible_path))

let lookup_value = fun imported_world path ->
  let surface_path = EntityId.surface_path path in
  if EntityId.is_bare path then
    implicit_open_modules imported_world
    |> List.find_map
      (fun ({ module_id; _ }: opened_module) ->
        PackageEnv.lookup_value imported_world.package_env module_id surface_path)
  else
    Option.and_then
      (resolve_visible_module_prefix imported_world surface_path)
      (fun ({ module_id; suffix; _ }: resolved_module) ->
        if SurfacePath.is_empty suffix then
          None
        else
          PackageEnv.lookup_value imported_world.package_env module_id suffix)

let lookup_module_scope = fun imported_world module_path ->
  Option.and_then
    (resolve_visible_module_prefix imported_world module_path)
    (fun ({ module_id; suffix; _ }: resolved_module) ->
      PackageEnv.lookup_module_scope imported_world.package_env module_id suffix)

let lookup_type_decl = fun imported_world path ->
  if SurfacePath.is_bare path then
    implicit_open_modules imported_world
    |> List.find_map
      (fun ({ visible_path; module_id }: opened_module) ->
        PackageEnv.lookup_type_decl imported_world.package_env module_id path
        |> Option.map (qualify_type_decl ~visible_path))
  else
    Option.and_then
      (resolve_visible_module_prefix imported_world path)
      (fun ({ visible_path; module_id; suffix }: resolved_module) ->
        if SurfacePath.is_empty suffix then
          None
        else
          PackageEnv.lookup_type_decl imported_world.package_env module_id suffix
          |> Option.map (qualify_type_decl ~visible_path))
