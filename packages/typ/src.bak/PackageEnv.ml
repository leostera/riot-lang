open Std
open Model
module VisibleTypes = Model.VisibleTypes

module ModuleId = struct
  type t =
    | Loaded of LocalModules.RequiredName.t
    | Local of LocalModules.InternalName.t
end

type entry = {
  artifact: ModuleTypings.t;
  compiled_scope: CompiledScope.t;
  visible_types: VisibleTypes.t;
}

type t = {
  by_id: (ModuleId.t, entry) Collections.HashMap.t;
  loaded_ids_by_name: (LocalModules.RequiredName.t, ModuleId.t) Collections.HashMap.t;
  local_ids_by_name: (LocalModules.InternalName.t, ModuleId.t) Collections.HashMap.t;
}

let entry_of_artifact = fun artifact ->
  let type_decls = ModuleTypings.type_decls artifact in
  {
    artifact;
    compiled_scope = ModuleTypings.compiled_scope artifact;
    visible_types = VisibleTypes.of_type_decls type_decls
  }

let empty = fun () ->
  {
    by_id = Collections.HashMap.with_capacity 64;
    loaded_ids_by_name = Collections.HashMap.with_capacity 64;
    local_ids_by_name = Collections.HashMap.with_capacity 64
  }

let add_entry = fun env module_id entry ->
  let _ = Collections.HashMap.insert env.by_id module_id entry in
  match module_id with
  | ModuleId.Loaded required_name ->
      let _ = Collections.HashMap.insert env.loaded_ids_by_name required_name module_id in
      ()
  | ModuleId.Local internal_name ->
      let _ = Collections.HashMap.insert env.local_ids_by_name internal_name module_id in
      ()

let add_loaded = fun env ~required_name artifact ->
  add_entry env (ModuleId.Loaded required_name) (entry_of_artifact artifact)

let add_local = fun env ~internal_name artifact ->
  add_entry env (ModuleId.Local internal_name) (entry_of_artifact artifact)

let of_loaded_modules = fun loaded_modules ->
  let env = empty () in
  LoadedModules.iter (fun required_name artifact -> add_loaded env ~required_name artifact) loaded_modules;
  env

let find_entry = fun env module_id ->
  Collections.HashMap.get env.by_id module_id

let find_artifact = fun env module_id ->
  find_entry env module_id |> Option.map (fun entry -> entry.artifact)

let find_compiled_scope = fun env module_id ->
  find_entry env module_id |> Option.map (fun entry -> entry.compiled_scope)

let find_loaded = fun env ~required_name ->
  Option.and_then
    (Collections.HashMap.get env.loaded_ids_by_name required_name)
    (fun module_id -> find_artifact env module_id)

let find_local = fun env ~internal_name ->
  Option.and_then
    (Collections.HashMap.get env.local_ids_by_name internal_name)
    (fun module_id -> find_artifact env module_id)

let lookup_value = fun env module_id path ->
  Option.and_then (find_entry env module_id)
    (fun entry ->
      CompiledScope.lookup_value entry.compiled_scope path)

let lookup_module_scope = fun env module_id path ->
  Option.and_then (find_entry env module_id)
    (fun entry ->
      CompiledScope.lookup_module entry.compiled_scope path)

let lookup_type_decl = fun env module_id path ->
  Option.and_then (find_entry env module_id)
    (fun entry ->
      if SurfacePath.is_empty path then
        None
      else
        CompiledScope.lookup_type_decl entry.compiled_scope path)

let lookup_type_decl_by_id = fun env module_id type_constructor_id ->
  Option.and_then (find_entry env module_id)
    (fun entry ->
      VisibleTypes.lookup_by_id entry.visible_types type_constructor_id)

let lookup_constructors = fun env module_id path ->
  Option.and_then
    (find_entry env module_id)
    (fun entry -> Some (CompiledScope.lookup_constructors entry.compiled_scope path))
  |> Option.unwrap_or ~default:[]

let lookup_owned_constructor = fun env module_id path owner_type_constructor_id ->
  Option.and_then (find_entry env module_id)
    (fun entry ->
      CompiledScope.lookup_owned_constructor entry.compiled_scope path owner_type_constructor_id)

let lookup_record_decls = fun env module_id label_name ->
  Option.and_then
    (find_entry env module_id)
    (fun entry -> Some (CompiledScope.lookup_record_decls entry.compiled_scope label_name))
  |> Option.unwrap_or ~default:[]

let lookup_record_decl_by_owner = fun env module_id owner_type_constructor_id ->
  Option.and_then (find_entry env module_id)
    (fun entry ->
      CompiledScope.lookup_record_decl_by_owner entry.compiled_scope owner_type_constructor_id)

let visible_type_decls = fun env visible_modules ->
  visible_modules |> List.filter_map
    (fun (visible_path, module_id) ->
      find_entry env module_id |> Option.map
        (fun entry ->
          let module_name = SurfacePath.to_string visible_path in
          ModuleSurface.qualify_type_decls ~module_name (VisibleTypes.type_decls entry.visible_types))) |> List.concat

let visible_type_decl_by_id = fun env module_ids type_constructor_id ->
  module_ids |> List.find_map
    (fun module_id ->
      Option.and_then (find_entry env module_id)
        (fun entry ->
          VisibleTypes.lookup_by_id entry.visible_types type_constructor_id))
