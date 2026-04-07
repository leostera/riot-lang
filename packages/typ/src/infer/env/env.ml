open Std
open Analysis
open Model

module Path_map = Collections.Map.Make (struct
  type t = IdentPath.t

  let compare = IdentPath.compare
end)

module Binding = Binding
module Constructor_env = Constructor_env
module Label_env = Label_env
module Module_env = Module_env
module Type_env = Type_env
module Value_env = Value_env

type bindings = Binding.t list

type t = {
  values: Value_env.t;
  modules: Module_env.t;
  types: Type_env.t;
  constructors: Constructor_env.t;
  labels: Label_env.t;
  opened_modules: Module_env.scope list;
}

type scope = {
  locals: t Path_map.t;
  opens: IdentPath.t list Path_map.t;
}

let binding_name_of_path = fun path ->
  match IdentPath.last_name path with
  | Some name -> name
  | None -> ""

let is_bare_path = fun path -> IdentPath.is_bare path

let partition_bindings = fun bindings ->
  bindings |> List.fold_left
    (fun (bare, qualified) binding ->
      if is_bare_path (Binding.path binding) then
        (binding :: bare, qualified)
      else
        (bare, binding :: qualified))
    ([], [])

let empty = {
  values = Value_env.empty;
  modules = Module_env.empty;
  types = Type_env.empty;
  constructors = Constructor_env.empty;
  labels = Label_env.empty;
  opened_modules = [];
}

let empty_scope = { locals = Path_map.empty; opens = Path_map.empty }

let make = fun values modules types constructors labels ->
  {
    values;
    modules;
    types;
    constructors;
    labels;
    opened_modules = [];
  }

let of_bindings = fun bindings ->
  let (bare_bindings, qualified_bindings) = partition_bindings bindings in
  make
    (Value_env.of_bindings (List.rev bare_bindings))
    (Module_env.of_bindings (List.rev qualified_bindings))
    Type_env.empty
    Constructor_env.empty
    Label_env.empty

let of_entries = fun ~make_ident ~provenance entries ->
  entries
  |> List.map
    (fun (path, scheme) ->
      Binding.make ~ident:(make_ident (binding_name_of_path path)) ~path ~scheme ~provenance)
  |> of_bindings

let singleton = fun ~make_ident ~name ~scheme ~provenance ->
  of_bindings
    [ Binding.make ~ident:(make_ident name) ~path:(IdentPath.of_name name) ~scheme ~provenance ]

let direct_bindings = fun env -> Value_env.bindings env.values @ Module_env.bindings env.modules

let bindings = fun env -> direct_bindings env

let type_decls = fun env -> Type_env.type_decls env.types

let types = fun env -> env.types

let unique = fun env ->
  env |> bindings |> Value_env.of_bindings |> Value_env.unique |> Value_env.bindings |> of_bindings

let render = fun env -> env |> bindings |> Value_env.of_bindings |> Value_env.render

let split_module_lookup_path = fun path ->
  IdentPath.split_last path
  |> Option.map (fun (module_path, name) -> (module_path, IdentPath.of_name name))

let lookup_opened_value = fun env path ->
  env.opened_modules |> List.find_map
    (fun scope ->
      Value_env.lookup (Module_env.scope_values scope) path)

let lookup_all_opened_values = fun env path ->
  env.opened_modules |> List.concat_map
    (fun scope ->
      Value_env.lookup_all (Module_env.scope_values scope) path)

let lookup_opened_constructors = fun env path ->
  let name = binding_name_of_path path in
  env.opened_modules |> List.concat_map
    (fun scope ->
      Constructor_env.lookup_all (Module_env.scope_constructors scope) name)

let lookup_opened_module = fun env module_path ->
  env.opened_modules |> List.find_map
    (fun scope ->
      Module_env.lookup (Module_env.scope_modules scope) module_path)

let dedupe_record_decls = fun record_decls ->
  let seen = Collections.HashSet.with_capacity (List.length record_decls) in
  record_decls |> List.filter
    (fun (record_decl: Label_env.record_decl) ->
      let owner_id = TypeConstructorId.to_int record_decl.owner_type_constructor_id in
      if Collections.HashSet.contains seen owner_id then
        false
      else
        let () = Collections.HashSet.insert seen owner_id |> ignore in
        true)

let direct_module_scopes = fun env -> Module_env.scopes env.modules

let visible_module_scopes = fun env -> direct_module_scopes env @ env.opened_modules

let lookup_module_scope = fun env module_path ->
  match Module_env.lookup env.modules module_path with
  | Some scope -> Some scope
  | None -> lookup_opened_module env module_path

let rec lookup_in_module_scope = fun scope path ->
  match Value_env.lookup (Module_env.scope_values scope) path with
  | Some binding -> Some binding
  | None -> (
      match split_module_lookup_path path with
      | Some (module_path, relative_path) -> Option.and_then
        (Module_env.lookup (Module_env.scope_modules scope) module_path)
        (fun nested_scope -> lookup_in_module_scope nested_scope relative_path)
      | None -> None
    )

let rec lookup_all_in_module_scope = fun scope path ->
  let direct = Value_env.lookup_all (Module_env.scope_values scope) path in
  if not (List.is_empty direct) then
    direct
  else
    match split_module_lookup_path path with
    | Some (module_path, relative_path) -> (
        match Module_env.lookup (Module_env.scope_modules scope) module_path with
        | Some nested_scope -> lookup_all_in_module_scope nested_scope relative_path
        | None -> []
      )
    | None -> []

let rec lookup_constructors_in_module_scope = fun scope path ->
  if is_bare_path path then
    Constructor_env.lookup_all (Module_env.scope_constructors scope) (binding_name_of_path path)
  else
    match split_module_lookup_path path with
    | Some (module_path, relative_path) -> (
        match Module_env.lookup (Module_env.scope_modules scope) module_path with
        | Some nested_scope -> lookup_constructors_in_module_scope nested_scope relative_path
        | None -> []
      )
    | None -> []

let lookup_direct = fun env path ->
  Value_env.lookup env.values path

let lookup = fun env path ->
  match lookup_direct env path with
  | Some binding -> Some binding
  | None ->
      if is_bare_path path then
        lookup_opened_value env path
      else
        match split_module_lookup_path path with
        | Some (module_path, relative_path) -> Option.and_then
          (lookup_module_scope env module_path)
          (fun module_scope -> lookup_in_module_scope module_scope relative_path)
        | None -> None

let lookup_all_direct = fun env path ->
  Value_env.lookup_all env.values path

let lookup_all = fun env path ->
  let direct = lookup_all_direct env path in
  if not (List.is_empty direct) then
    direct
  else if is_bare_path path then
    lookup_all_opened_values env path
  else
    match split_module_lookup_path path with
    | Some (module_path, relative_path) -> (
        match lookup_module_scope env module_path with
        | Some module_scope -> lookup_all_in_module_scope module_scope relative_path
        | None -> []
      )
    | None -> []

let lookup_direct_constructors = fun env name ->
  Constructor_env.lookup_all env.constructors name

let lookup_constructors = fun env path ->
  if is_bare_path path then
    let name = binding_name_of_path path in
    let direct = lookup_direct_constructors env name in
    if not (List.is_empty direct) then
      direct
    else
      lookup_opened_constructors env path
  else
    match split_module_lookup_path path with
    | Some (module_path, relative_path) -> (
        match lookup_module_scope env module_path with
        | Some module_scope -> lookup_constructors_in_module_scope module_scope relative_path
        | None -> []
      )
    | None -> []

let lookup_record_decls = fun env label_name ->
  let direct = Label_env.lookup_all env.labels label_name in
  let visible =
    visible_module_scopes env
    |> List.concat_map
      (fun scope ->
        Label_env.lookup_all (Module_env.scope_labels scope) label_name)
  in
  dedupe_record_decls (direct @ visible)

let record_decls = fun env ->
  let direct = Label_env.record_decls env.labels in
  let visible = visible_module_scopes env
  |> List.concat_map (fun scope -> Label_env.record_decls (Module_env.scope_labels scope)) in
  dedupe_record_decls (direct @ visible)

let names = fun env -> env |> bindings |> Value_env.of_bindings |> Value_env.names

let introduced_names = fun before after ->
  Value_env.introduced_names
    (Value_env.of_bindings (bindings before))
    (Value_env.of_bindings (bindings after))

let bind = fun env introduced ->
  {
    values = Value_env.bind env.values introduced.values;
    modules = Module_env.bind env.modules introduced.modules;
    types = Type_env.bind env.types introduced.types;
    constructors = Constructor_env.bind env.constructors introduced.constructors;
    labels = Label_env.bind env.labels introduced.labels;
    opened_modules = env.opened_modules;
  }

let bind_in_scope = fun env ~scope_path introduced ->
  if IdentPath.is_empty scope_path then
    bind env introduced
  else
    bind env
      {
        values = Value_env.empty;
        modules = Module_env.merge_scope
          Module_env.empty
          ~module_path:scope_path
          (Module_env.make_scope
            ~values:introduced.values
            ~modules:introduced.modules
            ~types:introduced.types
            ~constructors:introduced.constructors
            ~labels:introduced.labels);
        types = Type_env.empty;
        constructors = Constructor_env.empty;
        labels = Label_env.empty;
        opened_modules = [];
      }

let env_of_local_type_decls = fun type_decls ->
  let types = Type_env.of_type_decls type_decls in
  let constructors = Constructor_env.of_type_decls type_decls in
  let labels = Label_env.of_type_decls type_decls in
  make Value_env.empty Module_env.empty types constructors labels

let of_type_decls = fun type_decls ->
  type_decls |> List.fold_left
    (fun env (type_decl: FileSummary.type_decl) ->
      let local_decl = { type_decl with scope_path = IdentPath.empty } in
      let introduced = env_of_local_type_decls [ local_decl ] in
      if IdentPath.is_empty type_decl.scope_path then
        bind env introduced
      else
        bind_in_scope env ~scope_path:type_decl.scope_path introduced)
    empty

let extend = fun env introduced -> bind env (of_bindings introduced)

let with_local_open = fun env module_path ->
  match lookup_module_scope env module_path with
  | Some scope -> { env with opened_modules = scope :: env.opened_modules }
  | None -> env

let entries_for_include = fun env module_path ->
  match lookup_module_scope env module_path with
  | Some scope ->
      {
        values =
          Module_env.scope_values scope |> Value_env.bindings |> List.map
            (fun binding ->
              Binding.with_provenance (Binding.Included { module_path }) binding) |> Value_env.of_bindings;
        modules = Module_env.scope_modules scope;
        types = Module_env.scope_types scope;
        constructors = Module_env.scope_constructors scope;
        labels = Module_env.scope_labels scope;
        opened_modules = [];
      }
  | None -> empty

let export_names_for_module_alias = fun env ~alias_name ~module_path ->
  match lookup_module_scope env module_path with
  | Some scope ->
      Module_env.scope_bindings scope |> List.map
        (fun binding ->
          Binding.with_path (IdentPath.prepend_name alias_name (Binding.path binding)) binding) |> Value_env.of_bindings |> Value_env.canonicalize |> Value_env.bindings |> List.map
        (fun binding -> Binding.path binding |> IdentPath.to_string)
  | None -> []

let entries_for_module_alias = fun env ~alias_name ~module_path ->
  match lookup_module_scope env module_path with
  | Some scope ->
      {
        values = Value_env.empty;
        modules = Module_env.bind_alias Module_env.empty ~alias_name scope;
        types = Type_env.empty;
        constructors = Constructor_env.empty;
        labels = Label_env.empty;
        opened_modules = [];
      }
  | None -> empty

let export = fun config env ->
  env |> bindings |> Value_env.of_bindings |> Value_env.export config |> Value_env.bindings |> of_bindings

let export_with_forced_names = fun ~config ~forced_export_names env ->
  env
  |> bindings
  |> Value_env.of_bindings
  |> Value_env.export_with_forced_names ~config ~forced_export_names
  |> Value_env.bindings
  |> of_bindings

let introduced_entries = fun before after ->
  Value_env.introduced_entries
    (Value_env.of_bindings (bindings before))
    (Value_env.of_bindings (bindings after))
  |> Value_env.bindings
  |> of_bindings

let qualify = fun ~scope_path env ->
  if IdentPath.is_empty scope_path then
    env
  else
    {
      values = Value_env.empty;
      modules = Module_env.merge_scope
        Module_env.empty
        ~module_path:scope_path
        (Module_env.make_scope
          ~values:env.values
          ~modules:env.modules
          ~types:env.types
          ~constructors:env.constructors
          ~labels:env.labels);
      types = Type_env.empty;
      constructors = Constructor_env.empty;
      labels = Label_env.empty;
      opened_modules = [];
    }

let scope_locals_for = fun scope_locals scope_path ->
  IdentPath.prefixes scope_path |> List.fold_left
    (fun acc key ->
      match Path_map.find_opt key scope_locals with
      | Some entries -> bind acc entries
      | None -> acc)
    empty

let register_entries = fun scope ~scope_path env ->
  let existing =
    match Path_map.find_opt scope_path scope.locals with
    | Some entries -> entries
    | None -> empty
  in
  let updated = bind existing env in
  { scope with locals = Path_map.add scope_path updated scope.locals }

let scope_opens_for = fun scope_opens scope_path ->
  IdentPath.prefixes scope_path |> List.fold_left
    (fun acc key ->
      match Path_map.find_opt key scope_opens with
      | Some modules -> acc @ modules
      | None -> acc)
    []

let register_open = fun scope ~scope_path ~module_path ->
  let existing =
    match Path_map.find_opt scope_path scope.opens with
    | Some modules -> modules
    | None -> []
  in
  let updated = existing @ [ module_path ] in
  { scope with opens = Path_map.add scope_path updated scope.opens }

let for_item_scope = fun env scope ~scope_path ->
  let locals = scope_locals_for scope.locals scope_path in
  let base_env = bind env locals in
  scope_opens_for scope.opens scope_path |> List.fold_left with_local_open base_env
