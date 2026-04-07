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

type summary_delta = {
  bindings: bindings;
  type_decls: FileSummary.type_decl list;
}

type summary =
  | Summary_empty
  | Summary_snapshot of summary_delta
  | Summary_bind of summary * summary
  | Summary_bind_in_scope of summary * IdentPath.t * summary
  | Summary_open of summary * IdentPath.t
  | Summary_qualify of summary * IdentPath.t

type t = {
  summary: summary;
  values: Value_env.t;
  modules: Module_env.t;
  types: Type_env.t;
  constructors: Constructor_env.t;
  labels: Label_env.t;
}

type scope_locals = {
  summary: summary;
  mutable cached: t option;
}

type scope = {
  locals: scope_locals Path_map.t;
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
  summary = Summary_empty;
  values = Value_env.empty;
  modules = Module_env.empty;
  types = Type_env.empty;
  constructors = Constructor_env.empty;
  labels = Label_env.empty;
}

let empty_scope = { locals = Path_map.empty; opens = Path_map.empty }

let make = fun ?(summary = Summary_empty) values modules types constructors labels ->
  {
    summary;
    values;
    modules;
    types;
    constructors;
    labels;
  }

let of_bindings = fun bindings ->
  let (bare_bindings, qualified_bindings) = partition_bindings bindings in
  make
    ~summary:(Summary_snapshot { bindings; type_decls = [] })
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

let bindings = fun env -> Value_env.bindings env.values @ Module_env.bindings env.modules

let type_decls = fun env -> Type_env.type_decls env.types

let types = fun env -> env.types

let visible_bindings = fun env ->
  let seen = Collections.HashSet.with_capacity 32 in
  bindings env |> List.filter
    (fun binding ->
      let path = Binding.path binding in
      if Collections.HashSet.contains seen path then
        false
      else
        let () = Collections.HashSet.insert seen path |> ignore in
        true)

let canonical_bindings = fun env ->
  visible_bindings env
  |> List.sort
    (fun left right ->
      IdentPath.compare (Binding.path left) (Binding.path right))

let unique = fun env -> env |> visible_bindings |> of_bindings

let render = fun env -> env |> canonical_bindings |> List.map Binding.render

let split_module_lookup_path = fun path ->
  IdentPath.split_last path
  |> Option.map (fun (module_path, name) -> (module_path, IdentPath.of_name name))

let of_scope = fun (scope: Module_env.scope) ->
  {
    summary = Summary_empty;
    values = Module_env.scope_values scope;
    modules = Module_env.scope_modules scope;
    types = Module_env.scope_types scope;
    constructors = Module_env.scope_constructors scope;
    labels = Module_env.scope_labels scope;
  }

let lookup_module_scope = fun (env: t) module_path ->
  Module_env.lookup env.modules module_path

let rec lookup = fun (env: t) path ->
  if is_bare_path path then
    Value_env.lookup env.values path
  else
    match split_module_lookup_path path with
    | Some (module_path, relative_path) -> (
        match lookup_module_scope env module_path with
        | Some module_scope -> lookup (of_scope module_scope) relative_path
        | None -> None
      )
    | None -> None

let rec lookup_all = fun (env: t) path ->
  if is_bare_path path then
    Value_env.lookup_all env.values path
  else
    match split_module_lookup_path path with
    | Some (module_path, relative_path) -> (
        match lookup_module_scope env module_path with
        | Some module_scope -> lookup_all (of_scope module_scope) relative_path
        | None -> []
      )
    | None -> []

let rec lookup_constructors = fun (env: t) path ->
  if is_bare_path path then
    Constructor_env.lookup_all env.constructors (binding_name_of_path path)
  else
    match split_module_lookup_path path with
    | Some (module_path, relative_path) -> (
        match lookup_module_scope env module_path with
        | Some module_scope -> lookup_constructors (of_scope module_scope) relative_path
        | None -> []
      )
    | None -> []

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

let lookup_record_decls = fun (env: t) label_name ->
  Label_env.lookup_all env.labels label_name |> dedupe_record_decls

let record_decls = fun (env: t) -> Label_env.visible_record_decls env.labels |> dedupe_record_decls

let names = fun (env: t) ->
  env |> canonical_bindings |> List.map
    (fun binding -> Binding.path binding |> IdentPath.to_string)

let summary_delta_of_env = fun (env: t) ->
  {
    bindings = bindings env;
    type_decls = type_decls env;
  }

let snapshot_summary = fun (env: t) ->
  { env with summary = Summary_snapshot (summary_delta_of_env env) }

let introduced_names = fun before after ->
  let before_name_set =
    visible_bindings before |> List.fold_left
      (fun seen binding ->
        let () = Collections.HashSet.insert seen (Binding.path binding) |> ignore in
        seen)
      (Collections.HashSet.with_capacity 32)
  in
  visible_bindings after |> List.filter_map
    (fun binding ->
      let path = Binding.path binding in
      if Collections.HashSet.contains before_name_set path then
        None
      else
        Some (IdentPath.to_string path))

let bind = fun (env: t) (introduced: t) ->
  {
    summary = Summary_bind (env.summary, introduced.summary);
    values = Value_env.bind env.values introduced.values;
    modules = Module_env.bind env.modules introduced.modules;
    types = Type_env.bind env.types introduced.types;
    constructors = Constructor_env.bind env.constructors introduced.constructors;
    labels = Label_env.bind env.labels introduced.labels;
  }

let bind_in_scope = fun (env: t) ~scope_path (introduced: t) ->
  if IdentPath.is_empty scope_path then
    bind env introduced
  else
    {
      summary = Summary_bind_in_scope (env.summary, scope_path, introduced.summary);
      values = env.values;
      modules = Module_env.bind
        env.modules
        (Module_env.merge_scope
          Module_env.empty
          ~module_path:scope_path
          (Module_env.make_scope
            ~values:introduced.values
            ~modules:introduced.modules
            ~types:introduced.types
            ~constructors:introduced.constructors
            ~labels:introduced.labels));
      types = Type_env.bind env.types Type_env.empty;
      constructors = Constructor_env.bind env.constructors Constructor_env.empty;
      labels = Label_env.bind env.labels Label_env.empty;
    }

let env_of_local_type_decls = fun type_decls ->
  let types = Type_env.of_type_decls type_decls in
  let constructors = Constructor_env.of_type_decls type_decls in
  let labels = Label_env.of_type_decls type_decls in
  make
    ~summary:(Summary_snapshot { bindings = []; type_decls })
    Value_env.empty
    Module_env.empty
    types
    constructors
    labels

let of_type_decls = fun type_decls ->
  type_decls |> List.fold_left
    (fun env (type_decl: FileSummary.type_decl) ->
      let local_decl = { type_decl with scope_path = IdentPath.empty } in
      let introduced = env_of_local_type_decls [ local_decl ] in
      if IdentPath.is_empty type_decl.scope_path then
        bind env introduced
      else
        let env = bind env
          {
            summary = Summary_snapshot { bindings = []; type_decls = [] };
            values = Value_env.empty;
            modules = Module_env.empty;
            types = Type_env.empty;
            constructors = introduced.constructors;
            labels = introduced.labels;
          }
        in
        bind_in_scope env ~scope_path:type_decl.scope_path introduced)
    { empty with summary = Summary_snapshot { bindings = []; type_decls } }

let extend = fun env introduced -> bind env (of_bindings introduced)

let with_local_open = fun (env: t) module_path ->
  match lookup_module_scope env module_path with
  | Some scope ->
      {
        summary = Summary_open (env.summary, module_path);
        values = Value_env.add_open ~root:module_path (Module_env.scope_values scope) env.values;
        modules = Module_env.add_open ~root:module_path (Module_env.scope_modules scope) env.modules;
        types = Type_env.add_open ~root:module_path (Module_env.scope_types scope) env.types;
        constructors = Constructor_env.add_open
          ~root:module_path
          (Module_env.scope_constructors scope)
          env.constructors;
        labels = Label_env.add_open ~root:module_path (Module_env.scope_labels scope) env.labels;
      }
  | None -> env

let entries_for_include = fun (env: t) module_path ->
  match lookup_module_scope env module_path with
  | Some scope ->
      snapshot_summary {
        summary = Summary_empty;
        values =
          Module_env.scope_values scope |> Value_env.bindings |> List.map
            (fun binding ->
              Binding.with_provenance (Binding.Included { module_path }) binding) |> Value_env.of_bindings;
        modules = Module_env.local_only (Module_env.scope_modules scope);
        types = Type_env.local_only (Module_env.scope_types scope);
        constructors = Constructor_env.of_type_decls
          (Type_env.type_decls (Module_env.scope_types scope));
        labels = Label_env.of_type_decls (Type_env.type_decls (Module_env.scope_types scope));
      }
  | None -> empty

let export_names_for_module_alias = fun (env: t) ~alias_name ~module_path ->
  match lookup_module_scope env module_path with
  | Some scope ->
      Module_env.scope_bindings scope
      |> List.map
        (fun binding ->
          Binding.with_path (IdentPath.prepend_name alias_name (Binding.path binding)) binding)
      |> of_bindings
      |> canonical_bindings
      |> List.map
        (fun binding -> Binding.path binding |> IdentPath.to_string)
  | None -> []

let entries_for_module_alias = fun (env: t) ~alias_name ~module_path ->
  match lookup_module_scope env module_path with
  | Some scope ->
      snapshot_summary {
        summary = Summary_empty;
        values = Value_env.empty;
        modules = Module_env.bind_alias Module_env.empty ~alias_name scope;
        types = Type_env.empty;
        constructors = Constructor_env.empty;
        labels = Label_env.empty;
      }
  | None -> empty

let hidden_name_set = fun (config: TypConfig.t) ->
  Collections.HashSet.of_list (List.map fst (config.prelude @ config.ambient))

let export = fun config (env: t) ->
  let hidden_name_set = hidden_name_set config in
  env
  |> canonical_bindings
  |> List.filter
    (fun binding ->
      not (Collections.HashSet.contains hidden_name_set (Binding.path binding)))
  |> of_bindings

let export_with_forced_names = fun ~config ~forced_export_names env ->
  let hidden_name_set = hidden_name_set config in
  let forced_name_set = Collections.HashSet.of_list forced_export_names in
  env
  |> canonical_bindings
  |> List.filter
    (fun binding ->
      let path = Binding.path binding in
      let name = IdentPath.to_string path in
      not (Collections.HashSet.contains hidden_name_set path)
      || Collections.HashSet.contains forced_name_set name)
  |> of_bindings

let introduced_entries = fun (before: t) (after: t) ->
  let before_name_set =
    visible_bindings before |> List.fold_left
      (fun seen binding ->
        let () = Collections.HashSet.insert seen (Binding.path binding) |> ignore in
        seen)
      (Collections.HashSet.with_capacity 32)
  in
  visible_bindings after |> List.filter
    (fun binding -> not (Collections.HashSet.contains before_name_set (Binding.path binding)))
  |> of_bindings

let qualify = fun ~scope_path (env: t) ->
  if IdentPath.is_empty scope_path then
    env
  else
    {
      summary = Summary_qualify (env.summary, scope_path);
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
    }

let empty_summary = Summary_empty

let summary_snapshot = fun (env: t) -> env.summary

let summary_bind = fun summary (env: t) -> Summary_bind (summary, env.summary)

let summary_bind_in_scope = fun summary ~scope_path (env: t) ->
  Summary_bind_in_scope (summary, scope_path, env.summary)

let summary_open = fun summary module_path -> Summary_open (summary, module_path)

let summary_qualify = fun summary ~scope_path -> Summary_qualify (summary, scope_path)

let env_of_summary =
  let env_of_delta = fun delta ->
    bind
      (of_bindings delta.bindings)
      (of_type_decls delta.type_decls)
  in
  let rec loop = function
    | Summary_empty -> empty
    | Summary_snapshot delta -> env_of_delta delta
    | Summary_bind (summary, introduced) -> bind (loop summary) (loop introduced)
    | Summary_bind_in_scope (summary, scope_path, introduced) ->
        bind_in_scope (loop summary) ~scope_path (loop introduced)
    | Summary_open (summary, module_path) -> with_local_open (loop summary) module_path
    | Summary_qualify (summary, scope_path) -> qualify ~scope_path (loop summary)
  in
  loop

let scope_locals_of_summary = fun summary -> { summary; cached = None }

let scope_locals_for = fun scope_locals scope_path ->
  IdentPath.prefixes scope_path |> List.fold_left
    (fun acc key ->
      match Path_map.find_opt key scope_locals with
      | Some entries ->
          let env =
            match entries.cached with
            | Some env -> env
            | None ->
                let env = env_of_summary entries.summary in
                let () = entries.cached <- Some env in
                env
          in
          bind acc env
      | None -> acc)
    empty

let register_entries = fun scope ~scope_path (env: t) ->
  let existing =
    match Path_map.find_opt scope_path scope.locals with
    | Some entries -> entries
    | None -> scope_locals_of_summary empty_summary
  in
  let updated = { summary = summary_bind existing.summary env; cached = None } in
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
