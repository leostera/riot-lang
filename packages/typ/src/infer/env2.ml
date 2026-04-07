module Legacy_env = Env

open Std
open Analysis
open Model

module Binding = Legacy_env.Binding
module Constructor_env = Legacy_env.Constructor_env
module Label_env = Legacy_env.Label_env
module Type_env = Legacy_env.Type_env
module Value_env = Legacy_env.Value_env

module Name_map = Collections.Map.Make (String)

type scope = {
  values: Value_env.t;
  types: Type_env.t;
  constructors: Constructor_env.t;
  labels: Label_env.t;
  modules: module_table;
}

and module_binding = {
  name: string;
  components: scope;
}

and module_table = module_binding Name_map.t

type t = {
  summary: Summary2.t;
  values: Value_env.t;
  types: Type_env.t;
  constructors: Constructor_env.t;
  labels: Label_env.t;
  modules: module_table;
}

let empty_scope = {
  values = Value_env.empty;
  types = Type_env.empty;
  constructors = Constructor_env.empty;
  labels = Label_env.empty;
  modules = Name_map.empty;
}

let empty = {
  summary = Summary2.empty;
  values = Value_env.empty;
  types = Type_env.empty;
  constructors = Constructor_env.empty;
  labels = Label_env.empty;
  modules = Name_map.empty;
}

let scope_values : scope -> Value_env.t = fun scope -> scope.values

let scope_types : scope -> Type_env.t = fun scope -> scope.types

let scope_constructors : scope -> Constructor_env.t = fun scope -> scope.constructors

let scope_labels : scope -> Label_env.t = fun scope -> scope.labels

let summary_snapshot = fun env -> env.summary

let scope_of_env : t -> scope = fun env -> {
  values = env.values;
  types = env.types;
  constructors = env.constructors;
  labels = env.labels;
  modules = env.modules;
}

let dedupe_record_decls = fun record_decls ->
  let seen = Collections.HashSet.with_capacity (List.length record_decls) in
  record_decls |> List.filter
    (fun (record_decl: Label_env.record_decl) ->
      let owner_id = TypeConstructorId.to_int (Label_env.owner_type_constructor_id record_decl) in
      if Collections.HashSet.contains seen owner_id then
        false
      else
        let () = Collections.HashSet.insert seen owner_id |> ignore in
        true)

let merge_visible_module_tables : module_table -> module_table -> module_table = fun dominant rest ->
  Name_map.fold
    (fun name binding acc ->
      if Name_map.mem name acc then
        acc
      else
        Name_map.add name binding acc)
    rest
    dominant

let rec bind_scopes : scope -> scope -> scope = fun existing introduced ->
  {
    values = Value_env.bind existing.values introduced.values;
    types = Type_env.bind existing.types introduced.types;
    constructors = Constructor_env.bind existing.constructors introduced.constructors;
    labels = Label_env.bind existing.labels introduced.labels;
    modules = bind_module_tables existing.modules introduced.modules;
  }

and bind_module_tables : module_table -> module_table -> module_table = fun existing introduced ->
  Name_map.fold
    (fun name introduced_binding acc ->
      match Name_map.find_opt name acc with
      | Some existing_binding ->
          Name_map.add
            name
            {
              introduced_binding with
              components = bind_scopes existing_binding.components introduced_binding.components;
            }
            acc
      | None -> Name_map.add name introduced_binding acc)
    introduced
    existing

let rec qualify_scope : IdentPath.t -> scope -> scope = fun scope_path scope ->
  let values = Value_env.qualify_entries scope_path scope.values in
  let types = Type_env.qualify_entries scope_path scope.types in
  let qualified_type_decls = Type_env.type_decls types in
  {
    values;
    types;
    constructors = Constructor_env.of_type_decls qualified_type_decls;
    labels = Label_env.of_type_decls qualified_type_decls;
    modules = qualify_module_table scope_path scope.modules;
  }

and qualify_module_table : IdentPath.t -> module_table -> module_table = fun scope_path modules ->
  Name_map.map
    (fun binding ->
      let qualified_path = IdentPath.append_name scope_path binding.name in
      { binding with components = qualify_scope qualified_path binding.components })
    modules

let rec insert_scope_at_path : module_table -> module_path:IdentPath.t -> scope -> module_table =
 fun modules ~module_path introduced ->
  match IdentPath.uncons module_path with
  | None -> modules
  | Some (name, tail) ->
      if IdentPath.is_empty tail then
        let binding =
          match Name_map.find_opt name modules with
          | Some existing ->
              { name; components = bind_scopes existing.components introduced }
          | None ->
              { name; components = introduced }
        in
        Name_map.add name binding modules
      else
        let existing_components =
          match Name_map.find_opt name modules with
          | Some existing -> existing.components
          | None -> empty_scope
        in
        let binding = {
          name;
          components = {
            existing_components with
            modules = insert_scope_at_path existing_components.modules ~module_path:tail introduced;
          };
        } in
        Name_map.add name binding modules

let bind_in_scope_modules : t -> scope_path:IdentPath.t -> t -> t = fun env ~scope_path introduced ->
  {
    env with
    modules =
      insert_scope_at_path
        env.modules
        ~module_path:scope_path
        (qualify_scope scope_path (scope_of_env introduced));
  }

let split_relative_binding = fun binding ->
  match IdentPath.split_last (Binding.path binding) with
  | Some (scope_path, name) when not (IdentPath.is_empty scope_path) ->
      Some (scope_path, Binding.with_path (IdentPath.of_name name) binding)
  | _ -> None

let partition_bindings = fun bindings ->
  bindings |> List.fold_left
    (fun (bare, qualified) binding ->
      match split_relative_binding binding with
      | Some scoped -> (bare, scoped :: qualified)
      | None -> (binding :: bare, qualified))
    ([], [])

let env_of_local_type_decls : FileSummary.type_decl list -> t = fun type_decls ->
  let types = Type_env.of_type_decls type_decls in
  {
    empty with
    types;
    constructors = Constructor_env.of_type_decls type_decls;
    labels = Label_env.of_type_decls type_decls;
  }

let bind = fun env introduced ->
  {
    summary = Summary2.bind env.summary introduced.summary;
    values = Value_env.bind env.values introduced.values;
    types = Type_env.bind env.types introduced.types;
    constructors = Constructor_env.bind env.constructors introduced.constructors;
    labels = Label_env.bind env.labels introduced.labels;
    modules = bind_module_tables env.modules introduced.modules;
  }

let bind_in_scope = fun env ~scope_path introduced ->
  if IdentPath.is_empty scope_path then
    bind env introduced
  else
    {
      (bind_in_scope_modules env ~scope_path introduced) with
      summary = Summary2.bind_in_scope env.summary ~scope_path introduced.summary;
    }

let of_bindings = fun bindings ->
  let (bare_bindings, qualified_bindings) = partition_bindings bindings in
  qualified_bindings |> List.fold_left
    (fun env (scope_path, binding) ->
      bind_in_scope_modules
        env
        ~scope_path
        { empty with values = Value_env.of_bindings [ binding ] })
    {
      empty with
      summary = Summary2.snapshot ~bindings ~type_decls:[];
      values = Value_env.of_bindings (List.rev bare_bindings);
    }

let of_type_decls = fun type_decls ->
  type_decls |> List.fold_left
    (fun env (type_decl: FileSummary.type_decl) ->
      let local_decl = { type_decl with scope_path = IdentPath.empty } in
      let introduced = env_of_local_type_decls [ local_decl ] in
      if IdentPath.is_empty type_decl.scope_path then
        bind_scopes (scope_of_env env) (scope_of_env introduced) |> fun merged ->
        {
          env with
          values = merged.values;
          types = merged.types;
          constructors = merged.constructors;
          labels = merged.labels;
          modules = merged.modules;
        }
      else
        let env = {
          env with
          constructors = Constructor_env.bind env.constructors introduced.constructors;
          labels = Label_env.bind env.labels introduced.labels;
        } in
        bind_in_scope_modules env ~scope_path:type_decl.scope_path introduced)
    { empty with summary = Summary2.snapshot ~bindings:[] ~type_decls }

let rec lookup_module_scope_in : module_table -> IdentPath.t -> scope option = fun modules module_path ->
  match IdentPath.uncons module_path with
  | None -> None
  | Some (name, tail) -> (
      match Name_map.find_opt name modules with
      | None -> None
      | Some binding ->
          if IdentPath.is_empty tail then
            Some binding.components
          else
            lookup_module_scope_in binding.components.modules tail
    )

let lookup_module_scope = fun env module_path ->
  lookup_module_scope_in env.modules module_path

let with_local_open = fun env module_path ->
  match lookup_module_scope env module_path with
  | Some scope ->
      {
        summary = Summary2.open_ env.summary module_path;
        values = Value_env.bind scope.values env.values;
        types = Type_env.bind scope.types env.types;
        constructors = Constructor_env.bind scope.constructors env.constructors;
        labels = Label_env.bind scope.labels env.labels;
        modules = merge_visible_module_tables env.modules scope.modules;
      }
  | None -> env

let qualify = fun ~scope_path env ->
  if IdentPath.is_empty scope_path then
    env
  else
    {
      empty with
      summary = Summary2.qualify env.summary ~scope_path;
      modules =
        insert_scope_at_path
          Name_map.empty
          ~module_path:scope_path
          (qualify_scope scope_path (scope_of_env env));
    }

let split_module_lookup_path = fun path ->
  IdentPath.split_last path
  |> Option.map (fun (module_path, name) -> (module_path, IdentPath.of_name name))

let lookup = fun env path ->
  if IdentPath.is_bare path then
    Value_env.lookup env.values path
  else
    match split_module_lookup_path path with
    | Some (module_path, name) ->
        Option.and_then (lookup_module_scope env module_path)
          (fun scope -> Value_env.lookup scope.values name)
    | None -> None

let lookup_all = fun env path ->
  if IdentPath.is_bare path then
    Value_env.lookup_all env.values path
  else
    match split_module_lookup_path path with
    | Some (module_path, name) -> (
        match lookup_module_scope env module_path with
        | Some scope -> Value_env.lookup_all scope.values name
        | None -> []
      )
    | None -> []

let lookup_type = fun env path ->
  if IdentPath.is_bare path then
    Type_env.lookup env.types path
  else
    match split_module_lookup_path path with
    | Some (module_path, name) ->
        Option.and_then (lookup_module_scope env module_path)
          (fun scope -> Type_env.lookup scope.types name)
    | None -> None

let lookup_constructors = fun env path ->
  if IdentPath.is_bare path then
    match IdentPath.last_name path with
    | Some name -> Constructor_env.lookup_all env.constructors name
    | None -> []
  else
    match IdentPath.split_last path with
    | Some (module_path, name) -> (
        match lookup_module_scope env module_path with
        | Some scope -> Constructor_env.lookup_all scope.constructors name
        | None -> []
      )
    | None -> []

let lookup_owned_constructor = fun env path owner_type_constructor_id ->
  let lookup_local constructors path =
    match IdentPath.last_name path with
    | Some name -> Constructor_env.lookup_owned constructors name owner_type_constructor_id
    | None -> None
  in
  if IdentPath.is_bare path then
    lookup_local env.constructors path
  else
    match split_module_lookup_path path with
    | Some (module_path, name) -> (
        match lookup_module_scope env module_path with
        | Some scope -> lookup_local scope.constructors name
        | None -> None
      )
    | None -> None

let lookup_record_decls = fun env label_name ->
  Label_env.lookup_all env.labels label_name |> dedupe_record_decls

let lookup_record_decl_by_owner = fun env owner_type_constructor_id ->
  Label_env.lookup_owned env.labels owner_type_constructor_id

let rec module_bindings = fun modules ->
  Name_map.bindings modules |> List.concat_map
    (fun (_, binding) ->
      Value_env.bindings binding.components.values @ module_bindings binding.components.modules)

let bindings = fun env ->
  Value_env.bindings env.values @ module_bindings env.modules

let rec module_type_decls = fun modules ->
  Name_map.bindings modules |> List.concat_map
    (fun (_, binding) ->
      Type_env.type_decls binding.components.types @ module_type_decls binding.components.modules)

let type_decls = fun env ->
  Type_env.type_decls env.types @ module_type_decls env.modules

let record_decls = fun env ->
  Label_env.of_type_decls (type_decls env)
  |> Label_env.visible_record_decls
  |> dedupe_record_decls

let summary_cache : (Summary2.t, t) Collections.HashMap.t = Collections.HashMap.with_capacity 128

let env_of_summary =
  let env_of_delta delta = bind (of_bindings delta.Summary2.bindings) (of_type_decls delta.type_decls) in
  let rec loop summary =
    match Collections.HashMap.get summary_cache summary with
    | Some cached -> cached
    | None ->
        let resolved =
          match summary with
          | Summary2.Empty -> empty
          | Summary2.Snapshot delta -> env_of_delta delta
          | Summary2.Bind (summary, introduced) -> bind (loop summary) (loop introduced)
          | Summary2.Bind_in_scope (summary, scope_path, introduced) ->
              bind_in_scope (loop summary) ~scope_path (loop introduced)
          | Summary2.Open (summary, module_path) -> with_local_open (loop summary) module_path
          | Summary2.Qualify (summary, scope_path) -> qualify ~scope_path (loop summary)
        in
        let _ = Collections.HashMap.insert summary_cache summary resolved in
        resolved
  in
  loop

let env_of_legacy_summary = fun summary ->
  env_of_summary (Summary2.of_legacy_summary summary)

let of_legacy_env = fun env ->
  env_of_legacy_summary (Legacy_env.summary_snapshot env)

let to_legacy_env = fun env ->
  Legacy_env.env_of_summary (Summary2.to_legacy_summary env.summary)
