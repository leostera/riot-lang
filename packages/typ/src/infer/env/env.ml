open Std
open Analysis
open Model
module Binding = Binding
module Constructor_env = Constructor_env
module Label_env = Label_env
module Type_env = Type_env
module Value_env = Value_env

module Name_map = Collections.Map.Make (String)

module Path_map = Collections.Map.Make (struct
  type t = IdentPath.t

  let compare = IdentPath.compare
end)

type bindings = Binding.t list

type summary = Summary2.t

type module_scope = {
  values: Value_env.t;
  types: Type_env.t;
  constructors: Constructor_env.t;
  labels: Label_env.t;
  modules: module_table;
}

and module_binding = {
  name: string;
  components: module_scope;
}

and module_table = module_binding Name_map.t

type t = {
  summary: summary;
  values: Value_env.t;
  types: Type_env.t;
  constructors: Constructor_env.t;
  labels: Label_env.t;
  modules: module_table;
}

type scope_locals = {
  summary: summary;
  env: t;
}

type item_scope = {
  locals: scope_locals Path_map.t;
  opens: IdentPath.t list Path_map.t;
  mutable locals_cache: t Path_map.t;
  mutable opens_cache: IdentPath.t list Path_map.t;
}

let empty_module_scope = {
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

let empty_summary = Summary2.empty

let empty_item_scope = {
  locals = Path_map.empty;
  opens = Path_map.empty;
  locals_cache = Path_map.empty;
  opens_cache = Path_map.empty
}

let scope_values: module_scope -> Value_env.t = fun scope -> scope.values

let scope_types: module_scope -> Type_env.t = fun scope -> scope.types

let scope_constructors: module_scope -> Constructor_env.t = fun scope -> scope.constructors

let scope_labels: module_scope -> Label_env.t = fun scope -> scope.labels

let summary_snapshot = fun (env: t) -> env.summary

let summary_bind = fun summary (env: t) ->
  Summary2.bind summary env.summary

let summary_bind_in_scope = fun summary ~scope_path (env: t) ->
  Summary2.bind_in_scope summary ~scope_path env.summary

let summary_open = fun summary module_path ->
  Summary2.open_ summary module_path

let summary_qualify = fun summary ~scope_path -> Summary2.qualify summary ~scope_path

let module_scope_of_env: t -> module_scope = fun env ->
  {
    values = env.values;
    types = env.types;
    constructors = env.constructors;
    labels = env.labels;
    modules = env.modules;
  }

let binding_name_of_path = fun path ->
  match IdentPath.last_name path with
  | Some name -> name
  | None -> ""

let summary_ident_of_ident = fun ident ->
  { Summary2.local_id = Binding.ident_local_id ident; name = Binding.ident_name ident }

let ident_of_summary_ident = fun (ident: Summary2.ident) ->
  Binding.make_ident ~local_id:ident.local_id ~name:ident.name

let summary_provenance_of_provenance = function
  | Binding.Lowered_pattern pat_id -> Summary2.Lowered_pattern pat_id
  | Binding.Prelude -> Summary2.Prelude
  | Binding.Ambient -> Summary2.Ambient
  | Binding.Type_constructor { type_name; scope_path } -> Summary2.Type_constructor {
    type_name;
    scope_path
  }
  | Binding.Exception { name; scope_path } -> Summary2.Exception { name; scope_path }
  | Binding.Declared_value { name; scope_path } -> Summary2.Declared_value { name; scope_path }
  | Binding.Included { module_path } -> Summary2.Included { module_path }
  | Binding.Module_alias { alias_name; module_path } -> Summary2.Module_alias {
    alias_name;
    module_path
  }

let provenance_of_summary_provenance = function
  | Summary2.Lowered_pattern pat_id -> Binding.Lowered_pattern pat_id
  | Summary2.Prelude -> Binding.Prelude
  | Summary2.Ambient -> Binding.Ambient
  | Summary2.Type_constructor { type_name; scope_path } -> Binding.Type_constructor {
    type_name;
    scope_path
  }
  | Summary2.Exception { name; scope_path } -> Binding.Exception { name; scope_path }
  | Summary2.Declared_value { name; scope_path } -> Binding.Declared_value { name; scope_path }
  | Summary2.Included { module_path } -> Binding.Included { module_path }
  | Summary2.Module_alias { alias_name; module_path } -> Binding.Module_alias {
    alias_name;
    module_path
  }

let summary_binding_of_binding = fun binding ->
  {
    Summary2.ident = summary_ident_of_ident (Binding.ident binding);
    path = Binding.path binding;
    scheme = Binding.scheme binding;
    provenance = summary_provenance_of_provenance (Binding.provenance binding)
  }

let binding_of_summary_binding = fun (binding: Summary2.binding) ->
  Binding.make
    ~ident:(ident_of_summary_ident binding.ident)
    ~path:binding.path
    ~scheme:binding.scheme
    ~provenance:(provenance_of_summary_provenance binding.provenance)

let qualify_type_decl = fun prefix (type_decl: FileSummary.type_decl) ->
  { type_decl with scope_path = IdentPath.append_path prefix type_decl.scope_path }

let dedupe_record_decls = fun record_decls ->
  let seen = Collections.HashSet.with_capacity (List.length record_decls) in
  record_decls |> List.filter
    (fun (record_decl: Label_env.record_decl) ->
      let owner_id = Label_env.owner_type_constructor_id record_decl in
      if Collections.HashSet.contains seen owner_id then
        false
      else
        let () = Collections.HashSet.insert seen owner_id |> ignore in
        true)

let merge_visible_module_tables: module_table -> module_table -> module_table = fun dominant rest ->
  Name_map.fold
    (fun name binding acc ->
      if Name_map.mem name acc then
        acc
      else
        Name_map.add name binding acc)
    rest
    dominant

let rec bind_scopes: module_scope -> module_scope -> module_scope = fun existing introduced ->
  {
    values = Value_env.bind existing.values introduced.values;
    types = Type_env.bind existing.types introduced.types;
    constructors = Constructor_env.bind existing.constructors introduced.constructors;
    labels = Label_env.bind existing.labels introduced.labels;
    modules = bind_module_tables existing.modules introduced.modules;
  }

and bind_module_tables: module_table -> module_table -> module_table = fun existing introduced ->
  Name_map.fold
    (fun name introduced_binding acc ->
      match Name_map.find_opt name acc with
      | Some existing_binding -> Name_map.add
        name
        {
          introduced_binding
          with components = bind_scopes existing_binding.components introduced_binding.components
        }
        acc
      | None -> Name_map.add name introduced_binding acc)
    introduced
    existing

let rec insert_scope_at_path: module_table -> module_path:IdentPath.t -> module_scope -> module_table = fun modules ~module_path introduced ->
  match IdentPath.uncons module_path with
  | None -> modules
  | Some (name, tail) ->
      if IdentPath.is_empty tail then
        let binding =
          match Name_map.find_opt name modules with
          | Some existing -> { name; components = bind_scopes existing.components introduced }
          | None -> { name; components = introduced }
        in
        Name_map.add name binding modules
      else
        let existing_components =
          match Name_map.find_opt name modules with
          | Some existing -> existing.components
          | None -> empty_module_scope
        in
        let binding = {
          name;
          components = {
            existing_components
            with modules = insert_scope_at_path existing_components.modules ~module_path:tail introduced
          }
        } in
        Name_map.add name binding modules

let bind_in_scope_modules: t -> scope_path:IdentPath.t -> t -> t = fun env ~scope_path introduced ->
  {
    env
    with modules = insert_scope_at_path
      env.modules
      ~module_path:scope_path
      (module_scope_of_env introduced)
  }

let split_relative_binding = fun binding ->
  match IdentPath.split_last (Binding.path binding) with
  | Some (scope_path, name) when not (IdentPath.is_empty scope_path) -> Some (
    scope_path,
    Binding.with_path (IdentPath.of_name name) binding
  )
  | _ -> None

let partition_bindings = fun bindings ->
  bindings |> List.fold_left
    (fun (bare, qualified) binding ->
      match split_relative_binding binding with
      | Some scoped -> (bare, scoped :: qualified)
      | None -> (binding :: bare, qualified))
    ([], [])

let env_of_local_type_decls: FileSummary.type_decl list -> t = fun type_decls ->
  let types = Type_env.of_type_decls type_decls in
  {
    empty
    with types;
    constructors = Constructor_env.of_type_decls type_decls;
    labels = Label_env.of_type_decls type_decls
  }

let bind = fun (env: t) (introduced: t) ->
  {
    summary = Summary2.bind env.summary introduced.summary;
    values = Value_env.bind env.values introduced.values;
    types = Type_env.bind env.types introduced.types;
    constructors = Constructor_env.bind env.constructors introduced.constructors;
    labels = Label_env.bind env.labels introduced.labels;
    modules = bind_module_tables env.modules introduced.modules;
  }

let bind_in_scope = fun (env: t) ~scope_path (introduced: t) ->
  if IdentPath.is_empty scope_path then
    bind env introduced
  else
    {
      (bind_in_scope_modules env ~scope_path introduced)
      with summary = Summary2.bind_in_scope env.summary ~scope_path introduced.summary
    }

let of_bindings = fun bindings ->
  let (bare_bindings, qualified_bindings) = partition_bindings bindings in
  qualified_bindings
  |> List.fold_left
    (fun env (scope_path, binding) ->
      bind_in_scope_modules env ~scope_path { empty with values = Value_env.of_bindings [ binding ] })
    {
      empty
      with summary = Summary2.snapshot
        ~bindings:(List.map summary_binding_of_binding bindings)
        ~type_decls:[];
      values = Value_env.of_bindings (List.rev bare_bindings)
    }

let of_type_decls = fun type_decls ->
  type_decls |> List.fold_left
    (fun env (type_decl: FileSummary.type_decl) ->
      let local_decl = { type_decl with scope_path = IdentPath.empty } in
      let introduced = env_of_local_type_decls [ local_decl ] in
      if IdentPath.is_empty type_decl.scope_path then
        bind_scopes (module_scope_of_env env) (module_scope_of_env introduced) |> fun merged ->
          {
            env
            with values = merged.values;
            types = merged.types;
            constructors = merged.constructors;
            labels = merged.labels;
            modules = merged.modules;
          }
      else
        let qualified = env_of_local_type_decls [ type_decl ] in
        let env = {
          env
          with constructors = Constructor_env.bind env.constructors qualified.constructors;
          labels = Label_env.bind env.labels qualified.labels
        } in
        bind_in_scope_modules env ~scope_path:type_decl.scope_path introduced)
    { empty with summary = Summary2.snapshot ~bindings:[] ~type_decls }

let of_entries = fun ~make_ident ~provenance entries ->
  entries
  |> List.map
    (fun (path, scheme) ->
      Binding.make
        ~ident:(make_ident (binding_name_of_path path))
        ~path
        ~scheme:(TypeScheme.copy scheme)
        ~provenance)
  |> of_bindings

let singleton = fun ~make_ident ~name ~scheme ~provenance ->
  of_bindings
    [ Binding.make ~ident:(make_ident name) ~path:(IdentPath.of_name name) ~scheme ~provenance ]

let singleton_constructor = fun ~make_ident ~name ~scheme ~provenance ~owner_path ~owner_type_constructor_id ~constructor_id ~inline_record_labels ->
  let binding = Binding.make ~ident:(make_ident name) ~path:(IdentPath.of_name name) ~scheme ~provenance in
  {
    empty
    with values = Value_env.of_bindings [ binding ];
    constructors = Constructor_env.singleton
      ~owner_path
      ~owner_type_constructor_id
      ~constructor:{ TypeDecl.constructor_id; name; scheme; inline_record_labels }
  }

let extend = fun env introduced -> bind env (of_bindings introduced)

let rec lookup_module_scope_in: module_table -> IdentPath.t -> module_scope option = fun modules module_path ->
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

let lookup_module_scope = fun env module_path -> lookup_module_scope_in env.modules module_path

let with_local_open = fun (env: t) module_path ->
  match lookup_module_scope env module_path with
  | Some scope ->
      {
        summary = Summary2.open_ env.summary module_path;
        values = Value_env.add_open ~root:module_path scope.values env.values;
        types = Type_env.add_open ~root:module_path scope.types env.types;
        constructors = Constructor_env.add_open ~root:module_path scope.constructors env.constructors;
        labels = Label_env.add_open ~root:module_path scope.labels env.labels;
        modules = merge_visible_module_tables env.modules scope.modules;
      }
  | None -> env

let qualify = fun ~scope_path (env: t) ->
  if IdentPath.is_empty scope_path then
    env
  else
    {
      empty
      with summary = Summary2.qualify env.summary ~scope_path;
      modules = insert_scope_at_path Name_map.empty ~module_path:scope_path (module_scope_of_env env)
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
          (fun scope ->
            Value_env.lookup scope.values name |> Option.map
              (fun binding ->
                Binding.with_path (IdentPath.append_path module_path (Binding.path binding)) binding))
    | None -> None

let lookup_all = fun env path ->
  if IdentPath.is_bare path then
    Value_env.lookup_all env.values path
  else
    match split_module_lookup_path path with
    | Some (module_path, name) -> (
        match lookup_module_scope env module_path with
        | Some scope ->
            Value_env.lookup_all scope.values name |> List.map
              (fun binding ->
                Binding.with_path (IdentPath.append_path module_path (Binding.path binding)) binding)
        | None -> []
      )
    | None -> []

let lookup_type = fun env path ->
  if IdentPath.is_bare path then
    Type_env.lookup env.types path
  else
    match split_module_lookup_path path with
    | Some (module_path, name) -> Option.and_then
      (lookup_module_scope env module_path)
      (fun scope -> Type_env.lookup scope.types name |> Option.map (qualify_type_decl module_path))
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

let lookup_record_decls = fun env label_name -> Label_env.lookup_all env.labels label_name |> dedupe_record_decls

let lookup_record_decl_by_owner = fun env owner_type_constructor_id ->
  Label_env.lookup_owned env.labels owner_type_constructor_id

let rec scope_bindings_with_prefix: IdentPath.t -> module_scope -> bindings = fun prefix scope ->
  let values =
    Value_env.bindings scope.values
    |> List.map
      (fun binding ->
        if IdentPath.is_empty prefix then
          binding
        else
          Binding.with_path (IdentPath.append_path prefix (Binding.path binding)) binding)
  in
  let modules = bindings_with_prefix prefix scope.modules in
  values @ modules

and bindings_with_prefix: IdentPath.t -> module_table -> bindings = fun prefix modules ->
  Name_map.bindings modules |> List.concat_map
    (fun (_, binding) ->
      let module_prefix =
        if IdentPath.is_empty prefix then
          IdentPath.of_name binding.name
        else
          IdentPath.append_name prefix binding.name
      in
      scope_bindings_with_prefix module_prefix binding.components)

let bindings = fun env -> Value_env.bindings env.values @ bindings_with_prefix IdentPath.empty env.modules

let rec scope_type_decls_with_prefix: IdentPath.t -> module_scope -> FileSummary.type_decl list = fun prefix scope ->
  let local = Type_env.type_decls scope.types
  |> List.map
    (fun (type_decl: FileSummary.type_decl) ->
      { type_decl with scope_path = IdentPath.append_path prefix type_decl.scope_path }) in
  let nested = module_type_decls_with_prefix prefix scope.modules in
  local @ nested

and module_type_decls_with_prefix: IdentPath.t -> module_table -> FileSummary.type_decl list = fun prefix modules ->
  Name_map.bindings modules |> List.concat_map
    (fun (_, binding) ->
      let module_prefix =
        if IdentPath.is_empty prefix then
          IdentPath.of_name binding.name
        else
          IdentPath.append_name prefix binding.name
      in
      scope_type_decls_with_prefix module_prefix binding.components)

let type_decls = fun env ->
  Type_env.type_decls env.types @ module_type_decls_with_prefix IdentPath.empty env.modules

let visible_type_decls = fun env ->
  Type_env.visible_type_decls env.types @ module_type_decls_with_prefix IdentPath.empty env.modules

let types = fun env -> env.types

let record_decls = fun env -> Label_env.of_type_decls (type_decls env) |> Label_env.record_decls |> dedupe_record_decls

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

let visible_binding_map = fun env ->
  visible_bindings env |> List.fold_left
    (fun acc binding ->
      Path_map.add (Binding.path binding) binding acc)
    Path_map.empty

let canonical_bindings = fun env ->
  visible_bindings env |> List.sort
    (fun left right ->
      IdentPath.compare (Binding.path left) (Binding.path right))

let unique = fun env -> env |> visible_bindings |> of_bindings

let render = fun env -> env |> canonical_bindings |> List.map Binding.render

let names = fun env ->
  env |> canonical_bindings |> List.map (fun binding -> Binding.path binding |> IdentPath.to_string)

let introduced_names = fun before after ->
  let before_bindings = visible_binding_map before in
  visible_bindings after |> List.filter_map
    (fun binding ->
      let path = Binding.path binding in
      match Path_map.find_opt path before_bindings with
      | Some previous when Binding.same previous binding -> None
      | _ -> Some (IdentPath.to_string path))

let hidden_name_set = fun (config: TypConfig.t) ->
  Collections.HashSet.of_list (List.map fst (config.prelude @ config.ambient))

let is_hidden_export_binding = fun hidden_name_set binding ->
  Collections.HashSet.contains hidden_name_set (Binding.path binding)
  && match Binding.provenance binding with
  | Binding.Prelude
  | Binding.Ambient -> true
  | Binding.Lowered_pattern _
  | Binding.Type_constructor _
  | Binding.Exception _
  | Binding.Declared_value _
  | Binding.Included _
  | Binding.Module_alias _ -> false

let export = fun config env ->
  let hidden_name_set = hidden_name_set config in
  env
  |> canonical_bindings
  |> List.filter (fun binding -> not (is_hidden_export_binding hidden_name_set binding))
  |> of_bindings

let export_with_forced_names = fun ~config ~forced_export_names env ->
  let hidden_name_set = hidden_name_set config in
  let forced_name_set = Collections.HashSet.of_list forced_export_names in
  env |> canonical_bindings |> List.filter
    (fun binding ->
      let path = Binding.path binding in
      let name = IdentPath.to_string path in
      not (is_hidden_export_binding hidden_name_set binding)
      || Collections.HashSet.contains forced_name_set name) |> of_bindings

let introduced_entries = fun before after ->
  let before_bindings = visible_binding_map before in
  visible_bindings after |> List.filter
    (fun binding ->
      match Path_map.find_opt (Binding.path binding) before_bindings with
      | Some previous -> not (Binding.same previous binding)
      | None -> true) |> of_bindings

let module_table_singleton = fun binding ->
  Name_map.add binding.name binding Name_map.empty

let entries_for_include = fun env module_path ->
  match lookup_module_scope env module_path with
  | Some scope ->
      {
        summary = Summary2.snapshot ~bindings:[] ~type_decls:[];
        values =
          scope.values |> Value_env.bindings |> List.map
            (fun binding ->
              Binding.with_provenance (Binding.Included { module_path }) binding) |> Value_env.of_bindings;
        modules = scope.modules;
        types = Type_env.local_only scope.types;
        constructors = Constructor_env.of_type_decls (Type_env.type_decls scope.types);
        labels = Label_env.of_type_decls (Type_env.type_decls scope.types);
      }
  | None -> empty

let export_names_for_module_alias = fun env ~alias_name ~module_path ->
  match lookup_module_scope env module_path with
  | Some scope -> scope_bindings_with_prefix (IdentPath.of_name alias_name) scope
  |> of_bindings
  |> canonical_bindings
  |> List.map (fun binding -> Binding.path binding |> IdentPath.to_string)
  | None -> []

let entries_for_module_alias = fun env ~alias_name ~module_path ->
  match lookup_module_scope env module_path with
  | Some scope ->
      {
        summary = Summary2.snapshot ~bindings:[] ~type_decls:[];
        values = Value_env.empty;
        modules = module_table_singleton { name = alias_name; components = scope };
        types = Type_env.empty;
        constructors = Constructor_env.empty;
        labels = Label_env.empty;
      }
  | None -> empty

let summary_cache: (summary, t) Collections.HashMap.t = Collections.HashMap.with_capacity 128

let summary_relative_cache: ((summary * summary), t) Collections.HashMap.t = Collections.HashMap.with_capacity
  128

let env_of_summary =
  let env_of_delta delta = bind
    (of_bindings (List.map binding_of_summary_binding delta.Summary2.bindings))
    (of_type_decls delta.type_decls) in
  let rec loop summary =
    match Collections.HashMap.get summary_cache summary with
    | Some cached -> cached
    | None ->
        let resolved =
          match summary with
          | Summary2.Empty -> empty
          | Summary2.Snapshot delta -> env_of_delta delta
          | Summary2.Bind (summary, introduced) -> bind (loop summary) (loop introduced)
          | Summary2.Bind_in_scope (summary, scope_path, introduced) -> bind_in_scope
            (loop summary)
            ~scope_path
            (loop introduced)
          | Summary2.Open (summary, module_path) -> with_local_open (loop summary) module_path
          | Summary2.Qualify (summary, scope_path) -> qualify ~scope_path (loop summary)
        in
        let _ = Collections.HashMap.insert summary_cache summary resolved in
        resolved
  in
  loop

let env_of_summary_relative =
  let env_of_delta delta = bind
    (of_bindings (List.map binding_of_summary_binding delta.Summary2.bindings))
    (of_type_decls delta.type_decls) in
  let rec loop (env: t) summary =
    let key = (env.summary, summary) in
    match Collections.HashMap.get summary_relative_cache key with
    | Some cached -> cached
    | None ->
        let resolved =
          match summary with
          | Summary2.Empty -> env
          | Summary2.Snapshot delta -> bind env (env_of_delta delta)
          | Summary2.Bind (summary, introduced) -> bind
            (loop env summary)
            (env_of_summary introduced)
          | Summary2.Bind_in_scope (summary, scope_path, introduced) -> bind_in_scope
            (loop env summary)
            ~scope_path
            (env_of_summary introduced)
          | Summary2.Open (summary, module_path) -> with_local_open (loop env summary) module_path
          | Summary2.Qualify (summary, scope_path) -> qualify ~scope_path (loop env summary)
        in
        let _ = Collections.HashMap.insert summary_relative_cache key resolved in
        resolved
  in
  loop

let scope_locals_of_summary = fun summary -> { summary; env = env_of_summary summary }

let scope_locals_for = fun scope scope_path ->
  match Path_map.find_opt scope_path scope.locals_cache with
  | Some env -> env
  | None ->
      let env =
        IdentPath.prefixes scope_path
        |> List.fold_left
          (fun acc key ->
            match Path_map.find_opt key scope.locals with
            | Some entries -> env_of_summary_relative acc entries.summary
            | None -> acc)
          empty
      in
      let () =
        scope.locals_cache <- Path_map.add scope_path env scope.locals_cache
      in
      env

let register_entries = fun scope ~scope_path (env: t) ->
  let existing =
    match Path_map.find_opt scope_path scope.locals with
    | Some entries -> entries
    | None -> scope_locals_of_summary empty_summary
  in
  let updated = {
    summary = Summary2.bind existing.summary env.summary;
    env = env_of_summary_relative existing.env env.summary
  } in
  { scope with locals = Path_map.add scope_path updated scope.locals; locals_cache = Path_map.empty }

let scope_opens_for = fun scope scope_path ->
  match Path_map.find_opt scope_path scope.opens_cache with
  | Some opens -> opens
  | None ->
      let opens =
        IdentPath.prefixes scope_path
        |> List.fold_left
          (fun acc key ->
            match Path_map.find_opt key scope.opens with
            | Some modules -> acc @ modules
            | None -> acc)
          []
      in
      let () =
        scope.opens_cache <- Path_map.add scope_path opens scope.opens_cache
      in
      opens

let register_open = fun scope ~scope_path ~module_path ->
  let existing =
    match Path_map.find_opt scope_path scope.opens with
    | Some modules -> modules
    | None -> []
  in
  let updated = existing @ [ module_path ] in
  { scope with opens = Path_map.add scope_path updated scope.opens; opens_cache = Path_map.empty }

let for_item_scope = fun (env: t) scope ~scope_path ->
  let locals = scope_locals_for scope scope_path in
  let base_env = bind env locals in
  scope_opens_for scope scope_path |> List.fold_left with_local_open base_env
