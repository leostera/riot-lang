open Std
open Model

module Name_map = Collections.Map.Make (String)

type scope = {
  values: Value_env.t;
  modules: t;
  types: Type_env.t;
  constructors: Constructor_env.t;
  labels: Label_env.t;
}

and binding = {
  name: string;
  scope: scope;
}

and current = binding list Name_map.t

and components = scope Name_map.t

and layer =
  | Nothing
  | Open of { root: IdentPath.t; components: components; next: t }

and t = {
  bindings: binding list;
  current: current;
  layer: layer;
}

let empty = { bindings = []; current = Name_map.empty; layer = Nothing }

let empty_scope = {
  values = Value_env.empty;
  modules = empty;
  types = Type_env.empty;
  constructors = Constructor_env.empty;
  labels = Label_env.empty;
}

let make_scope = fun ~values ~modules ~types ~constructors ~labels ->
  {
    values;
    modules;
    types;
    constructors;
    labels;
  }

let scope_values = fun scope -> scope.values

let scope_modules = fun scope -> scope.modules

let scope_types = fun scope -> scope.types

let scope_constructors = fun scope -> scope.constructors

let scope_labels = fun scope -> scope.labels

let prepend_binding = fun index binding ->
  let existing = Name_map.find_opt binding.name index |> Option.unwrap_or ~default:[] in
  Name_map.add binding.name (binding :: existing) index

let index_of_bindings = fun bindings ->
  bindings |> List.rev |> List.fold_left prepend_binding Name_map.empty

let bindings_of_current = fun current -> Name_map.bindings current |> List.concat_map snd

let bindings = fun env ->
  let rec scope_bindings_with_prefix prefix scope =
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
  and bindings_with_prefix prefix env =
    bindings_of_current env.current
    |> List.concat_map
      (fun binding ->
        let module_prefix =
          if IdentPath.is_empty prefix then
            IdentPath.of_name binding.name
          else
            IdentPath.append_name prefix binding.name
        in
        scope_bindings_with_prefix module_prefix binding.scope)
  in
  bindings_with_prefix IdentPath.empty env

let local_only = fun env -> { env with layer = Nothing }

let visible_components_of_current = fun current ->
  Name_map.fold
    (fun name bindings acc ->
      match bindings with
      | binding :: _ -> Name_map.add name binding.scope acc
      | [] -> acc)
    current
    Name_map.empty

let add_open = fun ~root opened env ->
  {
    bindings = env.bindings;
    current = Name_map.empty;
    layer = Open { root; components = visible_components_of_current opened.current; next = env }
  }

let visible_binding = fun env name ->
  match Name_map.find_opt name env.current with
  | Some (binding :: _) -> Some binding
  | _ -> None

let replace_visible_binding = fun env binding ->
  match Name_map.find_opt binding.name env.current with
  | Some (_ :: rest) -> {
    env
    with current = Name_map.add binding.name (binding :: rest) env.current
  }
  | _ -> { env with current = Name_map.add binding.name [ binding ] env.current }

let rec bind_scope = fun existing introduced ->
  {
    values = Value_env.bind existing.values introduced.values;
    modules = bind existing.modules introduced.modules;
    types = Type_env.bind existing.types introduced.types;
    constructors = Constructor_env.bind existing.constructors introduced.constructors;
    labels = Label_env.bind existing.labels introduced.labels;
  }

and merge_current = fun introduced existing ->
  Name_map.fold
    (fun name introduced_bindings acc ->
      let current = Name_map.find_opt name acc |> Option.unwrap_or ~default:[] in
      let merged =
        match (introduced_bindings, current) with
        | (introduced_binding :: introduced_rest, current_binding :: current_rest) -> {
          introduced_binding
          with scope = bind_scope current_binding.scope introduced_binding.scope
        }
        :: (introduced_rest @ current_rest)
        | _ -> introduced_bindings @ current
      in
      Name_map.add name merged acc)
    introduced
    existing

and bind = fun env introduced ->
  if List.is_empty introduced.bindings then
    env
  else if List.is_empty env.bindings && env.layer = Nothing then
    introduced
  else
    {
      bindings = introduced.bindings @ env.bindings;
      current = merge_current introduced.current env.current;
      layer = env.layer
    }

and merge_scope = fun env ~module_path introduced ->
  match IdentPath.uncons module_path with
  | None -> env
  | Some (name, tail) ->
      if IdentPath.is_empty tail then
        match visible_binding env name with
        | Some existing ->
            let updated = { existing with scope = bind_scope existing.scope introduced } in
            replace_visible_binding env updated
        | None -> bind
          env
          {
            bindings = [ { name; scope = introduced } ];
            current = index_of_bindings [ { name; scope = introduced } ];
            layer = Nothing
          }
      else
        let nested_modules =
          match visible_binding env name with
          | Some existing -> merge_scope existing.scope.modules ~module_path:tail introduced
          | None -> merge_scope empty ~module_path:tail introduced
        in
        let nested_scope =
          match visible_binding env name with
          | Some existing -> { existing.scope with modules = nested_modules }
          | None -> { empty_scope with modules = nested_modules }
        in
        let binding = { name; scope = nested_scope } in
        match visible_binding env name with
        | Some _ -> replace_visible_binding env binding
        | None -> bind
          env
          { bindings = [ binding ]; current = index_of_bindings [ binding ]; layer = Nothing }

let bind_alias = fun env ~alias_name scope ->
  let binding = { name = alias_name; scope } in
  bind env { bindings = [ binding ]; current = index_of_bindings [ binding ]; layer = Nothing }

let rec scope_of_binding = fun binding ->
  match IdentPath.uncons (Binding.path binding) with
  | None ->
      empty_scope
  | Some (_, tail) when IdentPath.is_empty tail ->
      {
        values = Value_env.of_bindings [ binding ];
        modules = empty;
        types = Type_env.empty;
        constructors = Constructor_env.empty;
        labels = Label_env.empty;
      }
  | Some (module_name, tail) ->
      let relative_binding = Binding.with_path tail binding in
      {
        values = Value_env.empty;
        modules = merge_scope
          empty
          ~module_path:(IdentPath.of_name module_name)
          (scope_of_binding relative_binding);
        types = Type_env.empty;
        constructors = Constructor_env.empty;
        labels = Label_env.empty;
      }

let of_bindings = fun bindings ->
  bindings |> List.fold_left
    (fun env binding ->
      match IdentPath.uncons (Binding.path binding) with
      | None ->
          env
      | Some (_, tail) when IdentPath.is_empty tail ->
          env
      | Some (module_name, tail) ->
          let relative_binding = Binding.with_path tail binding in
          merge_scope
            env
            ~module_path:(IdentPath.of_name module_name)
            (scope_of_binding relative_binding))
    empty

let scope_scopes = fun scope ->
  let rec loop acc scope =
    let acc = scope :: acc in
    bindings_of_current scope.modules.current
    |> List.fold_left (fun acc binding -> loop acc binding.scope) acc
  in
  loop [] scope |> List.rev

let scopes = fun env ->
  bindings_of_current env.current
  |> List.fold_left (fun acc binding -> scope_scopes binding.scope @ acc) []

let scope_bindings = fun scope ->
  let rec scope_bindings_with_prefix prefix scope =
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
  and bindings_with_prefix prefix env =
    bindings_of_current env.current
    |> List.concat_map
      (fun binding ->
        let module_prefix =
          if IdentPath.is_empty prefix then
            IdentPath.of_name binding.name
          else
            IdentPath.append_name prefix binding.name
        in
        scope_bindings_with_prefix module_prefix binding.scope)
  in
  scope_bindings_with_prefix IdentPath.empty scope

let rec lookup_name = fun env name ->
  match visible_binding env name with
  | Some binding -> Some binding.scope
  | None -> (
      match env.layer with
      | Nothing -> None
      | Open { components; next; _ } -> (
          match Name_map.find_opt name components with
          | Some scope -> Some scope
          | None -> lookup_name next name
        )
    )

let rec lookup = fun env module_path ->
  match IdentPath.uncons module_path with
  | None -> None
  | Some (name, tail) -> (
      match lookup_name env name with
      | Some scope ->
          if IdentPath.is_empty tail then
            Some scope
          else
            lookup scope.modules tail
      | None -> None
    )
