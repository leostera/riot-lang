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

and t = binding list Name_map.t

let empty = Name_map.empty

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

let rec scope_scopes = fun scope -> scope :: scopes scope.modules

and scopes = fun env ->
  Name_map.fold
    (fun _ module_bindings acc ->
      match module_bindings with
      | [] -> acc
      | binding :: _ -> scope_scopes binding.scope @ acc)
    env
    []

let visible_binding = fun env name ->
  match Name_map.find_opt name env with
  | Some (binding :: _) -> Some binding
  | _ -> None

let prepend_binding = fun env binding ->
  let existing = Name_map.find_opt binding.name env |> Option.unwrap_or ~default:[] in
  Name_map.add binding.name (binding :: existing) env

let replace_visible_binding = fun env binding ->
  match Name_map.find_opt binding.name env with
  | Some (_ :: rest) -> Name_map.add binding.name (binding :: rest) env
  | _ -> Name_map.add binding.name [ binding ] env

let rec bind_scope = fun existing introduced ->
  {
    values = Value_env.bind existing.values introduced.values;
    modules = bind existing.modules introduced.modules;
    types = Type_env.bind existing.types introduced.types;
    constructors = Constructor_env.bind existing.constructors introduced.constructors;
    labels = Label_env.bind existing.labels introduced.labels;
  }

and bind = fun env introduced ->
  if Name_map.is_empty introduced then
    env
  else if Name_map.is_empty env then
    introduced
  else
    Name_map.fold
      (fun _ introduced_bindings acc ->
        introduced_bindings |> List.rev |> List.fold_left
          (fun acc binding ->
            match visible_binding acc binding.name with
            | Some existing -> replace_visible_binding
              acc
              { binding with scope = bind_scope existing.scope binding.scope }
            | None -> prepend_binding acc binding)
          acc)
      introduced
      env

and merge_scope = fun env ~module_path introduced ->
  match IdentPath.uncons module_path with
  | None -> env
  | Some (name, tail) ->
      if IdentPath.is_empty tail then
        match visible_binding env name with
        | Some existing -> replace_visible_binding
          env
          { existing with scope = bind_scope existing.scope introduced }
        | None -> prepend_binding env { name; scope = introduced }
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
        | None -> prepend_binding env binding

let bind_alias = fun env ~alias_name scope -> prepend_binding env { name = alias_name; scope }

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

let rec lookup = fun env module_path ->
  match IdentPath.uncons module_path with
  | None -> None
  | Some (name, tail) ->
      if IdentPath.is_empty tail then
        visible_binding env name |> Option.map (fun binding -> binding.scope)
      else
        Option.and_then (visible_binding env name) (fun binding -> lookup binding.scope.modules tail)

let rec scope_bindings_with_prefix = fun prefix scope ->
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

and bindings_with_prefix = fun prefix env ->
  Name_map.fold
    (fun _ module_bindings acc ->
      match module_bindings with
      | [] -> acc
      | binding :: _ ->
          let module_prefix =
            if IdentPath.is_empty prefix then
              IdentPath.of_name binding.name
            else
              IdentPath.append_name prefix binding.name
          in
          scope_bindings_with_prefix module_prefix binding.scope @ acc)
    env
    []

let scope_bindings = fun scope -> scope_bindings_with_prefix IdentPath.empty scope

let bindings = fun env -> bindings_with_prefix IdentPath.empty env
