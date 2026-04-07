open Std
open Model

module Name_map = Collections.Map.Make (String)

type scope = {
  values: Value_env.t;
  modules: t;
}

and binding = {
  name: string;
  scope: scope;
}

and t = binding list Name_map.t

let empty = Name_map.empty

let empty_scope = { values = Value_env.empty; modules = empty }

let make_scope = fun ~values ~modules -> { values; modules }

let scope_values = fun scope -> scope.values

let scope_modules = fun scope -> scope.modules

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
    modules = bind existing.modules introduced.modules
  }

and bind = fun env introduced ->
  Name_map.fold
    (fun name introduced_bindings acc ->
      let existing = Name_map.find_opt name acc |> Option.unwrap_or ~default:[] in
      Name_map.add name (introduced_bindings @ existing) acc)
    introduced
    env

and merge_scope = fun env ~module_path introduced ->
  match IdentPath.to_segments module_path with
  | [] ->
      env
  | [ name ] -> (
      match visible_binding env name with
      | Some existing -> replace_visible_binding
        env
        { existing with scope = bind_scope existing.scope introduced }
      | None -> prepend_binding env { name; scope = introduced }
    )
  | name :: rest ->
      let nested_modules =
        match visible_binding env name with
        | Some existing -> merge_scope
          existing.scope.modules
          ~module_path:(IdentPath.of_segments rest)
          introduced
        | None -> merge_scope empty ~module_path:(IdentPath.of_segments rest) introduced
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
  match Binding.path binding |> IdentPath.to_segments with
  | []
  | [ "" ] ->
      empty_scope
  | [ _ ] ->
      { values = Value_env.of_bindings [ binding ]; modules = empty }
  | module_name :: rest ->
      let relative_binding = Binding.with_path (IdentPath.of_segments rest) binding in
      {
        values = Value_env.empty;
        modules = merge_scope
          empty
          ~module_path:(IdentPath.of_name module_name)
          (scope_of_binding relative_binding)
      }

let of_bindings = fun bindings ->
  bindings |> List.fold_left
    (fun env binding ->
      match Binding.path binding |> IdentPath.to_segments with
      | []
      | [ _ ] -> env
      | module_name :: rest ->
          let relative_binding = Binding.with_path (IdentPath.of_segments rest) binding in
          merge_scope
            env
            ~module_path:(IdentPath.of_name module_name)
            (scope_of_binding relative_binding))
    empty

let rec lookup = fun env module_path ->
  match IdentPath.to_segments module_path with
  | []
  | [ "" ] -> None
  | [ name ] -> visible_binding env name |> Option.map (fun binding -> binding.scope)
  | name :: rest -> Option.and_then
    (visible_binding env name)
    (fun binding -> lookup binding.scope.modules (IdentPath.of_segments rest))

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
