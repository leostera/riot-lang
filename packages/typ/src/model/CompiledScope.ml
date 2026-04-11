open Std

type module_binding = {
  name: string;
  scope: t;
}

and t = {
  values: (string * TypeScheme.t) list;
  type_decls: FileSummary.type_decl list;
  modules: module_binding list;
}

let empty = { values = []; type_decls = []; modules = [] }

let rec upsert_module = fun modules ~name ~f ->
  match modules with
  | [] -> [ { name; scope = f empty } ]
  | ({ name = existing_name; scope } as binding) :: rest ->
      if String.equal name existing_name then
        { binding with scope = f scope } :: rest
      else
        binding :: upsert_module rest ~name ~f

let rec add_export = fun scope (path, scheme) ->
  match SurfacePath.uncons path with
  | None -> scope
  | Some (name, tail) ->
      if SurfacePath.is_empty tail then
        { scope with values = scope.values @ [ (name, scheme) ] }
      else
        {
          scope with modules = upsert_module scope.modules ~name ~f:(fun child ->
            add_export child (tail, scheme))
        }

let rec add_type_decl = fun scope (type_decl: FileSummary.type_decl) ->
  match SurfacePath.uncons type_decl.scope_path with
  | None -> { scope with type_decls = scope.type_decls @ [ type_decl ] }
  | Some (name, tail) ->
      {
        scope with modules = upsert_module scope.modules ~name ~f:(fun child ->
          add_type_decl child { type_decl with scope_path = tail })
      }

let of_module_surface = fun ~exports ~type_decls ->
  let scope = List.fold_left add_export empty exports in
  List.fold_left add_type_decl scope type_decls

let rec exports = fun scope ->
  let local_exports = scope.values |> List.map (fun (name, scheme) -> (SurfacePath.of_name name, scheme)) in
  let nested_exports = scope.modules |> List.concat_map
    (fun { name; scope } ->
      exports scope |> List.map (fun (path, scheme) -> (SurfacePath.prepend_name name path, scheme))) in
  local_exports @ nested_exports

let rec type_decls = fun scope ->
  let nested_type_decls = scope.modules |> List.concat_map
    (fun { name; scope } ->
      type_decls scope |> List.map
        (fun (type_decl: FileSummary.type_decl) -> {
          type_decl with scope_path = SurfacePath.prepend_name name type_decl.scope_path
        })) in
  scope.type_decls @ nested_type_decls
