open Std

type constructor_entry = {
  owner_path: SurfacePath.t;
  owner_type_constructor_id: TypeConstructorId.t;
  constructor: TypeDecl.constructor;
}

type record_decl = {
  owner_path: SurfacePath.t;
  owner_type_constructor_id: TypeConstructorId.t;
  param_ids: int list;
  labels: TypeDecl.label list;
}

type module_binding = { name: string; scope: t }
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
        ({ binding with scope = f scope }) :: rest
      else binding :: upsert_module rest ~name ~f

let rec add_export = fun scope (path, scheme) ->
  match SurfacePath.uncons path with
  | None -> scope
  | Some (name, tail) ->
      if SurfacePath.is_empty tail then
        {
          scope with
          values = scope.values @ [
            name, scheme;
          ]
        }
      else { scope with modules = upsert_module scope.modules ~name ~f:(
        fun child -> add_export child (tail, scheme)
      ) }

let rec add_type_decl = fun scope (type_decl: FileSummary.type_decl) ->
  match SurfacePath.uncons type_decl.scope_path with
  | None -> { scope with type_decls = scope.type_decls @ [ type_decl ] }
  | Some (name, tail) -> { scope with modules = upsert_module scope.modules ~name ~f:(
    fun child -> add_type_decl child ({ type_decl with scope_path = tail })
  ) }

let of_module_surface = fun ~exports ~type_decls ->
  let scope = List.fold_left add_export empty exports in List.fold_left add_type_decl scope type_decls

let rec exports = fun scope ->
  let local_exports =
    scope.values |> List.map
      (
        fun (name, scheme) -> (SurfacePath.of_name name, scheme)
      )
  in
  let nested_exports =
    scope.modules |> List.concat_map
      (
        fun { name; scope } ->
          exports scope |> List.map
            (
              fun (path, scheme) -> (SurfacePath.prepend_name name path, scheme)
            )
      )
  in
  local_exports @ nested_exports

let rec type_decls = fun scope ->
  let nested_type_decls =
    scope.modules |> List.concat_map
      (
        fun { name; scope } ->
          type_decls scope |> List.map
            (
              fun (type_decl: FileSummary.type_decl) -> { type_decl with scope_path = SurfacePath.prepend_name name type_decl.scope_path }
            )
      )
  in
  scope.type_decls @ nested_type_decls

let rec lookup_module = fun scope path ->
  if SurfacePath.is_empty path then
    Some scope
  else
    match SurfacePath.uncons path with
    | None -> Some scope
    | Some (name, tail) ->
        scope.modules |> List.find_map
          (
            fun binding ->
              if String.equal binding.name name then
                lookup_module binding.scope tail
              else None
          )

let lookup_value = fun scope path ->
  exports scope |> List.find_map
    (
      fun (candidate_path, scheme) ->
        if SurfacePath.equal candidate_path path then
          Some scheme
        else None
    )

let lookup_type_decl = fun scope path ->
  type_decls scope |> List.find_map
    (
      fun (type_decl: FileSummary.type_decl) ->
        let candidate_path = SurfacePath.append_name type_decl.scope_path type_decl.declaration.type_name in
        if SurfacePath.equal candidate_path path then
          Some type_decl
        else None
    )

let lookup_label_name = fun label_name ->
  match String.last_index label_name '.' with
  | Some index -> String.sub label_name (index + 1) (String.length label_name - index - 1)
  | None -> label_name

let local_constructor_entries = fun scope ->
  scope.type_decls |> List.concat_map
    (
      fun (type_decl: FileSummary.type_decl) ->
        let owner_path = SurfacePath.append_name type_decl.scope_path type_decl.declaration.type_name in
        type_decl.declaration.constructors |> List.map
          (
            fun (constructor: TypeDecl.constructor) -> { owner_path; owner_type_constructor_id = type_decl.declaration.type_constructor_id; constructor }
          )
    )

let qualify_constructor_entry = fun ~root (entry: constructor_entry) ->
  if SurfacePath.is_empty root then
    entry
  else { entry with owner_path = SurfacePath.append_path root entry.owner_path }

let lookup_constructors = fun scope path ->
  match SurfacePath.split_last path with
  | None -> (
    match SurfacePath.bare_name path with
    | Some name ->
        local_constructor_entries scope |> List.filter
          (
            fun entry -> String.equal entry.constructor.name name
          )
    | None -> []
  )
  | Some (module_path, name) ->
      lookup_module scope module_path |> Option.map
        (
          fun module_scope ->
            local_constructor_entries module_scope |> List.filter
              (
                fun entry -> String.equal entry.constructor.name name
              ) |> List.map (qualify_constructor_entry ~root:module_path)
        ) |> Option.unwrap_or ~default:[]

let lookup_owned_constructor = fun scope path owner_type_constructor_id ->
  lookup_constructors scope path |> List.find_map
    (
      fun (entry: constructor_entry) ->
        if TypeConstructorId.equal owner_type_constructor_id entry.owner_type_constructor_id then
          Some entry
        else None
    )

let local_record_decls = fun scope ->
  scope.type_decls |> List.filter_map
    (
      fun (type_decl: FileSummary.type_decl) ->
        match type_decl.declaration.labels with
        | [] -> None
        | labels ->
            Some {
              owner_path = SurfacePath.append_name type_decl.scope_path type_decl.declaration.type_name;
              owner_type_constructor_id = type_decl.declaration.type_constructor_id;
              param_ids = type_decl.declaration.param_ids;
              labels
            }
    )

let lookup_record_decls = fun scope label_name ->
  let lookup_name = lookup_label_name label_name in
  local_record_decls scope |> List.filter
    (
      fun (record_decl: record_decl) ->
        record_decl.labels |> List.exists
          (
            fun (label: TypeDecl.label) -> String.equal (lookup_label_name label.name) lookup_name
          )
    )

let lookup_record_decl_by_owner = fun scope owner_type_constructor_id ->
  local_record_decls scope |> List.find_map
    (
      fun (record_decl: record_decl) ->
        if TypeConstructorId.equal owner_type_constructor_id record_decl.owner_type_constructor_id then
          Some record_decl
        else None
    )
