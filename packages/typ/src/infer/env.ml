open Std
open Std.Collections
open Ast

module IdentMap = Map.Make (Model.Surface_path)

type binding = {
  scheme: TypeScheme.t;
  ordinal: int;
}

type type_binding = { declaration: type_declaration; ordinal: int }

type module_binding = { summary: module_summary; ordinal: int }

and module_summary = {
  values: binding IdentMap.t;
  constructors: binding IdentMap.t;
  types: type_binding IdentMap.t;
  modules: module_binding IdentMap.t;
}

type value_scope = {
  values: binding IdentMap.t;
}

type value_scopes =
  | ValueRoot of value_scope
  | ValueScope of { root: value_scope; current: value_scope; parent: value_scopes }

type module_frame = {
  name: ident option;
  values: value_scopes;
  constructors: binding IdentMap.t;
  types: type_binding IdentMap.t;
  modules: module_binding IdentMap.t;
}

type module_scopes =
  | ModuleRoot of module_frame
  | ModuleScope of { root: module_frame; current: module_frame; parent: module_scopes }

type t = { modules: module_scopes; next_ordinal: int }

let empty_value_scope = { values = IdentMap.empty }

let empty_module_frame ?name () = {
  name;
  values = ValueRoot empty_value_scope;
  constructors = IdentMap.empty;
  types = IdentMap.empty;
  modules = IdentMap.empty;
}

let create () = { modules = ModuleRoot (empty_module_frame ()); next_ordinal = 0 }

let root_value_scope = function
  | ValueRoot scope -> scope
  | ValueScope { root; _ } -> root

let map_current_value_scope scopes ~fn =
  match scopes with
  | ValueRoot current -> ValueRoot (fn current)
  | ValueScope ({ current; _ } as scope) -> ValueScope { scope with current = fn current }

let push_value_scope scopes =
  let root = root_value_scope scopes in
  let parent = scopes in
  ValueScope { root; current = empty_value_scope; parent }

let pop_value_scope = function
  | ValueRoot _ as scopes -> scopes
  | ValueScope { parent; _ } -> parent

let rec get_value_in_scopes scopes ~name =
  match scopes with
  | ValueRoot scope ->
      Option.map (IdentMap.get scope.values ~key:name) ~fn:(fun binding -> binding.scheme)
  | ValueScope { current; parent; _ } -> (
      match IdentMap.get current.values ~key:name with
      | Some binding -> Some binding.scheme
      | None -> get_value_in_scopes parent ~name
    )

let root_module = function
  | ModuleRoot frame -> frame
  | ModuleScope { root; _ } -> root

let current_module = function
  | ModuleRoot frame -> frame
  | ModuleScope { current; _ } -> current

let map_current_module modules ~fn =
  match modules with
  | ModuleRoot current -> ModuleRoot (fn current)
  | ModuleScope ({ current; _ } as scope) -> ModuleScope { scope with current = fn current }

let push_scope t =
  let modules =
    map_current_module
      t.modules
      ~fn:(fun current -> { current with values = push_value_scope current.values })
  in
  { t with modules }

let pop_scope t =
  let modules =
    map_current_module
      t.modules
      ~fn:(fun current -> { current with values = pop_value_scope current.values })
  in
  { t with modules }

let push_module t ~name =
  let root = root_module t.modules in
  let parent = t.modules in
  let current = empty_module_frame ~name () in
  { t with modules = ModuleScope { root; current; parent } }

let add_module_to_current modules ~name ~summary ~ordinal =
  let binding = { summary; ordinal } in
  map_current_module
    modules
    ~fn:(fun current -> {
      current with
      modules = IdentMap.insert current.modules ~key:name ~value:binding;
    })

let summary_of_frame frame = {
  values = (root_value_scope frame.values).values;
  constructors = frame.constructors;
  types = frame.types;
  modules = frame.modules;
}

let pop_module t =
  match t.modules with
  | ModuleRoot _ -> t
  | ModuleScope { current; parent; _ } -> (
      match current.name with
      | None -> { t with modules = parent }
      | Some name ->
          let summary = summary_of_frame current in
          {
            modules = add_module_to_current parent ~name ~summary ~ordinal:t.next_ordinal;
            next_ordinal = t.next_ordinal + 1;
          }
    )

let add_value t ~name ~scheme =
  let binding = { scheme; ordinal = t.next_ordinal } in
  let modules =
    map_current_module
      t.modules
      ~fn:(fun current -> {
        current with
        values = map_current_value_scope
          current.values
          ~fn:(fun scope -> { values = IdentMap.insert scope.values ~key:name ~value:binding });
      })
  in
  { modules; next_ordinal = t.next_ordinal + 1 }

let rec get_value_in_modules modules ~name =
  match get_value_in_scopes (current_module modules).values ~name with
  | Some _ as scheme -> scheme
  | None -> (
      match modules with
      | ModuleRoot _ -> None
      | ModuleScope { parent; _ } -> get_value_in_modules parent ~name
    )

let get_value t ~name = get_value_in_modules t.modules ~name

let has_value t ~name = Option.is_some (get_value t ~name)

let add_constructor t ~name ~scheme =
  let binding = { scheme; ordinal = t.next_ordinal } in
  let modules =
    map_current_module
      t.modules
      ~fn:(fun current -> {
        current with
        constructors = IdentMap.insert current.constructors ~key:name ~value:binding;
      })
  in
  { modules; next_ordinal = t.next_ordinal + 1 }

let rec get_constructor_in_modules modules ~name =
  match IdentMap.get (current_module modules).constructors ~key:name with
  | Some binding -> Some binding.scheme
  | None -> (
      match modules with
      | ModuleRoot _ -> None
      | ModuleScope { parent; _ } -> get_constructor_in_modules parent ~name
    )

let get_constructor t ~name = get_constructor_in_modules t.modules ~name

let has_constructor t ~name = Option.is_some (get_constructor t ~name)

let add_type t ~name ~declaration =
  let binding = { declaration; ordinal = t.next_ordinal } in
  let modules =
    map_current_module
      t.modules
      ~fn:(fun current -> {
        current with
        types = IdentMap.insert current.types ~key:name ~value:binding;
      })
  in
  { modules; next_ordinal = t.next_ordinal + 1 }

let rec get_type_in_modules modules ~name =
  match IdentMap.get (current_module modules).types ~key:name with
  | Some binding -> Some binding.declaration
  | None -> (
      match modules with
      | ModuleRoot _ -> None
      | ModuleScope { parent; _ } -> get_type_in_modules parent ~name
    )

let get_type t ~name = get_type_in_modules t.modules ~name

let has_type t ~name = Option.is_some (get_type t ~name)

let rec get_module_in_modules modules ~name =
  match IdentMap.get (current_module modules).modules ~key:name with
  | Some binding -> Some binding.summary
  | None -> (
      match modules with
      | ModuleRoot _ -> None
      | ModuleScope { parent; _ } -> get_module_in_modules parent ~name
    )

let get_module t ~name = get_module_in_modules t.modules ~name

let has_module t ~name = Option.is_some (get_module t ~name)

let module_get_value (summary: module_summary) ~name =
  Option.map (IdentMap.get summary.values ~key:name) ~fn:(fun binding -> binding.scheme)

let module_has_value summary ~name = Option.is_some (module_get_value summary ~name)

let module_get_constructor (summary: module_summary) ~name =
  Option.map (IdentMap.get summary.constructors ~key:name) ~fn:(fun binding -> binding.scheme)

let module_has_constructor summary ~name = Option.is_some (module_get_constructor summary ~name)

let module_get_type (summary: module_summary) ~name =
  Option.map (IdentMap.get summary.types ~key:name) ~fn:(fun binding -> binding.declaration)

let module_has_type summary ~name = Option.is_some (module_get_type summary ~name)

let module_get_module (summary: module_summary) ~name =
  Option.map (IdentMap.get summary.modules ~key:name) ~fn:(fun binding -> binding.summary)

let module_has_module summary ~name = Option.is_some (module_get_module summary ~name)

module ExportIter = struct
  type state = (ident * TypeScheme.t) list

  type item = ident * TypeScheme.t

  let next = function
    | [] -> (None, [])
    | item :: rest -> (Some item, rest)

  let size = List.length
end

let exports t =
  (root_value_scope (root_module t.modules).values).values
  |> IdentMap.to_list
  |> List.sort
    ~compare:(fun (_, (left: binding)) (_, (right: binding)) ->
      Int.compare
        left.ordinal
        right.ordinal)
  |> List.map ~fn:(fun (name, (binding: binding)) -> (name, binding.scheme))
  |> Iter.Iterator.make (module ExportIter)

module TypeExportIter = struct
  type state = (ident * type_declaration) list

  type item = ident * type_declaration

  let next = function
    | [] -> (None, [])
    | item :: rest -> (Some item, rest)

  let size = List.length
end

let exported_types t =
  (root_module t.modules).types
  |> IdentMap.to_list
  |> List.sort
    ~compare:(fun (_, (left: type_binding)) (_, (right: type_binding)) ->
      Int.compare
        left.ordinal
        right.ordinal)
  |> List.map ~fn:(fun (name, (binding: type_binding)) -> (name, binding.declaration))
  |> Iter.Iterator.make (module TypeExportIter)
