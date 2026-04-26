open Global
open Collections

type path_component =
  | Key of string
  | Index of int

type path = path_component list

type 'value kind =
  | Added of 'value
  | Removed of 'value
  | Changed of 'value * 'value

type 'value change = {
  path: path;
  kind: 'value kind;
}

type 'value diff =
  | Equal
  | Diff of 'value change list

let has_changes = fun changes -> List.length changes > 0

let additions = fun changes ->
  List.filter
    ~fn:(fun { kind; _ } ->
      match kind with
      | Added _ -> true
      | _ -> false)
    changes

let removals = fun changes ->
  List.filter
    ~fn:(fun { kind; _ } ->
      match kind with
      | Removed _ -> true
      | _ -> false)
    changes

let changes = fun changes ->
  List.filter
    ~fn:(fun { kind; _ } ->
      match kind with
      | Changed _ -> true
      | _ -> false)
    changes

let at_path = fun target_path changes ->
  List.filter
    ~fn:(fun { path; _ } -> path = target_path)
    changes
