open Std
open Model

module Name_map = Collections.Map.Make (String)

type record_decl = {
  owner_path: IdentPath.t;
  owner_type_constructor_id: TypeConstructorId.t;
  param_ids: int list;
  labels: TypeDecl.label list;
}

type t = {
  record_decls: record_decl list;
  by_label: record_decl list Name_map.t;
}

let empty = { record_decls = []; by_label = Name_map.empty }

let record_decl_of_type_decl = fun (type_decl: FileSummary.type_decl) ->
  match type_decl.declaration.labels with
  | [] -> None
  | labels -> Some {
    owner_path = IdentPath.append_name type_decl.scope_path type_decl.declaration.type_name;
    owner_type_constructor_id = type_decl.declaration.type_constructor_id;
    param_ids = type_decl.declaration.param_ids;
    labels
  }

let prepend_record_decl = fun index record_decl ->
  record_decl.labels |> List.fold_left
    (fun acc (label: TypeDecl.label) ->
      let existing = Name_map.find_opt label.name acc |> Option.unwrap_or ~default:[] in
      Name_map.add label.name (record_decl :: existing) acc)
    index

let index_of_record_decls = fun record_decls ->
  record_decls |> List.rev |> List.fold_left prepend_record_decl Name_map.empty

let of_type_decls = fun type_decls ->
  let record_decls = type_decls |> List.filter_map record_decl_of_type_decl in
  { record_decls; by_label = index_of_record_decls record_decls }

let bind = fun env introduced ->
  if List.is_empty introduced.record_decls then
    env
  else if List.is_empty env.record_decls then
    introduced
  else
    let by_label =
      Name_map.fold
        (fun label introduced_record_decls acc ->
          let existing = Name_map.find_opt label acc |> Option.unwrap_or ~default:[] in
          Name_map.add label (introduced_record_decls @ existing) acc)
        introduced.by_label
        env.by_label
    in
    { record_decls = introduced.record_decls @ env.record_decls; by_label }

let record_decls = fun env -> env.record_decls

let lookup_all = fun env label_name ->
  Name_map.find_opt label_name env.by_label |> Option.unwrap_or ~default:[]
