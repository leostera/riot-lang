open Std
open Model

module Name_map = Collections.Map.Make (String)

module Owner_map = Collections.Map.Make (struct
  type t = TypeConstructorId.t

  let compare = TypeConstructorId.compare
end)

type record_decl = {
  owner_path: IdentPath.t;
  owner_type_constructor_id: TypeConstructorId.t;
  param_ids: int list;
  labels: TypeDecl.label list;
  label_index: TypeDecl.label Name_map.t;
}

type current = record_decl list Name_map.t

type owner_index = record_decl Owner_map.t

type components = {
  by_name: record_decl list Name_map.t;
  by_owner: owner_index;
}

type layer =
  | Nothing
  | Open of { root: IdentPath.t; components: components; next: t }
  | Map of { map_record_decl: record_decl -> record_decl; next: t }

and t = {
  current: current;
  by_owner: owner_index;
  layer: layer;
}

let empty = { current = Name_map.empty; by_owner = Owner_map.empty; layer = Nothing }

let lookup_name = fun label_name ->
  match String.rindex_opt label_name '.' with
  | Some index -> String.sub label_name (index + 1) (String.length label_name - index - 1)
  | None -> label_name

let owner_path = fun (record_decl: record_decl) -> record_decl.owner_path

let owner_type_constructor_id = fun (record_decl: record_decl) -> record_decl.owner_type_constructor_id

let param_ids = fun (record_decl: record_decl) -> record_decl.param_ids

let labels = fun (record_decl: record_decl) -> record_decl.labels

let field = fun (record_decl: record_decl) label_name ->
  Name_map.find_opt (lookup_name label_name) record_decl.label_index

let field_names = fun (record_decl: record_decl) ->
  Name_map.bindings record_decl.label_index |> List.map fst

let matches_fields = fun (record_decl: record_decl) field_names ->
  List.for_all (fun field_name -> Option.is_some (field record_decl field_name)) field_names

let is_empty = fun env ->
  Name_map.is_empty env.current && Owner_map.is_empty env.by_owner && match env.layer with
  | Nothing -> true
  | Open _
  | Map _ -> false

let record_decl_of_type_decl = fun (type_decl: FileSummary.type_decl) ->
  match type_decl.declaration.labels with
  | [] -> None
  | labels ->
      let label_index =
        labels
        |> List.fold_left
          (fun acc (label: TypeDecl.label) ->
            Name_map.add label.name label acc)
          Name_map.empty
      in
      Some {
        owner_path = IdentPath.append_name type_decl.scope_path type_decl.declaration.type_name;
        owner_type_constructor_id = type_decl.declaration.type_constructor_id;
        param_ids = type_decl.declaration.param_ids;
        labels;
        label_index;
      }

let prepend_record_decl = fun index record_decl ->
  record_decl.labels |> List.fold_left
    (fun acc (label: TypeDecl.label) ->
      let existing = Name_map.find_opt label.name acc |> Option.unwrap_or ~default:[] in
      Name_map.add label.name (record_decl :: existing) acc)
    index

let current_of_record_decls = fun record_decls ->
  record_decls |> List.rev |> List.fold_left prepend_record_decl Name_map.empty

let owner_index_of_record_decls = fun record_decls ->
  record_decls |> List.fold_left
    (fun acc record_decl ->
      Owner_map.add record_decl.owner_type_constructor_id record_decl acc)
    Owner_map.empty

let current_visible_components = fun env ->
  {
    by_name = env.current;
    by_owner = env.by_owner;
  }

let merge_visible_by_name = fun dominant rest ->
  Name_map.fold
    (fun label record_decls acc ->
      let current = Name_map.find_opt label acc |> Option.unwrap_or ~default:[] in
      Name_map.add label (current @ record_decls) acc)
    rest
    dominant

let merge_visible_by_owner = fun dominant rest ->
  Owner_map.fold
    (fun owner_id record_decl acc ->
      if Owner_map.mem owner_id acc then
        acc
      else
        Owner_map.add owner_id record_decl acc)
    rest
    dominant

let merge_visible_components = fun dominant rest ->
  {
    by_name = merge_visible_by_name dominant.by_name rest.by_name;
    by_owner = merge_visible_by_owner dominant.by_owner rest.by_owner;
  }

let map_components = fun map_record_decl components ->
  {
    by_name = Name_map.map (List.map map_record_decl) components.by_name;
    by_owner = Owner_map.map map_record_decl components.by_owner;
  }

let qualify_record_decl = fun root record_decl ->
  { record_decl with owner_path = IdentPath.append_path root record_decl.owner_path }

let of_type_decls = fun type_decls ->
  let record_decls = type_decls |> List.filter_map record_decl_of_type_decl in
  {
    current = current_of_record_decls record_decls;
    by_owner = owner_index_of_record_decls record_decls;
    layer = Nothing
  }

let current_record_decls = fun current ->
  let dedupe = Collections.HashSet.create () in
  Name_map.bindings current |> List.concat_map snd |> List.filter
    (fun record_decl ->
      let owner_id = TypeConstructorId.to_int record_decl.owner_type_constructor_id in
      if Collections.HashSet.contains dedupe owner_id then
        false
      else
        let () = Collections.HashSet.insert dedupe owner_id |> ignore in
        true)

let rec visible_components = fun env ->
  let current = current_visible_components env in
  match env.layer with
  | Nothing -> current
  | Open { components; next; _ } ->
      current
      |> merge_visible_components components
      |> merge_visible_components (visible_components next)
  | Map { map_record_decl; next } ->
      current
      |> merge_visible_components (visible_components next |> map_components map_record_decl)

let record_decls =
  let rec loop acc env =
    let acc = List.rev_append (current_record_decls env.current) acc in
    match env.layer with
    | Nothing -> acc
    | Open { next; _ } -> loop acc next
    | Map { map_record_decl; next } -> loop acc next |> List.map map_record_decl
  in
  fun env -> loop [] env |> List.rev

let local_only = fun env ->
  let record_decls = record_decls env in
  {
    current = current_of_record_decls record_decls;
    by_owner = owner_index_of_record_decls record_decls;
    layer = Nothing
  }

let map = fun map_record_decl env ->
  if is_empty env then
    env
  else
    {
      current = Name_map.empty;
      by_owner = Owner_map.empty;
      layer = Map { map_record_decl; next = env }
    }

let add_open = fun ~root opened env ->
  {
    current = Name_map.empty;
    by_owner = Owner_map.empty;
    layer = Open {
      root;
      components = visible_components opened;
      next = env
    }
  }

let merge_current = fun introduced existing ->
  Name_map.fold
    (fun label introduced_record_decls acc ->
      let current = Name_map.find_opt label acc |> Option.unwrap_or ~default:[] in
      Name_map.add label (introduced_record_decls @ current) acc)
    introduced
    existing

let merge_owner_index = fun introduced existing ->
  Owner_map.fold Owner_map.add introduced existing

let bind = fun env introduced ->
  if is_empty introduced then
    env
  else if is_empty env then
    introduced
  else
    {
      current = merge_current introduced.current env.current;
      by_owner = merge_owner_index introduced.by_owner env.by_owner;
      layer = env.layer
    }

let rec lookup_all_label = fun env label_name ->
  let current = Name_map.find_opt label_name env.current |> Option.unwrap_or ~default:[] in
  match env.layer with
  | Nothing ->
      current
  | Open { root; components; next } ->
      let opened = Name_map.find_opt label_name components.by_name
      |> Option.unwrap_or ~default:[]
      |> List.map (qualify_record_decl root) in
      current @ opened @ lookup_all_label next label_name
  | Map { map_record_decl; next } ->
      current @ (lookup_all_label next label_name |> List.map map_record_decl)

let lookup_all = fun env label_name -> lookup_all_label env label_name

let rec lookup_owned = fun env owner_type_constructor_id ->
  match Owner_map.find_opt owner_type_constructor_id env.by_owner with
  | Some record_decl -> Some record_decl
  | None -> (
      match env.layer with
      | Nothing ->
          None
      | Open { root; components; next } -> (
          match Owner_map.find_opt owner_type_constructor_id components.by_owner with
          | Some record_decl -> Some (qualify_record_decl root record_decl)
          | None -> lookup_owned next owner_type_constructor_id
        )
      | Map { map_record_decl; next } ->
          lookup_owned next owner_type_constructor_id |> Option.map map_record_decl
    )

let visible_record_decls = fun env ->
  let seen = Collections.HashSet.create () in
  let add_record_decl acc record_decl =
    let owner_id = TypeConstructorId.to_int record_decl.owner_type_constructor_id in
    if Collections.HashSet.contains seen owner_id then
      acc
    else
      let () = Collections.HashSet.insert seen owner_id |> ignore in
      record_decl :: acc
  in
  let rec gather acc env =
    let acc = current_record_decls env.current |> List.fold_left add_record_decl acc in
    match env.layer with
    | Nothing ->
        acc
    | Open { root; components; next } ->
        let acc =
          Name_map.fold
            (fun _ record_decls acc ->
              record_decls
              |> List.fold_left
                (fun acc record_decl -> add_record_decl acc (qualify_record_decl root record_decl))
                acc)
            components.by_name
            acc
        in
        gather acc next
    | Map { map_record_decl; next } ->
        gather acc next |> List.map map_record_decl
  in
  gather [] env |> List.rev
