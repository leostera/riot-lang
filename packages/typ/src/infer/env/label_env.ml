open Std
open Model

module Name_map = Collections.Map.Make (String)

type record_decl = {
  owner_path: IdentPath.t;
  owner_type_constructor_id: TypeConstructorId.t;
  param_ids: int list;
  labels: TypeDecl.label list;
}

type current = record_decl list Name_map.t
type components = record_decl list Name_map.t

type layer =
  | Nothing
  | Open of {
      root: IdentPath.t;
      components: components;
      next: t;
    }

and t = {
  record_decls: record_decl list;
  current: current;
  layer: layer;
}

let empty = { record_decls = []; current = Name_map.empty; layer = Nothing }

let record_decl_of_type_decl = fun (type_decl: FileSummary.type_decl) ->
  match type_decl.declaration.labels with
  | [] ->
      None
  | labels ->
      Some {
        owner_path = IdentPath.append_name type_decl.scope_path type_decl.declaration.type_name;
        owner_type_constructor_id = type_decl.declaration.type_constructor_id;
        param_ids = type_decl.declaration.param_ids;
        labels;
      }

let prepend_record_decl = fun index record_decl ->
  record_decl.labels
  |> List.fold_left
    (fun acc (label: TypeDecl.label) ->
      let existing = Name_map.find_opt label.name acc |> Option.unwrap_or ~default:[] in
      Name_map.add label.name (record_decl :: existing) acc)
    index

let index_of_record_decls = fun record_decls ->
  record_decls |> List.rev |> List.fold_left prepend_record_decl Name_map.empty

let qualify_record_decl = fun root record_decl ->
  { record_decl with owner_path = IdentPath.append_path root record_decl.owner_path }

let of_type_decls = fun type_decls ->
  let record_decls = type_decls |> List.filter_map record_decl_of_type_decl in
  { record_decls; current = index_of_record_decls record_decls; layer = Nothing }

let record_decls = fun env -> env.record_decls

let local_only = fun env -> { env with layer = Nothing }

let visible_components_of_record_decls = fun record_decls ->
  record_decls |> List.rev |> List.fold_left prepend_record_decl Name_map.empty

let add_open = fun ~root opened env ->
  {
    record_decls = env.record_decls;
    current = Name_map.empty;
    layer = Open {
      root;
      components = visible_components_of_record_decls opened.record_decls;
      next = env;
    };
  }

let merge_current = fun introduced existing ->
  Name_map.fold
    (fun label introduced_record_decls acc ->
      let current = Name_map.find_opt label acc |> Option.unwrap_or ~default:[] in
      Name_map.add label (introduced_record_decls @ current) acc)
    introduced
    existing

let bind = fun env introduced ->
  if List.is_empty introduced.record_decls then
    env
  else if List.is_empty env.record_decls && env.layer = Nothing then
    introduced
  else
    {
      record_decls = introduced.record_decls @ env.record_decls;
      current = merge_current introduced.current env.current;
      layer = env.layer;
    }

let rec lookup_all_label = fun env label_name ->
  let current = Name_map.find_opt label_name env.current |> Option.unwrap_or ~default:[] in
  match env.layer with
  | Nothing ->
      current
  | Open { root; components; next } ->
      let opened =
        Name_map.find_opt label_name components
        |> Option.unwrap_or ~default:[]
        |> List.map (qualify_record_decl root)
      in
      current @ opened @ lookup_all_label next label_name

let lookup_all = fun env label_name -> lookup_all_label env label_name

let visible_record_decls = fun env ->
  let rec gather seen acc env =
    let acc =
      env.record_decls |> List.fold_left
        (fun acc record_decl ->
          let owner_id = TypeConstructorId.to_int record_decl.owner_type_constructor_id in
          if Collections.HashSet.contains seen owner_id then
            acc
          else
            let () = Collections.HashSet.insert seen owner_id |> ignore in
            record_decl :: acc)
        acc
    in
    match env.layer with
    | Nothing ->
        acc
    | Open { root; components; next } ->
        let acc =
          Name_map.fold
            (fun _ record_decls acc ->
              record_decls |> List.fold_left
                (fun acc record_decl ->
                  let qualified = qualify_record_decl root record_decl in
                  let owner_id = TypeConstructorId.to_int qualified.owner_type_constructor_id in
                  if Collections.HashSet.contains seen owner_id then
                    acc
                  else
                    let () = Collections.HashSet.insert seen owner_id |> ignore in
                    qualified :: acc)
                acc)
            components
            acc
        in
        gather seen acc next
  in
  gather (Collections.HashSet.create ()) [] env |> List.rev
