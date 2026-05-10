open Std

module Names = struct
  type t = string list

  let empty = []

  let singleton = fun name -> [ name ]

  let union = fun left right ->
    List.unique
      (List.sort (left @ right) ~compare:String.compare)
      ~compare:String.compare

  let elements = fun names -> names
end

type node =
  | Node of Names.t * t

and t = (string * node) list

let empty = []

let open_fallback_key = "\000open_fallback"

let bound = Node (Names.empty, [])

let singleton_name = Names.singleton

let make_leaf = fun name -> Node (Names.singleton name, [])

let make_node = fun map -> Node (Names.empty, map)

let rec remove = fun name ->
  fun __tmp1 ->
    match __tmp1 with
    | [] -> []
    | (key, _) :: rest when String.equal key name -> rest
    | entry :: rest -> entry :: remove name rest

let add = fun name node env -> (name, node) :: remove name env

let merge = fun left right ->
  List.fold_left
    right
    ~init:left
    ~fn:(fun env (name, node) ->
      add name node env)

let rec rebind = fun free_names ->
  fun (Node (_, children)) ->
    Node (free_names, List.map children ~fn:(fun (name, child) -> (name, rebind free_names child)))

let rebind_exports = fun free_names exports ->
  List.map
    exports
    ~fn:(fun (name, node) -> (name, rebind free_names node))

let rec add_path = fun env ~path ~free_names ->
  match path with
  | [] -> env
  | segment :: rest ->
      let existing =
        match List.find env ~fn:(fun (name, _) -> String.equal name segment) with
        | Some (_, node) -> node
        | None -> Node (Names.empty, [])
      in
      let Node (free, children) = existing in
      let updated_children =
        match rest with
        | [] -> children
        | _ -> add_path children ~path:rest ~free_names
      in
      add segment (Node (Names.union free free_names, updated_children)) env

let rec add_binding = fun env ~path ~free_names ~exports ->
  match path with
  | [] -> env
  | [ segment ] ->
      let existing =
        match List.find env ~fn:(fun (name, _) -> String.equal name segment) with
        | Some (_, node) -> node
        | None -> Node (Names.empty, [])
      in
      let Node (free, children) = existing in
      let merged_children = merge children (rebind_exports free_names exports) in
      add segment (Node (Names.union free free_names, merged_children)) env
  | segment :: rest ->
      let existing =
        match List.find env ~fn:(fun (name, _) -> String.equal name segment) with
        | Some (_, node) -> node
        | None -> Node (Names.empty, [])
      in
      let Node (free, children) = existing in
      let updated_children = add_binding children ~path:rest ~free_names ~exports in
      add segment (Node (free, updated_children)) env

let rec add_scoped_binding = fun env ~path ~free_names ~exports ->
  match path with
  | [] -> env
  | [ segment ] ->
      let existing =
        match List.find env ~fn:(fun (name, _) -> String.equal name segment) with
        | Some (_, node) -> node
        | None -> Node (Names.empty, [])
      in
      let Node (free, children) = existing in
      let merged_children = merge children exports in
      add segment (Node (Names.union free free_names, merged_children)) env
  | segment :: rest ->
      let existing =
        match List.find env ~fn:(fun (name, _) -> String.equal name segment) with
        | Some (_, node) -> node
        | None -> Node (Names.empty, [])
      in
      let Node (free, children) = existing in
      let updated_children = add_scoped_binding children ~path:rest ~free_names ~exports in
      add segment (Node (free, updated_children)) env

let top_free = fun (Node (free, _)) -> free

let children = fun (Node (_, children)) -> children

let rec collect_free = fun (Node (free, children)) ->
  List.fold_left
    children
    ~init:free
    ~fn:(fun acc (_, child) -> Names.union acc (collect_free child))

let merge_children = fun env node -> merge env (children node)

let find = fun name env ->
  List.find env ~fn:(fun (key, _) -> String.equal key name)
  |> Option.map ~fn:(fun (_, value) -> value)

let open_fallback_free = fun env ->
  match find open_fallback_key env with
  | Some (Node (free, _)) -> Some free
  | None -> None

let add_open_fallback = fun env ~free_names ->
  let existing =
    match open_fallback_free env with
    | Some names -> names
    | None -> Names.empty
  in
  add open_fallback_key (Node (Names.union existing free_names, [])) env

let has_children = fun (Node (_, children)) -> not (List.is_empty children)

let rec lookup_free = fun ~use_open_fallback segments env ->
  match segments with
  | [] -> None
  | segment :: rest ->
      match find segment env with
      | None ->
          if use_open_fallback then
            open_fallback_free env
          else
            None
      | Some (Node (free, children)) -> (
          match rest with
          | [] -> Some free
          | _ -> (
              match lookup_free ~use_open_fallback rest children with
              | Some child_free -> Some (Names.union free child_free)
              | None -> Some free
            )
        )

let rec lookup_map = fun segments env ->
  match segments with
  | [] -> None
  | [ segment ] -> find segment env
  | segment :: rest ->
      match find segment env with
      | None -> None
      | Some (Node (_, children)) -> lookup_map rest children

let open_path = fun env ~path ->
  match lookup_map path env with
  | Some node -> merge_children env node
  | None -> env
