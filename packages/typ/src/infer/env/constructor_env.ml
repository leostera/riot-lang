open Std
open Model

module Name_map = Collections.Map.Make (String)

module Owner_map = Collections.Map.Make (struct
  type t = TypeConstructorId.t

  let compare = TypeConstructorId.compare
end)

type entry = {
  owner_path: IdentPath.t;
  owner_type_constructor_id: TypeConstructorId.t;
  constructor: TypeDecl.constructor;
}

type current = entry list Name_map.t

type owner_index = entry Name_map.t Owner_map.t

type components = {
  by_name: entry list Name_map.t;
  by_owner: owner_index;
}

type layer =
  | Nothing
  | Open of { root: IdentPath.t; components: components; next: t }
  | Map of { map_entry: entry -> entry; next: t }

and t = {
  current: current;
  by_owner: owner_index;
  layer: layer;
}

let empty = { current = Name_map.empty; by_owner = Owner_map.empty; layer = Nothing }

let is_empty = fun env ->
  Name_map.is_empty env.current && Owner_map.is_empty env.by_owner && match env.layer with
  | Nothing -> true
  | Open _
  | Map _ -> false

let name = fun entry -> entry.constructor.name

let constructor_id = fun entry -> entry.constructor.constructor_id

let owner_path = fun entry -> entry.owner_path

let owner_type_constructor_id = fun entry -> entry.owner_type_constructor_id

let scheme = fun entry -> entry.constructor.scheme

let inline_record_labels = fun entry -> entry.constructor.inline_record_labels

let prepend_entry = fun index entry ->
  let existing = Name_map.find_opt (name entry) index |> Option.unwrap_or ~default:[] in
  Name_map.add (name entry) (entry :: existing) index

let current_of_entries = fun entries ->
  entries |> List.rev |> List.fold_left prepend_entry Name_map.empty

let add_owner_entry = fun index entry ->
  let owner_entries = Owner_map.find_opt entry.owner_type_constructor_id index
  |> Option.unwrap_or ~default:Name_map.empty in
  let updated = Name_map.add (name entry) entry owner_entries in
  Owner_map.add entry.owner_type_constructor_id updated index

let owner_index_of_entries = fun entries -> entries |> List.fold_left add_owner_entry Owner_map.empty

let current_visible_components = fun env -> { by_name = env.current; by_owner = env.by_owner }

let merge_visible_by_name = fun dominant rest ->
  Name_map.fold
    (fun entry_name rest_entries acc ->
      let current = Name_map.find_opt entry_name acc |> Option.unwrap_or ~default:[] in
      Name_map.add entry_name (current @ rest_entries) acc)
    rest
    dominant

let merge_visible_by_owner = fun dominant rest ->
  Owner_map.fold
    (fun owner_id rest_entries acc ->
      let current = Owner_map.find_opt owner_id acc |> Option.unwrap_or ~default:Name_map.empty in
      let merged =
        Name_map.fold
          (fun entry_name entry acc ->
            if Name_map.mem entry_name acc then
              acc
            else
              Name_map.add entry_name entry acc)
          rest_entries
          current
      in
      Owner_map.add owner_id merged acc)
    rest
    dominant

let merge_visible_components = fun dominant rest ->
  {
    by_name = merge_visible_by_name dominant.by_name rest.by_name;
    by_owner = merge_visible_by_owner dominant.by_owner rest.by_owner
  }

let map_components = fun map_entry components ->
  {
    by_name = Name_map.map (List.map map_entry) components.by_name;
    by_owner = Owner_map.map (Name_map.map map_entry) components.by_owner
  }

let qualify_entry = fun root entry ->
  { entry with owner_path = IdentPath.append_path root entry.owner_path }

let of_type_decls = fun type_decls ->
  let entries =
    type_decls
    |> List.concat_map
      (fun (type_decl: FileSummary.type_decl) ->
        let owner_path = IdentPath.append_name type_decl.scope_path type_decl.declaration.type_name in
        type_decl.declaration.constructors
        |> List.map
          (fun constructor ->
            {
              owner_path;
              owner_type_constructor_id = type_decl.declaration.type_constructor_id;
              constructor
            }))
  in
  {
    current = current_of_entries entries;
    by_owner = owner_index_of_entries entries;
    layer = Nothing
  }

let singleton = fun ~owner_path ~owner_type_constructor_id ~constructor ->
  let entry = { owner_path; owner_type_constructor_id; constructor } in
  {
    current = current_of_entries [ entry ];
    by_owner = owner_index_of_entries [ entry ];
    layer = Nothing
  }

let current_entries = fun current -> Name_map.bindings current |> List.concat_map snd

let rec visible_components = fun env ->
  let current = current_visible_components env in
  match env.layer with
  | Nothing -> current
  | Open { components; next; _ } -> current
  |> merge_visible_components components
  |> merge_visible_components (visible_components next)
  | Map { map_entry; next } ->
      current |> merge_visible_components
        (visible_components next |> map_components map_entry)

let entries =
  let rec loop acc env =
    let acc = List.rev_append (current_entries env.current) acc in
    match env.layer with
    | Nothing -> acc
    | Open { next; _ } -> loop acc next
    | Map { map_entry; next } -> loop acc next |> List.map map_entry
  in
  fun env -> loop [] env |> List.rev

let local_only = fun env ->
  let entries = entries env in
  {
    current = current_of_entries entries;
    by_owner = owner_index_of_entries entries;
    layer = Nothing
  }

let map = fun map_entry env ->
  if is_empty env then
    env
  else
    { current = Name_map.empty; by_owner = Owner_map.empty; layer = Map { map_entry; next = env } }

let add_open = fun ~root opened env ->
  {
    current = Name_map.empty;
    by_owner = Owner_map.empty;
    layer = Open { root; components = visible_components opened; next = env }
  }

let merge_current = fun introduced existing ->
  Name_map.fold
    (fun entry_name introduced_entries acc ->
      let current = Name_map.find_opt entry_name acc |> Option.unwrap_or ~default:[] in
      Name_map.add entry_name (introduced_entries @ current) acc)
    introduced
    existing

let merge_owner_index = fun introduced existing ->
  Owner_map.fold
    (fun owner_id introduced_entries acc ->
      let current = Owner_map.find_opt owner_id acc |> Option.unwrap_or ~default:Name_map.empty in
      let merged = Name_map.fold Name_map.add introduced_entries current in
      Owner_map.add owner_id merged acc)
    introduced
    existing

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

let rec lookup_all_name = fun env entry_name ->
  let current = Name_map.find_opt entry_name env.current |> Option.unwrap_or ~default:[] in
  match env.layer with
  | Nothing ->
      current
  | Open { root; components; next } ->
      let opened = Name_map.find_opt entry_name components.by_name
      |> Option.unwrap_or ~default:[]
      |> List.map (qualify_entry root) in
      current @ opened @ lookup_all_name next entry_name
  | Map { map_entry; next } ->
      current @ (lookup_all_name next entry_name |> List.map map_entry)

let lookup_all = fun env entry_name -> lookup_all_name env entry_name

let rec lookup_owned = fun env entry_name owner_type_constructor_id ->
  let lookup_local owner_index =
    Option.and_then (Owner_map.find_opt owner_type_constructor_id owner_index)
      (fun entries ->
        Name_map.find_opt entry_name entries)
  in
  match lookup_local env.by_owner with
  | Some entry -> Some entry
  | None -> (
      match env.layer with
      | Nothing ->
          None
      | Open { root; components; next } -> (
          match lookup_local components.by_owner with
          | Some entry -> Some (qualify_entry root entry)
          | None -> lookup_owned next entry_name owner_type_constructor_id
        )
      | Map { map_entry; next } ->
          lookup_owned next entry_name owner_type_constructor_id |> Option.map map_entry
    )
