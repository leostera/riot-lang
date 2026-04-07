open Std
open Model

module Name_map = Collections.Map.Make (String)

type entry = {
  owner_path: IdentPath.t;
  owner_type_constructor_id: TypeConstructorId.t;
  constructor: TypeDecl.constructor;
}

type current = entry list Name_map.t

type components = entry list Name_map.t

type layer =
  | Nothing
  | Open of { root: IdentPath.t; components: components; next: t }
  | Map of { map_entry: entry -> entry; next: t }

and t = {
  current: current;
  layer: layer;
}

let empty = { current = Name_map.empty; layer = Nothing }

let is_empty = fun env ->
  Name_map.is_empty env.current
  &&
  match env.layer with
  | Nothing -> true
  | Open _
  | Map _ -> false

let name = fun entry -> entry.constructor.name

let constructor_id = fun entry -> entry.constructor.constructor_id

let owner_path = fun entry -> entry.owner_path

let owner_type_constructor_id = fun entry -> entry.owner_type_constructor_id

let scheme = fun entry -> entry.constructor.scheme

let prepend_entry = fun index entry ->
  let existing = Name_map.find_opt (name entry) index |> Option.unwrap_or ~default:[] in
  Name_map.add (name entry) (entry :: existing) index

let current_of_entries = fun entries ->
  entries |> List.rev |> List.fold_left prepend_entry Name_map.empty

let qualify_entry = fun root entry ->
  { entry with owner_path = IdentPath.append_path root entry.owner_path }

let of_type_decls = fun type_decls ->
  let entries =
    type_decls |> List.concat_map
      (fun (type_decl: FileSummary.type_decl) ->
        let owner_path = IdentPath.append_name type_decl.scope_path type_decl.declaration.type_name in
        type_decl.declaration.constructors |> List.map
          (fun constructor ->
            {
              owner_path;
              owner_type_constructor_id = type_decl.declaration.type_constructor_id;
              constructor;
            }))
  in
  { current = current_of_entries entries; layer = Nothing }

let current_entries = fun current -> Name_map.bindings current |> List.concat_map snd

let entries =
  let rec loop acc env =
    let acc = List.rev_append (current_entries env.current) acc in
    match env.layer with
    | Nothing -> acc
    | Open { next; _ } -> loop acc next
    | Map { map_entry; next } -> loop acc next |> List.map map_entry
  in
  fun env -> loop [] env |> List.rev

let local_only = fun env -> env |> entries |> fun xs -> { current = current_of_entries xs; layer = Nothing }

let visible_components_of_entries = fun entries ->
  entries |> List.rev |> List.fold_left
    (fun acc entry ->
      let existing = Name_map.find_opt (name entry) acc |> Option.unwrap_or ~default:[] in
      Name_map.add (name entry) (entry :: existing) acc)
    Name_map.empty

let map = fun map_entry env ->
  if is_empty env then
    env
  else
    { current = Name_map.empty; layer = Map { map_entry; next = env } }

let add_open = fun ~root opened env ->
  {
    current = Name_map.empty;
    layer = Open { root; components = visible_components_of_entries (entries opened); next = env };
  }

let merge_current = fun introduced existing ->
  Name_map.fold
    (fun entry_name introduced_entries acc ->
      let current = Name_map.find_opt entry_name acc |> Option.unwrap_or ~default:[] in
      Name_map.add entry_name (introduced_entries @ current) acc)
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
      layer = env.layer;
    }

let rec lookup_all_name = fun env entry_name ->
  let current = Name_map.find_opt entry_name env.current |> Option.unwrap_or ~default:[] in
  match env.layer with
  | Nothing -> current
  | Open { root; components; next } ->
      let opened =
        Name_map.find_opt entry_name components
        |> Option.unwrap_or ~default:[]
        |> List.map (qualify_entry root)
      in
      current @ opened @ lookup_all_name next entry_name
  | Map { map_entry; next } ->
      current @ (lookup_all_name next entry_name |> List.map map_entry)

let lookup_all = fun env entry_name -> lookup_all_name env entry_name
