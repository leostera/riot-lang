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
  | Open of {
      root: IdentPath.t;
      components: components;
      next: t;
    }

and t = {
  entries: entry list;
  current: current;
  layer: layer;
}

let empty = { entries = []; current = Name_map.empty; layer = Nothing }

let name = fun entry -> entry.constructor.name

let constructor_id = fun entry -> entry.constructor.constructor_id

let owner_path = fun entry -> entry.owner_path

let owner_type_constructor_id = fun entry -> entry.owner_type_constructor_id

let scheme = fun entry -> entry.constructor.scheme

let prepend_entry = fun index entry ->
  let existing = Name_map.find_opt (name entry) index |> Option.unwrap_or ~default:[] in
  Name_map.add (name entry) (entry :: existing) index

let index_of_entries = fun entries ->
  entries |> List.rev |> List.fold_left prepend_entry Name_map.empty

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
              constructor;
            }))
  in
  { entries; current = index_of_entries entries; layer = Nothing }

let entries = fun env -> env.entries

let local_only = fun env -> { env with layer = Nothing }

let visible_components_of_entries = fun entries ->
  entries
  |> List.rev
  |> List.fold_left
    (fun acc entry ->
      let existing = Name_map.find_opt (name entry) acc |> Option.unwrap_or ~default:[] in
      Name_map.add (name entry) (entry :: existing) acc)
    Name_map.empty

let add_open = fun ~root opened env ->
  {
    entries = env.entries;
    current = Name_map.empty;
    layer = Open {
      root;
      components = visible_components_of_entries opened.entries;
      next = env;
    };
  }

let merge_current = fun introduced existing ->
  Name_map.fold
    (fun name introduced_entries acc ->
      let current = Name_map.find_opt name acc |> Option.unwrap_or ~default:[] in
      Name_map.add name (introduced_entries @ current) acc)
    introduced
    existing

let bind = fun env introduced ->
  if List.is_empty introduced.entries then
    env
  else if List.is_empty env.entries && env.layer = Nothing then
    introduced
  else
    {
      entries = introduced.entries @ env.entries;
      current = merge_current introduced.current env.current;
      layer = env.layer;
    }

let rec lookup_all_name = fun env name ->
  let current = Name_map.find_opt name env.current |> Option.unwrap_or ~default:[] in
  match env.layer with
  | Nothing ->
      current
  | Open { root; components; next } ->
      let opened =
        Name_map.find_opt name components
        |> Option.unwrap_or ~default:[]
        |> List.map (qualify_entry root)
      in
      current @ opened @ lookup_all_name next name

let lookup_all = fun env name -> lookup_all_name env name
