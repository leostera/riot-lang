open Std
open Model

module Name_map = Collections.Map.Make (String)

type entry = {
  owner_path: IdentPath.t;
  owner_type_constructor_id: TypeConstructorId.t;
  constructor: TypeDecl.constructor;
}

type t = {
  entries: entry list;
  by_name: entry list Name_map.t;
}

let empty = { entries = []; by_name = Name_map.empty }

let name = fun entry -> entry.constructor.name

let constructor_id = fun entry -> entry.constructor.constructor_id

let owner_path = fun entry -> entry.owner_path

let owner_type_constructor_id = fun entry -> entry.owner_type_constructor_id

let scheme = fun entry -> entry.constructor.scheme

let prepend_entry = fun index entry ->
  let existing = Name_map.find_opt (name entry) index |> Option.unwrap_or ~default:[] in
  Name_map.add (name entry) (entry :: existing) index

let index_of_entries = fun entries -> entries |> List.rev |> List.fold_left prepend_entry Name_map.empty

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
  { entries; by_name = index_of_entries entries }

let bind = fun env introduced ->
  if List.is_empty introduced.entries then
    env
  else if List.is_empty env.entries then
    introduced
  else
    let by_name =
      Name_map.fold
        (fun name introduced_entries acc ->
          let existing = Name_map.find_opt name acc |> Option.unwrap_or ~default:[] in
          Name_map.add name (introduced_entries @ existing) acc)
        introduced.by_name
        env.by_name
    in
    { entries = introduced.entries @ env.entries; by_name }

let entries = fun env -> env.entries

let lookup_all = fun env name -> Name_map.find_opt name env.by_name |> Option.unwrap_or ~default:[]
