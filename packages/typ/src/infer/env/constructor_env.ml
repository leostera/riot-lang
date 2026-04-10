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
  | Open of {
      root: IdentPath.t;
      type_decls: FileSummary.type_decl list;
      components: components;
      next: t
    }
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

let type_decl_key = fun (type_decl: FileSummary.type_decl) ->
  IdentPath.append_name type_decl.scope_path type_decl.declaration.type_name

let qualify_scheme_with_local_types = fun ~root type_decls scheme ->
  if List.is_empty type_decls then
    scheme
  else
    let by_id = Collections.HashMap.with_capacity (List.length type_decls) in
    let () =
      type_decls
      |> List.iter
        (fun (type_decl: FileSummary.type_decl) ->
          let _ = Collections.HashMap.insert by_id type_decl.declaration.type_constructor_id type_decl in
          ())
    in
    let rec qualify_type ty =
      let ty = TypeRepr.prune ty in
      match TypeRepr.view ty with
      | TypeRepr.Int
      | TypeRepr.Float
      | TypeRepr.Bool
      | TypeRepr.String
      | TypeRepr.Char
      | TypeRepr.Unit
      | TypeRepr.Hole _
      | TypeRepr.Var _ ->
          ty
      | TypeRepr.Option element ->
          let qualified_element = qualify_type element in
          if Std.Ptr.equal element qualified_element then
            ty
          else
            TypeRepr.option qualified_element
      | TypeRepr.Result (ok_ty, error_ty) ->
          let qualified_ok_ty = qualify_type ok_ty in
          let qualified_error_ty = qualify_type error_ty in
          if Std.Ptr.equal ok_ty qualified_ok_ty && Std.Ptr.equal error_ty qualified_error_ty then
            ty
          else
            TypeRepr.result qualified_ok_ty qualified_error_ty
      | TypeRepr.Array element ->
          let qualified_element = qualify_type element in
          if Std.Ptr.equal element qualified_element then
            ty
          else
            TypeRepr.array qualified_element
      | TypeRepr.List element ->
          let qualified_element = qualify_type element in
          if Std.Ptr.equal element qualified_element then
            ty
          else
            TypeRepr.list qualified_element
      | TypeRepr.Seq element ->
          let qualified_element = qualify_type element in
          if Std.Ptr.equal element qualified_element then
            ty
          else
            TypeRepr.seq qualified_element
      | TypeRepr.Named { head; arguments } ->
          let qualified_arguments = List.map qualify_type arguments in
          let qualified_head =
            match Collections.HashMap.get by_id head.type_constructor_id with
            | Some type_decl -> {
              head
              with name = IdentPath.append_path root (type_decl_key type_decl)
            }
            | None -> head
          in
          if
            Std.Ptr.equal head qualified_head && List.for_all2 Std.Ptr.equal arguments qualified_arguments
          then
            ty
          else
            TypeRepr.named ~head:qualified_head ~arguments:qualified_arguments
      | TypeRepr.PolyVariant { bound; tags; inherited } ->
          let qualified_tags =
            tags
            |> List.map
              (fun (tag: TypeRepr.poly_variant_tag) ->
                match tag.payload_type with
                | Some payload_type ->
                    let qualified_payload_type = qualify_type payload_type in
                    if Std.Ptr.equal payload_type qualified_payload_type then
                      tag
                    else
                      { tag with payload_type = Some qualified_payload_type }
                | None -> tag)
          in
          let qualified_inherited = List.map qualify_type inherited in
          if
            List.for_all2 Std.Ptr.equal tags qualified_tags
            && List.for_all2 Std.Ptr.equal inherited qualified_inherited
          then
            ty
          else
            TypeRepr.poly_variant ~bound ~tags:qualified_tags ~inherited:qualified_inherited
      | TypeRepr.Tuple members ->
          let qualified_members = List.map qualify_type members in
          if List.for_all2 Std.Ptr.equal members qualified_members then
            ty
          else
            TypeRepr.tuple qualified_members
      | TypeRepr.Arrow { label; lhs; rhs } ->
          let qualified_lhs = qualify_type lhs in
          let qualified_rhs = qualify_type rhs in
          if Std.Ptr.equal lhs qualified_lhs && Std.Ptr.equal rhs qualified_rhs then
            ty
          else
            TypeRepr.arrow ~label ~lhs:qualified_lhs ~rhs:qualified_rhs
      | TypeRepr.Package signature ->
          let qualified_values =
            signature.values
            |> List.map
              (fun (value: TypeRepr.package_value) ->
                let qualified_scheme = qualify_type value.scheme in
                if Std.Ptr.equal value.scheme qualified_scheme then
                  value
                else
                  { value with scheme = qualified_scheme })
          in
          if List.for_all2 Std.Ptr.equal signature.values qualified_values then
            ty
          else
            TypeRepr.package ~values:qualified_values
    in
    let quantified, body = TypeScheme.to_explicit scheme in
    let qualified_body = qualify_type body in
    if Std.Ptr.equal body qualified_body then
      scheme
    else
      TypeScheme.of_explicit ~quantified qualified_body

let qualify_inline_record_labels = fun ~root type_decls labels ->
  if List.is_empty type_decls then
    labels
  else
    let by_id = Collections.HashMap.with_capacity (List.length type_decls) in
    let () =
      type_decls
      |> List.iter
        (fun (type_decl: FileSummary.type_decl) ->
          let _ = Collections.HashMap.insert by_id type_decl.declaration.type_constructor_id type_decl in
          ())
    in
    let rec qualify_type ty =
      let ty = TypeRepr.prune ty in
      match TypeRepr.view ty with
      | TypeRepr.Named { head; arguments } ->
          let qualified_arguments = List.map qualify_type arguments in
          let qualified_head =
            match Collections.HashMap.get by_id head.type_constructor_id with
            | Some type_decl -> {
              head
              with name = IdentPath.append_path root (type_decl_key type_decl)
            }
            | None -> head
          in
          if
            Std.Ptr.equal head qualified_head && List.for_all2 Std.Ptr.equal arguments qualified_arguments
          then
            ty
          else
            TypeRepr.named ~head:qualified_head ~arguments:qualified_arguments
      | TypeRepr.Option element ->
          let qualified_element = qualify_type element in
          if Std.Ptr.equal element qualified_element then
            ty
          else
            TypeRepr.option qualified_element
      | TypeRepr.Result (ok_ty, error_ty) ->
          let qualified_ok_ty = qualify_type ok_ty in
          let qualified_error_ty = qualify_type error_ty in
          if Std.Ptr.equal ok_ty qualified_ok_ty && Std.Ptr.equal error_ty qualified_error_ty then
            ty
          else
            TypeRepr.result qualified_ok_ty qualified_error_ty
      | TypeRepr.Array element ->
          let qualified_element = qualify_type element in
          if Std.Ptr.equal element qualified_element then
            ty
          else
            TypeRepr.array qualified_element
      | TypeRepr.List element ->
          let qualified_element = qualify_type element in
          if Std.Ptr.equal element qualified_element then
            ty
          else
            TypeRepr.list qualified_element
      | TypeRepr.Seq element ->
          let qualified_element = qualify_type element in
          if Std.Ptr.equal element qualified_element then
            ty
          else
            TypeRepr.seq qualified_element
      | TypeRepr.Tuple members ->
          let qualified_members = List.map qualify_type members in
          if List.for_all2 Std.Ptr.equal members qualified_members then
            ty
          else
            TypeRepr.tuple qualified_members
      | TypeRepr.Arrow { label; lhs; rhs } ->
          let qualified_lhs = qualify_type lhs in
          let qualified_rhs = qualify_type rhs in
          if Std.Ptr.equal lhs qualified_lhs && Std.Ptr.equal rhs qualified_rhs then
            ty
          else
            TypeRepr.arrow ~label ~lhs:qualified_lhs ~rhs:qualified_rhs
      | TypeRepr.PolyVariant { bound; tags; inherited } ->
          let qualified_tags =
            tags
            |> List.map
              (fun (tag: TypeRepr.poly_variant_tag) ->
                match tag.payload_type with
                | Some payload_type ->
                    let qualified_payload_type = qualify_type payload_type in
                    if Std.Ptr.equal payload_type qualified_payload_type then
                      tag
                    else
                      { tag with payload_type = Some qualified_payload_type }
                | None -> tag)
          in
          let qualified_inherited = List.map qualify_type inherited in
          if
            List.for_all2 Std.Ptr.equal tags qualified_tags
            && List.for_all2 Std.Ptr.equal inherited qualified_inherited
          then
            ty
          else
            TypeRepr.poly_variant ~bound ~tags:qualified_tags ~inherited:qualified_inherited
      | TypeRepr.Package signature ->
          let qualified_values =
            signature.values
            |> List.map
              (fun (value: TypeRepr.package_value) ->
                let qualified_scheme = qualify_type value.scheme in
                if Std.Ptr.equal value.scheme qualified_scheme then
                  value
                else
                  { value with scheme = qualified_scheme })
          in
          if List.for_all2 Std.Ptr.equal signature.values qualified_values then
            ty
          else
            TypeRepr.package ~values:qualified_values
      | TypeRepr.Int
      | TypeRepr.Float
      | TypeRepr.Bool
      | TypeRepr.String
      | TypeRepr.Char
      | TypeRepr.Unit
      | TypeRepr.Hole _
      | TypeRepr.Var _ ->
          ty
    in
    labels |> List.map
      (fun (label: TypeDecl.label) ->
        let qualified_field_type = qualify_type label.field_type in
        if Std.Ptr.equal label.field_type qualified_field_type then
          label
        else
          { label with field_type = qualified_field_type })

let qualify_entry = fun ~root ~type_decls entry ->
  let qualified_scheme = qualify_scheme_with_local_types ~root type_decls entry.constructor.scheme in
  let qualified_inline_record_labels = entry.constructor.inline_record_labels
  |> Option.map (qualify_inline_record_labels ~root type_decls) in
  {
    owner_path = IdentPath.append_path root entry.owner_path;
    owner_type_constructor_id = entry.owner_type_constructor_id;
    constructor = {
      entry.constructor
      with scheme = qualified_scheme;
      inline_record_labels = qualified_inline_record_labels
    }
  }

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

let add_open = fun ~root ~type_decls opened env ->
  {
    current = Name_map.empty;
    by_owner = Owner_map.empty;
    layer = Open { root; type_decls; components = visible_components opened; next = env }
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
  | Open { root; type_decls; components; next } ->
      let opened = Name_map.find_opt entry_name components.by_name
      |> Option.unwrap_or ~default:[]
      |> List.map (qualify_entry ~root ~type_decls) in
      current @ opened @ lookup_all_name next entry_name
  | Map { map_entry; next } ->
      current @ (lookup_all_name next entry_name |> List.map map_entry)

let lookup_all = lookup_all_name

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
      | Open { root; type_decls; components; next } -> (
          match lookup_local components.by_owner with
          | Some entry -> Some (qualify_entry ~root ~type_decls entry)
          | None -> lookup_owned next entry_name owner_type_constructor_id
        )
      | Map { map_entry; next } ->
          lookup_owned next entry_name owner_type_constructor_id |> Option.map map_entry
    )
