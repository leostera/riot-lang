open Std
open Model

type frame = {
  boundary_level: int;
  mutable nodes: TypeRepr.t list;
}

type t = {
  mutable current_level: int;
  mutable current_mark: int;
  mutable frames: frame list;
}

let create = fun () -> { current_level = 0; current_mark = 0; frames = [] }

let current_frame = fun (regions: t) ->
  match regions.frames with
  | frame :: _ -> Some frame
  | [] -> None

let current_level = fun (regions: t) -> regions.current_level

let next_mark = fun (regions: t) ->
  let generation = regions.current_mark in
  let () =
    regions.current_mark <- generation + 1
  in
  generation

let track_node = fun (regions: t) node ->
  let () =
    match current_frame regions with
    | Some frame -> frame.nodes <- node :: frame.nodes
    | None -> ()
  in
  node

let fresh_var = fun (regions: t) id ->
  TypeRepr.make_var ~level:regions.current_level id |> track_node regions

let merge_child_into_parent = fun (regions: t) (child: frame) ->
  match regions.frames with
  | parent :: _ ->
      let survivors = child.nodes
      |> List.filter
        (fun node ->
          TypeRepr.level node > parent.boundary_level
          && not (TypeRepr.is_generic_level (TypeRepr.level node))) in
      parent.nodes <- survivors @ parent.nodes
  | [] -> ()

let exit_region = fun (regions: t) (child: frame) ->
  match regions.frames with
  | frame :: rest when Std.Ptr.equal frame child ->
      let () =
        regions.frames <- rest
      in
      let () = merge_child_into_parent regions child in
      regions.current_level <- child.boundary_level
  | _ -> raise (Failure "Region.exit_region")

let with_region = fun (regions: t) f ->
  let child = { boundary_level = regions.current_level; nodes = [] } in
  let () =
    regions.current_level <- child.boundary_level + 1
  in
  let () =
    regions.frames <- child :: regions.frames
  in
  try
    let result = f child in
    let () = exit_region regions child in
    result
  with
  | exn ->
      let () = exit_region regions child in
      raise exn

let boundary_level = fun (frame: frame) -> frame.boundary_level

let generalize_reachable_vars = fun (regions: t) (frame: frame) ty ->
  let generation = next_mark regions in
  let next_order () = 0 in
  let () = TypeRepr.mark_reachable_vars ~generation ~next_order ty in
  frame.nodes |> List.iter
    (fun node ->
      let node = TypeRepr.prune node in
      if TypeRepr.level node > frame.boundary_level then
        match TypeRepr.view node with
        | TypeRepr.Var _ when Int.equal node.mark generation -> TypeRepr.set_generic_var node
        | _ -> ())

let local_reachable_vars = fun (regions: t) (frame: frame) ty ->
  let generation = next_mark regions in
  let next_order =
    let order = ref 0 in
    fun () ->
      let current = !order in
      let () =
        order := current + 1
      in
      current
  in
  let () = TypeRepr.mark_reachable_vars ~generation ~next_order ty in
  frame.nodes |> List.filter_map
    (fun node ->
      let node = TypeRepr.prune node in
      if TypeRepr.level node > frame.boundary_level then
        match TypeRepr.view node with
        | TypeRepr.Var { id; _ } when Int.equal node.mark generation
        && not (TypeRepr.is_generic_var node) -> Some (node.mark_order, id)
        | _ -> None
      else
        None) |> List.sort
    (fun (left, _) (right, _) ->
      Int.compare left right) |> List.map snd
