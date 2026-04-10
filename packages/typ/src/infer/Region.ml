open Std
open Model

type frame = {
  level: int;
  boundary_level: int;
  mutable nodes: TypeRepr.t list;
}

type t = {
  mutable current_level: int;
  mutable current_mark: int;
  mutable frames: frame list;
}

let create = fun () ->
  let root = { level = 0; boundary_level = 0; nodes = [] } in
  { current_level = 0; current_mark = 0; frames = [ root ] }

let current_level = fun (regions: t) -> regions.current_level

let next_mark = fun (regions: t) ->
  let generation = regions.current_mark in
  regions.current_mark <- generation + 1;
  generation

let rec frame_for_level = fun level frames ->
  match frames with
  | [] -> None
  | frame :: rest ->
      if level >= frame.level then
        Some frame
      else
        frame_for_level level rest

let add_to_pool = fun (regions: t) ~level node ->
  (
    match frame_for_level level regions.frames with
    | Some frame ->
        if TypeRepr.pool_level node = Some frame.level then
          ()
        else (
          TypeRepr.set_pool_level node (Some frame.level);
          frame.nodes <- node :: frame.nodes
        )
    | None -> TypeRepr.set_pool_level node None
  );
  node

let track_node = fun (regions: t) node -> add_to_pool regions ~level:(TypeRepr.level node) node

let exit_region = fun (regions: t) (child: frame) ->
  match regions.frames with
  | frame :: rest when Std.Ptr.equal frame child ->
      child.nodes |> List.iter
        (fun node ->
          if TypeRepr.pool_level node = Some child.level then
            TypeRepr.set_pool_level node None);
      regions.frames <- rest;
      regions.current_level <- child.boundary_level
  | _ -> raise (Failure "Region.exit_region")

let with_region_finalize = fun (regions: t) ~finalize f ->
  let child = {
    level = regions.current_level + 1;
    boundary_level = regions.current_level;
    nodes = []
  } in
  regions.current_level <- child.level;
  regions.frames <- child :: regions.frames;
  try
    let result = f child in
    let result = finalize child result in
    exit_region regions child;
    result
  with
  | exn ->
      exit_region regions child;
      raise exn

let with_region = fun (regions: t) f ->
  with_region_finalize regions ~finalize:(fun _ result -> result) f

let boundary_level = fun (frame: frame) -> frame.boundary_level

let mark_roots = fun (regions: t) roots ->
  let generation = next_mark regions in
  let next_order () = 0 in
  List.iter (TypeRepr.mark_reachable_vars ~generation ~next_order) roots;
  generation

let iter_owned_nodes = fun (frame: frame) f ->
  frame.nodes |> List.iter
    (fun node ->
      if TypeRepr.pool_level node = Some frame.level then
        f node)

let generalize_reachable_vars = fun (regions: t) (frame: frame) roots ->
  let generation = mark_roots regions roots in
  iter_owned_nodes frame
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
      order := current + 1;
      current
  in
  TypeRepr.mark_reachable_vars ~generation ~next_order ty;
  frame.nodes |> List.filter_map
    (fun node ->
      let node = TypeRepr.prune node in
      if TypeRepr.pool_level node = Some frame.level && TypeRepr.level node > frame.boundary_level then
        match TypeRepr.view node with
        | TypeRepr.Var { id; _ } when Int.equal node.mark generation
        && not (TypeRepr.is_generic_var node) -> Some (node.mark_order, id)
        | _ -> None
      else
        None) |> List.sort
    (fun (left, _) (right, _) ->
      Int.compare left right) |> List.map snd
