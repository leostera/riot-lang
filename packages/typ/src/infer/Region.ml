open Std
open Model

type frame = {
  boundary_level: int;
  mutable vars: TypeRepr.var list;
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

let fresh_var = fun (regions: t) id ->
  let fresh = TypeRepr.make_var ~level:regions.current_level id in
  let () =
    match (current_frame regions, fresh) with
    | (Some frame, TypeRepr.Var var) -> frame.vars <- var :: frame.vars
    | _ -> ()
  in
  fresh

let merge_child_into_parent = fun (regions: t) (child: frame) ->
  match regions.frames with
  | parent :: _ ->
      let survivors = child.vars
      |> List.filter (fun (var: TypeRepr.var) -> var.level > parent.boundary_level) in
      parent.vars <- survivors @ parent.vars
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
  let child = { boundary_level = regions.current_level; vars = [] } in
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

let next_mark = fun (regions: t) ->
  let generation = regions.current_mark in
  let () =
    regions.current_mark <- generation + 1
  in
  generation

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
  frame.vars |> List.filter_map
    (fun (var: TypeRepr.var) ->
      if var.level > frame.boundary_level && var.mark = generation then
        Some (var.mark_order, var.id)
      else
        None) |> List.sort
    (fun (left, _) (right, _) ->
      Int.compare left right) |> List.map snd
