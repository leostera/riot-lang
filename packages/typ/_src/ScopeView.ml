open Std
open Model

type visible_module = { path: SurfacePath.t; module_id: PackageEnv.ModuleId.t; depth: int }

type t = {
  visible_modules: visible_module list;
  visible_module_ids: PackageEnv.ModuleId.t list;
  visible_modules_by_head: (string, visible_module list) Collections.HashMap.t;
  implicit_open_modules: visible_module list;
}

let empty = fun () ->
  {
    visible_modules = [];
    visible_module_ids = [];
    visible_modules_by_head = Collections.HashMap.with_capacity 4;
    implicit_open_modules = []
  }

let create = fun ~visible_modules ~implicit_open_modules ->
  let visible_modules =
    visible_modules |> List.map
      (
        fun (path, module_id) -> { path; module_id; depth = List.length (SurfacePath.to_segments path) }
      )
  in
  let implicit_open_modules =
    implicit_open_modules |> List.map
      (
        fun (path, module_id) -> { path; module_id; depth = List.length (SurfacePath.to_segments path) }
      )
  in
  let visible_modules_by_head = Collections.HashMap.with_capacity (List.length visible_modules) in
  let visible_module_ids_rev = ref [] in
  let () =
    visible_modules |> List.iter
      (
        fun ({ path; module_id; _ } as visible_module) ->
          visible_module_ids_rev := module_id :: !visible_module_ids_rev;
          match SurfacePath.uncons path with
          | Some (head, _tail) ->
              let existing = Collections.HashMap.get visible_modules_by_head head |> Option.unwrap_or ~default:[] in
              let _ = Collections.HashMap.insert visible_modules_by_head head (visible_module :: existing) in ()
          | None -> ()
      )
  in
  let () =
    visible_modules_by_head |> Collections.HashMap.iter
      (
        fun head candidates ->
          let sorted =
            List.sort
              (
                fun left right -> Int.compare right.depth left.depth
              )
              candidates
          in
          let _ = Collections.HashMap.insert visible_modules_by_head head sorted in ()
      )
  in
  {
    visible_modules;
    visible_module_ids = List.rev !visible_module_ids_rev;
    visible_modules_by_head;
    implicit_open_modules
  }

let resolve_visible_module_prefix = fun scope_view path ->
  let rec choose = function
    | [] -> None
    | { path = module_path; module_id; _ } :: rest -> (
      match SurfacePath.strip_prefix ~prefix:module_path path with
      | Some suffix -> Some (module_path, module_id, suffix)
      | None -> choose rest
    )
  in
  match SurfacePath.uncons path with
  | Some (head, _tail) -> Option.and_then (Collections.HashMap.get scope_view.visible_modules_by_head head) choose
  | None -> None

let visible_modules = fun scope_view ->
  scope_view.visible_modules |> List.map
    (
      fun visible_module -> (visible_module.path, visible_module.module_id)
    )

let implicit_open_modules = fun scope_view ->
  scope_view.implicit_open_modules |> List.map
    (
      fun visible_module -> (visible_module.path, visible_module.module_id)
    )

let visible_module_ids = fun scope_view -> scope_view.visible_module_ids
