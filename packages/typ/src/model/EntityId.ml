open Std

type t =
  | Unresolved of SurfacePath.t
  | Resolved of {
      binding_id: BindingId.t;
      surface_path: SurfacePath.t;
    }
  | Apply of t * t

let empty = Unresolved SurfacePath.empty

let of_surface_path = fun surface_path -> Unresolved surface_path

let of_name = fun name -> of_surface_path (SurfacePath.of_name name)

let of_segments = fun segments -> of_surface_path (SurfacePath.of_segments segments)

let of_string = fun text -> of_surface_path (SurfacePath.of_string text)

let resolved = fun ~binding_id ~surface_path -> Resolved { binding_id; surface_path }

let of_binding_id = fun binding_id ->
  resolved ~binding_id ~surface_path:(SurfacePath.of_name (BindingId.name binding_id))

let binding_id = function
  | Resolved { binding_id; _ } ->
      Some binding_id
  | Unresolved _
  | Apply _ ->
      None

let rec surface_path = function
  | Unresolved surface_path
  | Resolved { surface_path; _ } ->
      surface_path
  | Apply (callee, argument) ->
      SurfacePath.of_string (to_string callee ^ "(" ^ to_string argument ^ ")")

and to_string = fun entity -> surface_path entity |> SurfacePath.to_string

let is_empty = fun entity -> surface_path entity |> SurfacePath.is_empty

let is_bare = fun entity -> surface_path entity |> SurfacePath.is_bare

let bare_name = fun entity -> surface_path entity |> SurfacePath.bare_name

let to_segments = fun entity -> surface_path entity |> SurfacePath.to_segments

let compare = fun left right ->
  match (left, right) with
  | (Unresolved left, Unresolved right) ->
      SurfacePath.compare left right
  | (Unresolved _, _) ->
      -1
  | (_, Unresolved _) ->
      1
  | (Resolved left, Resolved right) -> (
      match BindingId.compare left.binding_id right.binding_id with
      | 0 ->
          SurfacePath.compare left.surface_path right.surface_path
      | order ->
          order
    )
  | (Resolved _, _) ->
      -1
  | (_, Resolved _) ->
      1
  | (Apply (left_callee, left_argument), Apply (right_callee, right_argument)) -> (
      match compare left_callee right_callee with
      | 0 ->
          compare left_argument right_argument
      | order ->
          order
    )

let equal = fun left right -> Int.equal (compare left right) 0

let with_surface_path = fun new_surface_path entity ->
  match entity with
  | Unresolved _ ->
      Unresolved new_surface_path
  | Resolved { binding_id; _ } ->
      Resolved { binding_id; surface_path = new_surface_path }
  | Apply _ ->
      Unresolved new_surface_path

let append_name = fun entity name ->
  of_surface_path (SurfacePath.append_name (surface_path entity) name)

let prepend_name = fun name entity ->
  with_surface_path (SurfacePath.prepend_name name (surface_path entity)) entity

let append_path = fun left right ->
  let right_segments = right |> surface_path |> SurfacePath.to_segments in
  right_segments
  |> List.fold_left append_name left

let qualify = fun ~prefix entity ->
  with_surface_path (SurfacePath.append_path prefix (surface_path entity)) entity

let last_name = fun entity -> surface_path entity |> SurfacePath.last_name

let uncons = fun entity ->
  entity
  |> surface_path
  |> SurfacePath.uncons
  |> Option.map (fun (name, tail) -> (name, of_surface_path tail))

let split_last = fun entity ->
  entity
  |> surface_path
  |> SurfacePath.split_last
  |> Option.map (fun (prefix, name) -> (with_surface_path prefix entity, name))

let strip_prefix = fun ~prefix entity ->
  entity
  |> surface_path
  |> SurfacePath.strip_prefix ~prefix
  |> Option.map (fun suffix -> with_surface_path suffix entity)

let prefixes = fun entity ->
  entity
  |> surface_path
  |> SurfacePath.prefixes
  |> List.map (fun prefix -> with_surface_path prefix entity)
