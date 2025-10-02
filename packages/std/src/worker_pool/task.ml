open Miniriot

type t = Task : { value : 'v; ref : 'v Ref.t } -> t

let make value ref = Task { value; ref }

let value : type v. t -> v Ref.t -> v option =
 fun (Task { ref; value }) ref' ->
  match Ref.type_equal ref ref' with
  | Some Type.Equal -> Some value
  | None -> None

let ref : type v. t -> v Ref.t -> v Ref.t option =
 fun (Task { ref; _ }) ref' ->
  match Ref.type_equal ref ref' with
  | Some Type.Equal -> Some ref
  | None -> None
