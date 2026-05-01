open Kernel

let panic = Kernel.SystemError.panic

type 'a t = 'a option =
  | None
  | Some of 'a

let some = fun value -> Some value

let none = None

let equal = fun left right ~fn ->
  match (left, right) with
  | (None, None) -> true
  | (Some left, Some right) -> fn left right
  | _ -> false

let is_some = fun __tmp1 ->
  match __tmp1 with
  | Some _ -> true
  | None -> false

let is_none = fun __tmp1 ->
  match __tmp1 with
  | Some _ -> false
  | None -> true

let is_some_and = fun value ~fn ->
  match value with
  | Some value -> fn value
  | None -> false

let is_none_or = fun value ~fn ->
  match value with
  | None -> true
  | Some value -> fn value

let map = fun value ~fn ->
  match value with
  | Some value -> Some (fn value)
  | None -> None

let map_or = fun value ~default ~fn ->
  match value with
  | Some value -> fn value
  | None -> default

let map_or_default = fun value ~default ~fn ->
  match value with
  | Some value -> fn value
  | None -> default ()

let map_or_else = fun value ~default ~fn ->
  match value with
  | Some value -> fn value
  | None -> default ()

let and_ = fun left right ->
  match left with
  | Some _ -> right
  | None -> None

let and_then = fun value ~fn ->
  match value with
  | Some value -> fn value
  | None -> None

let or_ = fun left right ->
  match left with
  | Some _ -> left
  | None -> right

let or_else = fun value ~fn ->
  match value with
  | Some _ -> value
  | None -> fn ()

let xor = fun left right ->
  match (left, right) with
  | (Some _, None) -> left
  | (None, Some _) -> right
  | _ -> None

let unwrap = fun __tmp1 ->
  match __tmp1 with
  | Some value -> value
  | None -> panic "called Option.unwrap on a None value"

let unwrap_or = fun value ~default ->
  match value with
  | Some value -> value
  | None -> default

let unwrap_or_else = fun value ~fn ->
  match value with
  | Some value -> value
  | None -> fn ()

let expect = fun ~msg value ->
  match value with
  | Some value -> value
  | None -> panic msg

let unwrap_none = fun __tmp1 ->
  match __tmp1 with
  | None -> ()
  | Some _ -> panic "called Option.unwrap_none on a Some value"

let inspect = fun value ~fn ->
  (
    match value with
    | Some value -> fn value
    | None -> ()
  );
  value

let for_each = fun value ~fn ->
  match value with
  | Some value -> fn value
  | None -> ()

let ok_or = fun ~error value ->
  match value with
  | Some value -> Ok value
  | None -> Error error

let ok_or_else = fun ~error value ->
  match value with
  | Some value -> Ok value
  | None -> Error (error ())

let to_result = fun ~error value -> ok_or ~error value

let to_list = fun __tmp1 ->
  match __tmp1 with
  | Some value -> [ value ]
  | None -> []

let transpose = fun __tmp1 ->
  match __tmp1 with
  | Some (Ok value) -> Ok (Some value)
  | Some (Error error) -> Error error
  | None -> Ok None

let filter = fun value ~fn ->
  match value with
  | Some item when fn item -> Some item
  | _ -> None

let flatten = fun __tmp1 ->
  match __tmp1 with
  | Some value -> value
  | None -> None

let zip = fun left right ->
  match (left, right) with
  | (Some left, Some right) -> Some (left, right)
  | _ -> None

let zip_with = fun left right ~fn ->
  match (left, right) with
  | (Some left, Some right) -> Some (fn left right)
  | _ -> None

let unzip = fun __tmp1 ->
  match __tmp1 with
  | Some (left, right) -> (Some left, Some right)
  | None -> (None, None)

let all = fun values ->
  let rec go = fun acc ->
    fun __tmp1 ->
      match __tmp1 with
      | [] -> Some (List.reverse acc)
      | (Some value) :: rest -> go (value :: acc) rest
      | None :: _ -> None
  in
  go [] values
