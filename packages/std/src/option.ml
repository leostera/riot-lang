open Kernel

let panic = Kernel.SystemError.panic

type 'a t = 'a option =
  | None
  | Some of 'a

let some = fun value -> Some value

let none = None

let equal = fun eq left right ->
  match (left, right) with
  | None, None -> true
  | Some left, Some right -> eq left right
  | _ -> false

let is_some = function
  | Some _ -> true
  | None -> false

let is_none = function
  | Some _ -> false
  | None -> true

let is_some_and = fun predicate ->
  function
  | Some value -> predicate value
  | None -> false

let is_none_or = fun predicate ->
  function
  | None -> true
  | Some value -> predicate value

let map = fun fn ->
  function
  | Some value -> Some (fn value)
  | None -> None

let map_or = fun ~default fn ->
  function
  | Some value -> fn value
  | None -> default

let map_or_default = fun ~default fn ->
  function
  | Some value -> fn value
  | None -> default ()

let map_or_else = fun ~default fn ->
  function
  | Some value -> fn value
  | None -> default ()

let and_ = fun left right ->
  match left with
  | Some _ -> right
  | None -> None

let and_then = fun value fn ->
  match value with
  | Some value -> fn value
  | None -> None

let or_ = fun left right ->
  match left with
  | Some _ -> left
  | None -> right

let or_else = fun value fn ->
  match value with
  | Some _ -> value
  | None -> fn ()

let xor = fun left right ->
  match (left, right) with
  | Some _, None -> left
  | None, Some _ -> right
  | _ -> None

let unwrap = function
  | Some value -> value
  | None -> panic "called Option.unwrap on a None value"

let unwrap_or = fun ~default ->
  function
  | Some value -> value
  | None -> default

let unwrap_or_else = fun ~fn ->
  function
  | Some value -> value
  | None -> fn ()

let expect = fun ~msg ->
  function
  | Some value -> value
  | None -> panic msg

let unwrap_none = function
  | None -> ()
  | Some _ -> panic "called Option.unwrap_none on a Some value"

let inspect = fun fn value ->
  (
    match value with
    | Some value -> fn value
    | None -> ()
  );
  value

let iter = fun fn ->
  function
  | Some value -> fn value
  | None -> ()

let ok_or = fun ~error ->
  function
  | Some value -> Ok value
  | None -> Error error

let ok_or_else = fun ~error ->
  function
  | Some value -> Ok value
  | None -> Error (error ())

let to_result = fun ~error value -> ok_or ~error value

let to_list = function
  | Some value -> [ value ]
  | None -> []

let transpose = function
  | Some (Ok value) -> Ok (Some value)
  | Some (Error error) -> Error error
  | None -> Ok None

let filter = fun predicate ->
  function
  | Some value when predicate value -> Some value
  | _ -> None

let flatten = function
  | Some value -> value
  | None -> None

let zip = fun left right ->
  match (left, right) with
  | Some left, Some right -> Some (left, right)
  | _ -> None

let zip_with = fun fn left right ->
  match (left, right) with
  | Some left, Some right -> Some (fn left right)
  | _ -> None

let unzip = function
  | Some (left, right) -> (Some left, Some right)
  | None -> (None, None)

let all = fun values ->
  let rec go = fun acc ->
    function
    | [] -> Some (List.rev acc)
    | Some value :: rest -> go (value :: acc) rest
    | None :: _ -> None
  in
  go [] values
