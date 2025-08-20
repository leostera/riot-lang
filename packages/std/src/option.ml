(** Option type utilities *)

type 'a t = 'a option = None | Some of 'a

let some x = Some x
let none = None
let is_some = function Some _ -> true | None -> false
let is_none = function Some _ -> false | None -> true
let map f = function Some x -> Some (f x) | None -> None
let bind opt f = match opt with Some x -> f x | None -> None
let ( >>= ) = bind
let ( >>| ) opt f = map f opt
let value opt ~default = match opt with Some x -> x | None -> default

let value_exn = function
  | Some x -> x
  | None -> failwith "Option.value_exn: None"

let value_map opt ~default ~f = match opt with Some x -> f x | None -> default
let fold ~none ~some = function None -> none | Some x -> some x
let iter f = function Some x -> f x | None -> ()
let filter pred = function Some x when pred x -> Some x | _ -> None
let join = function Some opt -> opt | None -> None

let all options =
  let rec go acc = function
    | [] -> Some (List.rev acc)
    | Some x :: rest -> go (x :: acc) rest
    | None :: _ -> None
  in
  go [] options

let both opt1 opt2 =
  match (opt1, opt2) with Some x, Some y -> Some (x, y) | _ -> None

let to_result ~error = function
  | Some x -> Result.Ok x
  | None -> Result.Error error

let to_list = function Some x -> [ x ] | None -> []

let unwrap = function
  | Some x -> x
  | None -> failwith "called Option.unwrap on a None value"

let unwrap_none = function
  | None -> ()
  | Some _ -> failwith "called Option.unwrap_none on a Some value"
