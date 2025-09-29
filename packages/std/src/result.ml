open Global
(** Result type for error handling *)

type ('a, 'e) t = ('a, 'e) Stdlib.result = Ok of 'a | Error of 'e

(* Constructors *)
let ok x = Ok x
let err e = Error e

(* Querying *)
let is_ok = function Ok _ -> true | Error _ -> false
let is_error = function Ok _ -> false | Error _ -> true
let is_err = is_error

let is_ok_and f = function
  | Ok x -> f x
  | Error _ -> false

let is_err_and f = function
  | Error e -> f e
  | Ok _ -> false

(* Transforming *)
let map f = function Ok x -> Ok (f x) | Error e -> Error e
let map_error f = function Ok x -> Ok x | Error e -> Error (f e)
let map_err = map_error

let map_or ~default f = function
  | Ok x -> f x
  | Error _ -> default

let map_or_else ~default f = function
  | Ok x -> f x
  | Error e -> default e

(* Chaining *)
let and_then r f = match r with Ok x -> f x | Error e -> Error e

let or_ r1 r2 = match r1 with
  | Ok _ -> r1
  | Error _ -> r2

let or_else r f = match r with
  | Ok x -> Ok x
  | Error e -> f e

(* Extracting values *)
let unwrap = function
  | Ok x -> x
  | Error _ -> panic "called Result.unwrap on an Error value"

let unwrap_or ~default = function
  | Ok x -> x
  | Error _ -> default

let unwrap_or_default ~default = function
  | Ok x -> x
  | Error _ -> default ()

let unwrap_or_else ~fn = function
  | Ok x -> x
  | Error _ -> fn ()

let unwrap_err = function
  | Error e -> e
  | Ok _ -> panic "called Result.unwrap_err on an Ok value"

let expect ~msg = function
  | Ok x -> x
  | Error _ -> panic msg

let expect_err ~msg = function
  | Error e -> e
  | Ok _ -> panic msg

let ok_value = function
  | Ok x -> Some x
  | Error _ -> None

let err_value = function
  | Error e -> Some e
  | Ok _ -> None

(* Inspecting *)
let inspect f r =
  (match r with Ok x -> f x | Error _ -> ());
  r

let inspect_err f r =
  (match r with Error e -> f e | Ok _ -> ());
  r

(* Iterating *)
let iter f = function Ok x -> f x | Error _ -> ()
let iter_error f = function Ok _ -> () | Error e -> f e

(* Converting *)
let to_option = function Ok x -> Some x | Error _ -> None
let of_option ~error = function Some x -> Ok x | None -> Error error

let transpose = function
  | Ok None -> None
  | Ok (Some x) -> Some (Ok x)
  | Error e -> Some (Error e)

(* Flattening *)
let flatten = function Ok r -> r | Error e -> Error e

(* Collecting *)
let all results =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | Ok x :: rest -> go (x :: acc) rest
    | Error e :: _ -> Error e
  in
  go [] results

let both r1 r2 =
  match (r1, r2) with
  | Ok x, Ok y -> Ok (x, y)
  | Error e, _ -> Error e
  | _, Error e -> Error e

(* Misc *)
let fold ~ok ~error = function Ok x -> ok x | Error e -> error e