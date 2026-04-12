open Kernel

let panic = Kernel.SystemError.panic

(** Result type for error handling *)
type ('a, 'e) t = ('a, 'e) Kernel.Result.t =
  | Ok of 'a
  | Error of 'e

(* Constructors *)

let ok = fun x -> Ok x

let err = fun e -> Error e

(* Querying *)

let is_ok = function
  | Ok _ -> true
  | Error _ -> false

let is_error = function
  | Ok _ -> false
  | Error _ -> true

let is_err = is_error

let is_ok_and = fun f ->
  function
  | Ok x -> f x
  | Error _ -> false

let is_err_and = fun f ->
  function
  | Error e -> f e
  | Ok _ -> false

(* Transforming *)

let map = fun f ->
  function
  | Ok x -> Ok (f x)
  | Error e -> Error e

let map_error = fun f ->
  function
  | Ok x -> Ok x
  | Error e -> Error (f e)

let map_err = map_error

let map_or = fun ~default f ->
  function
  | Ok x -> f x
  | Error _ -> default

let map_or_else = fun ~default f ->
  function
  | Ok x -> f x
  | Error e -> default e

(* Chaining *)

let and_then = fun r f ->
  match r with
  | Ok x -> f x
  | Error e -> Error e

let or_ = fun r1 r2 ->
  match r1 with
  | Ok _ -> r1
  | Error _ -> r2

let or_else = fun r f ->
  match r with
  | Ok x -> Ok x
  | Error e -> f e

(* Extracting values *)

let unwrap = function
  | Ok x -> x
  | Error _ -> panic "called Result.unwrap on an Error value"

let unwrap_or = fun ~default ->
  function
  | Ok x -> x
  | Error _ -> default

let unwrap_or_default = fun ~default ->
  function
  | Ok x -> x
  | Error _ -> default ()

let unwrap_or_else = fun ~fn ->
  function
  | Ok x -> x
  | Error _ -> fn ()

let unwrap_err = function
  | Error e -> e
  | Ok _ -> panic "called Result.unwrap_err on an Ok value"

let expect = fun ~msg ->
  function
  | Ok x -> x
  | Error _ -> panic msg

let expect_err = fun ~msg ->
  function
  | Error e -> e
  | Ok _ -> panic msg

let ok_value = function
  | Ok x -> Some x
  | Error _ -> None

let err_value = function
  | Error e -> Some e
  | Ok _ -> None

(* Inspecting *)

let inspect = fun f r ->
  (
    match r with
    | Ok x -> f x
    | Error _ -> ()
  );
  r

let inspect_err = fun f r ->
  (
    match r with
    | Error e -> f e
    | Ok _ -> ()
  );
  r

(* Iterating *)

let iter = fun f ->
  function
  | Ok x -> f x
  | Error _ -> ()

let iter_error = fun f ->
  function
  | Ok _ -> ()
  | Error e -> f e

(* Converting *)

let to_option = function
  | Ok x -> Some x
  | Error _ -> None

let of_option = fun ~error ->
  function
  | Some x -> Ok x
  | None -> Error error

let transpose = function
  | Ok None -> None
  | Ok (Some x) -> Some (Ok x)
  | Error e -> Some (Error e)

(* Flattening *)

let flatten = function
  | Ok r -> r
  | Error e -> Error e

(* Collecting *)

let all = fun results ->
  let rec go = fun acc ->
    function
    | [] -> Ok (List.rev acc)
    | Ok x :: rest -> go (x :: acc) rest
    | Error e :: _ -> Error e
  in
  go [] results

let both = fun r1 r2 ->
  match (r1, r2) with
  | Ok x, Ok y -> Ok (x, y)
  | Error e, _ -> Error e
  | _, Error e -> Error e

(* Misc *)

let fold = fun ~ok ~error ->
  function
  | Ok x -> ok x
  | Error e -> error e
