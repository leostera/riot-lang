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

let is_ok = fun __tmp1 ->
  match __tmp1 with
  | Ok _ -> true
  | Error _ -> false

let is_error = fun __tmp1 ->
  match __tmp1 with
  | Ok _ -> false
  | Error _ -> true

let is_err = is_error

let is_ok_and = fun f ->
  fun __tmp1 ->
    match __tmp1 with
    | Ok x -> f x
    | Error _ -> false

let is_err_and = fun f ->
  fun __tmp1 ->
    match __tmp1 with
    | Error e -> f e
    | Ok _ -> false

(* Transforming *)

let map = fun value ~fn ->
  match value with
  | Ok x -> Ok (fn x)
  | Error e -> Error e

let map_error = fun value ~fn ->
  match value with
  | Ok x -> Ok x
  | Error e -> Error (fn e)

let map_err = map_error

let map_or = fun value ~default ~fn ->
  match value with
  | Ok x -> fn x
  | Error _ -> default

let map_or_else = fun value ~default ~fn ->
  match value with
  | Ok x -> fn x
  | Error e -> default e

(* Chaining *)

let and_then = fun value ~fn ->
  match value with
  | Ok x -> fn x
  | Error e -> Error e

let or_ = fun r1 r2 ->
  match r1 with
  | Ok _ -> r1
  | Error _ -> r2

let or_else = fun value ~fn ->
  match value with
  | Ok x -> Ok x
  | Error e -> fn e

(* Extracting values *)

let unwrap = fun __tmp1 ->
  match __tmp1 with
  | Ok x -> x
  | Error _ -> panic "called Result.unwrap on an Error value"

let unwrap_or = fun value ~default ->
  match value with
  | Ok x -> x
  | Error _ -> default

let unwrap_or_default = fun ~default ->
  fun __tmp1 ->
    match __tmp1 with
    | Ok x -> x
    | Error _ -> default ()

let unwrap_or_else = fun value ~fn ->
  match value with
  | Ok x -> x
  | Error _ -> fn ()

let unwrap_err = fun __tmp1 ->
  match __tmp1 with
  | Error e -> e
  | Ok _ -> panic "called Result.unwrap_err on an Ok value"

let expect = fun ~msg ->
  fun __tmp1 ->
    match __tmp1 with
    | Ok x -> x
    | Error _ -> panic msg

let expect_err = fun ~msg ->
  fun __tmp1 ->
    match __tmp1 with
    | Error e -> e
    | Ok _ -> panic msg

let ok_value = fun __tmp1 ->
  match __tmp1 with
  | Ok x -> Some x
  | Error _ -> None

let err_value = fun __tmp1 ->
  match __tmp1 with
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

let iter = fun value ~fn ->
  match value with
  | Ok x -> fn x
  | Error _ -> ()

let iiter_err = fun value ~fn ->
  match value with
  | Ok _ -> ()
  | Error e -> fn e

let iter_err = iiter_err

let iter_error = iiter_err

(* Converting *)

let to_option = fun __tmp1 ->
  match __tmp1 with
  | Ok x -> Some x
  | Error _ -> None

let from_option = fun ~error ->
  fun __tmp1 ->
    match __tmp1 with
    | Some x -> Ok x
    | None -> Error error

let transpose = fun __tmp1 ->
  match __tmp1 with
  | Ok None -> None
  | Ok (Some x) -> Some (Ok x)
  | Error e -> Some (Error e)

(* Flattening *)

let flatten = fun __tmp1 ->
  match __tmp1 with
  | Ok r -> r
  | Error e -> Error e

(* Collecting *)

let all = fun results ->
  let rec go = fun acc ->
    fun __tmp1 ->
      match __tmp1 with
      | [] -> Ok (List.reverse acc)
      | (Ok x) :: rest -> go (x :: acc) rest
      | (Error e) :: _ -> Error e
  in
  go [] results

let both = fun r1 r2 ->
  match (r1, r2) with
  | (Ok x, Ok y) -> Ok (x, y)
  | (Error e, _) -> Error e
  | (_, Error e) -> Error e

(* Misc *)

let fold = fun value ~ok ~error ->
  match value with
  | Ok x -> ok x
  | Error e -> error e

module Syntax = struct
  let ( let* ) t fn = and_then t ~fn
end
