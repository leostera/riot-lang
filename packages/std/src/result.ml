(** Result type for error handling *)

type ('a, 'e) t = ('a, 'e) Stdlib.result = Ok of 'a | Error of 'e

let ok x = Ok x
let err e = Error e
let error = err (* Alias for compatibility *)
let is_ok = function Ok _ -> true | Error _ -> false
let is_error = function Ok _ -> false | Error _ -> true
let map f = function Ok x -> Ok (f x) | Error e -> Error e
let map_error f = function Ok x -> Ok x | Error e -> Error (f e)
let bind r f = match r with Ok x -> f x | Error e -> Error e
let ( >>= ) = bind
let ( >>| ) r f = map f r
let get_ok = function Ok x -> Some x | Error _ -> None
let get_error = function Ok _ -> None | Error e -> Some e

let get_ok_exn = function
  | Ok x -> x
  | Error _ -> failwith "Result.get_ok_exn: not Ok"

let get_error_exn = function
  | Ok _ -> failwith "Result.get_error_exn: not Error"
  | Error e -> e

let fold ~ok ~error = function Ok x -> ok x | Error e -> error e
let iter f = function Ok x -> f x | Error _ -> ()
let iter_error f = function Ok _ -> () | Error e -> f e
let to_option = function Ok x -> Some x | Error _ -> None
let of_option ~error = function Some x -> Ok x | None -> Error error
let join = function Ok r -> r | Error e -> Error e

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

let unwrap = function
  | Ok x -> x
  | Error _ -> failwith "called Result.unwrap on an Error value"

let unwrap_err = function
  | Error e -> e
  | Ok _ -> failwith "called Result.unwrap_err on an Ok value"
