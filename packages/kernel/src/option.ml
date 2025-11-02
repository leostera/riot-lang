(** Option type utilities *)

type 'a t = 'a option = None | Some of 'a

(* Constructors *)
let some x = Some x
let none = None

(* Querying *)
let is_some = function Some _ -> true | None -> false
let is_none = function Some _ -> false | None -> true
let is_some_and f = function Some x -> f x | None -> false
let is_none_or f = function None -> true | Some x -> f x

(* Transforming *)
let map f = function Some x -> Some (f x) | None -> None
let map_or ~default f = function Some x -> f x | None -> default
let map_or_default ~default f = function Some x -> f x | None -> default ()
let map_or_else ~default f = function Some x -> f x | None -> default ()

(* Chaining *)
let and_ opt1 opt2 = match opt1 with Some _ -> opt2 | None -> None
let and_then opt f = match opt with Some x -> f x | None -> None
let or_ opt1 opt2 = match opt1 with Some _ -> opt1 | None -> opt2
let or_else opt f = match opt with Some _ -> opt | None -> f ()

let xor opt1 opt2 =
  match (opt1, opt2) with
  | Some _, None -> opt1
  | None, Some _ -> opt2
  | _ -> None

(* Extracting values *)
let get = function
  | Some x -> x
  | None -> invalid_arg "Option.get"

let unwrap = function
  | Some x -> x
  | None ->
      let exception Panic of string in
      raise (Panic "called Option.unwrap on a None value")

let unwrap_or ~default = function Some x -> x | None -> default
let unwrap_or_default ~default = function Some x -> x | None -> default ()
let unwrap_or_else ~fn = function Some x -> x | None -> fn ()

let expect ~msg = function
  | Some x -> x
  | None ->
      let exception Panic of string in
      raise (Panic msg)

let unwrap_none = function
  | None -> ()
  | Some _ ->
      let exception Panic of string in
      raise (Panic "called Option.unwrap_none on a Some value")

(* Inspecting *)
let inspect f opt =
  (match opt with Some x -> f x | None -> ());
  opt

(* Iterating *)
let iter f = function Some x -> f x | None -> ()

(* Converting *)
let ok_or ~error = function Some x -> Result.Ok x | None -> Result.Error error

let ok_or_else ~error = function
  | Some x -> Result.Ok x
  | None -> Result.Error (error ())

let to_result ~error = ok_or ~error
let to_list = function Some x -> [ x ] | None -> []

let transpose = function
  | Some (Result.Ok x) -> Result.Ok (Some x)
  | Some (Result.Error e) -> Result.Error e
  | None -> Result.Ok None

(* Filtering *)
let filter pred = function Some x when pred x -> Some x | _ -> None

(* Flattening *)
let flatten = function Some opt -> opt | None -> None

(* Zipping *)
let zip opt1 opt2 =
  match (opt1, opt2) with Some x, Some y -> Some (x, y) | _ -> None

let zip_with f opt1 opt2 =
  match (opt1, opt2) with Some x, Some y -> Some (f x y) | _ -> None

let unzip = function Some (x, y) -> (Some x, Some y) | None -> (None, None)

(* Collecting *)
let all options =
  let rec go acc = function
    | [] -> Some (List.rev acc)
    | Some x :: rest -> go (x :: acc) rest
    | None :: _ -> None
  in
  go [] options
