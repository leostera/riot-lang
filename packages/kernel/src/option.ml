open Global0

(** Option type utilities *)
type 'a t = 'a option =
  None
  | Some of 'a

(* Constructors *)

let some = fun x -> Some x

let none = None

(* Querying *)

let equal = fun eq opt1 opt2 ->
  match (opt1, opt2) with
  | None, None -> true
  | Some x, Some y -> eq x y
  | _ -> false

let is_some = function
  | Some _ -> true
  | None -> false

let is_none = function
  | Some _ -> false
  | None -> true

let is_some_and = fun f ->
  function
  | Some x -> f x
  | None -> false

let is_none_or = fun f ->
  function
  | None -> true
  | Some x -> f x

(* Transforming *)

let map = fun f ->
  function
  | Some x -> Some (f x)
  | None -> None

let map_or = fun ~default f ->
  function
  | Some x -> f x
  | None -> default

let map_or_default = fun ~default f ->
  function
  | Some x -> f x
  | None -> default ()

let map_or_else = fun ~default f ->
  function
  | Some x -> f x
  | None -> default ()

(* Chaining *)

let and_ = fun opt1 opt2 ->
  match opt1 with
  | Some _ -> opt2
  | None -> None

let and_then = fun opt f ->
  match opt with
  | Some x -> f x
  | None -> None

let or_ = fun opt1 opt2 ->
  match opt1 with
  | Some _ -> opt1
  | None -> opt2

let or_else = fun opt f ->
  match opt with
  | Some _ -> opt
  | None -> f ()

let xor = fun opt1 opt2 ->
  match (opt1, opt2) with
  | Some _, None -> opt1
  | None, Some _ -> opt2
  | _ -> None

(* Extracting values *)

let unwrap = function
  | Some x -> x
  | None -> (panic "called Option.unwrap on a None value")

let unwrap_or = fun ~default ->
  function
  | Some x -> x
  | None -> default

let unwrap_or_default = fun ~default ->
  function
  | Some x -> x
  | None -> default ()

let unwrap_or_else = fun ~fn ->
  function
  | Some x -> x
  | None -> fn ()

let expect = fun ~msg ->
  function
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

let inspect = fun f opt ->
  (
    match opt with
    | Some x -> f x
    | None -> ()
  );
  opt

(* Iterating *)

let iter = fun f ->
  function
  | Some x -> f x
  | None -> ()

(* Converting *)

let ok_or = fun ~error ->
  function
  | Some x -> Result.Ok x
  | None -> Result.Error error

let ok_or_else = fun ~error ->
  function
  | Some x -> Result.Ok x
  | None -> Result.Error (error ())

let to_result = fun ~error -> ok_or ~error

let to_list = function
  | Some x -> [ x ]
  | None -> []

let transpose = function
  | Some (Result.Ok x) -> Result.Ok (Some x)
  | Some (Result.Error e) -> Result.Error e
  | None -> Result.Ok None

(* Filtering *)

let filter = fun pred ->
  function
  | Some x when pred x -> Some x
  | _ -> None

(* Flattening *)

let flatten = function
  | Some opt -> opt
  | None -> None

(* Zipping *)

let zip = fun opt1 opt2 ->
  match (opt1, opt2) with
  | Some x, Some y -> Some (x, y)
  | _ -> None

let zip_with = fun f opt1 opt2 ->
  match (opt1, opt2) with
  | Some x, Some y -> Some (f x y)
  | _ -> None

let unzip = function
  | Some (x, y) -> (Some x, Some y)
  | None -> (None, None)

(* Collecting *)

let all = fun options ->
  let rec go = fun acc ->
    function
    | [] -> Some (Stdlib.List.rev acc)
    | Some x :: rest -> go (x :: acc) rest
    | None :: _ -> None
  in
  go [] options
