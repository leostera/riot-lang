open Std

type t =
  | Null
  | Int of int
  | Float of float
  | String of string
  | Bool of bool
  | Bytes of bytes
  | Timestamp of Time.Instant.t

let null = Null
let int n = Int n
let string s = String s
let bool b = Bool b
let float f = Float f
let bytes b = Bytes b
let timestamp t = Timestamp t
let to_int = function Int n -> Some n | _ -> None
let to_string_value = function String s -> Some s | _ -> None
let to_bool = function Bool b -> Some b | _ -> None
let to_float = function Float f -> Some f | _ -> None
let to_bytes = function Bytes b -> Some b | _ -> None
let to_timestamp = function Timestamp t -> Some t | _ -> None
let is_null = function Null -> true | _ -> false

let to_string = function
  | Null -> "NULL"
  | Int n -> string_of_int n
  | Float f -> string_of_float f
  | String s -> Printf.sprintf "\"%s\"" s
  | Bool b -> string_of_bool b
  | Bytes b -> Printf.sprintf "<bytes:%d>" (Bytes.length b)
  | Timestamp _ -> "<timestamp>"

let equal a b =
  match (a, b) with
  | Null, Null -> true
  | Int x, Int y -> x = y
  | Float x, Float y -> x = y
  | String x, String y -> x = y
  | Bool x, Bool y -> x = y
  | Bytes x, Bytes y -> Bytes.equal x y
  | Timestamp x, Timestamp y -> Time.Instant.equal x y
  | _ -> false

let compare a b =
  match (a, b) with
  | Null, Null -> 0
  | Null, _ -> -1
  | _, Null -> 1
  | Int x, Int y -> Int.compare x y
  | Float x, Float y -> Float.compare x y
  | String x, String y -> String.compare x y
  | Bool x, Bool y -> Bool.compare x y
  | Bytes x, Bytes y -> Bytes.compare x y
  | Timestamp x, Timestamp y -> Time.Instant.compare x y
  | Int _, _ -> -1
  | _, Int _ -> 1
  | Float _, _ -> -1
  | _, Float _ -> 1
  | String _, _ -> -1
  | _, String _ -> 1
  | Bool _, _ -> -1
  | _, Bool _ -> 1
  | Bytes _, _ -> -1
  | _, Bytes _ -> 1
