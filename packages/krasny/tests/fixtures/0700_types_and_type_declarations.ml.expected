(* TODO(@leostera): we need to add more examples here for:

   - [x] type alias with params
   - [x] constraints
   - [x] GADTs
   - [x] large number of constructors for variants (mixed with GADT fields too)
   - [x] inline records for variants
   - [x] large number of fields in a record (think ~5/25/100 fields)
   - [x] records with very long field names and complex field types
   - [x] abstract types
   - [x] records with local universal quantification
   - [x] polymorphic variants (closed, open (> and <), extensions)
   - [x] examples of function types with normal, named, and optional parameters
   - [x] recursive types (self-recursion and `type-and` definitions)
   - [x] external definitions

  *)

type id = int

type 'a box = 'a

type point = {
  x : int;
  y : int;
}

type huge_record = {
  first_name : string;
  last_name : string;
  age : int;
  city : string;
  country : string;
  postal_code : string;
}

type configuration_snapshot = {
  request_correlation_identifier_for_observability_pipeline : string;
  decoded_response_payloads_grouped_by_endpoint_name :
    (string * (int * string option) list) list;
  retry_schedule_overrides_by_http_status_code :
    (int * (float * bool) list) list option;
  transform_domain_events_into_renderable_view_models :
    (message list -> (string * int) list) option;
}

type color =
  | Red
  | Green
  | Blue
  | Cyan
  | Magenta
  | Yellow
  | Black

type 'a tree =
  | Leaf of 'a
  | Node of 'a tree * 'a tree

type chain = {
  value : int;
  next : chain option;
}

type ('ok, 'err) result_like =
  | Ok of 'ok
  | Error of 'err

type message =
  | Ping of { id : int; sent_at : float }
  | Pong of { id : int; ok : bool }

type opaque

type mapper = {
  run : 'a. 'a list -> 'a list;
}

type 'a constrained = 'a constraint 'a = int

type closed_poly = [ `A | `B of int ]

type open_poly = [> `A | `B of string ]

type limited_poly = [< `A | `B | `C ]

type extended_poly = [ closed_poly | `D of string ]

type printer = string -> width:int -> ?indent:int -> unit -> string

type 'a decoder = string -> ('a, string) result

type response = {
  status : (int, string) result;
  body : string option;
}

type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr
  | Pair : 'a expr * 'b expr -> ('a * 'b) expr

type node =
  | File of string
  | Directory of string * forest
and forest = node list

external unsafe_string_get : string -> int -> char = "%string_safe_get"

external int64_of_nativeint : nativeint -> int64 = "%nativeint_to_int64"
