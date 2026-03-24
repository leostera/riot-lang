(* TODO(@leostera): we need to add more examples here for:

   - [x] type alias with params
   - [x] constraints
   - [x] GADTs
   - [x] large number of constructors for variants (mixed with GADT fields too)
   - [x] inline records for variants
   - [x] large number of fields in a record (think ~5/25/100 fields)
   - [x] abstract types
   - [x] records with local universal quantification
   - [x] polymorphic variants (closed, open (> and <), extensions)
   - [x] examples of function types with normal, named, and optional parameters
   - [x] recursive types (self-recursion and `type-and` definitions)

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
