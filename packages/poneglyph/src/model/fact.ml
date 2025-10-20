open Std
open Std.UUID

type value =
  | String of string
  | Int of int
  | Bool of bool
  | Float of float
  | Uri of Uri.t
  | DateTime of Datetime.t

type t = {
  fact_uri : Uri.t;
  entity : Uri.t;
  attribute : Uri.t;
  value : value;
  stated_at : Datetime.t;
  tx_id : int;
  retracted : bool;
}

let generate_fact_uri () =
  let uuid = UUID.v7 () in
  Uri.of_string (format "@fact:%s" (UUID.to_string uuid))

let make ~entity ~attribute ~value ~stated_at ~tx_id =
  {
    fact_uri = generate_fact_uri ();
    entity;
    attribute;
    value;
    stated_at;
    tx_id;
    retracted = false;
  }

let for_entity entity fact_builders = List.map (fun f -> f entity) fact_builders

let rec value_to_string = function
  | String s -> format "\"%s\"" s
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Float f -> string_of_float f
  | Uri u -> Uri.to_string u
  | DateTime dt -> Datetime.to_iso8601 dt
