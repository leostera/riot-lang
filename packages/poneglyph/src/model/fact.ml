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
  source_uri : Uri.t;
  entity : Uri.t;
  attribute : Uri.t;
  value : value;
  stated_at : Datetime.t;
  tx_id : UUID.t;  (** Transaction ID - UUIDv7 for time-ordered, restart-safe IDs *)
  retracted : bool;
}

let generate_fact_uri () =
  let uuid = UUID.v7 () in
  Uri.of_string ("@fact:" ^ UUID.to_string uuid)

let make ~source ~entity ~attribute ~value ~stated_at ~tx_id =
  {
    fact_uri = generate_fact_uri ();
    source_uri = source;
    entity;
    attribute;
    value;
    stated_at;
    tx_id;
    retracted = false;
  }

let for_entity entity fact_builders = List.map (fun f -> f entity) fact_builders

let rec value_to_string = function
  | String s -> "\"" ^ s ^ "\""
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Float f -> string_of_float f
  | Uri u -> Uri.to_string u
  | DateTime dt -> Datetime.to_iso8601 dt

let value_equal v1 v2 =
  match (v1, v2) with
  | String s1, String s2 -> s1 = s2
  | Int i1, Int i2 -> i1 = i2
  | Bool b1, Bool b2 -> b1 = b2
  | Float f1, Float f2 -> f1 = f2
  | Uri u1, Uri u2 -> Uri.equal u1 u2
  | DateTime dt1, DateTime dt2 -> Datetime.to_timestamp dt1 = Datetime.to_timestamp dt2
  | _ -> false

let value_hash = function
  | String s -> 
      (* Simple string hash *)
      let h = ref 0 in
      String.iter (fun c -> h := (!h * 31) + Char.code c) s;
      !h
  | Int i -> i
  | Bool b -> if b then 1 else 0
  | Float f -> int_of_float (f *. 1000.0)
  | Uri u -> 
      (* Use first 8 bytes of SHA-256 hash for hashing *)
      let module Bytes = Kernel.IO.Bytes in
      Int64.to_int (Bytes.get_int64_be u.Uri.sha256 0)
  | DateTime dt -> int_of_float (Datetime.to_timestamp dt)
