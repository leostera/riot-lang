open Std

(** Interner for URIs - shared across all stores for now *)
module Interner = struct
  type t = {
    string_to_id : (string, int) Hashtbl.t;
    id_to_string : (int, string) Hashtbl.t;
    mutable next_id : int;
  }

  let create () =
    {
      string_to_id = Hashtbl.create 1000;
      id_to_string = Hashtbl.create 1000;
      next_id = 1;
    }

  let intern interner str =
    match Hashtbl.find_opt interner.string_to_id str with
    | Some id -> id
    | None ->
        let id = interner.next_id in
        interner.next_id <- id + 1;
        Hashtbl.add interner.string_to_id str id;
        Hashtbl.add interner.id_to_string id str;
        id

  let to_string interner id =
    match Hashtbl.find_opt interner.id_to_string id with
    | Some str -> str
    | None -> format "<unknown-uri-%d>" id
end

(** Global interner - URIs are interned globally *)
let global_interner = Interner.create ()

module Uri = struct
  type t = int

  let of_string str = Interner.intern global_interner str

  let to_string id = Interner.to_string global_interner id

  let equal = Int.equal

  let compare = Int.compare
end

module Value = struct
  type t =
    | String of string
    | Int of int
    | Bool of bool
    | Float of float
    | Uri of Uri.t
    | DateTime of Datetime.t
    | List of t list

  let rec to_string = function
    | String s -> format "\"%s\"" s
    | Int i -> string_of_int i
    | Bool b -> string_of_bool b
    | Float f -> string_of_float f
    | Uri u -> Uri.to_string u
    | DateTime dt -> Datetime.to_iso8601 dt
    | List vs -> format "[%s]" (String.concat ", " (List.map to_string vs))
end

module Fact = struct
  type t = { entity : Uri.t; attribute : Uri.t; value : Value.t }

  let fact entity attribute value = { entity; attribute; value }

  let ( let+ ) entity (attribute, value) = { entity; attribute; value }
end

(** Graph store *)
type t = { facts : (Uri.t * Uri.t, Value.t) Hashtbl.t }

let create () = { facts = Hashtbl.create 10000 }

let state store facts =
  List.iter
    (fun fact ->
      Hashtbl.replace store.facts (fact.Fact.entity, fact.Fact.attribute)
        fact.Fact.value)
    facts

let get store ~entity ~attr = Hashtbl.find_opt store.facts (entity, attr)

let exists store entity =
  Hashtbl.fold
    (fun (e, _attr) _value acc -> acc || Uri.equal e entity)
    store.facts false
