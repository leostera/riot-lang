open Std
open Std.IO

(* Simple entity record without graph dependencies *)
type t = {
  uri : Uri.t;
  kind : Uri.t option;
  facts : (Uri.t * Fact.value) list;
}

let make ~uri ~kind ~facts = { uri; kind; facts }

let get_attr entity attr =
  List.find_map
    (fun (a, v) -> if Uri.equal a attr then Some v else None)
    entity.facts

let to_string entity =
  let buffer = Buffer.create 256 in
  Buffer.add_string buffer ("Entity: " ^ Uri.to_string entity.uri ^ "\n");
  (match entity.kind with
  | Some k -> Buffer.add_string buffer ("Kind: " ^ Uri.to_string k ^ "\n")
  | None -> ());
  Buffer.add_string buffer "Facts:\n";
  List.iter
    (fun (attr, value) ->
      Buffer.add_string buffer
        ("  " ^ Uri.to_string attr ^ ": " ^ Fact.value_to_string value ^ "\n"))
    entity.facts;
  Buffer.contents buffer
