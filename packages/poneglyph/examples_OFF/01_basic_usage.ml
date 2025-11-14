(** Example 1: Basic Usage - Creating a graph, stating facts, and querying *)

open Std
open Poneglyph

let () =
  Log.info "=== Example 1: Basic Usage ===";

  (* Create an in-memory graph *)
  let graph = create () in
  Log.info "Created in-memory graph";

  (* Create a simple URI for a file *)
  let file_uri = Uri.of_string "example:file:main.ml" in
  let hash_attr = Uri.of_string "example:content_hash" in
  let size_attr = Uri.of_string "example:size_bytes" in

  (* State some facts *)
  let facts =
    [
      Fact.make ~entity:file_uri ~attribute:hash_attr
        ~value:(Fact.String "abc123def456") ~stated_at:(Datetime.now ())
        ~tx_id:0;
      Fact.make ~entity:file_uri ~attribute:size_attr ~value:(Fact.Int 4096)
        ~stated_at:(Datetime.now ()) ~tx_id:0;
    ]
  in

  let tx_id = state graph facts in
  Log.info ("Stated " ^ string_of_int (List.length facts) ^ " facts in transaction " ^ string_of_int tx_id);

  (* Query the facts *)
  (match get graph ~entity:file_uri ~attr:hash_attr with
  | Some (Fact.String hash) -> Log.info ("Content hash: " ^ hash)
  | _ -> Log.warn "Hash not found");

  (match get graph ~entity:file_uri ~attr:size_attr with
  | Some (Fact.Int size) -> Log.info ("File size: " ^ string_of_int size ^ " bytes")
  | _ -> Log.warn "Size not found");

  (* Check if entity exists *)
  if exists graph file_uri then Log.info "Entity exists!"
  else Log.warn "Entity not found";

  (* Get all facts about the entity *)
  let all_facts = get_current_facts graph ~entity:file_uri in
  Log.info ("Entity has " ^ string_of_int (List.length all_facts) ^ " facts");

  (* Show statistics *)
  let stats_list = stats graph in
  Log.info "Graph statistics:";
  List.iter
    (fun (key, value) -> Log.info ("  " ^ key ^ ": " ^ string_of_int value))
    stats_list;

  Log.info "=== Example 1 Complete ==="
