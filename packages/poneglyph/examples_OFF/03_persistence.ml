(** Example 3: Persistence - Saving and loading graphs from disk *)

open Std
open Poneglyph

let db_path = "/tmp/poneglyph_example.db"

let () =
  Log.info "=== Example 3: Persistence ===";

  (* Clean up any existing database *)
  let _ = Fs.remove_file (Path.v db_path) in

  (* Create a persistent graph *)
  Log.info ("Creating persistent graph at " ^ db_path);
  let graph = create_persistent db_path in

  (* Add some data *)
  let entity1 = Uri.of_string "example:entity:1" in
  let entity2 = Uri.of_string "example:entity:2" in
  let name_attr = Uri.of_string "example:name" in
  let count_attr = Uri.of_string "example:count" in

  let facts =
    [
      Fact.make ~entity:entity1 ~attribute:name_attr
        ~value:(Fact.String "First Entity") ~stated_at:(Datetime.now ())
        ~tx_id:0;
      Fact.make ~entity:entity1 ~attribute:count_attr ~value:(Fact.Int 42)
        ~stated_at:(Datetime.now ()) ~tx_id:0;
      Fact.make ~entity:entity2 ~attribute:name_attr
        ~value:(Fact.String "Second Entity") ~stated_at:(Datetime.now ())
        ~tx_id:0;
      Fact.make ~entity:entity2 ~attribute:count_attr ~value:(Fact.Int 100)
        ~stated_at:(Datetime.now ()) ~tx_id:0;
    ]
  in

  let _ = state graph facts in
  Log.info ("Stated " ^ string_of_int (List.length facts) ^ " facts (auto-saved to disk)");

  let before_stats = stats graph in
  Log.info "Stats before reload:";
  List.iter (fun (k, v) -> Log.info ("  " ^ k ^ ": " ^ string_of_int v)) before_stats;

  (* Drop the graph reference - simulate program restart *)
  Log.info "";
  Log.info "Simulating program restart...";
  Log.info "";

  (* Load from disk *)
  Log.info ("Loading graph from " ^ db_path);
  let loaded_graph = load db_path in

  let after_stats = stats loaded_graph in
  Log.info "Stats after reload:";
  List.iter (fun (k, v) -> Log.info ("  " ^ k ^ ": " ^ string_of_int v)) after_stats;

  (* Verify data persisted *)
  (match get loaded_graph ~entity:entity1 ~attr:name_attr with
  | Some (Fact.String name) -> Log.info ("✓ Entity 1 name: " ^ name)
  | _ -> Log.warn "✗ Entity 1 name not found");

  (match get loaded_graph ~entity:entity2 ~attr:count_attr with
  | Some (Fact.Int count) -> Log.info ("✓ Entity 2 count: " ^ string_of_int count)
  | _ -> Log.warn "✗ Entity 2 count not found");

  (* Add more data to the loaded graph *)
  let entity3 = Uri.of_string "example:entity:3" in
  let new_facts =
    [
      Fact.make ~entity:entity3 ~attribute:name_attr
        ~value:(Fact.String "Third Entity") ~stated_at:(Datetime.now ())
        ~tx_id:0;
    ]
  in

  let _ = state loaded_graph new_facts in
  Log.info "Added entity 3 (auto-saved)";

  let final_stats = stats loaded_graph in
  Log.info "Final stats:";
  List.iter (fun (k, v) -> Log.info ("  " ^ k ^ ": " ^ string_of_int v)) final_stats;

  Log.info "=== Example 3 Complete ==="
