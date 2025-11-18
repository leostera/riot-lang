(** Example 4: Build System Metadata Analysis
    
    This example shows a real-world use case:
    - Tracking build artifacts and their dependencies
    - Finding files that need rebuilding
    - Analyzing stale dependencies
*)

open Std
open Std.UUID
open Poneglyph

let main () : (unit, Miniriot.Process.exit_reason) result =
  Log.info "=== Build System Metadata Example ===";
  Log.info "";
  
  let graph = create () in
  let source = Uri.of_string "build:2024-11-14" in
  
  (* Define build artifacts *)
  let src_main = Uri.of_string "file:src/main.ml" in
  let src_utils = Uri.of_string "file:src/utils.ml" in
  let src_types = Uri.of_string "file:src/types.ml" in
  let test_main = Uri.of_string "file:test/test_main.ml" in
  
  (* Attributes *)
  let depends_on = Uri.of_string "depends_on" in
  let hash_attr = Uri.of_string "content_hash" in
  let compiled_attr = Uri.of_string "compiled" in
  let modified_attr = Uri.of_string "modified_time" in
  
  Log.info "Simulating build state:";
  Log.info "  main.ml: hash=abc123, compiled=true, depends on utils.ml and types.ml";
  Log.info "  utils.ml: hash=def456, compiled=true, depends on types.ml";
  Log.info "  types.ml: hash=ghi789, compiled=false (MODIFIED!)";
  Log.info "  test_main.ml: hash=jkl012, compiled=true, depends on main.ml";
  Log.info "";
  
  let facts = [
    (* main.ml *)
    Fact.make ~source ~entity:src_main ~attribute:hash_attr
      ~value:(Fact.String "abc123")
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ());
    Fact.make ~source ~entity:src_main ~attribute:compiled_attr
      ~value:(Fact.String "true")
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ());
    Fact.make ~source ~entity:src_main ~attribute:depends_on
      ~value:(Fact.Uri src_utils)
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ());
    Fact.make ~source ~entity:src_main ~attribute:depends_on
      ~value:(Fact.Uri src_types)
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ());
    
    (* utils.ml *)
    Fact.make ~source ~entity:src_utils ~attribute:hash_attr
      ~value:(Fact.String "def456")
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ());
    Fact.make ~source ~entity:src_utils ~attribute:compiled_attr
      ~value:(Fact.String "true")
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ());
    Fact.make ~source ~entity:src_utils ~attribute:depends_on
      ~value:(Fact.Uri src_types)
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ());
    
    (* types.ml - MODIFIED, needs recompile *)
    Fact.make ~source ~entity:src_types ~attribute:hash_attr
      ~value:(Fact.String "ghi789")
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ());
    Fact.make ~source ~entity:src_types ~attribute:compiled_attr
      ~value:(Fact.String "false")
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ());
    
    (* test_main.ml *)
    Fact.make ~source ~entity:test_main ~attribute:hash_attr
      ~value:(Fact.String "jkl012")
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ());
    Fact.make ~source ~entity:test_main ~attribute:compiled_attr
      ~value:(Fact.String "true")
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ());
    Fact.make ~source ~entity:test_main ~attribute:depends_on
      ~value:(Fact.Uri src_main)
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ());
  ] in
  
  let _ = state graph facts in
  
  (* Query 1: Find uncompiled files *)
  Log.info "Query 1: Which files need compilation?";
  let _ = match query_entities graph
    ~rules:[]
    ~query_str:"compiled(F, \"false\")"
    ~var:"F"
  with
  | Error e -> Log.error ("Query failed: " ^ e)
  | Ok files ->
      Log.info "Files needing compilation:";
      files
      |> Iter.MutIterator.for_each ~fn:(fun uri ->
          Log.info ("  🔨 " ^ Uri.to_string uri)
        );
      Log.info ""
  in
  
  (* Query 2: Find files that depend on uncompiled files (need rebuild) *)
  Log.info "Query 2: Which files depend on uncompiled files?";
  Log.info "(These need rebuilding even if they're compiled)";
  let _ = match query_entities graph
    ~rules:[
      "needs_rebuild(F) :- depends_on(F, D), compiled(D, \"false\").";
    ]
    ~query_str:"needs_rebuild(F)"
    ~var:"F"
  with
  | Error e -> Log.error ("Query failed: " ^ e)
  | Ok files ->
      Log.info "Files needing rebuild:";
      files
      |> Iter.MutIterator.for_each ~fn:(fun uri ->
          Log.info ("  🔨 " ^ Uri.to_string uri ^ " (stale dependency)")
        );
      Log.info ""
in
  
  (* Query 3: Transitive rebuild - everything affected *)
  Log.info "Query 3: ALL files affected by stale dependencies?";
    let _ = match query_entities graph
    ~rules:[
      "path(X, Y) :- depends_on(X, Y).";
      "path(X, Z) :- depends_on(X, Y), path(Y, Z).";
      "transitively_stale(F) :- path(F, D), compiled(D, \"false\").";
    ]
    ~query_str:"transitively_stale(F)"
    ~var:"F"
  with
  | Error e -> Log.error ("Query failed: " ^ e)
  | Ok files ->
      Log.info "All affected files (transitive):";
      files
      |> Iter.MutIterator.to_list
      |> List.sort_uniq Uri.compare
      |> List.iter (fun uri ->
          Log.info ("  🔨 " ^ Uri.to_string uri)
        );
      Log.info "";
    Log.info "💡 Insight: types.ml changed, so utils.ml, main.ml, and test_main.ml";
    Log.info "   all need to be rebuilt!";
    Log.info "";
  in
    
    Log.info "=== Example Complete ===";
      Ok ()

let () = Miniriot.run
  ~main:(fun ~args:_ -> main ())
  ~args:Env.args ()
