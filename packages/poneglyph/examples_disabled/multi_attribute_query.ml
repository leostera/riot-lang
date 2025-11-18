(** Example 3: Multi-Attribute Queries
    
    This example shows how to:
    - Store multiple types of facts about entities
    - Query based on multiple attributes
    - Find entities that match complex criteria
*)

open Std
open Std.UUID
open Poneglyph

let main () : (unit, Miniriot.Process.exit_reason) result =
  Log.info "=== Multi-Attribute Query Example ===";
  Log.info "";
  
  let graph = create () in
  let source = Uri.of_string "example:multi" in
  
  (* Define files with multiple attributes *)
  let file_a = Uri.of_string "file:src/main.ml" in
  let file_b = Uri.of_string "file:src/utils.ml" in
  let file_c = Uri.of_string "file:test/test.ml" in
  
  let formatted_attr = Uri.of_string "formatted" in
  let has_tests_attr = Uri.of_string "has_tests" in
  let depends_on = Uri.of_string "depends_on" in
  
  Log.info "Creating file metadata:";
  Log.info "  main.ml: formatted=true, has_tests=false, depends_on utils.ml";
  Log.info "  utils.ml: formatted=true, has_tests=true";
  Log.info "  test.ml: formatted=false, has_tests=true";
  Log.info "";
  
  let facts = [
    (* main.ml *)
    Fact.make ~source ~entity:file_a ~attribute:formatted_attr
      ~value:(Fact.String "true")
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ());
    Fact.make ~source ~entity:file_a ~attribute:has_tests_attr
      ~value:(Fact.String "false")
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ());
    Fact.make ~source ~entity:file_a ~attribute:depends_on
      ~value:(Fact.Uri file_b)
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ());
    
    (* utils.ml *)
    Fact.make ~source ~entity:file_b ~attribute:formatted_attr
      ~value:(Fact.String "true")
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ());
    Fact.make ~source ~entity:file_b ~attribute:has_tests_attr
      ~value:(Fact.String "true")
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ());
    
    (* test.ml *)
    Fact.make ~source ~entity:file_c ~attribute:formatted_attr
      ~value:(Fact.String "false")
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ());
    Fact.make ~source ~entity:file_c ~attribute:has_tests_attr
      ~value:(Fact.String "true")
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ());
  ] in
  
  let _ = state graph facts in
  
  (* Query 1: Find formatted files *)
  Log.info "Query 1: Which files are formatted?";
  let _ = match query_entities graph
    ~rules:[]
    ~query_str:"formatted(F, \"true\")"
    ~var:"F"
  with
  | Error e -> Log.error ("Query failed: " ^ e)
  | Ok files ->
      Log.info "Formatted files:";
      files
      |> Iter.MutIterator.for_each ~fn:(fun uri ->
          Log.info ("  ✓ " ^ Uri.to_string uri)
        );
      Log.info ""
  in
  
  (* Query 2: Find formatted files WITH tests *)
  Log.info "Query 2: Which files are formatted AND have tests?";
  let _ = match query_entities graph
    ~rules:[
      "good_file(F) :- formatted(F, \"true\"), has_tests(F, \"true\").";
    ]
    ~query_str:"good_file(F)"
    ~var:"F"
  with
  | Error e -> Log.error ("Query failed: " ^ e)
  | Ok files ->
      Log.info "Well-maintained files:";
      files
      |> Iter.MutIterator.for_each ~fn:(fun uri ->
          Log.info ("  ✓ " ^ Uri.to_string uri)
        );
      Log.info ""
in
  
  (* Query 3: Find formatted files that have dependencies *)
  Log.info "Query 3: Formatted files with dependencies?";
  let _ = match query_entities graph
    ~rules:[
      "formatted_with_deps(F) :- formatted(F, \"true\"), depends_on(F, _).";
    ]
    ~query_str:"formatted_with_deps(F)"
    ~var:"F"
  with
  | Error e -> Log.error ("Query failed: " ^ e)
  | Ok files ->
      Log.info "Formatted files with dependencies:";
      files
      |> Iter.MutIterator.for_each ~fn:(fun uri ->
          Log.info ("  ✓ " ^ Uri.to_string uri)
        );
      Log.info "";
  in
  
  (* Query 4: Get full facts for query results *)
  Log.info "Query 4: Get all facts for formatted files";
    let _ = match query_facts graph
    ~rules:[]
    ~query_str:"formatted(F, \"true\")"
    ~entities_from:"F"
  with
  | Error e -> Log.error ("Query failed: " ^ e)
  | Ok facts ->
      Log.info "All facts for formatted files:";
      facts
      |> Iter.MutIterator.for_each ~fn:(fun fact ->
          let entity_str = Uri.to_string fact.Fact.entity in
          let attr_str = Uri.to_string fact.Fact.attribute in
          let value_str = match fact.Fact.value with
            | Fact.String s -> s
            | Fact.Uri u -> Uri.to_string u
            | _ -> "?" in
          Log.info ("  " ^ entity_str ^ "." ^ attr_str ^ " = " ^ value_str)
        );
      Log.info ""
  in
      Log.info "=== Example Complete ===";
      Ok ()
  ;;

let () = Miniriot.run
  ~main:(fun ~args:_ -> main ())
  ~args:Env.args ()
