(** Example 1: Basic Datalog Query
    
    This example shows how to:
    - Create a graph with module dependencies
    - Query direct dependencies using Datalog
    - Use query_entities to extract URIs
*)

open Std
open Std.UUID
open Poneglyph

let main () : (unit, Miniriot.Process.exit_reason) result =
  Log.info "=== Basic Datalog Query Example ===";
  Log.info "";
  
  (* Create an in-memory graph *)
  let graph = create () in
  let source = Uri.of_string "example:basic" in
  
  (* Define some modules and their dependencies *)
  let module_a = Uri.of_string "module:A" in
  let module_b = Uri.of_string "module:B" in
  let module_c = Uri.of_string "module:C" in
  let depends_on = Uri.of_string "depends_on" in
  
  Log.info "Creating dependency facts:";
  Log.info "  A depends_on B";
  Log.info "  B depends_on C";
  Log.info "";
  
  (* State facts: A -> B -> C *)
  let facts = [
    Fact.make ~source ~entity:module_a ~attribute:depends_on 
      ~value:(Fact.Uri module_b)
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ());
    Fact.make ~source ~entity:module_b ~attribute:depends_on 
      ~value:(Fact.Uri module_c)
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ());
  ] in
  
  let _ = state graph facts in
  
  (* Query: Find all dependency relationships *)
  Log.info "Query: depends_on(X, Y)";
  let _ = match query graph ~rules:[] ~query:"depends_on(X, Y)" with
  | Error e -> Log.error ("Query failed: " ^ e)
  | Ok results ->
      Log.info "Results:";
      results
      |> Iter.MutIterator.for_each ~fn:(fun _subst ->
          (* We could inspect substitutions here, but for simplicity
             we'll just count them *)
          ()
        );
      
      (* Rerun to count (iterator is consumed) *)
      (match query graph ~rules:[] ~query:"depends_on(X, Y)" with
      | Ok results -> 
          let count = Iter.MutIterator.count results in
          Log.info ("  Found " ^ string_of_int count ^ " dependency relationships")
      | _ -> ());
      
      Log.info ""
  in
  
  (* Use query_entities for easier URI extraction *)
  Log.info "Query: What does module A depend on?";
  let _ = match query_entities graph
    ~rules:[]
    ~query_str:"depends_on(\"module:A\", X)"
    ~var:"X"
  with
  | Error e -> Log.error ("Query failed: " ^ e)
  | Ok entities ->
      Log.info "Results:";
      entities
      |> Iter.MutIterator.for_each ~fn:(fun uri ->
          Log.info ("  A depends on: " ^ Uri.to_string uri)
        );
      Log.info ""
  in
  
  Log.info "=== Example Complete ===";
  Ok ()

let () = Miniriot.run
  ~main:(fun ~args:_ -> main ())
  ~args:Env.args ()
