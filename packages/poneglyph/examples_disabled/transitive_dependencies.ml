(** Example 2: Transitive Dependencies with Datalog Rules
    
    This example shows how to:
    - Define Datalog rules for transitive relationships
    - Query transitive dependencies
    - Use multi-attribute queries
*)

open Std
open Std.UUID
open Poneglyph

let main () : (unit, Miniriot.Process.exit_reason) result =
  Log.info "=== Transitive Dependencies Example ===";
  Log.info "";
  
  let graph = create () in
  let source = Uri.of_string "example:transitive" in
  
  (* Create a dependency chain: A -> B -> C -> D *)
  let module_a = Uri.of_string "module:A" in
  let module_b = Uri.of_string "module:B" in
  let module_c = Uri.of_string "module:C" in
  let module_d = Uri.of_string "module:D" in
  let depends_on = Uri.of_string "depends_on" in
  
  Log.info "Creating dependency chain:";
  Log.info "  A -> B -> C -> D";
  Log.info "";
  
  let facts = [
    Fact.make ~source ~entity:module_a ~attribute:depends_on 
      ~value:(Fact.Uri module_b)
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ());
    Fact.make ~source ~entity:module_b ~attribute:depends_on 
      ~value:(Fact.Uri module_c)
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ());
    Fact.make ~source ~entity:module_c ~attribute:depends_on 
      ~value:(Fact.Uri module_d)
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ());
  ] in
  
  let _ = state graph facts in
  
  (* Define transitive closure rules *)
  Log.info "Defining transitive path rules:";
  Log.info "  path(X, Y) :- depends_on(X, Y).";
  Log.info "  path(X, Z) :- depends_on(X, Y), path(Y, Z).";
  Log.info "";
  
  let rules = [
    "path(X, Y) :- depends_on(X, Y).";
    "path(X, Z) :- depends_on(X, Y), path(Y, Z).";
  ] in
  
  (* Query: What can A reach transitively? *)
  Log.info "Query: What modules can A reach transitively?";
  let _ = match query_entities graph
    ~rules
    ~query_str:"path(\"module:A\", X)"
    ~var:"X"
  with
  | Error e -> Log.error ("Query failed: " ^ e)
  | Ok entities ->
      Log.info "Modules reachable from A:";
      entities
      |> Iter.MutIterator.for_each ~fn:(fun uri ->
          Log.info ("  -> " ^ Uri.to_string uri)
        );
      Log.info ""
  in
  
  (* Query all transitive paths *)
  Log.info "Query: All transitive paths in the system";
  let _ = match query_entities graph
    ~rules
    ~query_str:"path(X, Y)"
    ~var:"X"
  with
  | Error e -> Log.error ("Query failed: " ^ e)
  | Ok entities ->
      let unique = entities 
        |> Iter.MutIterator.to_list 
        |> List.sort_uniq Uri.compare in
      Log.info "Modules that depend on something:";
      List.iter (fun uri ->
        Log.info ("  " ^ Uri.to_string uri)
      ) unique;
      Log.info ""
  in
  
  Log.info "=== Example Complete ===";
  Ok ()

let () = Miniriot.run
  ~main:(fun ~args:_ -> main ())
  ~args:Env.args ()
