(** Example 6: Datalog Integration - Poneglyph as Datalog Storage Backend
    
    This example demonstrates how Poneglyph implements Datalog's pluggable
    storage interface, allowing Datalog queries to run directly on Poneglyph
    graphs without copying facts.
    
    NOTE: This shows the storage layer integration. Full Datalog query evaluation
    (with rules and inference) requires the Datalog runtime engine (coming Week 2).
*)

open Std
open Poneglyph

let () =
  Log.info "=== Example 6: Datalog Integration ===";
  Log.info "";

  (* Create a graph with dependency data *)
  let graph = create () in
  Log.info "Created graph";

  (* Define entities *)
  let module_a = Uri.of_string "module:A" in
  let module_b = Uri.of_string "module:B" in
  let module_c = Uri.of_string "module:C" in
  let module_d = Uri.of_string "module:D" in

  (* Define attributes (these become Datalog predicates) *)
  let depends_on = Uri.of_string "depends_on" in
  let formatted = Uri.of_string "formatted" in
  let hash = Uri.of_string "hash" in

  (* State facts into Poneglyph *)
  let facts = [
    (* Dependencies: A -> B -> C, A -> D *)
    Fact.make ~entity:module_a ~attribute:depends_on 
      ~value:(Fact.Uri module_b) ~stated_at:(Datetime.now ()) ~tx_id:0;
    Fact.make ~entity:module_b ~attribute:depends_on 
      ~value:(Fact.Uri module_c) ~stated_at:(Datetime.now ()) ~tx_id:0;
    Fact.make ~entity:module_a ~attribute:depends_on 
      ~value:(Fact.Uri module_d) ~stated_at:(Datetime.now ()) ~tx_id:0;
    
    (* Module properties *)
    Fact.make ~entity:module_a ~attribute:formatted 
      ~value:(Fact.Bool true) ~stated_at:(Datetime.now ()) ~tx_id:0;
    Fact.make ~entity:module_b ~attribute:formatted 
      ~value:(Fact.Bool false) ~stated_at:(Datetime.now ()) ~tx_id:0;
    Fact.make ~entity:module_a ~attribute:hash 
      ~value:(Fact.String "abc123") ~stated_at:(Datetime.now ()) ~tx_id:0;
    Fact.make ~entity:module_b ~attribute:hash 
      ~value:(Fact.String "def456") ~stated_at:(Datetime.now ()) ~tx_id:0;
  ] in

  let _ = state graph facts in
  Log.info ("Stated " ^ string_of_int (List.length facts) ^ " facts");
  Log.info "";

  (* === Datalog Storage Layer === *)
  
  Log.info "Datalog Storage Layer:";
  Log.info "---------------------";
  
  (* List available predicates (attributes become predicates) *)
  let predicates = Datalog.predicates graph in
  Log.info ("Available predicates: [" ^ String.concat "; " predicates ^ "]");
  Log.info "";

  (* Get facts for each predicate *)
  List.iter (fun predicate ->
    Log.info ("Predicate: " ^ predicate);
    let facts = Poneglyph.Datalog.get_facts graph ~predicate in
    let count = Datalog.Relation.length facts in
    Log.info ("  Facts: " ^ string_of_int count);
    
    (* Show the facts *)
    Datalog.Relation.iter (fun tuple ->
      match tuple with
      | [entity; value] ->
          let entity_str = match entity with
            | Datalog.Value.Uri s -> s
            | Datalog.Value.String s -> "\"" ^ s ^ "\""
            | Datalog.Value.Int i -> string_of_int i
          in
          let value_str = match value with
            | Datalog.Value.Uri s -> s
            | Datalog.Value.String s -> "\"" ^ s ^ "\""
            | Datalog.Value.Int i -> string_of_int i
          in
          Log.info ("    " ^ predicate ^ "(" ^ entity_str ^ ", " ^ value_str ^ ")")
      | _ -> ()
    ) facts;
    Log.info ""
  ) predicates;

  (* === Understanding the Mapping === *)
  
  Log.info "Understanding the Mapping:";
  Log.info "-------------------------";
  Log.info "Poneglyph EAV Facts -> Datalog Binary Predicates";
  Log.info "";
  Log.info "Poneglyph:";
  Log.info "  Fact { entity: 'module:A', attribute: 'depends_on', value: Uri 'module:B' }";
  Log.info "";
  Log.info "Becomes Datalog:";
  Log.info "  depends_on('module:A', 'module:B')";
  Log.info "";
  
  (* === Zero-Copy Access === *)
  
  Log.info "Key Benefits:";
  Log.info "-------------";
  Log.info "✓ Zero-copy: Datalog reads directly from Poneglyph's HashMap indices";
  Log.info "✓ Lazy evaluation: Facts only fetched when needed";
  Log.info "✓ Native performance: Uses Poneglyph's existing optimizations";
  Log.info "";

  (* === Future: Full Datalog Queries === *)
  
  Log.info "Coming Soon (Week 2):";
  Log.info "--------------------";
  Log.info "When Datalog runtime is ready, you'll be able to write:";
  Log.info "";
  Log.info "  let results = Poneglyph.Datalog.query graph";
  Log.info "    ~rules:[";
  Log.info "      \"path(X, Y) :- depends_on(X, Y).\";";
  Log.info "      \"path(X, Z) :- depends_on(X, Y), path(Y, Z).\";";
  Log.info "    ]";
  Log.info "    ~query:\"path('module:A', X)\"";
  Log.info "";
  Log.info "  (* Results: all transitive dependencies of module A *)";
  Log.info "";

  Log.info "=== Example 6 Complete ===";
  Log.info "";
  Log.info "Next Steps:";
  Log.info "- Wait for Datalog evaluation engine (Week 2)";
  Log.info "- Then: declarative transitive queries!";
  Log.info "- Then: complex graph patterns with rules!"
