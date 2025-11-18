(** Poneglyph Storage Backend for Datalog
    
    This module implements Datalog's pluggable storage interface,
    allowing Datalog to query Poneglyph graphs directly without
    copying facts.
    
    Design: Each Poneglyph attribute becomes a binary Datalog predicate.
    For example:
      - Fact(entity: "module:A", attribute: "depends_on", value: Uri "module:B")
      - Becomes: depends_on("module:A", "module:B")
*)

open Std
open Model

(** Convert Poneglyph fact value to Datalog value *)
let fact_value_to_datalog_value (v : Fact.value) : Datalog.Value.t =
  match v with
  | Fact.String s -> Datalog.Value.String s
  | Fact.Int i -> Datalog.Value.Int i
  | Fact.Bool b -> Datalog.Value.String (string_of_bool b)
  | Fact.Float f -> Datalog.Value.String (string_of_float f)
  | Fact.Uri u -> Datalog.Value.Uri (Uri.to_string u)
  | Fact.DateTime dt -> Datalog.Value.String (string_of_float (Datetime.to_timestamp dt))

(** Convert Datalog value to Poneglyph fact value (for queries) *)
let datalog_value_to_fact_value (v : Datalog.Value.t) : Fact.value option =
  match v with
  | Datalog.Value.String s -> Some (Fact.String s)
  | Datalog.Value.Int i -> Some (Fact.Int i)
  | Datalog.Value.Uri u -> Some (Fact.Uri (Uri.of_string u))

(** Poneglyph as a Datalog storage backend *)
module PoneglyphStorage : Datalog.Storage.STORAGE with type t = Graph_store.t = struct
  type t = Graph_store.t
  
  (** Get all facts for a predicate (attribute).
      
      Strategy: Use AVET index with attribute prefix for fast lookup.
      Return binary tuples: (entity, value)
  *)
  
  (** Get facts as streaming iterator - NO materialization!
      
      This is the streaming version used for query execution.
      Returns an iterator that lazily converts Poneglyph facts to Datalog tuples.
  *)
  let get_facts_iter (graph : t) ~(predicate : string) : Datalog.Storage.fact_tuple Iter.MutIterator.t =
    let attr = Uri.of_string predicate in
    
    Log.info ("Datalog_storage.get_facts_iter: predicate=" ^ predicate ^ " (streaming)");
    
    (* Stream directly from Graph_store - NO to_list! *)
    Graph_store.get_facts_by_attribute graph ~attribute:attr
    |> Iter.MutIterator.map ~fn:(fun fact -> 
        [
          Datalog.Value.String (Uri.to_string fact.Fact.entity);
          fact_value_to_datalog_value fact.Fact.value
        ])
  
  (** Get facts for a predicate as a materialized Relation.
      
      NOTE: This materializes results. For streaming queries, use get_facts_iter instead.
  *)
  let get_facts (graph : t) ~(predicate : string) : Datalog.Storage.fact_tuple Datalog.Relation.t =
    (* Since get_facts_iter returns tuples sorted by entity (from AVET index),
       and Relation expects sorted input, we can use it directly! *)
    let iter = get_facts_iter graph ~predicate in
    Datalog.Relation.of_iter iter
  
  (** List all available predicates (attributes in the graph) *)
  let predicates (graph : t) : string list =
    let all_facts = Graph_store.get_all_current_facts graph in
    
    (* Collect unique attribute URIs *)
    all_facts
    |> Iter.MutIterator.map ~fn:(fun fact -> Uri.to_string fact.Fact.attribute)
    |> Iter.MutIterator.to_list
    |> List.sort_uniq String.compare
  
  (** Iterate over facts without materializing the entire relation *)
  let iter_facts (graph : t) ~(predicate : string) (f : Datalog.Storage.fact_tuple -> unit) : unit =
    let attr = Uri.of_string predicate in
    
    (* Use AVET index for efficient attribute lookup instead of scanning entire database! *)
    let facts = Graph_store.get_facts_by_attribute graph ~attribute:attr in
    
    Iter.MutIterator.for_each facts ~fn:(fun fact ->
      f [
        Datalog.Value.String (Uri.to_string fact.Fact.entity);  (* Store as String *)
        fact_value_to_datalog_value fact.Fact.value
      ])
  
  (** Get facts matching a pattern (optimized with Poneglyph indices).
      
      Pattern format: [Some v] for constants, [None] for wildcards
      Example: [Some (Uri "module:A"); None] = all facts for entity "module:A"
      
      Note: Graph_store.t already represents a specific snapshot, so no tx_id needed here.
  *)
  let get_facts_matching (graph : t) ~(predicate : string) ~(pattern : Datalog.Value.t option list) : Datalog.Storage.fact_tuple Datalog.Relation.t =
    let attr = Uri.of_string predicate in
    
    Log.info ("Datalog_storage.get_facts_matching: predicate=" ^ predicate);
    Log.info ("Datalog_storage.get_facts_matching: pattern length=" ^ string_of_int (List.length pattern));
    List.iteri (fun i opt ->
      match opt with
      | Some (Datalog.Value.Uri s) -> Log.info ("  pattern[" ^ string_of_int i ^ "] = Uri(" ^ s ^ ")")
      | Some (Datalog.Value.String s) -> Log.info ("  pattern[" ^ string_of_int i ^ "] = String(" ^ s ^ ")")
      | Some (Datalog.Value.Int n) -> Log.info ("  pattern[" ^ string_of_int i ^ "] = Int(" ^ string_of_int n ^ ")")
      | None -> Log.info ("  pattern[" ^ string_of_int i ^ "] = None (wildcard)")
    ) pattern;
    
    match pattern with
    | [Some (Datalog.Value.String entity_str); None] | [Some (Datalog.Value.Uri entity_str); None] ->
        (* Optimized: Query specific entity using Poneglyph's index *)
        (* Accept both String and Uri for backwards compatibility *)
        Log.info ("Datalog_storage.get_facts_matching: Using optimized path for entity=" ^ entity_str);
        let entity = Uri.of_string entity_str in
        (match Graph_store.get graph ~entity ~attr with
        | Some value ->
            (* Found a matching fact *)
            Log.info ("Datalog_storage.get_facts_matching: Found fact for entity");
            Datalog.Relation.singleton [
              Datalog.Value.String entity_str;  (* Return as String *)
              fact_value_to_datalog_value value
            ]
        | None ->
            (* No fact found *)
            Log.warn ("Datalog_storage.get_facts_matching: No fact found for entity=" ^ entity_str ^ " attr=" ^ predicate);
            Datalog.Relation.empty ())
    
    | [None; Some target_value] ->
        (* Optimized: Use AVET index for value queries - FULLY STREAMING! *)
        Log.info ("Datalog_storage.get_facts_matching: Using AVET index for value query");
        
        (match datalog_value_to_fact_value target_value with
        | None ->
            (* Value type not supported for index lookup - fall back to scan *)
            Log.warn ("Datalog_storage.get_facts_matching: Cannot convert value, falling back to scan");
            let tuples = get_facts_iter graph ~predicate
              |> Iter.MutIterator.filter ~fn:(fun tuple ->
                  match tuple with
                  | [_; v] -> Datalog.Value.equal v target_value
                  | _ -> false) in
            Datalog.Relation.of_iter tuples
        | Some fact_value ->
            (* AVET scan returns SORTED iterator of entities (sorted by entity_id) *)
            let entity_iter = Graph_store.find_entities graph ~attr ~value:fact_value in
            
            (* Map to tuples - preserves sorted order *)
            let tuple_iter = Iter.MutIterator.map entity_iter ~fn:(fun entity ->
              [Datalog.Value.String (Uri.to_string entity); target_value]
            ) in
            
            (* Create relation from sorted iterator - NO MATERIALIZATION! *)
            Datalog.Relation.of_iter tuple_iter)
    
    | _ ->
        (* General case: fetch all facts and filter *)
        Log.info ("Datalog_storage.get_facts_matching: Using general case filter");
        let tuples = get_facts_iter graph ~predicate
          |> Iter.MutIterator.filter ~fn:(Datalog.Storage.matches_pattern pattern) in
        Datalog.Relation.of_iter tuples
end
