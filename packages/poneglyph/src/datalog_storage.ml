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
      
      Strategy: Use attribute URI as predicate name.
      Return binary tuples: (entity, value)
  *)
  let get_facts (graph : t) ~(predicate : string) : Datalog.Storage.fact_tuple Datalog.Relation.t =
    let attr = Uri.of_string predicate in
    
    (* Get all current (non-retracted) facts via Graph_store *)
    let all_facts = Graph_store.get_all_current_facts graph in
    
    (* Filter by attribute and convert to tuples *)
    let tuples = 
      all_facts
      |> List.filter (fun fact -> Uri.equal fact.Fact.attribute attr)
      |> List.map (fun fact -> [
          Datalog.Value.Uri (Uri.to_string fact.Fact.entity);
          fact_value_to_datalog_value fact.Fact.value
        ])
    in
    
    Datalog.Relation.of_list tuples
  
  (** List all available predicates (attributes in the graph) *)
  let predicates (graph : t) : string list =
    let all_facts = Graph_store.get_all_current_facts graph in
    
    (* Collect unique attribute URIs *)
    all_facts
    |> List.map (fun fact -> Uri.to_string fact.Fact.attribute)
    |> List.sort_uniq String.compare
  
  (** Iterate over facts without materializing the entire relation *)
  let iter_facts (graph : t) ~(predicate : string) (f : Datalog.Storage.fact_tuple -> unit) : unit =
    let attr = Uri.of_string predicate in
    let all_facts = Graph_store.get_all_current_facts graph in
    
    List.iter (fun fact ->
      if Uri.equal fact.Fact.attribute attr then
        f [
          Datalog.Value.Uri (Uri.to_string fact.Fact.entity);
          fact_value_to_datalog_value fact.Fact.value
        ]
    ) all_facts
  
  (** Get facts matching a pattern (optimized with Poneglyph indices).
      
      Pattern format: [Some v] for constants, [None] for wildcards
      Example: [Some (Uri "module:A"); None] = all facts for entity "module:A"
  *)
  let get_facts_matching (graph : t) ~(predicate : string) ~(pattern : Datalog.Value.t option list) : Datalog.Storage.fact_tuple Datalog.Relation.t =
    let attr = Uri.of_string predicate in
    
    match pattern with
    | [Some (Datalog.Value.Uri entity_str); None] ->
        (* Optimized: Query specific entity using Poneglyph's index *)
        let entity = Uri.of_string entity_str in
        (match Graph_store.get graph ~entity ~attr with
        | Some value ->
            (* Found a matching fact *)
            Datalog.Relation.singleton [
              Datalog.Value.Uri entity_str;
              fact_value_to_datalog_value value
            ]
        | None ->
            (* No fact for this entity+attribute *)
            Datalog.Relation.empty ())
    
    | [None; Some target_value] ->
        (* Query by value - requires scanning (no index in Poneglyph yet) *)
        let all_facts = get_facts graph ~predicate in
        Datalog.Relation.filter (fun tuple ->
          match tuple with
          | [_; v] -> Datalog.Value.equal v target_value
          | _ -> false
        ) all_facts
    
    | _ ->
        (* General case: fetch all facts and filter *)
        let facts = get_facts graph ~predicate in
        Datalog.Relation.filter (Datalog.Storage.matches_pattern pattern) facts
end
