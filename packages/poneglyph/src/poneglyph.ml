(** Poneglyph - EAV Graph Store *)

open Std

(* Re-export all submodules *)
module Uri = Model.Uri
module Fact = Model.Fact
module Schema = Model.Schema
module Entity = Model.Entity
module Storage = Storage

(* LSM Storage modules *)
module Ref_store = Storage.Lsm.Ref_store

(* Type and core operations from GraphStore *)
type t = Graph_store.t

let create () = Graph_store.create ()
let state = Graph_store.state
let retract = Graph_store.retract
let get = Graph_store.get
let get_all_facts = Graph_store.get_all_facts
let get_current_facts = Graph_store.get_current_facts
let exists = Graph_store.exists
let get_kind = Graph_store.get_kind
let list_schemas = Graph_store.list_schemas
let save = Graph_store.save
let transitive = Graph_store.transitive
let count_entities = Graph_store.count_entities
let count_facts = Graph_store.count_facts
let count_current_facts = Graph_store.count_current_facts
let find_entities = Graph_store.find_entities
let find_by_kind = Graph_store.find_by_kind
let find_by_source = Graph_store.find_by_source
let retract_by_source = Graph_store.retract_by_source

(* Additional high-level API *)
let create_persistent path = Graph_store.create ~persistent:path ()
let load = create_persistent

let register_schema graph defs =
  let open Model in
  let facts = List.concat_map (fun (_uri, facts) -> facts) defs in
  let _ = state graph facts in
  ()

let bootstrap graph =
  let open Model in
  let facts = Schema.bootstrap ~stated_at:(Datetime.now ()) in
  let _ = state graph facts in
  ()

let load_entity graph uri =
  let open Model in
  if not (exists graph uri) then None
  else
    let kind = get_kind graph uri in
    let all_facts = get_current_facts graph ~entity:uri in
    let facts =
      List.map (fun fact -> (fact.Fact.attribute, fact.Fact.value)) all_facts
    in
    Some (Entity.make ~uri ~kind ~facts)

let stats graph =
  [
    ("entities", count_entities graph);
    ("facts", count_facts graph);
    ("current_facts", count_current_facts graph);
  ]

(** {2 Datalog Integration} *)

(** Datalog query interface - NOTE: Requires Datalog evaluation engine (coming Week 2) *)
module Datalog = struct
  module Backend = Datalog_storage.PoneglyphStorage
  
  (** Get list of available predicates (attributes) in the graph *)
  let predicates (graph : t) : string list = 
    Backend.predicates graph
  
  (** Get all facts for a predicate as a Datalog relation *)
  let get_facts (graph : t) ~(predicate : string) = 
    Backend.get_facts graph ~predicate
  
  (** Check if storage backend is working *)
  let test_storage (graph : t) : unit =
    let preds = predicates graph in
    Log.info ("Available predicates: " ^ String.concat ", " preds);
    List.iter (fun pred ->
      let facts = get_facts graph ~predicate:pred in
      let count = Datalog.Relation.length facts in
      Log.info ("  " ^ pred ^ ": " ^ string_of_int count ^ " facts")
    ) preds
end
