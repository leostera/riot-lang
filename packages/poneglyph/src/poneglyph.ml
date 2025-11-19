(** Poneglyph - EAV Graph Store *)

open Std

(* Re-export all submodules *)
module Uri = Model.Uri
module Fact = Model.Fact
module Schema = Model.Schema
module Entity = Model.Entity
module Storage = Storage
module Cli = Cli

(* LSM Storage modules *)
module Ref_store = Storage.Lsm.Ref_store

(* Type and core operations from GraphStore *)
type t = Graph_store.t
type create_config = Graph_store.create_config =
  | InMemory
  | Persistent of string
  | Lsm of string

let create ?config () = Graph_store.create ?config ()

(* Primary API *)
let open_shared = Graph_store.open_shared
let open_exclusive = Graph_store.open_exclusive

(* Graph operations *)
let state = Graph_store.state
let retract = Graph_store.retract
let close = Graph_store.close
let flush = Graph_store.flush
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

(* Convenience alias for Fact.make *)
let fact = Fact.make

let facts ~source ~tx_id ~stated_at ~entity fxs= 
  List.map (fun (attribute, value) ->
    fact ~source ~tx_id ~stated_at ~entity ~attribute ~value) fxs

(* Legacy convenience functions *)
let create_persistent path = Graph_store.create ~config:(Graph_store.Persistent path) ()
let load = create_persistent
let create_lsm data_dir = Graph_store.create ~config:(Graph_store.Lsm data_dir) ()

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
      all_facts
      |> Iter.MutIterator.map ~fn:(fun fact -> (fact.Fact.attribute, fact.Fact.value))
      |> Iter.MutIterator.to_list
    in
    Some (Entity.make ~uri ~kind ~facts)

let stats graph =
  [
    ("entities", count_entities graph);
    ("facts", count_facts graph);
    ("current_facts", count_current_facts graph);
  ]

let execute_query db ~query =
  (* Phase 0: Query-only Datalog - no rules *)
  let query_str = query in
  
  (* 1. Create universe with Poneglyph storage *)
  let module U = Datalog.Universe.Make(Datalog_storage.PoneglyphStorage) in
  let universe = U.create db in
  
  (* 2. Parse query *)
  (match Datalog.Parser.parse_query query_str with
  | Error diagnostics ->
      let diag_str = 
        List.map (fun d -> Datalog.Parser.Diagnostic.to_string d) diagnostics 
        |> String.concat "; " 
      in
      Error ("Query parse error: " ^ diag_str)
  | Ok query_cst ->
      (match Datalog.Ast_from_cst.query_of_cst query_cst with
      | Error e -> Error ("Failed to convert query to AST: " ^ e)
      | Ok query_ast ->
           (* Execute query based on type *)
           (match query_ast with
            | Datalog.Ast.Single atom ->
                (* Single-goal query - pure streaming! *)
                let module Eval = Datalog.Evaluator.Make(U) in
                let results = Eval.query universe atom in
                Ok results
           | Datalog.Ast.Multi clauses ->
                (* Multi-goal query - streaming join! *)
                let module Eval = Datalog.Evaluator.Make(U) in
                let results = Eval.multi_query universe clauses in
                Ok results)))
