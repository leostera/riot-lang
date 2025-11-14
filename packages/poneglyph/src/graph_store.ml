open Std
open Std.Collections
open Model

type storage_impl =
  | Inmemory of Storage.Inmemory.t
  | SimpleFile of Storage.Simple_file.t

type t = {
  storage : storage_impl;
  mutable filename : string option;
}

let create ?persistent () =
  match persistent with
  | None -> { storage = Inmemory (Storage.Inmemory.create ()); filename = None }
  | Some path ->
      let store = Storage.Simple_file.load path in
      { storage = SimpleFile store; filename = Some path }

let state graph facts =
  match graph.storage with
  | Inmemory store -> Storage.Inmemory.state store facts
  | SimpleFile store -> Storage.Simple_file.state store facts

let retract graph ~fact_uri =
  match graph.storage with
  | Inmemory store -> Storage.Inmemory.retract store ~fact_uri
  | SimpleFile store -> Storage.Simple_file.retract store ~fact_uri

let get graph ~entity ~attr =
  match graph.storage with
  | Inmemory store -> Storage.Inmemory.get store ~entity ~attr
  | SimpleFile store -> Storage.Simple_file.get store ~entity ~attr

let get_all_facts graph ~entity =
  match graph.storage with
  | Inmemory store -> Storage.Inmemory.get_all_facts store ~entity
  | SimpleFile store -> Storage.Simple_file.get_all_facts store ~entity

let get_current_facts graph ~entity =
  match graph.storage with
  | Inmemory store -> Storage.Inmemory.get_current_facts store ~entity
  | SimpleFile store -> Storage.Simple_file.get_current_facts store ~entity

let exists graph entity =
  match graph.storage with
  | Inmemory store -> Storage.Inmemory.exists store entity
  | SimpleFile store -> Storage.Simple_file.exists store entity

let get_kind graph entity =
  match graph.storage with
  | Inmemory store -> Storage.Inmemory.get_kind store entity
  | SimpleFile store -> Storage.Simple_file.get_kind store entity

let list_schemas graph =
  match graph.storage with
  | Inmemory store -> Storage.Inmemory.list_schemas store
  | SimpleFile store -> Storage.Simple_file.list_schemas store

let save graph =
  match (graph.storage, graph.filename) with
  | SimpleFile store, Some path -> Storage.Simple_file.save store path
  | Inmemory _, Some path ->
      Log.warn ("Cannot save in-memory graph to " ^ path)
  | _, None -> ()

let transitive graph ~start ~edge ~max_depth =
  let visited = HashMap.create () in
  let results = ref [] in

  let rec traverse current depth =
    if
      Option.is_some (HashMap.get visited current)
      || (match max_depth with Some d -> depth > d | None -> false)
    then ()
    else (
      let _ = HashMap.insert visited current true in
      results := current :: !results;

      (* Find all facts where current entity has the edge attribute *)
      let facts = get_current_facts graph ~entity:current in
      List.iter
        (fun fact ->
          if Uri.equal fact.Fact.attribute edge then
            match fact.Fact.value with
            | Fact.Uri next -> traverse next (depth + 1)
            | _ -> ())
        facts)
  in

  traverse start 0;
  List.rev !results

let count_entities graph =
  match graph.storage with
  | Inmemory store -> Storage.Inmemory.entity_count store
  | SimpleFile store -> Storage.Simple_file.entity_count store

let count_facts graph =
  match graph.storage with
  | Inmemory store -> Storage.Inmemory.fact_count store
  | SimpleFile store -> Storage.Simple_file.fact_count store

let count_current_facts graph =
  match graph.storage with
  | Inmemory store -> Storage.Inmemory.current_fact_count store
  | SimpleFile store -> Storage.Simple_file.current_fact_count store

let find_entities graph ~attr ~value =
  match graph.storage with
  | Inmemory store -> Storage.Inmemory.find_entities_by_attr_value store ~attr ~value
  | SimpleFile store -> Storage.Simple_file.find_entities_by_attr_value store ~attr ~value

let find_by_kind graph ~kind =
  let instance_of_attr = Uri.of_string "@field:instance_of" in
  find_entities graph ~attr:instance_of_attr ~value:(Fact.Uri kind)

let get_all_current_facts graph =
  match graph.storage with
  | Inmemory store -> Storage.Inmemory.get_all_current_facts store
  | SimpleFile store -> Storage.Simple_file.get_all_current_facts store

let find_by_source graph ~source =
  let all_facts = get_all_current_facts graph in
  List.filter_map (fun fact ->
    if Uri.equal fact.Fact.source_uri source then
      Some fact.Fact.entity
    else None
  ) all_facts
  |> List.sort_uniq Uri.compare

let retract_by_source graph ~source =
  let all_facts = get_all_current_facts graph in
  List.iter (fun fact ->
    if Uri.equal fact.Fact.source_uri source then
      retract graph ~fact_uri:fact.Fact.fact_uri
  ) all_facts
