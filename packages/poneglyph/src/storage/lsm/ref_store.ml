(** Reference store - Ground truth oracle for LSM testing *)

open Std
open Std.Collections
open Std.UUID
open Model

type t = {
  (* Store ALL versions: (fact_uri, tx_id) -> fact *)
  facts : ((Uri.t * UUID.t), Fact.t) HashMap.t;
  (* Mutable flag to track compaction state *)
  mutable is_compacted : bool;
}

let empty () = { facts = HashMap.create (); is_compacted = false }

let add_fact store fact =
  (* Add fact with (fact_uri, tx_id) as key to store ALL versions *)
  let key = (fact.Fact.fact_uri, fact.Fact.tx_id) in
  let _ = HashMap.insert store.facts key fact in
  store.is_compacted <- false;
  ()

(** Get the latest version of each fact (last-tx-wins) *)
let get_latest_facts store =
  (* Build map: fact_uri -> latest fact *)
  let latest = HashMap.create () in

  HashMap.iter
    (fun (_fact_uri, _tx_id) fact ->
      let fact_uri = fact.Fact.fact_uri in
      match HashMap.get latest fact_uri with
      | None ->
          let _ = HashMap.insert latest fact_uri fact in
          ()
      | Some existing ->
          (* Keep fact with higher tx_id (lexicographic UUID comparison) *)
          if UUID.compare fact.Fact.tx_id existing.Fact.tx_id > 0 then
            let _ = HashMap.insert latest fact_uri fact in
            ())
    store.facts;

  latest

(** Filter out retracted facts *)
let is_live fact = not fact.Fact.retracted

let compact store =
  if store.is_compacted then ()
  else begin
    (* Keep only latest version of each fact *)
    let latest = get_latest_facts store in

    (* Remove retracted facts and rebuild with (fact_uri, tx_id) keys *)
    HashMap.clear store.facts;
    HashMap.iter
      (fun uri fact ->
        if is_live fact then
          let key = (uri, fact.Fact.tx_id) in
          let _ = HashMap.insert store.facts key fact in
          ())
      latest;

    store.is_compacted <- true
  end

(** Get all latest live facts *)
let all_live_facts store =
  let latest = get_latest_facts store in
  let result = ref [] in
  HashMap.iter
    (fun _uri fact -> if is_live fact then result := fact :: !result)
    latest;
  !result

let query_entity store ~entity =
  all_live_facts store |> List.filter (fun f -> Uri.equal f.Fact.entity entity)

let query_attr_value store ~attr ~value =
  all_live_facts store
  |> List.filter (fun f ->
         Uri.equal f.Fact.attribute attr
         && Fact.value_equal f.Fact.value value)

let query_source store ~source =
  all_live_facts store
  |> List.filter (fun f -> Uri.equal f.Fact.source_uri source)

let query_fact_id store ~fact_id =
  (* Need to find the latest version of this fact_uri *)
  let latest = get_latest_facts store in
  match HashMap.get latest fact_id with
  | None -> None
  | Some fact -> if is_live fact then Some fact else None

(** Statistics *)

let fact_count store = HashMap.len store.facts

let live_fact_count store = List.length (all_live_facts store)

let entity_count store =
  let entities = HashSet.create () in
  List.iter
    (fun f ->
      let _ = HashSet.insert entities f.Fact.entity in
      ())
    (all_live_facts store);
  HashSet.len entities
