open Std
open Std.Collections
open Model

type t = {
  facts_by_uri : (Uri.t, Fact.t) HashMap.t;
  facts_by_entity : (Uri.t, Fact.t list) HashMap.t;
  mutable next_tx_id : int;
}

let create () =
  {
    facts_by_uri = HashMap.create ();
    facts_by_entity = HashMap.create ();
    next_tx_id = 1;
  }

let load _filename = failwith "InMemory.load not implemented - use SimpleFile"

let save _store _filename =
  failwith "InMemory.save not implemented - use SimpleFile"

let state store facts =
  let tx_id = store.next_tx_id in
  store.next_tx_id <- tx_id + 1;

  List.iter
    (fun fact ->
      let _ = HashMap.insert store.facts_by_uri fact.Fact.fact_uri fact in
      let existing =
        match HashMap.get store.facts_by_entity fact.Fact.entity with
        | Some fs -> fs
        | None -> []
      in
      let _ =
        HashMap.insert store.facts_by_entity fact.Fact.entity (fact :: existing)
      in
      ())
    facts;

  tx_id

let retract store ~fact_uri =
  match HashMap.get store.facts_by_uri fact_uri with
  | None -> ()
  | Some fact ->
      let retracted_fact = { fact with Fact.retracted = true } in
      let _ = HashMap.insert store.facts_by_uri fact_uri retracted_fact in
      ()

let get store ~entity ~attr =
  match HashMap.get store.facts_by_entity entity with
  | None -> None
  | Some facts -> (
      let matching =
        List.filter
          (fun f -> Uri.equal f.Fact.attribute attr && not f.Fact.retracted)
          facts
      in
      match matching with [] -> None | f :: _ -> Some f.Fact.value)

let get_all_facts store ~entity =
  match HashMap.get store.facts_by_entity entity with
  | None -> []
  | Some facts -> List.rev facts

let get_current_facts store ~entity =
  match HashMap.get store.facts_by_entity entity with
  | None -> []
  | Some facts -> List.filter (fun f -> not f.Fact.retracted) facts |> List.rev

let exists store entity =
  match HashMap.get store.facts_by_entity entity with
  | None -> false
  | Some facts -> List.exists (fun f -> not f.Fact.retracted) facts

let get_kind store entity =
  let instance_of_attr = Uri.of_string "@field:instance_of" in
  match get store ~entity ~attr:instance_of_attr with
  | Some (Fact.Uri kind) -> Some kind
  | _ -> None

let list_schemas store =
  let schema_kind = Uri.of_string "@kind:schema" in
  let instance_of_attr = Uri.of_string "@field:instance_of" in

  HashMap.fold
    (fun entity facts acc ->
      let is_schema =
        List.exists
          (fun f ->
            Uri.equal f.Fact.attribute instance_of_attr
            &&
            match f.Fact.value with
            | Fact.Uri kind -> Uri.equal kind schema_kind
            | _ -> false)
          facts
      in
      if is_schema then entity :: acc else acc)
    store.facts_by_entity []

let with_facts store facts =
  let _ = state store facts in
  store
