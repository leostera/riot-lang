open Std
open Std.Collections
open Model

type t = {
  facts_by_uri : (Uri.t, Fact.t) HashMap.t;
  facts_by_entity : (Uri.t, Fact.t list) HashMap.t;
  (* Reverse index: (attr, value) -> [entity_uri] for fast lookups *)
  reverse_index : ((Uri.t * Fact.value), Uri.t list) HashMap.t;
  (* Statistics *)
  mutable entity_count : int;
  mutable fact_count : int;
  mutable current_fact_count : int;
  mutable next_tx_id : int;
}

let create () =
  {
    facts_by_uri = HashMap.create ();
    facts_by_entity = HashMap.create ();
    reverse_index = HashMap.create ();
    entity_count = 0;
    fact_count = 0;
    current_fact_count = 0;
    next_tx_id = 1;
  }

let load _filename = panic "InMemory.load not implemented - use SimpleFile"

let save _store _filename =
  panic "InMemory.save not implemented - use SimpleFile"

let state store facts =
  let tx_id = store.next_tx_id in
  store.next_tx_id <- tx_id + 1;

  List.iter
    (fun fact ->
      (* Track if this is a new entity *)
      let is_new_entity = 
        match HashMap.get store.facts_by_entity fact.Fact.entity with
        | None -> true
        | Some _ -> false
      in
      if is_new_entity then store.entity_count <- store.entity_count + 1;
      
      (* Insert into facts_by_uri *)
      let _ = HashMap.insert store.facts_by_uri fact.Fact.fact_uri fact in
      
      (* Insert into facts_by_entity *)
      let existing =
        match HashMap.get store.facts_by_entity fact.Fact.entity with
        | Some fs -> fs
        | None -> []
      in
      let _ =
        HashMap.insert store.facts_by_entity fact.Fact.entity (fact :: existing)
      in
      
      (* Update reverse index: (attr, value) -> [entities] *)
      let key = (fact.Fact.attribute, fact.Fact.value) in
      let entities = match HashMap.get store.reverse_index key with
        | Some es -> fact.Fact.entity :: es
        | None -> [fact.Fact.entity]
      in
      let _ = HashMap.insert store.reverse_index key entities in
      
      (* Update statistics *)
      store.fact_count <- store.fact_count + 1;
      if not fact.Fact.retracted then
        store.current_fact_count <- store.current_fact_count + 1;
      
      ())
    facts;

  tx_id

let retract store ~fact_uri =
  match HashMap.get store.facts_by_uri fact_uri with
  | None -> ()
  | Some fact ->
      let retracted_fact = { fact with Fact.retracted = true } in
      (* Update in facts_by_uri *)
      let _ = HashMap.insert store.facts_by_uri fact_uri retracted_fact in
      (* Also update in facts_by_entity *)
      (match HashMap.get store.facts_by_entity fact.Fact.entity with
      | None -> ()
      | Some facts ->
          let updated_facts =
            List.map (fun f ->
              if Uri.equal f.Fact.fact_uri fact_uri then retracted_fact else f
            ) facts
          in
          let _ = HashMap.insert store.facts_by_entity fact.Fact.entity updated_facts in
          ());
      
      (* Update statistics *)
      if not fact.Fact.retracted then
        store.current_fact_count <- store.current_fact_count - 1

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
  | None -> 
      (* Empty iterator *)
      let module EmptyIter = struct
        type state = unit
        type item = Fact.t
        let next () = None
        let size () = 0
        let clone () = ()
      end in
      Iter.MutIterator.make (module EmptyIter) ()
  | Some facts ->
      (* Convert list to iterator *)
      let module ListIter = struct
        type state = { mutable remaining : Fact.t list }
        type item = Fact.t
        let next state = match state.remaining with
          | [] -> None
          | x :: xs -> state.remaining <- xs; Some x
        let size state = List.length state.remaining
        let clone state = { remaining = state.remaining }
      end in
      Iter.MutIterator.make (module ListIter) { remaining = List.rev facts }

let get_current_facts store ~entity =
  match HashMap.get store.facts_by_entity entity with
  | None ->
      (* Empty iterator *)
      let module EmptyIter = struct
        type state = unit
        type item = Fact.t
        let next () = None
        let size () = 0
        let clone () = ()
      end in
      Iter.MutIterator.make (module EmptyIter) ()
  | Some facts ->
      (* Filter and convert to iterator *)
      let filtered = List.filter (fun f -> not f.Fact.retracted) facts |> List.rev in
      let module ListIter = struct
        type state = { mutable remaining : Fact.t list }
        type item = Fact.t
        let next state = match state.remaining with
          | [] -> None
          | x :: xs -> state.remaining <- xs; Some x
        let size state = List.length state.remaining
        let clone state = { remaining = state.remaining }
      end in
      Iter.MutIterator.make (module ListIter) { remaining = filtered }

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

  let schemas = HashMap.fold
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
    store.facts_by_entity [] in
  
  (* Convert to iterator *)
  let module ListIter = struct
    type state = { mutable remaining : Uri.t list }
    type item = Uri.t
    let next state = match state.remaining with
      | [] -> None
      | x :: xs -> state.remaining <- xs; Some x
    let size state = List.length state.remaining
    let clone state = { remaining = state.remaining }
  end in
  Iter.MutIterator.make (module ListIter) { remaining = schemas }

let with_facts store facts =
  let _ = state store facts in
  store

let get_all_current_facts store =
  let open Std.Collections in
  let all = ref [] in
  HashMap.iter (fun _entity facts ->
    List.iter (fun fact ->
      if not fact.Fact.retracted then
        all := fact :: !all
    ) facts
  ) store.facts_by_entity;
  
  (* Convert to iterator *)
  let module ListIter = struct
    type state = { mutable remaining : Fact.t list }
    type item = Fact.t
    let next state = match state.remaining with
      | [] -> None
      | x :: xs -> state.remaining <- xs; Some x
    let size state = List.length state.remaining
    let clone state = { remaining = state.remaining }
  end in
  Iter.MutIterator.make (module ListIter) { remaining = !all }

let find_entities_by_attr_value store ~attr ~value =
  let key = (attr, value) in
  match HashMap.get store.reverse_index key with
  | Some entities -> 
      (* Filter to only entities that still have this non-retracted fact *)
      let filtered = List.filter (fun entity ->
        match get store ~entity ~attr with
        | Some v -> Fact.value_equal v value
        | None -> false
      ) entities in
      
      (* Convert to iterator *)
      let module ListIter = struct
        type state = { mutable remaining : Uri.t list }
        type item = Uri.t
        let next state = match state.remaining with
          | [] -> None
          | x :: xs -> state.remaining <- xs; Some x
        let size state = List.length state.remaining
        let clone state = { remaining = state.remaining }
      end in
      Iter.MutIterator.make (module ListIter) { remaining = filtered }
  | None ->
      (* Empty iterator *)
      let module EmptyIter = struct
        type state = unit
        type item = Uri.t
        let next () = None
        let size () = 0
        let clone () = ()
      end in
      Iter.MutIterator.make (module EmptyIter) ()

let entity_count store = store.entity_count
let fact_count store = store.fact_count
let current_fact_count store = store.current_fact_count
