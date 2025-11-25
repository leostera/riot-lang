(** Tests for storage backends - inmemory and file *)

open Std
open Std.UUID
open Poneglyph

let test_inmemory_storage () =
  let graph = create () in

  let entity = Uri.of_string "test:entity:1" in
  let attr = Uri.of_string "test:value" in
  let source = Uri.of_string "test:source:storage-test" in

  (* State a fact *)
  let facts =
    [
      Fact.make ~source ~entity ~attribute:attr ~value:(Fact.Int 42)
        ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ());
    ]
  in

  let tx_id = state graph facts in
  (* tx_id is now int returned by state, not the UUID inside the fact *)
  if tx_id <= 0 then
    Error "Transaction ID should be positive"
  else
    (* Retrieve it *)
    match get graph ~entity ~attr with
    | Some (Fact.Int 42) ->
      (* Check existence *)
      if not (exists graph entity) then
        Error "Entity should exist"
      else if exists graph (Uri.of_string "test:nonexistent") then
        Error "Nonexistent entity should not exist"
      else
        Ok ()
    | _ -> Error "Expected Int 42"

let test_retraction () =
  let graph = create () in

  let entity = Uri.of_string "test:entity:retract" in
  let attr = Uri.of_string "test:value" in
  let source = Uri.of_string "test:source:retraction-test" in

  let facts =
    [
      Fact.make ~source ~entity ~attribute:attr ~value:(Fact.Int 100)
        ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ());
    ]
  in

  let _ = state graph facts in

  (* Fact should exist *)
  match get graph ~entity ~attr with
  | Some (Fact.Int 100) ->
    (* Get the fact URI *)
    let all_facts = get_all_facts graph ~entity 
      |> Iter.MutIterator.to_list in
    if List.length all_facts = 0 then
      Error "Should have at least one fact"
    else
      let fact = List.hd all_facts in

      (* Retract it *)
      retract graph ~fact_uri:fact.Fact.fact_uri;

      (* Should no longer be current *)
      (match get graph ~entity ~attr with
      | None ->
        (* But should still be in history *)
        let all = get_all_facts graph ~entity 
          |> Iter.MutIterator.to_list in
        if List.length all != 1 then
          Error "Should have exactly one fact in history"
        else if not (List.hd all).Fact.retracted then
          Error "Fact should be marked as retracted"
        else
          Ok ()
      | _ -> Error "Fact should be None after retraction")
  | _ -> Error "Fact should exist before retraction"

let tests =
  Test.[
    case "Inmemory storage" test_inmemory_storage;
    case "Retraction" test_retraction;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"poneglyph/storage" ~tests ~args)
    ~args:Env.args ()
