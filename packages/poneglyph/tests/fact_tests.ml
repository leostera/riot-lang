(** Tests for Fact module - fact construction and values *)

open Std
open Std.UUID
open Poneglyph

let test_fact_creation () =
  let entity = Uri.of_string "test:entity:1" in
  let attr = Uri.of_string "test:name" in
  let value = Fact.String "Alice" in
  let source = Uri.of_string "test:source:unit-test" in

  let tx_id = UUID.v7_monotonic () in
  let fact =
    Fact.make ~source ~entity ~attribute:attr ~value ~stated_at:(Datetime.now ())
      ~tx_id
  in

  if not (Uri.equal fact.Fact.entity entity) then
    Error "Fact entity doesn't match"
  else if not (Uri.equal fact.Fact.attribute attr) then
    Error "Fact attribute doesn't match"
  else if not (match fact.Fact.value with Fact.String "Alice" -> true | _ -> false) then
    Error "Fact value doesn't match"
  else if not (UUID.equal fact.Fact.tx_id tx_id) then
    Error "Fact tx_id doesn't match"
  else if fact.Fact.retracted then
    Error "Fact should not be retracted"
  else
    Ok ()

let test_fact_for_entity () =
  let entity = Uri.of_string "test:entity:2" in
  let name_attr = Uri.of_string "test:name" in
  let age_attr = Uri.of_string "test:age" in
  let source = Uri.of_string "test:source:unit-test" in

  let make_name _entity =
    Fact.make ~source ~entity ~attribute:name_attr ~value:(Fact.String "Bob")
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ())
  in

  let make_age _entity =
    Fact.make ~source ~entity ~attribute:age_attr ~value:(Fact.Int 30)
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ())
  in

  let facts = Fact.for_entity entity [ make_name; make_age ] in

  if List.length facts != 2 then
    Error "for_entity should create 2 facts"
  else if not (List.for_all (fun f -> Uri.equal f.Fact.entity entity) facts) then
    Error "All facts should have the same entity"
  else
    Ok ()

let test_value_types () =
  let test_value v expected =
    let actual = Fact.value_to_string v in
    if actual != expected then
      Error ("Value string mismatch: expected " ^ expected ^ " but got " ^ actual)
    else
      Ok ()
  in

  match test_value (Fact.String "hello") "\"hello\"" with
  | Error e -> Error e
  | Ok () ->
    match test_value (Fact.Int 42) "42" with
    | Error e -> Error e
    | Ok () ->
      match test_value (Fact.Bool true) "true" with
      | Error e -> Error e
      | Ok () ->
        test_value (Fact.Float 3.14) "3.14"

let tests =
  Test.[
    case "Fact creation" test_fact_creation;
    case "Fact.for_entity" test_fact_for_entity;
    case "Value types" test_value_types;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"poneglyph/fact" ~tests ~args)
    ~args:Env.args ()
