(** Tests for schema definition and registration *)

open Std
open Poneglyph

module TestSchema = struct
  open Schema

  let ns = namespace "test"

  let person = kind ~ns "person" |> doc "A person entity"

  let name =
    field ~ns "name" |> used_on person |> value_type Type.string
    |> doc "Person's name"

  let age =
    field ~ns "age" |> used_on person |> value_type Type.int
    |> cardinality "one" |> required true |> doc "Person's age"

  let all_defs = [ person; name; age ]

  (* Fact builders *)
  let name_fact ~value = string_value ~field:name ~value
  let age_fact ~value = int_value ~field:age ~value
end

let test_schema_definition () =
  let (person_uri, person_facts) = TestSchema.person in
  let person_str = Uri.to_string person_uri in
  if person_str != "test:person" then
    Error ("Person URI mismatch: expected 'test:person', got '" ^ person_str ^ "'")
  else if List.length person_facts <= 0 then
    Error "Person should have associated facts"
  else
    let (name_uri, name_facts) = TestSchema.name in
    let name_str = Uri.to_string name_uri in
    if name_str != "test:name" then
      Error ("Name URI mismatch: expected 'test:name', got '" ^ name_str ^ "'")
    else if List.length name_facts <= 0 then
      Error "Name should have associated facts"
    else
      Ok ()

let test_schema_registration () =
  let graph = create () in

  (* Register schema *)
  register_schema graph TestSchema.all_defs;

  (* Check that schema facts were stored *)
  let (person_uri, _) = TestSchema.person in
  if not (exists graph person_uri) then
    Error "Person kind not found in graph"
  else
    let (name_uri, _) = TestSchema.name in
    if not (exists graph name_uri) then
      Error "Name field not found in graph"
    else
      (* Check doc was stored *)
      let doc_attr = Uri.of_string "@field:doc" in
      match get graph ~entity:person_uri ~attr:doc_attr with
      | Some (Fact.String "A person entity") -> Ok ()
      | _ -> Error "Schema doc not found or incorrect"

let test_fact_builders () =
  let graph = create () in
  register_schema graph TestSchema.all_defs;

  let person_uri = Uri.make Uri.[ ns "test"; kind "person"; id "alice" ] in

  let facts =
    Fact.for_entity person_uri
      [ TestSchema.name_fact ~value:"Alice"; TestSchema.age_fact ~value:30 ]
  in

  let _ = state graph facts in

  (* Verify facts were stored correctly *)
  let (name_attr, _) = TestSchema.name in
  match get graph ~entity:person_uri ~attr:name_attr with
  | Some (Fact.String "Alice") ->
    let (age_attr, _) = TestSchema.age in
    (match get graph ~entity:person_uri ~attr:age_attr with
    | Some (Fact.Int 30) -> Ok ()
    | _ -> Error "Age not stored correctly")
  | _ -> Error "Name not stored correctly"

let test_bootstrap_schema () =
  let graph = create () in

  (* Register bootstrap schema manually *)
  let bootstrap_facts = Schema.bootstrap ~stated_at:(Datetime.now ()) in
  let _ = state graph bootstrap_facts in

  (* Check that core schema entities exist *)
  let kind_kind = Uri.of_string "@kind:kind" in
  if not (exists graph kind_kind) then
    Error "Bootstrap kind:kind not found"
  else
    let field_doc = Uri.of_string "@field:doc" in
    if not (exists graph field_doc) then
      Error "Bootstrap field:doc not found"
    else
      Ok ()

let tests =
  Test.[
    case "Schema definition" test_schema_definition;
    case "Schema registration" test_schema_registration;
    case "Fact builders" test_fact_builders;
    case "Bootstrap schema" test_bootstrap_schema;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"poneglyph/schema" ~tests ~args)
    ~args:Env.args ()
