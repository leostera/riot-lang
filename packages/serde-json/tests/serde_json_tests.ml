open Std
open Serde

module Test = Std.Test

let ( let* ) = Result.and_then

type pet =
  | Cat
  | Dog of string

type person = {
  name: string;
  age: int;
  active: bool;
  tags: string list;
  nickname: string option;
  pet: pet;
}

type prefix_record = {
  help: int;
  hello: int;
  hellsinborg: int;
}

type person_field =
  | Field_name
  | Field_age
  | Field_active
  | Field_tags
  | Field_nickname
  | Field_pet

type prefix_field =
  | Prefix_help
  | Prefix_hello
  | Prefix_hellsinborg

let person_fields =
  fields [
    field "name" Field_name;
    field "age" Field_age;
    field "active" Field_active;
    field "tags" Field_tags;
    field "nickname" Field_nickname;
    field "pet" Field_pet;
  ]

let prefix_fields =
  fields [
    field "help" Prefix_help;
    field "hello" Prefix_hello;
    field "hellsinborg" Prefix_hellsinborg;
  ]

let pet_decode =
  variant [
    Variant.unit "Cat" Cat;
    Variant.newtype "Dog" string (fun value -> Dog value);
  ]

let person_decode =
  record
    ~fields:person_fields
    ~init:(None, None, None, None, None, None)
    ~step:(fun reader (name, age, active, tags, nickname, pet) field ->
      match field with
      | Some Field_name ->
          (Some (read reader string), age, active, tags, nickname, pet)
      | Some Field_age ->
          (name, Some (read reader int), active, tags, nickname, pet)
      | Some Field_active ->
          (name, age, Some (read reader bool), tags, nickname, pet)
      | Some Field_tags ->
          (name, age, active, Some (read reader (list string)), nickname, pet)
      | Some Field_nickname ->
          (name, age, active, tags, Some (read reader (option string)), pet)
      | Some Field_pet ->
          (name, age, active, tags, nickname, Some (read reader pet_decode))
      | None ->
          let () = read reader skip_any in
          (name, age, active, tags, nickname, pet))
    ~finish:(fun (name, age, active, tags, nickname, pet) ->
      match (name, age, active, tags, nickname, pet) with
      | (Some name, Some age, Some active, Some tags, Some nickname, Some pet) ->
          { name; age; active; tags; nickname; pet }
      | _ ->
          missing_field ())

let prefix_decode =
  record
    ~fields:prefix_fields
    ~init:(None, None, None)
    ~step:(fun reader (help, hello, hellsinborg) field ->
      match field with
      | Some Prefix_help ->
          (Some (read reader int), hello, hellsinborg)
      | Some Prefix_hello ->
          (help, Some (read reader int), hellsinborg)
      | Some Prefix_hellsinborg ->
          (help, hello, Some (read reader int))
      | None ->
          let () = read reader skip_any in
          (help, hello, hellsinborg))
    ~finish:(fun (help, hello, hellsinborg) ->
      match (help, hello, hellsinborg) with
      | (Some help, Some hello, Some hellsinborg) ->
          { help; hello; hellsinborg }
      | _ ->
          missing_field ())

let expect_equal = fun ~expected ~actual ~message ->
  if actual = expected then
    Ok ()
  else
    Error message

let test_decodes_record_and_skips_unknown_fields = fun _ctx ->
  let input =
    {|{
      "name":"Le\u006F",
      "age":33,
      "active":true,
      "tags":["riot","serde"],
      "nickname":null,
      "pet":{"Dog":"Rex"},
      "unknown":{"nested":[1,2,3],"more":{"answer":42}}
    }|} in
  let expected = {
    name = "Leo";
    age = 33;
    active = true;
    tags = [ "riot"; "serde" ];
    nickname = None;
    pet = Dog "Rex";
  } in
  match Serde_json.of_string person_decode input with
  | Ok actual ->
      expect_equal
        ~expected
        ~actual
        ~message:"expected serde-json decoder to parse a record and skip unknown fields"
  | Error err ->
      Error ("decode failed: " ^ Serde.Error.to_string err)

let test_decodes_unit_variant = fun _ctx ->
  let input =
    {|{"name":"Leo","age":33,"active":true,"tags":["riot"],"nickname":"captain","pet":"Cat"}|} in
  let expected = {
    name = "Leo";
    age = 33;
    active = true;
    tags = [ "riot" ];
    nickname = Some "captain";
    pet = Cat;
  } in
  match Serde_json.of_string person_decode input with
  | Ok actual ->
      expect_equal
        ~expected
        ~actual
        ~message:"expected serde-json decoder to handle string-form unit variants"
  | Error err ->
      Error ("unit-variant decode failed: " ^ Serde.Error.to_string err)

let test_matches_shared_prefix_fields = fun _ctx ->
  let input = {|{"help":1,"hello":2,"hellsinborg":3}|} in
  let expected = { help = 1; hello = 2; hellsinborg = 3 } in
  match Serde_json.of_string prefix_decode input with
  | Ok actual ->
      expect_equal
        ~expected
        ~actual
        ~message:"expected serde-json decoder to distinguish shared-prefix field names"
  | Error err ->
      Error ("shared-prefix decode failed: " ^ Serde.Error.to_string err)

let test_decodes_numeric_scalars = fun _ctx ->
  let expect_ok = fun decode input expected message ->
    match Serde_json.of_string decode input with
    | Ok actual when actual = expected ->
        Ok ()
    | Ok _ ->
        Error message
    | Error err ->
        Error ("numeric decode failed: " ^ Serde.Error.to_string err)
  in
  let* () =
    expect_ok
      int
      "-12345"
      (-12345)
      "expected serde-json decoder to parse top-level ints"
  in
  expect_ok
    float
    "1.25e3"
    1250.0
    "expected serde-json decoder to parse top-level floats with exponents"

let tests =
  Test.[
    case "serde-json parses records and skips unknown fields" test_decodes_record_and_skips_unknown_fields;
    case "serde-json handles unit variants" test_decodes_unit_variant;
    case "serde-json matches shared-prefix fields" test_matches_shared_prefix_fields;
    case "serde-json parses numeric scalars" test_decodes_numeric_scalars;
  ]

let () =
  Actors.run ~main:(fun ~args -> Test.Cli.main ~name:"serde_json_tests" ~tests ~args) ~args:Env.args ()
