open Std
open Std.Result.Syntax

module Vector = Collections.Vector
module Test = Std.Test
module De = Serde.De
module Ser = Serde.Ser

let io_writer_of_buffer =
  let module Write = struct
    type t = IO.Buffer.t

    let write = fun buffer ~from ->
      let written = IO.Buffer.readable_bytes from in
      IO.Buffer.append_slice buffer (IO.Buffer.readable from)
      |> Result.expect ~msg:"serde-json test writer should append buffer contents";
      Ok written

    let write_vectored = fun buffer ~from ->
      let written = ref 0 in
      IO.IoVec.for_each
        ~fn:(fun chunk ->
          IO.Buffer.append_slice buffer chunk
          |> Result.expect ~msg:"serde-json test writer should append slices";
          written := !written + IO.IoSlice.length chunk)
        from;
      Ok !written

    let flush = fun _buffer -> Ok ()
  end in
  fun buffer -> IO.Writer.from_sink (module Write) buffer

type pet =
  | Cat
  | Dog of string

type person = {
  name: string;
  age: int;
  active: bool;
  tags: string vec;
  nickname: string option;
  pet: pet;
}

type prefix_record = { help: int; hello: int; hellsinborg: int }

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

type person_builder = {
  mutable name: string option;
  mutable age: int option;
  mutable active: bool option;
  mutable tags: string vec option;
  mutable nickname: string option option;
  mutable pet: pet option;
}

type prefix_builder = {
  mutable help: int option;
  mutable hello: int option;
  mutable hellsinborg: int option;
}

let person_fields =
  De.fields
    [
      De.field "name" Field_name;
      De.field "age" Field_age;
      De.field "active" Field_active;
      De.field "tags" Field_tags;
      De.field "nickname" Field_nickname;
      De.field "pet" Field_pet;
    ]

let prefix_fields =
  De.fields
    [
      De.field "help" Prefix_help;
      De.field "hello" Prefix_hello;
      De.field "hellsinborg" Prefix_hellsinborg;
    ]

let pet_decode =
  De.variant
    [
      De.Variant.unit "Cat" Cat;
      De.Variant.newtype "Dog" De.string (fun value -> Dog value);
    ]

let pet_encode =
  Ser.variant
    [
      Ser.Variant.unit
        "Cat"
        (fun __tmp1 ->
          match __tmp1 with
          | Cat -> true
          | Dog _ -> false);
      Ser.Variant.newtype
        "Dog"
        Ser.string
        (fun __tmp1 ->
          match __tmp1 with
          | Dog value -> Some value
          | Cat -> None);
    ]

let person_decode =
  De.record_mut
    ~fields:person_fields
    ~create:(fun (): person_builder ->
      {
        name = None;
        age = None;
        active = None;
        tags = None;
        nickname = None;
        pet = None;
      })
    ~step:(fun reader builder field ->
      match field with
      | Some Field_name -> builder.name <- Some (De.read reader De.string)
      | Some Field_age -> builder.age <- Some (De.read reader De.int)
      | Some Field_active -> builder.active <- Some (De.read reader De.bool)
      | Some Field_tags -> builder.tags <- Some (De.read reader (De.list De.string))
      | Some Field_nickname -> builder.nickname <- Some (De.read reader (De.option De.string))
      | Some Field_pet -> builder.pet <- Some (De.read reader pet_decode)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun (builder: person_builder) ->
      match (builder.name, builder.age, builder.active, builder.tags, builder.nickname, builder.pet) with
      | (Some name, Some age, Some active, Some tags, Some nickname, Some pet) ->
          ({
            name;
            age;
            active;
            tags;
            nickname;
            pet;
          }: person)
      | _ -> De.missing_field ())

let person_encode =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "name" Ser.string (fun (value: person) -> value.name);
          Ser.field "age" Ser.int (fun (value: person) -> value.age);
          Ser.field "active" Ser.bool (fun (value: person) -> value.active);
          Ser.field "tags" (Ser.list Ser.string) (fun (value: person) -> value.tags);
          Ser.field "nickname" (Ser.option Ser.string) (fun (value: person) -> value.nickname);
          Ser.field "pet" pet_encode (fun (value: person) -> value.pet);
        ]
    )

let prefix_decode =
  De.record_mut
    ~fields:prefix_fields
    ~create:(fun (): prefix_builder -> { help = None; hello = None; hellsinborg = None })
    ~step:(fun reader builder field ->
      match field with
      | Some Prefix_help -> builder.help <- Some (De.read reader De.int)
      | Some Prefix_hello -> builder.hello <- Some (De.read reader De.int)
      | Some Prefix_hellsinborg -> builder.hellsinborg <- Some (De.read reader De.int)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun (builder: prefix_builder) ->
      match (builder.help, builder.hello, builder.hellsinborg) with
      | (Some help, Some hello, Some hellsinborg) -> ({ help; hello; hellsinborg }: prefix_record)
      | _ -> De.missing_field ())

let expect_equal = fun ~expected ~actual ~message ->
  if actual = expected then
    Ok ()
  else
    Error message

let vec_to_list = fun values ->
  let items = ref [] in
  Vector.for_each values ~fn:(fun value -> items := value :: !items);
  List.rev !items

let equal_person = fun (left: person) (right: person) ->
  String.equal left.name right.name
  && Int.equal left.age right.age
  && Bool.equal left.active right.active
  && vec_to_list left.tags = vec_to_list right.tags
  && left.nickname = right.nickname
  && left.pet = right.pet

let test_decodes_record_and_skips_unknown_fields = fun _ctx ->
  let input =
    {|{
      "name":"Luff\u0079",
      "age":19,
      "active":true,
      "tags":["riot","serde"],
      "nickname":null,
      "pet":{"Dog":"Chouchou"},
      "unknown":{"nested":[1,2,3],"more":{"answer":42}}
    }|}
  in
  let expected: person = {
    name = "Luffy";
    age = 19;
    active = true;
    tags = Vector.from_list [ "riot"; "serde" ];
    nickname = None;
    pet = Dog "Chouchou";
  }
  in
  match Serde_json.from_string person_decode input with
  | Ok actual ->
      if equal_person actual expected then
        Ok ()
      else
        Error "expected serde-json decoder to parse a record and skip unknown fields"
  | Error err -> Error ("decode failed: " ^ Serde.Error.to_string err)

let test_decodes_unit_variant = fun _ctx ->
  let input =
    {|{"name":"Luffy","age":19,"active":true,"tags":["riot"],"nickname":"strawhat","pet":"Cat"}|}
  in
  let expected: person = {
    name = "Luffy";
    age = 19;
    active = true;
    tags = Vector.from_list [ "riot" ];
    nickname = Some "strawhat";
    pet = Cat;
  }
  in
  match Serde_json.from_string person_decode input with
  | Ok actual ->
      if equal_person actual expected then
        Ok ()
      else
        Error "expected serde-json decoder to handle string-form unit variants"
  | Error err -> Error ("unit-variant decode failed: " ^ Serde.Error.to_string err)

let test_decodes_from_reader = fun _ctx ->
  let input =
    {|{"name":"Luffy","age":19,"active":true,"tags":["riot"],"nickname":"strawhat","pet":"Cat"}|}
  in
  let expected: person = {
    name = "Luffy";
    age = 19;
    active = true;
    tags = Vector.from_list [ "riot" ];
    nickname = Some "strawhat";
    pet = Cat;
  }
  in
  match Serde_json.from_reader person_decode (String.to_reader ~chunk_size:1 input) with
  | Ok actual ->
      if equal_person actual expected then
        Ok ()
      else
        let actual_json =
          match Serde_json.to_string person_encode actual with
          | Ok value -> value
          | Error err -> "<failed to encode actual: " ^ Serde.Error.to_string err ^ ">"
        in
        Error ("expected serde-json decoder to read from an IO.Reader, got " ^ actual_json)
  | Error err -> Error ("reader decode failed: " ^ Serde.Error.to_string err)

let test_matches_shared_prefix_fields = fun _ctx ->
  let input = {|{"help":1,"hello":2,"hellsinborg":3}|} in
  let expected: prefix_record = { help = 1; hello = 2; hellsinborg = 3 } in
  match Serde_json.from_string prefix_decode input with
  | Ok actual ->
      expect_equal
        ~expected
        ~actual
        ~message:"expected serde-json decoder to distinguish shared-prefix field names"
  | Error err -> Error ("shared-prefix decode failed: " ^ Serde.Error.to_string err)

let test_decodes_numeric_scalars = fun _ctx ->
  let expect_ok decode input expected message =
    match Serde_json.from_string decode input with
    | Ok actual when actual = expected -> Ok ()
    | Ok _ -> Error message
    | Error err -> Error ("numeric decode failed: " ^ Serde.Error.to_string err)
  in
  let* () = expect_ok De.int "-12345" (-12_345) "expected serde-json decoder to parse top-level ints" in
  expect_ok
    De.float
    "1.25e3"
    1_250.0
    "expected serde-json decoder to parse top-level floats with exponents"

let test_encodes_record = fun _ctx ->
  let person: person = {
    name = "Luffy";
    age = 19;
    active = true;
    tags = Vector.from_list [ "riot"; "serde" ];
    nickname = None;
    pet = Dog "Chouchou";
  }
  in
  let expected =
    {|{"name":"Luffy","age":19,"active":true,"tags":["riot","serde"],"nickname":null,"pet":{"Dog":"Chouchou"}}|}
  in
  match Serde_json.to_string person_encode person with
  | Ok actual ->
      expect_equal
        ~expected
        ~actual
        ~message:"expected serde-json encoder to serialize records using the promoted Serde.Ser API"
  | Error err -> Error ("encode failed: " ^ Serde.Error.to_string err)

let test_encodes_escaped_strings = fun _ctx ->
  let person: person = {
    name = "Luf\"fy\n";
    age = 19;
    active = true;
    tags = Vector.from_list [ "ri\\ot" ];
    nickname = Some "strawhat\t";
    pet = Cat;
  }
  in
  let expected =
    {|{"name":"Luf\"fy\n","age":19,"active":true,"tags":["ri\\ot"],"nickname":"strawhat\t","pet":"Cat"}|}
  in
  match Serde_json.to_string person_encode person with
  | Ok actual ->
      expect_equal
        ~expected
        ~actual
        ~message:"expected serde-json encoder to escape string contents correctly"
  | Error err -> Error ("escaped-string encode failed: " ^ Serde.Error.to_string err)

let test_writes_to_writer = fun _ctx ->
  let person: person = {
    name = "Luffy";
    age = 19;
    active = true;
    tags = Vector.from_list [ "riot"; "serde" ];
    nickname = None;
    pet = Dog "Chouchou";
  }
  in
  let expected =
    {|{"name":"Luffy","age":19,"active":true,"tags":["riot","serde"],"nickname":null,"pet":{"Dog":"Chouchou"}}|}
  in
  let buffer = IO.Buffer.create ~size:128 in
  match Serde_json.to_writer person_encode (io_writer_of_buffer buffer) person with
  | Ok () ->
      expect_equal
        ~expected
        ~actual:(IO.Buffer.contents buffer)
        ~message:"expected serde-json encoder to write JSON to an IO.Writer"
  | Error err -> Error ("writer encode failed: " ^ Serde.Error.to_string err)

let test_roundtrips_record = fun _ctx ->
  let person: person = {
    name = "Luffy";
    age = 19;
    active = true;
    tags = Vector.from_list [ "riot"; "serde" ];
    nickname = Some "strawhat";
    pet = Cat;
  }
  in
  let* encoded =
    match Serde_json.to_string person_encode person with
    | Ok encoded -> Ok encoded
    | Error err -> Error ("roundtrip encode failed: " ^ Serde.Error.to_string err)
  in
  match Serde_json.from_string person_decode encoded with
  | Ok actual ->
      if equal_person actual person then
        Ok ()
      else
        Error "expected serde-json encode/decode to roundtrip person values"
  | Error err -> Error ("roundtrip decode failed: " ^ Serde.Error.to_string err)

let test_roundtrips_arrays = fun _ctx ->
  let values = [|1; 2; 3; 5; 8|] in
  let* encoded =
    match Serde_json.to_string (Ser.array Ser.int) values with
    | Ok encoded -> Ok encoded
    | Error err -> Error ("array encode failed: " ^ Serde.Error.to_string err)
  in
  match Serde_json.from_string (De.array De.int) encoded with
  | Ok actual when actual = values -> Ok ()
  | Ok _ -> Error "expected serde-json array roundtrip to preserve elements"
  | Error err -> Error ("array decode failed: " ^ Serde.Error.to_string err)

let test_roundtrips_large_float = fun _ctx ->
  let value = 907_309_392_156.125 in
  let* encoded =
    match Serde_json.to_string Ser.float value with
    | Ok encoded -> Ok encoded
    | Error err -> Error ("float encode failed: " ^ Serde.Error.to_string err)
  in
  match Serde_json.from_string De.float encoded with
  | Ok actual when Float.equal actual value -> Ok ()
  | Ok actual ->
      Error ("expected serde-json to preserve large floats, got "
      ^ Float.to_string actual
      ^ " from "
      ^ encoded)
  | Error err -> Error ("float decode failed: " ^ Serde.Error.to_string err)

let test_decodes_negative_int64_across_reader_chunk_boundary = fun _ctx ->
  match Serde_json.from_reader De.int64 (String.to_reader ~chunk_size:1 "-1689690667") with
  | Ok actual ->
      expect_equal
        ~expected:(-1_689_690_667L)
        ~actual
        ~message:"expected serde-json to decode a negative int64 when the sign and digits are split across reader chunks"
  | Error err -> Error ("reader decode failed: " ^ Serde.Error.to_string err)

let tests =
  Test.[
    case
      "serde-json parses records and skips unknown fields"
      test_decodes_record_and_skips_unknown_fields;
    case "serde-json handles unit variants" test_decodes_unit_variant;
    case "serde-json decodes from readers" test_decodes_from_reader;
    case "serde-json matches shared-prefix fields" test_matches_shared_prefix_fields;
    case "serde-json parses numeric scalars" test_decodes_numeric_scalars;
    case "serde-json encodes records" test_encodes_record;
    case "serde-json escapes encoded strings" test_encodes_escaped_strings;
    case "serde-json writes to writers" test_writes_to_writer;
    case "serde-json roundtrips records" test_roundtrips_record;
    case "serde-json roundtrips arrays" test_roundtrips_arrays;
    case "serde-json roundtrips large floats" test_roundtrips_large_float;
    case
      "serde-json decodes negative int64 across reader chunk boundary"
      test_decodes_negative_int64_across_reader_chunk_boundary;
  ]

let main ~args = Test.Cli.main ~name:"serde_json_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
