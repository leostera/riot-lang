open Std
module Vector = Collections.Vector
module Test = Std.Test
module De = Serde.De
module Ser = Serde.Ser

let ( let* ) = Result.and_then

let io_reader_of_string = fun ?(chunk_size = 1) value ->
  let offset = ref 0 in
  let module Read = struct
    type t = string

    type err = IO.error

    let read = fun source ?timeout:_ buf ->
      let remaining = String.length source - !offset in
      if Int.equal remaining 0 then
        Ok 0
      else
        let to_read = min chunk_size (min remaining (IO.Bytes.length buf)) in
        IO.Bytes.blit_string source !offset buf 0 to_read;
        offset := !offset + to_read;
        Ok to_read

    let read_vectored = fun source bufs ->
      let total = ref 0 in
      let continue = ref true in
      IO.Iovec.iter bufs
        (fun { ba; off; len } ->
          if !continue then
            let remaining = String.length source - !offset in
            if Int.equal remaining 0 then
              continue := false
            else
              let to_read = min chunk_size (min remaining len) in
              IO.Bytes.blit_string source !offset ba off to_read;
              offset := !offset + to_read;
              total := !total + to_read;
              if to_read < len then
                continue := false);
      Ok !total

    let direct_string = fun _source -> None
  end in
  IO.Reader.of_read_src (module Read) value

let io_writer_of_buffer =
  let module Write = struct
    type t = IO.Buffer.t

    type err = IO.error

    let write = fun buffer ~buf ->
      IO.Buffer.add_string buffer buf;
      Ok (String.length buf)

    let write_owned_vectored = fun buffer ~bufs ->
      let written = ref 0 in
      IO.Iovec.iter bufs
        (fun { ba; off; len } ->
          IO.Buffer.add_string buffer (IO.Bytes.sub_string ba off len);
          written := !written + len);
      Ok !written

    let flush = fun _buffer -> Ok ()
  end in
  fun buffer ->
    IO.Writer.of_write_src (module Write) buffer

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

let person_fields = De.fields
  [
    De.field "name" Field_name;
    De.field "age" Field_age;
    De.field "active" Field_active;
    De.field "tags" Field_tags;
    De.field "nickname" Field_nickname;
    De.field "pet" Field_pet;
  ]

let prefix_fields = De.fields
  [
    De.field "help" Prefix_help;
    De.field "hello" Prefix_hello;
    De.field "hellsinborg" Prefix_hellsinborg;
  ]

let pet_decode = De.variant
  [ De.Variant.unit "Cat" Cat; De.Variant.newtype "Dog" De.string (fun value -> Dog value); ]

let pet_encode = Ser.variant
  [ Ser.Variant.unit "Cat"
      (
        function
        | Cat -> true
        | Dog _ -> false
      ); Ser.Variant.newtype "Dog" Ser.string
      (
        function
        | Dog value -> Some value
        | Cat -> None
      ); ]

let person_decode =
  De.record ~fields:person_fields ~init:(None, None, None, None, None, None)
    ~step:(fun reader (name, age, active, tags, nickname, pet) field ->
      match field with
      | Some Field_name ->
          (Some (De.read reader De.string), age, active, tags, nickname, pet)
      | Some Field_age ->
          (name, Some (De.read reader De.int), active, tags, nickname, pet)
      | Some Field_active ->
          (name, age, Some (De.read reader De.bool), tags, nickname, pet)
      | Some Field_tags ->
          (name, age, active, Some (De.read reader (De.list De.string)), nickname, pet)
      | Some Field_nickname ->
          (name, age, active, tags, Some (De.read reader (De.option De.string)), pet)
      | Some Field_pet ->
          (name, age, active, tags, nickname, Some (De.read reader pet_decode))
      | None ->
          let () = De.read reader De.skip_any in
          (name, age, active, tags, nickname, pet))
    ~finish:(fun (name, age, active, tags, nickname, pet) ->
      match (name, age, active, tags, nickname, pet) with
      | (Some name, Some age, Some active, Some tags, Some nickname, Some pet) ->
          {
            name;
            age;
            active;
            tags;
            nickname;
            pet;
          }
      | _ -> De.missing_field ())

let person_encode = Ser.record
  (Ser.fields
    [
      Ser.field "name" Ser.string (fun value -> value.name);
      Ser.field "age" Ser.int (fun value -> value.age);
      Ser.field "active" Ser.bool (fun value -> value.active);
      Ser.field "tags" (Ser.list Ser.string) (fun value -> value.tags);
      Ser.field "nickname" (Ser.option Ser.string) (fun value -> value.nickname);
      Ser.field "pet" pet_encode (fun value -> value.pet);
    ])

let prefix_decode =
  De.record ~fields:prefix_fields ~init:(None, None, None)
    ~step:(fun reader (help, hello, hellsinborg) field ->
      match field with
      | Some Prefix_help ->
          (Some (De.read reader De.int), hello, hellsinborg)
      | Some Prefix_hello ->
          (help, Some (De.read reader De.int), hellsinborg)
      | Some Prefix_hellsinborg ->
          (help, hello, Some (De.read reader De.int))
      | None ->
          let () = De.read reader De.skip_any in
          (help, hello, hellsinborg))
    ~finish:(fun (help, hello, hellsinborg) ->
      match (help, hello, hellsinborg) with
      | (Some help, Some hello, Some hellsinborg) -> { help; hello; hellsinborg }
      | _ -> De.missing_field ())

let expect_equal = fun ~expected ~actual ~message ->
  if actual = expected then
    Ok ()
  else
    Error message

let vec_to_list = fun values ->
  let items = ref [] in
  Vector.iter (fun value -> items := value :: !items) values;
  List.rev !items

let equal_person = fun left right ->
  String.equal left.name right.name
  && Int.equal left.age right.age
  && Bool.equal left.active right.active
  && vec_to_list left.tags = vec_to_list right.tags
  && left.nickname = right.nickname
  && left.pet = right.pet

let test_decodes_record_and_skips_unknown_fields = fun _ctx ->
  let input = {|{
      "name":"Le\u006F",
      "age":33,
      "active":true,
      "tags":["riot","serde"],
      "nickname":null,
      "pet":{"Dog":"Rex"},
      "unknown":{"nested":[1,2,3],"more":{"answer":42}}
    }|}
  in
  let expected = {
    name = "Leo";
    age = 33;
    active = true;
    tags = Vector.of_list [ "riot"; "serde" ];
    nickname = None;
    pet = Dog "Rex";
  }
  in
  match Serde_json.of_string person_decode input with
  | Ok actual ->
      if equal_person actual expected then
        Ok ()
      else
        Error "expected serde-json decoder to parse a record and skip unknown fields"
  | Error err -> Error ("decode failed: " ^ Serde.Error.to_string err)

let test_decodes_unit_variant = fun _ctx ->
  let input = {|{"name":"Leo","age":33,"active":true,"tags":["riot"],"nickname":"captain","pet":"Cat"}|} in
  let expected = {
    name = "Leo";
    age = 33;
    active = true;
    tags = Vector.of_list [ "riot" ];
    nickname = Some "captain";
    pet = Cat;
  }
  in
  match Serde_json.of_string person_decode input with
  | Ok actual ->
      if equal_person actual expected then
        Ok ()
      else
        Error "expected serde-json decoder to handle string-form unit variants"
  | Error err -> Error ("unit-variant decode failed: " ^ Serde.Error.to_string err)

let test_decodes_from_reader = fun _ctx ->
  let input = {|{"name":"Leo","age":33,"active":true,"tags":["riot"],"nickname":"captain","pet":"Cat"}|} in
  let expected = {
    name = "Leo";
    age = 33;
    active = true;
    tags = Vector.of_list [ "riot" ];
    nickname = Some "captain";
    pet = Cat;
  }
  in
  match Serde_json.of_reader person_decode (io_reader_of_string ~chunk_size:1 input) with
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
  let expected = { help = 1; hello = 2; hellsinborg = 3 } in
  match Serde_json.of_string prefix_decode input with
  | Ok actual -> expect_equal ~expected ~actual ~message:"expected serde-json decoder to distinguish shared-prefix field names"
  | Error err -> Error ("shared-prefix decode failed: " ^ Serde.Error.to_string err)

let test_decodes_numeric_scalars = fun _ctx ->
  let expect_ok decode input expected message =
    match Serde_json.of_string decode input with
    | Ok actual when actual = expected -> Ok ()
    | Ok _ -> Error message
    | Error err -> Error ("numeric decode failed: " ^ Serde.Error.to_string err)
  in
  let* () = expect_ok De.int "-12345" (-12_345) "expected serde-json decoder to parse top-level ints" in
  expect_ok De.float "1.25e3" 1250.0 "expected serde-json decoder to parse top-level floats with exponents"

let test_encodes_record = fun _ctx ->
  let person = {
    name = "Leo";
    age = 33;
    active = true;
    tags = Vector.of_list [ "riot"; "serde" ];
    nickname = None;
    pet = Dog "Rex";
  }
  in
  let expected = {|{"name":"Leo","age":33,"active":true,"tags":["riot","serde"],"nickname":null,"pet":{"Dog":"Rex"}}|} in
  match Serde_json.to_string person_encode person with
  | Ok actual -> expect_equal ~expected ~actual ~message:"expected serde-json encoder to serialize records using the promoted Serde.Ser API"
  | Error err -> Error ("encode failed: " ^ Serde.Error.to_string err)

let test_encodes_escaped_strings = fun _ctx ->
  let person = {
    name = "Le\"o\n";
    age = 33;
    active = true;
    tags = Vector.of_list [ "ri\\ot" ];
    nickname = Some "captain\t";
    pet = Cat;
  }
  in
  let expected = {|{"name":"Le\"o\n","age":33,"active":true,"tags":["ri\\ot"],"nickname":"captain\t","pet":"Cat"}|} in
  match Serde_json.to_string person_encode person with
  | Ok actual -> expect_equal ~expected ~actual ~message:"expected serde-json encoder to escape string contents correctly"
  | Error err -> Error ("escaped-string encode failed: " ^ Serde.Error.to_string err)

let test_writes_to_writer = fun _ctx ->
  let person = {
    name = "Leo";
    age = 33;
    active = true;
    tags = Vector.of_list [ "riot"; "serde" ];
    nickname = None;
    pet = Dog "Rex";
  }
  in
  let expected = {|{"name":"Leo","age":33,"active":true,"tags":["riot","serde"],"nickname":null,"pet":{"Dog":"Rex"}}|} in
  let buffer = IO.Buffer.create 128 in
  match Serde_json.to_writer person_encode (io_writer_of_buffer buffer) person with
  | Ok () -> expect_equal ~expected ~actual:(IO.Buffer.contents buffer) ~message:"expected serde-json encoder to write JSON to an IO.Writer"
  | Error err -> Error ("writer encode failed: " ^ Serde.Error.to_string err)

let test_roundtrips_record = fun _ctx ->
  let person = {
    name = "Leo";
    age = 33;
    active = true;
    tags = Vector.of_list [ "riot"; "serde" ];
    nickname = Some "captain";
    pet = Cat;
  }
  in
  let* encoded =
    match Serde_json.to_string person_encode person with
    | Ok encoded -> Ok encoded
    | Error err -> Error ("roundtrip encode failed: " ^ Serde.Error.to_string err)
  in
  match Serde_json.of_string person_decode encoded with
  | Ok actual ->
      if equal_person actual person then
        Ok ()
      else
        Error "expected serde-json encode/decode to roundtrip person values"
  | Error err -> Error ("roundtrip decode failed: " ^ Serde.Error.to_string err)

let tests =
  Test.[
    case "serde-json parses records and skips unknown fields" test_decodes_record_and_skips_unknown_fields;
    case "serde-json handles unit variants" test_decodes_unit_variant;
    case "serde-json decodes from readers" test_decodes_from_reader;
    case "serde-json matches shared-prefix fields" test_matches_shared_prefix_fields;
    case "serde-json parses numeric scalars" test_decodes_numeric_scalars;
    case "serde-json encodes records" test_encodes_record;
    case "serde-json escapes encoded strings" test_encodes_escaped_strings;
    case "serde-json writes to writers" test_writes_to_writer;
    case "serde-json roundtrips records" test_roundtrips_record;
  ]

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"serde_json_tests" ~tests ~args)
    ~args:Env.args
    ()
