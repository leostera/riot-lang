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
      let len = IO.Buffer.length from in
      IO.Buffer.add_bytes buffer (IO.Buffer.to_bytes from);
      Ok len

    let write_vectored = fun buffer ~from ->
      let written = ref 0 in
      IO.IoVec.for_each
        ~fn:(fun chunk ->
          let bytes = IO.IoSlice.to_bytes chunk in
          IO.Buffer.add_bytes buffer bytes;
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
  score: int64;
}

type person_field =
  | Field_name
  | Field_age
  | Field_active
  | Field_tags
  | Field_nickname
  | Field_pet
  | Field_score

type person_builder = {
  mutable name: string option;
  mutable age: int option;
  mutable active: bool option;
  mutable tags: string vec option;
  mutable nickname: string option option;
  mutable pet: pet option;
  mutable score: int64 option;
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
      De.field "score" Field_score;
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
        score = None;
      })
    ~step:(fun reader builder field ->
      match field with
      | Some Field_name -> builder.name <- Some (De.read reader De.string)
      | Some Field_age -> builder.age <- Some (De.read reader De.int)
      | Some Field_active -> builder.active <- Some (De.read reader De.bool)
      | Some Field_tags -> builder.tags <- Some (De.read reader (De.list De.string))
      | Some Field_nickname -> builder.nickname <- Some (De.read reader (De.option De.string))
      | Some Field_pet -> builder.pet <- Some (De.read reader pet_decode)
      | Some Field_score -> builder.score <- Some (De.read reader De.int64)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun (builder: person_builder) ->
      match (
        builder.name,
        builder.age,
        builder.active,
        builder.tags,
        builder.nickname,
        builder.pet,
        builder.score
      ) with
      | (Some name, Some age, Some active, Some tags, Some nickname, Some pet, Some score) ->
          ({
            name;
            age;
            active;
            tags;
            nickname;
            pet;
            score;
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
          Ser.field "score" Ser.int64 (fun (value: person) -> value.score);
        ]
    )

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
  && Int64.equal left.score right.score

let byte_values = fun value ->
  List.init
    ~count:(String.length value)
    ~fn:(fun index -> Char.code (String.get_unchecked value ~at:index))

let expect_equal = fun ~expected ~actual ~message ->
  if expected = actual then
    Ok ()
  else
    Error message

let test_roundtrips_record = fun _ctx ->
  let person: person = {
    name = "Luffy";
    age = 19;
    active = true;
    tags = Vector.from_list [ "riot"; "serde"; "bin" ];
    nickname = Some "strawhat";
    pet = Dog "Chouchou";
    score = 42L;
  }
  in
  let* encoded =
    match Serde_bin.to_string person_encode person with
    | Ok encoded -> Ok encoded
    | Error err -> Error ("roundtrip encode failed: " ^ Serde.Error.to_string err)
  in
  match Serde_bin.from_string person_decode encoded with
  | Ok actual when equal_person actual person -> Ok ()
  | Ok _ -> Error "expected serde-bin roundtrip to preserve person values"
  | Error err -> Error ("roundtrip decode failed: " ^ Serde.Error.to_string err)

let test_decodes_from_reader = fun _ctx ->
  let encoded = "\004\000\000\000riot" in
  match Serde_bin.from_reader De.string (String.to_reader ~chunk_size:2 encoded) with
  | Ok "riot" -> Ok ()
  | Ok actual -> Error ("expected reader decode to return the string payload, got " ^ actual)
  | Error err -> Error ("reader decode failed: " ^ Serde.Error.to_string err)

let test_int32_uses_raw_little_endian_bytes = fun _ctx ->
  match Serde_bin.to_string Ser.int32 0x0102_0304l with
  | Ok encoded ->
      expect_equal
        ~expected:[ 4; 3; 2; 1; ]
        ~actual:(byte_values encoded)
        ~message:"expected int32 to use raw little-endian bytes"
  | Error err -> Error ("int32 encode failed: " ^ Serde.Error.to_string err)

let test_int_uses_raw_little_endian_bytes = fun _ctx ->
  match Serde_bin.to_string Ser.int (-1) with
  | Ok encoded ->
      expect_equal
        ~expected:[ 255; 255; 255; 255; 255; 255; 255; 255; ]
        ~actual:(byte_values encoded)
        ~message:"expected int to use raw 8-byte little-endian bytes"
  | Error err -> Error ("int encode failed: " ^ Serde.Error.to_string err)

let test_int32_roundtrips_negative_values = fun _ctx ->
  match Serde_bin.from_string De.int32 "\255\255\255\255" with
  | Ok value when Int32.equal value (-1l) -> Ok ()
  | Ok value -> Error ("expected int32 decode to preserve raw bits, got " ^ Int32.to_string value)
  | Error err -> Error ("int32 decode failed: " ^ Serde.Error.to_string err)

let test_int64_uses_raw_little_endian_bytes = fun _ctx ->
  match Serde_bin.to_string Ser.int64 0x0102_0304_0506_0708L with
  | Ok encoded ->
      expect_equal
        ~expected:[ 8; 7; 6; 5; 4; 3; 2; 1; ]
        ~actual:(byte_values encoded)
        ~message:"expected int64 to use raw little-endian bytes"
  | Error err -> Error ("int64 encode failed: " ^ Serde.Error.to_string err)

let test_float_uses_raw_ieee754_bytes = fun _ctx ->
  match Serde_bin.to_string Ser.float 1.0 with
  | Ok encoded ->
      expect_equal
        ~expected:[ 0; 0; 0; 0; 0; 0; 240; 63; ]
        ~actual:(byte_values encoded)
        ~message:"expected float to use raw IEEE754 little-endian bytes"
  | Error err -> Error ("float encode failed: " ^ Serde.Error.to_string err)

let test_writes_to_writer = fun _ctx ->
  let buffer = IO.Buffer.create ~size:32 in
  match Serde_bin.to_writer pet_encode (io_writer_of_buffer buffer) (Dog "Chouchou") with
  | Ok () ->
      expect_equal
        ~expected:[ 1; 8; 0; 0; 0; 67; 104; 111; 117; 99; 104; 111; 117; ]
        ~actual:(byte_values (IO.Buffer.contents buffer))
        ~message:"expected serde-bin to emit compact variant bytes"
  | Error err -> Error ("writer encode failed: " ^ Serde.Error.to_string err)

let test_size_matches_encoded_length = fun _ctx ->
  let person: person = {
    name = "Luffy";
    age = 19;
    active = false;
    tags = Vector.from_list [ "a"; "b" ];
    nickname = None;
    pet = Cat;
    score = 99L;
  }
  in
  let* expected_len =
    match Serde_bin.size_of person_encode person with
    | Ok len -> Ok len
    | Error err -> Error ("size_of failed: " ^ Serde.Error.to_string err)
  in
  let* encoded =
    match Serde_bin.to_string person_encode person with
    | Ok encoded -> Ok encoded
    | Error err -> Error ("to_string failed: " ^ Serde.Error.to_string err)
  in
  expect_equal
    ~expected:expected_len
    ~actual:(String.length encoded)
    ~message:"expected size_of to match encoded string length"

let test_encode_into_bytes = fun _ctx ->
  let dst = IO.Bytes.create ~size:16 in
  match Serde_bin.encode_into_bytes pet_encode dst (Dog "Chouchou") with
  | Ok written ->
      let actual =
        IO.Bytes.sub_string dst ~offset:0 ~len:written
        |> byte_values
      in
      expect_equal
        ~expected:[ 1; 8; 0; 0; 0; 67; 104; 111; 117; 99; 104; 111; 117; ]
        ~actual
        ~message:"expected encode_into_bytes to write the compact variant payload"
  | Error err -> Error ("encode_into_bytes failed: " ^ Serde.Error.to_string err)

let test_short_destination_errors = fun _ctx ->
  let dst = IO.Bytes.create ~size:2 in
  match Serde_bin.encode_into_bytes pet_encode dst (Dog "Chouchou") with
  | Error (`Msg msg) when String.starts_with ~prefix:"serde-bin destination buffer is too small" msg ->
      Ok ()
  | Error err -> Error ("expected short destination error, got " ^ Serde.Error.to_string err)
  | Ok _ -> Error "expected encode_into_bytes to fail for too-small destinations"

let test_decode_prefix_reports_consumed_bytes = fun _ctx ->
  let* encoded =
    match Serde_bin.to_string pet_encode (Dog "Chouchou") with
    | Ok encoded -> Ok encoded
    | Error err -> Error ("to_string failed: " ^ Serde.Error.to_string err)
  in
  match Serde_bin.decode_prefix pet_decode (encoded ^ "\255\254") with
  | Ok (Dog "Chouchou", consumed) when Int.equal consumed (String.length encoded) -> Ok ()
  | Ok (_value, _consumed) ->
      Error "expected decode_prefix to return the decoded value and consumed byte count"
  | Error err -> Error ("decode_prefix failed: " ^ Serde.Error.to_string err)

let test_rejects_invalid_bool = fun _ctx ->
  match Serde_bin.from_string De.bool "\002" with
  | Error (`Msg msg) when String.starts_with ~prefix:"invalid bool value" msg -> Ok ()
  | Error err -> Error ("expected invalid bool error, got " ^ Serde.Error.to_string err)
  | Ok _ -> Error "expected invalid bool input to fail"

let test_rejects_truncated_string = fun _ctx ->
  match Serde_bin.from_string De.string "\005\000\000\000ab" with
  | Error (`Msg msg) when String.starts_with
    ~prefix:"unexpected end of input while decoding string"
    msg -> Ok ()
  | Error err -> Error ("expected truncated string error, got " ^ Serde.Error.to_string err)
  | Ok _ -> Error "expected truncated string input to fail"

let test_rejects_trailing_bytes = fun _ctx ->
  match Serde_bin.from_string De.bool "\001\000" with
  | Error (`Msg msg) when String.starts_with ~prefix:"extra input after binary value" msg -> Ok ()
  | Error err -> Error ("expected trailing-byte error, got " ^ Serde.Error.to_string err)
  | Ok _ -> Error "expected trailing bytes to fail strict decoding"

let test_roundtrips_arrays = fun _ctx ->
  let values = [|7; 11; 42; 99|] in
  let* encoded =
    match Serde_bin.to_string (Ser.array Ser.int) values with
    | Ok encoded -> Ok encoded
    | Error err -> Error ("array encode failed: " ^ Serde.Error.to_string err)
  in
  match Serde_bin.from_string (De.array De.int) encoded with
  | Ok actual when actual = values -> Ok ()
  | Ok _ -> Error "expected serde-bin array roundtrip to preserve elements"
  | Error err -> Error ("array decode failed: " ^ Serde.Error.to_string err)

let tests =
  Test.[
    case "serde-bin roundtrips records" test_roundtrips_record;
    case "serde-bin roundtrips arrays" test_roundtrips_arrays;
    case "serde-bin decodes from readers" test_decodes_from_reader;
    case "serde-bin int uses raw little-endian bytes" test_int_uses_raw_little_endian_bytes;
    case "serde-bin int32 uses raw little-endian bytes" test_int32_uses_raw_little_endian_bytes;
    case "serde-bin int32 roundtrips negative values" test_int32_roundtrips_negative_values;
    case "serde-bin int64 uses raw little-endian bytes" test_int64_uses_raw_little_endian_bytes;
    case "serde-bin float uses raw IEEE754 bytes" test_float_uses_raw_ieee754_bytes;
    case "serde-bin writes to writers" test_writes_to_writer;
    case "serde-bin size_of matches encoded length" test_size_matches_encoded_length;
    case "serde-bin encode_into_bytes writes compact bytes" test_encode_into_bytes;
    case "serde-bin encode_into_bytes errors on short destinations" test_short_destination_errors;
    case "serde-bin decode_prefix reports consumed bytes" test_decode_prefix_reports_consumed_bytes;
    case "serde-bin rejects invalid bools" test_rejects_invalid_bool;
    case "serde-bin rejects truncated strings" test_rejects_truncated_string;
    case "serde-bin rejects trailing bytes" test_rejects_trailing_bytes;
  ]

let main ~args = Test.Cli.main ~name:"serde_bin_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
