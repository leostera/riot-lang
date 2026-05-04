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
      |> Result.expect ~msg:"serde-urlencoded test writer should append buffer contents";
      Ok written

    let write_vectored = fun buffer ~from ->
      let written = ref 0 in
      IO.IoVec.for_each
        from
        ~fn:(fun chunk ->
          IO.Buffer.append_slice buffer chunk
          |> Result.expect ~msg:"serde-urlencoded test writer should append slices";
          written := !written + IO.IoSlice.length chunk);
      Ok !written

    let flush = fun _buffer -> Ok ()
  end in
  fun buffer -> IO.Writer.from_sink (module Write) buffer

type status =
  | Active
  | Draft
  | Archived

type sample = {
  name: string;
  age: int;
  active: bool;
  small: int32;
  big: int64;
  ratio: float;
  tags: string vec;
  scores: int array;
  nickname: string option;
  status: status;
}

type sample_field =
  | Field_name
  | Field_age
  | Field_active
  | Field_small
  | Field_big
  | Field_ratio
  | Field_tags
  | Field_scores
  | Field_nickname
  | Field_status

type sample_builder = {
  mutable name: string option;
  mutable age: int option;
  mutable active: bool option;
  mutable small: int32 option;
  mutable big: int64 option;
  mutable ratio: float option;
  mutable tags: string vec option;
  mutable scores: int array option;
  mutable nickname: string option option;
  mutable status: status option;
}

type address = { city: string }

type nested = {
  title: string;
  address: address;
}

type pet =
  | Cat
  | Dog of string

let sample_fields =
  De.fields
    [
      De.field "name" Field_name;
      De.field "age" Field_age;
      De.field "active" Field_active;
      De.field "small" Field_small;
      De.field "big" Field_big;
      De.field "ratio" Field_ratio;
      De.field "tags" Field_tags;
      De.field "scores" Field_scores;
      De.field "nickname" Field_nickname;
      De.field "status" Field_status;
    ]

let status_decode =
  De.variant
    [
      De.Variant.unit "Active" Active;
      De.Variant.unit "Draft" Draft;
      De.Variant.unit "Archived" Archived;
    ]

let status_encode =
  Ser.variant
    [
      Ser.Variant.unit
        "Active"
        (fun __tmp1 ->
          match __tmp1 with
          | Active -> true
          | _ -> false);
      Ser.Variant.unit
        "Draft"
        (fun __tmp1 ->
          match __tmp1 with
          | Draft -> true
          | _ -> false);
      Ser.Variant.unit
        "Archived"
        (fun __tmp1 ->
          match __tmp1 with
          | Archived -> true
          | _ -> false);
    ]

let sample_decode =
  De.record_mut
    ~fields:sample_fields
    ~create:(fun (): sample_builder ->
      {
        name = None;
        age = None;
        active = None;
        small = None;
        big = None;
        ratio = None;
        tags = None;
        scores = None;
        nickname = None;
        status = None;
      })
    ~step:(fun reader builder field ->
      match field with
      | Some Field_name -> builder.name <- Some (De.read reader De.string)
      | Some Field_age -> builder.age <- Some (De.read reader De.int)
      | Some Field_active -> builder.active <- Some (De.read reader De.bool)
      | Some Field_small -> builder.small <- Some (De.read reader De.int32)
      | Some Field_big -> builder.big <- Some (De.read reader De.int64)
      | Some Field_ratio -> builder.ratio <- Some (De.read reader De.float)
      | Some Field_tags -> builder.tags <- Some (De.read reader (De.list De.string))
      | Some Field_scores -> builder.scores <- Some (De.read reader (De.array De.int))
      | Some Field_nickname -> builder.nickname <- Some (De.read reader (De.option De.string))
      | Some Field_status -> builder.status <- Some (De.read reader status_decode)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun (builder: sample_builder) ->
      match (
        builder.name,
        builder.age,
        builder.active,
        builder.small,
        builder.big,
        builder.ratio,
        builder.tags,
        builder.scores,
        builder.status
      ) with
      | (
          Some name,
          Some age,
          Some active,
          Some small,
          Some big,
          Some ratio,
          Some tags,
          Some scores,
          Some status
        ) ->
          let nickname =
            match builder.nickname with
            | Some nickname -> nickname
            | None -> None
          in
          ({
            name;
            age;
            active;
            small;
            big;
            ratio;
            tags;
            scores;
            nickname;
            status;
          }: sample)
      | _ -> De.missing_field ())

let sample_encode =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "name" Ser.string (fun (value: sample) -> value.name);
          Ser.field "age" Ser.int (fun (value: sample) -> value.age);
          Ser.field "active" Ser.bool (fun (value: sample) -> value.active);
          Ser.field "small" Ser.int32 (fun (value: sample) -> value.small);
          Ser.field "big" Ser.int64 (fun (value: sample) -> value.big);
          Ser.field "ratio" Ser.float (fun (value: sample) -> value.ratio);
          Ser.field "tags" (Ser.list Ser.string) (fun (value: sample) -> value.tags);
          Ser.field "scores" (Ser.array Ser.int) (fun (value: sample) -> value.scores);
          Ser.field "nickname" (Ser.option Ser.string) (fun (value: sample) -> value.nickname);
          Ser.field "status" status_encode (fun (value: sample) -> value.status);
        ]
    )

let address_encode =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "city" Ser.string (fun (value: address) -> value.city);
        ]
    )

let nested_encode =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "title" Ser.string (fun (value: nested) -> value.title);
          Ser.field "address" address_encode (fun (value: nested) -> value.address);
        ]
    )

let pet_encode =
  Ser.variant
    [
      Ser.Variant.unit
        "Cat"
        (fun __tmp1 ->
          match __tmp1 with
          | Cat -> true
          | _ -> false);
      Ser.Variant.newtype
        "Dog"
        Ser.string
        (fun __tmp1 ->
          match __tmp1 with
          | Dog value -> Some value
          | _ -> None);
    ]

let vec_to_list = fun values ->
  let items = ref [] in
  Vector.for_each values ~fn:(fun value -> items := value :: !items);
  List.rev !items

let equal_sample = fun (left: sample) (right: sample) ->
  String.equal left.name right.name
  && Int.equal left.age right.age
  && Bool.equal left.active right.active
  && Int32.equal left.small right.small
  && Int64.equal left.big right.big
  && Float.equal left.ratio right.ratio
  && vec_to_list left.tags = vec_to_list right.tags
  && left.scores = right.scores
  && left.nickname = right.nickname
  && left.status = right.status

let expect_equal = fun ~expected ~actual ~message ->
  if actual = expected then
    Ok ()
  else
    Error message

let sample_value: sample = ({
  name = "Monkey D. Luffy";
  age = 19;
  active = true;
  small = 12l;
  big = 345L;
  ratio = 1.25;
  tags = Vector.from_list [ "riot"; "serde ml" ];
  scores = [|1; 2|];
  nickname = None;
  status = Draft;
}: sample)

let sample_value_with_nickname: sample = ({
  name = "Monkey D. Luffy";
  age = 19;
  active = true;
  small = 12l;
  big = 345L;
  ratio = 1.25;
  tags = Vector.from_list [ "riot"; "serde ml" ];
  scores = [|1; 2|];
  nickname = Some "strawhat";
  status = Draft;
}: sample)

let encoded_sample_without_nickname =
  "name=Monkey+D.+Luffy&age=19&active=true&small=12&big=345&ratio=1.25&tags=riot&tags=serde+ml&scores=1&scores=2&status=Draft"

let encoded_sample_with_nickname =
  "name=Monkey+D.+Luffy&age=19&active=true&small=12&big=345&ratio=1.25&tags=riot&tags=serde+ml&scores=1&scores=2&nickname=strawhat&status=Draft"

let test_encodes_records = fun _ctx ->
  match Serde_urlencoded.to_string sample_encode sample_value with
  | Ok actual ->
      expect_equal
        ~expected:encoded_sample_without_nickname
        ~actual
        ~message:"expected serde-urlencoded encoder to serialize flat records with repeated keys for sequences"
  | Error err -> Error ("encode failed: " ^ Serde.Error.to_string err)

let test_decodes_records = fun _ctx ->
  let input = "unused=ignored&" ^ encoded_sample_without_nickname in
  match Serde_urlencoded.from_string sample_decode input with
  | Ok actual ->
      if equal_sample actual sample_value then
        Ok ()
      else
        Error "expected serde-urlencoded decoder to parse flat records and repeated keys"
  | Error err -> Error ("decode failed: " ^ Serde.Error.to_string err)

let test_roundtrips_records = fun _ctx ->
  let* encoded =
    match Serde_urlencoded.to_string sample_encode sample_value_with_nickname with
    | Ok encoded -> Ok encoded
    | Error err -> Error ("roundtrip encode failed: " ^ Serde.Error.to_string err)
  in
  match Serde_urlencoded.from_string sample_decode encoded with
  | Ok actual ->
      if equal_sample actual sample_value_with_nickname then
        Ok ()
      else
        Error "expected serde-urlencoded encode/decode to roundtrip supported record values"
  | Error err -> Error ("roundtrip decode failed: " ^ Serde.Error.to_string err)

let test_decodes_from_reader = fun _ctx ->
  match Serde_urlencoded.from_reader
    sample_decode
    (String.to_reader ~chunk_size:2 encoded_sample_with_nickname) with
  | Ok actual ->
      if equal_sample actual sample_value_with_nickname then
        Ok ()
      else
        Error "expected serde-urlencoded decoder to read from an IO.Reader"
  | Error err -> Error ("reader decode failed: " ^ Serde.Error.to_string err)

let test_writes_to_writer = fun _ctx ->
  let buffer = IO.Buffer.create ~size:128 in
  match Serde_urlencoded.to_writer
    sample_encode
    (io_writer_of_buffer buffer)
    sample_value_with_nickname with
  | Ok () ->
      expect_equal
        ~expected:encoded_sample_with_nickname
        ~actual:(IO.Buffer.contents buffer)
        ~message:"expected serde-urlencoded encoder to write to an IO.Writer"
  | Error err -> Error ("writer encode failed: " ^ Serde.Error.to_string err)

let test_rejects_nested_record_values = fun _ctx ->
  let value = { title = "Sunny"; address = { city = "Wano" } } in
  match Serde_urlencoded.to_string nested_encode value with
  | Error (`Msg message) when String.contains message "record" -> Ok ()
  | Error err ->
      Error ("expected nested record encode to fail clearly, got " ^ Serde.Error.to_string err)
  | Ok encoded -> Error ("expected nested record encode to fail, got " ^ encoded)

let test_rejects_payload_variant_values = fun _ctx ->
  match Serde_urlencoded.to_string
    (
      Ser.record
        (
          Ser.fields
            [
              Ser.field "pet" pet_encode (fun value -> value);
            ]
        )
    )
    (Dog "Chouchou") with
  | Error (`Msg message) when String.contains message "payload variant" -> Ok ()
  | Error err ->
      Error ("expected payload variant encode to fail clearly, got " ^ Serde.Error.to_string err)
  | Ok encoded -> Error ("expected payload variant encode to fail, got " ^ encoded)

let test_rejects_top_level_scalars = fun _ctx ->
  match Serde_urlencoded.to_string Ser.int 42 with
  | Error (`Msg message) when String.contains message "top-level" -> Ok ()
  | Error err ->
      Error ("expected top-level scalar encode to fail clearly, got " ^ Serde.Error.to_string err)
  | Ok encoded -> Error ("expected top-level scalar encode to fail, got " ^ encoded)

let test_encodes_top_level_unit_as_empty = fun _ctx ->
  match Serde_urlencoded.to_string Ser.null () with
  | Ok actual ->
      expect_equal ~expected:"" ~actual ~message:"expected top-level unit encoding to be empty"
  | Error err -> Error ("unit encode failed: " ^ Serde.Error.to_string err)

let tests =
  Test.[
    case "serde-urlencoded encodes records" test_encodes_records;
    case "serde-urlencoded decodes records" test_decodes_records;
    case "serde-urlencoded roundtrips records" test_roundtrips_records;
    case "serde-urlencoded decodes from readers" test_decodes_from_reader;
    case "serde-urlencoded writes to writers" test_writes_to_writer;
    case "serde-urlencoded rejects nested record values" test_rejects_nested_record_values;
    case "serde-urlencoded rejects payload variant values" test_rejects_payload_variant_values;
    case "serde-urlencoded rejects top-level scalars" test_rejects_top_level_scalars;
    case "serde-urlencoded encodes top-level unit as empty" test_encodes_top_level_unit_as_empty;
  ]

let main ~args = Test.Cli.main ~name:"serde_urlencoded_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
