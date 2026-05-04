open Std
open Propane
open Std.Result.Syntax

module Test = Std.Test
module Vector = Collections.Vector
module De = Serde.De
module Ser = Serde.Ser

let primitive_examples = 5_000

let composite_examples = 1_000

let io_chunk_size = 3

let io_writer_of_buffer =
  let module Write = struct
    type t = IO.Buffer.t

    let write = fun buffer ~from ->
      let written = IO.Buffer.readable_bytes from in
      IO.Buffer.append_slice buffer (IO.Buffer.readable from)
      |> Result.expect ~msg:"serde-urlencoded property writer should append buffer contents";
      Ok written

    let write_vectored = fun buffer ~from ->
      let written = ref 0 in
      IO.IoVec.for_each
        from
        ~fn:(fun chunk ->
          IO.Buffer.append_slice buffer chunk
          |> Result.expect ~msg:"serde-urlencoded property writer should append slices";
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

let single_field_decode = fun field_name decode ->
  let fields = De.fields [ De.field field_name () ] in
  De.record_mut
    ~fields
    ~create:(fun () -> ref None)
    ~step:(fun reader value field ->
      match field with
      | Some () -> value := Some (De.read reader decode)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun value ->
      match !value with
      | Some value -> value
      | None -> De.missing_field ())

let single_field_encode = fun field_name encode ->
  Ser.record
    (
      Ser.fields
        [
          Ser.field field_name encode (fun value -> value);
        ]
    )

let optional_field_decode = fun field_name decode ->
  let fields = De.fields [ De.field field_name () ] in
  De.record_mut
    ~fields
    ~create:(fun () -> ref None)
    ~step:(fun reader value field ->
      match field with
      | Some () -> value := Some (De.read reader decode)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun value ->
      match !value with
      | Some value -> value
      | None -> None)

let empty_decode =
  De.record_mut
    ~fields:(De.fields [])
    ~create:(fun () -> ())
    ~step:(fun _reader _builder _field -> ())
    ~finish:(fun () -> ())

let empty_encode = Ser.record (Ser.fields [])

let vec_to_list = fun values ->
  let items = ref [] in
  Vector.for_each values ~fn:(fun value -> items := value :: !items);
  List.rev !items

let equal_vec = fun left right -> vec_to_list left = vec_to_list right

let equal_float = fun left right -> Float.equal left right

let equal_status = fun left right -> left = right

let equal_sample = fun (left: sample) (right: sample) ->
  String.equal left.name right.name
  && Int.equal left.age right.age
  && Bool.equal left.active right.active
  && Int32.equal left.small right.small
  && Int64.equal left.big right.big
  && equal_float left.ratio right.ratio
  && equal_vec left.tags right.tags
  && left.scores = right.scores
  && left.nickname = right.nickname
  && equal_status left.status right.status

let print_status = fun __tmp1 ->
  match __tmp1 with
  | Active -> "Active"
  | Draft -> "Draft"
  | Archived -> "Archived"

let print_sample = fun (value: sample) ->
  String.concat
    ""
    [
      "{ name = ";
      Printer.string value.name;
      "; age = ";
      Printer.int value.age;
      "; active = ";
      Printer.bool value.active;
      "; small = ";
      Printer.int32 value.small;
      "; big = ";
      Printer.int64 value.big;
      "; ratio = ";
      Printer.float value.ratio;
      "; tags = ";
      Printer.vector Printer.string value.tags;
      "; scores = ";
      Printer.array Printer.int value.scores;
      "; nickname = ";
      Printer.option Printer.string value.nickname;
      "; status = ";
      print_status value.status;
      " }";
    ]

let finite_float_limit = 1.0e12

let finite_float_gen = Generator.float_range (-.finite_float_limit) finite_float_limit

let finite_float_arb = Arbitrary.make ~shrink:Shrinker.float ~print:Printer.float finite_float_gen

let status_gen =
  Generator.frequency
    [ (1, Generator.return Active); (1, Generator.return Draft); (1, Generator.return Archived); ]

let status_arb = Arbitrary.make ~print:print_status status_gen

let non_empty_string_vec_gen = Generator.vector_size (Generator.int_range 1 10) Generator.string

let non_empty_string_vec_arb =
  Arbitrary.make ~print:(Printer.vector Printer.string) non_empty_string_vec_gen

let non_empty_int_array_gen = Generator.array_size (Generator.int_range 1 10) Generator.int

let non_empty_int_array_arb =
  Arbitrary.make ~print:(Printer.array Printer.int) non_empty_int_array_gen

let sample_gen =
  Generator.map3
    (fun (((name, age), active), (small, big, ratio)) (tags, scores, nickname) status ->
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
      }: sample))
    (Generator.pair
      (Generator.pair (Generator.pair Generator.string Generator.int) Generator.bool)
      (Generator.triple Generator.int32 Generator.int64 finite_float_gen))
    (Generator.triple
      non_empty_string_vec_gen
      non_empty_int_array_gen
      (Generator.option Generator.string))
    status_gen

let sample_arb = Arbitrary.make ~print:print_sample sample_gen

let run_property = fun ?(examples = primitive_examples) name arb predicate ->
  let config = { Property.default_config with test_count = examples } in
  let prop = Property.for_all arb predicate in
  Test.property
    ~size:Test.Large
    name
    ~examples
    (fun _ctx ->
      match Property.check ~config ~on_progress:(Test.Context.emit_progress _ctx) prop with
      | Property.Success -> Ok ()
      | Property.Failure { counter_example; shrink_steps } ->
          Error (String.concat
            "\n"
            [
              "Property failed";
              "Counter-example (after " ^ Int.to_string shrink_steps ^ " shrink steps):";
              counter_example;
            ])
      | Property.Error { exception_; backtrace } ->
          Error (String.concat
            "\n"
            [ "Exception raised:"; Exception.to_string exception_; backtrace ])
      | Property.Assumption_violated ->
          Error "Too many test cases violated assumptions (>10x test count)")

let roundtrip_in_memory = fun encode decode equal value ->
  match Serde_urlencoded.to_string encode value with
  | Ok encoded -> (
      match Serde_urlencoded.from_string decode encoded with
      | Ok decoded -> equal decoded value
      | Error err -> fail ("decode failed: " ^ Serde.Error.to_string err)
    )
  | Error err -> fail ("encode failed: " ^ Serde.Error.to_string err)

let roundtrip_io = fun encode decode equal value ->
  let buffer = IO.Buffer.create ~size:64 in
  match Serde_urlencoded.to_writer encode (io_writer_of_buffer buffer) value with
  | Ok () -> (
      match Serde_urlencoded.from_reader
        decode
        (String.to_reader ~chunk_size:io_chunk_size (IO.Buffer.contents buffer)) with
      | Ok decoded -> equal decoded value
      | Error err -> fail ("reader decode failed: " ^ Serde.Error.to_string err)
    )
  | Error err -> fail ("writer encode failed: " ^ Serde.Error.to_string err)

let unit_roundtrip_prop =
  run_property
    "serde-urlencoded property empty record roundtrips"
    Arbitrary.bool
    (fun _ ->
      roundtrip_in_memory empty_encode empty_decode (fun () () -> true) ())

let bool_roundtrip_prop =
  run_property
    "serde-urlencoded property bool field roundtrips"
    Arbitrary.bool
    (roundtrip_in_memory
      (single_field_encode "value" Ser.bool)
      (single_field_decode "value" De.bool)
      Bool.equal)

let int_roundtrip_prop =
  run_property
    "serde-urlencoded property int field roundtrips"
    Arbitrary.int
    (roundtrip_in_memory
      (single_field_encode "value" Ser.int)
      (single_field_decode "value" De.int)
      Int.equal)

let int32_roundtrip_prop =
  run_property
    "serde-urlencoded property int32 field roundtrips"
    Arbitrary.int32
    (roundtrip_in_memory
      (single_field_encode "value" Ser.int32)
      (single_field_decode "value" De.int32)
      Int32.equal)

let int64_roundtrip_prop =
  run_property
    "serde-urlencoded property int64 field roundtrips"
    Arbitrary.int64
    (roundtrip_in_memory
      (single_field_encode "value" Ser.int64)
      (single_field_decode "value" De.int64)
      Int64.equal)

let float_roundtrip_prop =
  run_property
    "serde-urlencoded property float field roundtrips"
    finite_float_arb
    (roundtrip_in_memory
      (single_field_encode "value" Ser.float)
      (single_field_decode "value" De.float)
      equal_float)

let string_roundtrip_prop =
  run_property
    "serde-urlencoded property string field roundtrips"
    Arbitrary.string
    (roundtrip_in_memory
      (single_field_encode "value" Ser.string)
      (single_field_decode "value" De.string)
      String.equal)

let option_string_roundtrip_prop =
  run_property
    "serde-urlencoded property option string field roundtrips"
    Arbitrary.(option string)
    (roundtrip_in_memory
      (single_field_encode "value" (Ser.option Ser.string))
      (optional_field_decode "value" (De.option De.string))
      ( = ))

let string_list_roundtrip_prop =
  run_property
    ~examples:composite_examples
    "serde-urlencoded property string list field roundtrips"
    non_empty_string_vec_arb
    (roundtrip_in_memory
      (single_field_encode "value" (Ser.list Ser.string))
      (single_field_decode "value" (De.list De.string))
      equal_vec)

let int_array_roundtrip_prop =
  run_property
    ~examples:composite_examples
    "serde-urlencoded property int array field roundtrips"
    non_empty_int_array_arb
    (roundtrip_in_memory
      (single_field_encode "value" (Ser.array Ser.int))
      (single_field_decode "value" (De.array De.int))
      ( = ))

let status_roundtrip_prop =
  run_property
    ~examples:composite_examples
    "serde-urlencoded property unit enum field roundtrips"
    status_arb
    (roundtrip_in_memory
      (single_field_encode "value" status_encode)
      (single_field_decode "value" status_decode)
      equal_status)

let sample_roundtrip_prop =
  run_property
    ~examples:composite_examples
    "serde-urlencoded property sample roundtrips"
    sample_arb
    (roundtrip_in_memory sample_encode sample_decode equal_sample)

let sample_io_roundtrip_prop =
  run_property
    ~examples:composite_examples
    "serde-urlencoded property sample roundtrips over io"
    sample_arb
    (roundtrip_io sample_encode sample_decode equal_sample)

let tests = [
  unit_roundtrip_prop;
  bool_roundtrip_prop;
  int_roundtrip_prop;
  int32_roundtrip_prop;
  int64_roundtrip_prop;
  float_roundtrip_prop;
  string_roundtrip_prop;
  option_string_roundtrip_prop;
  string_list_roundtrip_prop;
  int_array_roundtrip_prop;
  status_roundtrip_prop;
  sample_roundtrip_prop;
  sample_io_roundtrip_prop;
]

let main ~args = Test.Cli.main ~name:"serde_urlencoded_property_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
