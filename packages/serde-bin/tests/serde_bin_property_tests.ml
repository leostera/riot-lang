open Std
open Propane

module Test = Std.Test
module Vector = Collections.Vector
module De = Serde.De
module Ser = Serde.Ser

let primitive_examples = 5_000

let composite_examples = 1_000

let io_chunk_size = 3

let finite_float_limit = 1.0e12

let finite_float_gen = Generator.float_range (-.finite_float_limit) finite_float_limit

let finite_float_arb = Arbitrary.make ~shrink:Shrinker.float ~print:Printer.float finite_float_gen

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

type mode =
  | Idle
  | Named of string
  | Counted of int
  | Sampled of float

type pet =
  | Cat
  | Dog of string

type sample = {
  ready: bool;
  count: int;
  small: int32;
  big: int64;
  ratio: float;
  label: string;
  alias: string option;
  pet: pet;
  mode: mode;
  tags: string vec;
  scores: int array;
}

type sample_field =
  | Field_ready
  | Field_count
  | Field_small
  | Field_big
  | Field_ratio
  | Field_label
  | Field_alias
  | Field_pet
  | Field_mode
  | Field_tags
  | Field_scores

type sample_builder = {
  mutable ready: bool option;
  mutable count: int option;
  mutable small: int32 option;
  mutable big: int64 option;
  mutable ratio: float option;
  mutable label: string option;
  mutable alias: string option option;
  mutable pet: pet option;
  mutable mode: mode option;
  mutable tags: string vec option;
  mutable scores: int array option;
}

let sample_fields =
  De.fields
    [
      De.field "ready" Field_ready;
      De.field "count" Field_count;
      De.field "small" Field_small;
      De.field "big" Field_big;
      De.field "ratio" Field_ratio;
      De.field "label" Field_label;
      De.field "alias" Field_alias;
      De.field "pet" Field_pet;
      De.field "mode" Field_mode;
      De.field "tags" Field_tags;
      De.field "scores" Field_scores;
    ]

let mode_decode =
  De.variant
    [
      De.Variant.unit "Idle" Idle;
      De.Variant.newtype "Named" De.string (fun value -> Named value);
      De.Variant.newtype "Counted" De.int (fun value -> Counted value);
      De.Variant.newtype "Sampled" De.float (fun value -> Sampled value);
    ]

let mode_encode =
  Ser.variant
    [
      Ser.Variant.unit
        "Idle"
        (fun __tmp1 ->
          match __tmp1 with
          | Idle -> true
          | _ -> false);
      Ser.Variant.newtype
        "Named"
        Ser.string
        (fun __tmp1 ->
          match __tmp1 with
          | Named value -> Some value
          | _ -> None);
      Ser.Variant.newtype
        "Counted"
        Ser.int
        (fun __tmp1 ->
          match __tmp1 with
          | Counted value -> Some value
          | _ -> None);
      Ser.Variant.newtype
        "Sampled"
        Ser.float
        (fun __tmp1 ->
          match __tmp1 with
          | Sampled value -> Some value
          | _ -> None);
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
          | _ -> false);
      Ser.Variant.newtype
        "Dog"
        Ser.string
        (fun __tmp1 ->
          match __tmp1 with
          | Dog value -> Some value
          | _ -> None);
    ]

let sample_decode =
  De.record_mut
    ~fields:sample_fields
    ~create:(fun (): sample_builder ->
      {
        ready = None;
        count = None;
        small = None;
        big = None;
        ratio = None;
        label = None;
        alias = None;
        pet = None;
        mode = None;
        tags = None;
        scores = None;
      })
    ~step:(fun reader builder field ->
      match field with
      | Some Field_ready -> builder.ready <- Some (De.read reader De.bool)
      | Some Field_count -> builder.count <- Some (De.read reader De.int)
      | Some Field_small -> builder.small <- Some (De.read reader De.int32)
      | Some Field_big -> builder.big <- Some (De.read reader De.int64)
      | Some Field_ratio -> builder.ratio <- Some (De.read reader De.float)
      | Some Field_label -> builder.label <- Some (De.read reader De.string)
      | Some Field_alias -> builder.alias <- Some (De.read reader (De.option De.string))
      | Some Field_pet -> builder.pet <- Some (De.read reader pet_decode)
      | Some Field_mode -> builder.mode <- Some (De.read reader mode_decode)
      | Some Field_tags -> builder.tags <- Some (De.read reader (De.list De.string))
      | Some Field_scores -> builder.scores <- Some (De.read reader (De.array De.int))
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun (builder: sample_builder) ->
      match (
        builder.ready,
        builder.count,
        builder.small,
        builder.big,
        builder.ratio,
        builder.label,
        builder.alias,
        builder.pet,
        builder.mode,
        builder.tags,
        builder.scores
      ) with
      | (
          Some ready,
          Some count,
          Some small,
          Some big,
          Some ratio,
          Some label,
          Some alias,
          Some pet,
          Some mode,
          Some tags,
          Some scores
        ) -> ({
        ready;
        count;
        small;
        big;
        ratio;
        label;
        alias;
        pet;
        mode;
        tags;
        scores;
      }: sample)
      | _ -> De.missing_field ())

let sample_encode =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "ready" Ser.bool (fun (value: sample) -> value.ready);
          Ser.field "count" Ser.int (fun (value: sample) -> value.count);
          Ser.field "small" Ser.int32 (fun (value: sample) -> value.small);
          Ser.field "big" Ser.int64 (fun (value: sample) -> value.big);
          Ser.field "ratio" Ser.float (fun (value: sample) -> value.ratio);
          Ser.field "label" Ser.string (fun (value: sample) -> value.label);
          Ser.field "alias" (Ser.option Ser.string) (fun (value: sample) -> value.alias);
          Ser.field "pet" pet_encode (fun (value: sample) -> value.pet);
          Ser.field "mode" mode_encode (fun (value: sample) -> value.mode);
          Ser.field "tags" (Ser.list Ser.string) (fun (value: sample) -> value.tags);
          Ser.field "scores" (Ser.array Ser.int) (fun (value: sample) -> value.scores);
        ]
    )

let vec_to_list = fun values ->
  let items = ref [] in
  Vector.for_each values ~fn:(fun value -> items := value :: !items);
  List.rev !items

let equal_string_vec = fun left right -> vec_to_list left = vec_to_list right

let equal_mode = fun left right ->
  match (left, right) with
  | (Idle, Idle) -> true
  | (Named left, Named right) -> String.equal left right
  | (Counted left, Counted right) -> Int.equal left right
  | (Sampled left, Sampled right) -> Float.equal left right
  | _ -> false

let equal_pet = fun left right ->
  match (left, right) with
  | (Cat, Cat) -> true
  | (Dog left, Dog right) -> String.equal left right
  | _ -> false

let equal_sample = fun (left: sample) (right: sample) ->
  Bool.equal left.ready right.ready
  && Int.equal left.count right.count
  && Int32.equal left.small right.small
  && Int64.equal left.big right.big
  && Float.equal left.ratio right.ratio
  && String.equal left.label right.label
  && left.alias = right.alias
  && equal_pet left.pet right.pet
  && equal_mode left.mode right.mode
  && equal_string_vec left.tags right.tags
  && left.scores = right.scores

let print_mode = fun __tmp1 ->
  match __tmp1 with
  | Idle -> "Idle"
  | Named value -> "Named " ^ Printer.string value
  | Counted value -> "Counted " ^ Printer.int value
  | Sampled value -> "Sampled " ^ Printer.float value

let print_pet = fun __tmp1 ->
  match __tmp1 with
  | Cat -> "Cat"
  | Dog value -> "Dog " ^ Printer.string value

let print_sample = fun (value: sample) ->
  String.concat
    ""
    [
      "{ ready = ";
      Printer.bool value.ready;
      "; count = ";
      Printer.int value.count;
      "; small = ";
      Printer.int32 value.small;
      "; big = ";
      Printer.int64 value.big;
      "; ratio = ";
      Printer.float value.ratio;
      "; label = ";
      Printer.string value.label;
      "; alias = ";
      Printer.option Printer.string value.alias;
      "; pet = ";
      print_pet value.pet;
      "; mode = ";
      print_mode value.mode;
      "; tags = ";
      Printer.vector Printer.string value.tags;
      "; scores = ";
      Printer.array Printer.int value.scores;
      " }";
    ]

let mode_gen =
  Generator.frequency
    [
      (1, Generator.return Idle);
      (3, Generator.map (fun value -> Named value) Generator.string);
      (3, Generator.map (fun value -> Counted value) Generator.int);
      (3, Generator.map (fun value -> Sampled value) finite_float_gen);
    ]

let mode_arb = Arbitrary.make ~print:print_mode mode_gen

let pet_gen =
  Generator.frequency
    [
      (1, Generator.return Cat);
      (3, Generator.map (fun value -> Dog value) Generator.string);
    ]

let pet_arb = Arbitrary.make ~print:print_pet pet_gen

let sample_gen =
  Generator.map3
    (fun (((ready, count), small), (big, ratio)) (label, alias, tags) (scores, pet, mode) ->
      ({
        ready;
        count;
        small;
        big;
        ratio;
        label;
        alias;
        pet;
        mode;
        tags;
        scores;
      }: sample))
    (Generator.pair
      (Generator.pair (Generator.pair Generator.bool Generator.int) Generator.int32)
      (Generator.pair Generator.int64 finite_float_gen))
    (Generator.triple
      Generator.string
      (Generator.option Generator.string)
      (Generator.vector Generator.string))
    (Generator.triple (Generator.array Generator.int) pet_gen mode_gen)

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
  match Serde_bin.to_string encode value with
  | Ok encoded -> (
      match Serde_bin.from_string decode encoded with
      | Ok decoded -> equal decoded value
      | Error err -> fail ("decode failed: " ^ Serde.Error.to_string err)
    )
  | Error err -> fail ("encode failed: " ^ Serde.Error.to_string err)

let roundtrip_io = fun encode decode equal value ->
  let buffer = IO.Buffer.create ~size:64 in
  match Serde_bin.to_writer encode (io_writer_of_buffer buffer) value with
  | Ok () -> (
      match Serde_bin.from_reader
        decode
        (String.to_reader ~chunk_size:io_chunk_size (IO.Buffer.contents buffer)) with
      | Ok decoded -> equal decoded value
      | Error err -> fail ("reader decode failed: " ^ Serde.Error.to_string err)
    )
  | Error err -> fail ("writer encode failed: " ^ Serde.Error.to_string err)

let bool_roundtrip_prop =
  run_property
    "serde-bin property bool roundtrips"
    Arbitrary.bool
    (roundtrip_in_memory Ser.bool De.bool Bool.equal)

let int_roundtrip_prop =
  run_property
    "serde-bin property int roundtrips"
    Arbitrary.int
    (roundtrip_in_memory Ser.int De.int Int.equal)

let int32_roundtrip_prop =
  run_property
    "serde-bin property int32 roundtrips"
    Arbitrary.int32
    (roundtrip_in_memory Ser.int32 De.int32 Int32.equal)

let int64_roundtrip_prop =
  run_property
    "serde-bin property int64 roundtrips"
    Arbitrary.int64
    (roundtrip_in_memory Ser.int64 De.int64 Int64.equal)

let float_roundtrip_prop =
  run_property
    "serde-bin property float roundtrips"
    finite_float_arb
    (roundtrip_in_memory Ser.float De.float Float.equal)

let string_roundtrip_prop =
  run_property
    "serde-bin property string roundtrips"
    Arbitrary.string
    (roundtrip_in_memory Ser.string De.string String.equal)

let option_string_roundtrip_prop =
  run_property
    "serde-bin property option string roundtrips"
    Arbitrary.(option string)
    (roundtrip_in_memory (Ser.option Ser.string) (De.option De.string) ( = ))

let string_list_roundtrip_prop =
  run_property
    ~examples:composite_examples
    "serde-bin property string list roundtrips"
    Arbitrary.(vector string)
    (roundtrip_in_memory (Ser.list Ser.string) (De.list De.string) equal_string_vec)

let int_array_roundtrip_prop =
  run_property
    ~examples:composite_examples
    "serde-bin property int array roundtrips"
    Arbitrary.(array int)
    (roundtrip_in_memory (Ser.array Ser.int) (De.array De.int) ( = ))

let mode_roundtrip_prop =
  run_property
    ~examples:composite_examples
    "serde-bin property mode roundtrips"
    mode_arb
    (roundtrip_in_memory mode_encode mode_decode equal_mode)

let pet_roundtrip_prop =
  run_property
    ~examples:composite_examples
    "serde-bin property pet roundtrips"
    pet_arb
    (roundtrip_in_memory pet_encode pet_decode equal_pet)

let sample_roundtrip_prop =
  run_property
    ~examples:composite_examples
    "serde-bin property sample roundtrips"
    sample_arb
    (roundtrip_in_memory sample_encode sample_decode equal_sample)

let sample_io_roundtrip_prop =
  run_property
    ~examples:composite_examples
    "serde-bin property sample roundtrips over io"
    sample_arb
    (roundtrip_io sample_encode sample_decode equal_sample)

let tests = [
  bool_roundtrip_prop;
  int_roundtrip_prop;
  int32_roundtrip_prop;
  int64_roundtrip_prop;
  float_roundtrip_prop;
  string_roundtrip_prop;
  option_string_roundtrip_prop;
  string_list_roundtrip_prop;
  int_array_roundtrip_prop;
  mode_roundtrip_prop;
  pet_roundtrip_prop;
  sample_roundtrip_prop;
  sample_io_roundtrip_prop;
]

let main ~args = Test.Cli.main ~name:"serde_bin_property_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
