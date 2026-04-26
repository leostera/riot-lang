open Std
open Propane

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
      |> Result.expect ~msg:"serde-yaml property writer should append buffer contents";
      Ok written

    let write_vectored = fun buffer ~from ->
      let written = ref 0 in
      IO.IoVec.for_each
        from
        ~fn:(fun chunk ->
          IO.Buffer.append_slice buffer chunk
          |> Result.expect ~msg:"serde-yaml property writer should append slices";
          written := !written + IO.IoSlice.length chunk);
      Ok !written

    let flush = fun _buffer -> Ok ()
  end in
  fun buffer -> IO.Writer.from_sink (module Write) buffer

type mode =
  | Idle
  | Named of string
  | Counted of int
  | Sampled of float

type companion =
  | NewsCoo
  | Reindeer of string

type berth = { island: string; berth: int }

type sample = {
  ready: bool;
  count: int;
  small: int32;
  big: int64;
  ratio: float;
  label: string;
  alias: string option;
  mode: mode;
  companion: companion;
  marker: unit;
  home: berth;
  tags: string vec;
  scores: int array;
}

type berth_field =
  | Berth_island
  | Berth_berth

type sample_field =
  | Field_ready
  | Field_count
  | Field_small
  | Field_big
  | Field_ratio
  | Field_label
  | Field_alias
  | Field_mode
  | Field_companion
  | Field_marker
  | Field_home
  | Field_tags
  | Field_scores

type berth_builder = {
  mutable island: string option;
  mutable berth: int option;
}

type sample_builder = {
  mutable ready: bool option;
  mutable count: int option;
  mutable small: int32 option;
  mutable big: int64 option;
  mutable ratio: float option;
  mutable label: string option;
  mutable alias: string option option;
  mutable mode: mode option;
  mutable companion: companion option;
  mutable marker: unit option;
  mutable home: berth option;
  mutable tags: string vec option;
  mutable scores: int array option;
}

let berth_fields = De.fields [ De.field "island" Berth_island; De.field "berth" Berth_berth ]

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
      De.field "mode" Field_mode;
      De.field "companion" Field_companion;
      De.field "marker" Field_marker;
      De.field "home" Field_home;
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
        (
          function
          | Idle -> true
          | _ -> false
        );
      Ser.Variant.newtype
        "Named"
        Ser.string
        (
          function
          | Named value -> Some value
          | _ -> None
        );
      Ser.Variant.newtype
        "Counted"
        Ser.int
        (
          function
          | Counted value -> Some value
          | _ -> None
        );
      Ser.Variant.newtype
        "Sampled"
        Ser.float
        (
          function
          | Sampled value -> Some value
          | _ -> None
        );
    ]

let companion_decode =
  De.variant
    [
      De.Variant.unit "NewsCoo" NewsCoo;
      De.Variant.newtype "Reindeer" De.string (fun value -> Reindeer value);
    ]

let companion_encode =
  Ser.variant
    [
      Ser.Variant.unit
        "NewsCoo"
        (
          function
          | NewsCoo -> true
          | _ -> false
        );
      Ser.Variant.newtype
        "Reindeer"
        Ser.string
        (
          function
          | Reindeer value -> Some value
          | _ -> None
        );
    ]

let berth_decode =
  De.record_mut
    ~fields:berth_fields
    ~create:(fun (): berth_builder -> { island = None; berth = None })
    ~step:(fun reader builder field ->
      match field with
      | Some Berth_island -> builder.island <- Some (De.read reader De.string)
      | Some Berth_berth -> builder.berth <- Some (De.read reader De.int)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (builder.island, builder.berth) with
      | (Some island, Some berth) -> ({ island; berth }: berth)
      | _ -> De.missing_field ())

let berth_encode =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "island" Ser.string (fun (value: berth) -> value.island);
          Ser.field "berth" Ser.int (fun (value: berth) -> value.berth);
        ]
    )

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
        mode = None;
        companion = None;
        marker = None;
        home = None;
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
      | Some Field_mode -> builder.mode <- Some (De.read reader mode_decode)
      | Some Field_companion -> builder.companion <- Some (De.read reader companion_decode)
      | Some Field_marker -> builder.marker <- Some (De.read reader (De.const ()))
      | Some Field_home -> builder.home <- Some (De.read reader berth_decode)
      | Some Field_tags -> builder.tags <- Some (De.read reader (De.list De.string))
      | Some Field_scores -> builder.scores <- Some (De.read reader (De.array De.int))
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (
        builder.ready,
        builder.count,
        builder.small,
        builder.big,
        builder.ratio,
        builder.label,
        builder.alias,
        builder.mode,
        builder.companion,
        builder.marker,
        builder.home,
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
        Some mode,
        Some companion,
        Some marker,
        Some home,
        Some tags,
        Some scores
      ) ->
          ({
            ready;
            count;
            small;
            big;
            ratio;
            label;
            alias;
            mode;
            companion;
            marker;
            home;
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
          Ser.field "mode" mode_encode (fun (value: sample) -> value.mode);
          Ser.field "companion" companion_encode (fun (value: sample) -> value.companion);
          Ser.field "marker" Ser.null (fun (value: sample) -> value.marker);
          Ser.field "home" berth_encode (fun (value: sample) -> value.home);
          Ser.field "tags" (Ser.list Ser.string) (fun (value: sample) -> value.tags);
          Ser.field "scores" (Ser.array Ser.int) (fun (value: sample) -> value.scores);
        ]
    )

let vec_to_list = fun values ->
  let items = ref [] in
  Vector.for_each values ~fn:(fun value -> items := value :: !items);
  List.rev !items

let equal_mode = fun left right ->
  match (left, right) with
  | (Idle, Idle) -> true
  | (Named left, Named right) -> String.equal left right
  | (Counted left, Counted right) -> Int.equal left right
  | (Sampled left, Sampled right) -> Float.equal left right
  | _ -> false

let equal_companion = fun left right ->
  match (left, right) with
  | (NewsCoo, NewsCoo) -> true
  | (Reindeer left, Reindeer right) -> String.equal left right
  | _ -> false

let equal_berth = fun (left: berth) (right: berth) ->
  String.equal left.island right.island && Int.equal left.berth right.berth

let equal_string_vec = fun left right -> vec_to_list left = vec_to_list right

let equal_sample = fun (left: sample) (right: sample) ->
  Bool.equal left.ready right.ready
  && Int.equal left.count right.count
  && Int32.equal left.small right.small
  && Int64.equal left.big right.big
  && Float.equal left.ratio right.ratio
  && String.equal left.label right.label
  && left.alias = right.alias
  && equal_mode left.mode right.mode
  && equal_companion left.companion right.companion
  && equal_berth left.home right.home
  && equal_string_vec left.tags right.tags
  && left.scores = right.scores

let print_mode = function
  | Idle -> "Idle"
  | Named value -> "Named " ^ Printer.string value
  | Counted value -> "Counted " ^ Printer.int value
  | Sampled value -> "Sampled " ^ Printer.float value

let print_companion = function
  | NewsCoo -> "NewsCoo"
  | Reindeer value -> "Reindeer " ^ Printer.string value

let print_berth = fun (value: berth) ->
  "{ island = " ^ Printer.string value.island ^ "; berth = " ^ Printer.int value.berth ^ " }"

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
      "; mode = ";
      print_mode value.mode;
      "; companion = ";
      print_companion value.companion;
      "; home = ";
      print_berth value.home;
      "; tags = ";
      Printer.vector Printer.string value.tags;
      "; scores = ";
      Printer.array Printer.int value.scores;
      " }";
    ]

let finite_float_limit = 1.0e12

let finite_float_gen = Generator.float_range (-.finite_float_limit) finite_float_limit

let finite_float_arb = Arbitrary.make ~shrink:Shrinker.float ~print:Printer.float finite_float_gen

let mode_gen =
  Generator.frequency
    [
      (1, Generator.return Idle);
      (3, Generator.map (fun value -> Named value) Generator.string);
      (3, Generator.map (fun value -> Counted value) Generator.int);
      (3, Generator.map (fun value -> Sampled value) finite_float_gen);
    ]

let mode_arb = Arbitrary.make ~print:print_mode mode_gen

let companion_gen =
  Generator.frequency
    [
      (1, Generator.return NewsCoo);
      (3, Generator.map (fun value -> Reindeer value) Generator.string);
    ]

let companion_arb = Arbitrary.make ~print:print_companion companion_gen

let berth_gen =
  Generator.map
    (fun (island, berth) -> ({ island; berth }: berth))
    (Generator.pair Generator.string Generator.int)

let berth_arb = Arbitrary.make ~print:print_berth berth_gen

let sample_gen =
  Generator.map3
    (fun (((ready, count), small), (big, ratio)) (label, alias, tags) (
      scores,
      mode,
      companion,
      home
    ) -> ({
      ready;
      count;
      small;
      big;
      ratio;
      label;
      alias;
      mode;
      companion;
      marker = ();
      home;
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
    (Generator.map
      (fun ((scores, mode), (companion, home)) -> (scores, mode, companion, home))
      (Generator.pair
        (Generator.pair (Generator.array Generator.int) mode_gen)
        (Generator.pair companion_gen berth_gen)))

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
  match Serde_yaml.to_string encode value with
  | Ok encoded -> (
      match Serde_yaml.from_string decode encoded with
      | Ok decoded ->
          if equal decoded value then
            true
          else
            fail ("roundtrip mismatch\nencoded: " ^ encoded)
      | Error err -> fail ("decode failed: " ^ Serde.Error.to_string err)
    )
  | Error err -> fail ("encode failed: " ^ Serde.Error.to_string err)

let roundtrip_io = fun encode decode equal value ->
  let buffer = IO.Buffer.create ~size:64 in
  match Serde_yaml.to_writer encode (io_writer_of_buffer buffer) value with
  | Ok () -> (
      match Serde_yaml.from_reader
        decode
        (String.to_reader ~chunk_size:io_chunk_size (IO.Buffer.contents buffer)) with
      | Ok decoded -> equal decoded value
      | Error err -> fail ("reader decode failed: " ^ Serde.Error.to_string err)
    )
  | Error err -> fail ("writer encode failed: " ^ Serde.Error.to_string err)

let bool_roundtrip_prop =
  run_property
    "serde-yaml property bool roundtrips"
    Arbitrary.bool
    (roundtrip_in_memory Ser.bool De.bool Bool.equal)

let int_roundtrip_prop =
  run_property
    "serde-yaml property int roundtrips"
    Arbitrary.int
    (roundtrip_in_memory Ser.int De.int Int.equal)

let int32_roundtrip_prop =
  run_property
    "serde-yaml property int32 roundtrips"
    Arbitrary.int32
    (roundtrip_in_memory Ser.int32 De.int32 Int32.equal)

let int64_roundtrip_prop =
  run_property
    "serde-yaml property int64 roundtrips"
    Arbitrary.int64
    (roundtrip_in_memory Ser.int64 De.int64 Int64.equal)

let float_roundtrip_prop =
  run_property
    "serde-yaml property float roundtrips"
    finite_float_arb
    (roundtrip_in_memory Ser.float De.float Float.equal)

let string_roundtrip_prop =
  run_property
    "serde-yaml property string roundtrips"
    Arbitrary.string
    (roundtrip_in_memory Ser.string De.string String.equal)

let option_string_roundtrip_prop =
  run_property
    "serde-yaml property option string roundtrips"
    Arbitrary.(option string)
    (roundtrip_in_memory (Ser.option Ser.string) (De.option De.string) ( = ))

let mode_roundtrip_prop =
  run_property
    ~examples:composite_examples
    "serde-yaml property mode roundtrips"
    mode_arb
    (roundtrip_in_memory mode_encode mode_decode equal_mode)

let companion_roundtrip_prop =
  run_property
    ~examples:composite_examples
    "serde-yaml property companion roundtrips"
    companion_arb
    (roundtrip_in_memory companion_encode companion_decode equal_companion)

let berth_roundtrip_prop =
  run_property
    ~examples:composite_examples
    "serde-yaml property berth roundtrips"
    berth_arb
    (roundtrip_in_memory berth_encode berth_decode equal_berth)

let string_vec_roundtrip_prop =
  run_property
    ~examples:composite_examples
    "serde-yaml property string vectors roundtrip"
    Arbitrary.(vector string)
    (roundtrip_in_memory (Ser.list Ser.string) (De.list De.string) equal_string_vec)

let int_array_roundtrip_prop =
  run_property
    ~examples:composite_examples
    "serde-yaml property int arrays roundtrip"
    Arbitrary.(array int)
    (roundtrip_in_memory (Ser.array Ser.int) (De.array De.int) ( = ))

let sample_roundtrip_prop =
  run_property
    ~examples:composite_examples
    "serde-yaml property sample roundtrips"
    sample_arb
    (roundtrip_in_memory sample_encode sample_decode equal_sample)

let sample_io_roundtrip_prop =
  run_property
    ~examples:composite_examples
    "serde-yaml property sample roundtrips over io"
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
  mode_roundtrip_prop;
  companion_roundtrip_prop;
  berth_roundtrip_prop;
  string_vec_roundtrip_prop;
  int_array_roundtrip_prop;
  sample_roundtrip_prop;
  sample_io_roundtrip_prop;
]

let main ~args = Test.Cli.main ~name:"serde_yaml_property_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
