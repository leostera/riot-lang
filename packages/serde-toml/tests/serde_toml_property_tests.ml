open Std
open Propane
open Std.Result.Syntax

module Test = Std.Test
module Array = Collections.Array
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
      |> Result.expect ~msg:"serde-toml property writer should append buffer contents";
      Ok written

    let write_vectored = fun buffer ~from ->
      let written = ref 0 in
      IO.IoVec.for_each
        from
        ~fn:(fun chunk ->
          IO.Buffer.append_slice buffer chunk
          |> Result.expect ~msg:"serde-toml property writer should append slices";
          written := !written + IO.IoSlice.length chunk);
      Ok !written

    let flush = fun _buffer -> Ok ()
  end in
  fun buffer -> IO.Writer.from_sink (module Write) buffer

type status =
  | Active
  | Draft
  | Archived

type pet =
  | NewsCoo
  | Reindeer of string

type pose = { island: string; bearing: float }

type stop = { island: string; supplies: int }

type sample = {
  title: string;
  active: bool;
  count: int;
  small: int32;
  big: int64;
  ratio: float;
  nickname: string option;
  status: status;
  pet: pet;
  marker: unit;
  pose: pose;
  tags: string vec;
  scores: int array;
  stops: stop vec;
  mirrors: stop array;
}

type pose_field =
  | Pose_island
  | Pose_bearing

type stop_field =
  | Stop_island
  | Stop_supplies

type sample_field =
  | Field_title
  | Field_active
  | Field_count
  | Field_small
  | Field_big
  | Field_ratio
  | Field_nickname
  | Field_status
  | Field_pet
  | Field_marker
  | Field_pose
  | Field_tags
  | Field_scores
  | Field_stops
  | Field_mirrors

type pose_builder = {
  mutable island: string option;
  mutable bearing: float option;
}

type stop_builder = {
  mutable island: string option;
  mutable supplies: int option;
}

type sample_builder = {
  mutable title: string option;
  mutable active: bool option;
  mutable count: int option;
  mutable small: int32 option;
  mutable big: int64 option;
  mutable ratio: float option;
  mutable nickname: string option option;
  mutable status: status option;
  mutable pet: pet option;
  mutable marker: unit option;
  mutable pose: pose option;
  mutable tags: string vec option;
  mutable scores: int array option;
  mutable stops: stop vec option;
  mutable mirrors: stop array option;
}

let pose_fields = De.fields [ De.field "island" Pose_island; De.field "bearing" Pose_bearing ]

let stop_fields = De.fields [ De.field "island" Stop_island; De.field "supplies" Stop_supplies ]

let sample_fields =
  De.fields
    [
      De.field "title" Field_title;
      De.field "active" Field_active;
      De.field "count" Field_count;
      De.field "small" Field_small;
      De.field "big" Field_big;
      De.field "ratio" Field_ratio;
      De.field "nickname" Field_nickname;
      De.field "status" Field_status;
      De.field "pet" Field_pet;
      De.field "marker" Field_marker;
      De.field "pose" Field_pose;
      De.field "tags" Field_tags;
      De.field "scores" Field_scores;
      De.field "stops" Field_stops;
      De.field "mirrors" Field_mirrors;
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
        (
          function
          | Active -> true
          | _ -> false
        );
      Ser.Variant.unit
        "Draft"
        (
          function
          | Draft -> true
          | _ -> false
        );
      Ser.Variant.unit
        "Archived"
        (
          function
          | Archived -> true
          | _ -> false
        );
    ]

let pet_decode =
  De.variant
    [
      De.Variant.unit "NewsCoo" NewsCoo;
      De.Variant.newtype "Reindeer" De.string (fun value -> Reindeer value);
    ]

let pet_encode =
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

let pose_decode =
  De.record_mut
    ~fields:pose_fields
    ~create:(fun (): pose_builder -> { island = None; bearing = None })
    ~step:(fun reader builder field ->
      match field with
      | Some Pose_island -> builder.island <- Some (De.read reader De.string)
      | Some Pose_bearing -> builder.bearing <- Some (De.read reader De.float)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (builder.island, builder.bearing) with
      | (Some island, Some bearing) -> (({ island; bearing }: pose))
      | _ -> De.missing_field ())

let pose_encode =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "island" Ser.string (fun (value: pose) -> value.island);
          Ser.field "bearing" Ser.float (fun (value: pose) -> value.bearing);
        ]
    )

let stop_decode =
  De.record_mut
    ~fields:stop_fields
    ~create:(fun (): stop_builder -> { island = None; supplies = None })
    ~step:(fun reader builder field ->
      match field with
      | Some Stop_island -> builder.island <- Some (De.read reader De.string)
      | Some Stop_supplies -> builder.supplies <- Some (De.read reader De.int)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (builder.island, builder.supplies) with
      | (Some island, Some supplies) -> (({ island; supplies }: stop))
      | _ -> De.missing_field ())

let stop_encode =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "island" Ser.string (fun (value: stop) -> value.island);
          Ser.field "supplies" Ser.int (fun (value: stop) -> value.supplies);
        ]
    )

let sample_decode =
  De.record_mut
    ~fields:sample_fields
    ~create:(fun (): sample_builder ->
      {
        title = None;
        active = None;
        count = None;
        small = None;
        big = None;
        ratio = None;
        nickname = None;
        status = None;
        pet = None;
        marker = None;
        pose = None;
        tags = None;
        scores = None;
        stops = None;
        mirrors = None;
      })
    ~step:(fun reader builder field ->
      match field with
      | Some Field_title -> builder.title <- Some (De.read reader De.string)
      | Some Field_active -> builder.active <- Some (De.read reader De.bool)
      | Some Field_count -> builder.count <- Some (De.read reader De.int)
      | Some Field_small -> builder.small <- Some (De.read reader De.int32)
      | Some Field_big -> builder.big <- Some (De.read reader De.int64)
      | Some Field_ratio -> builder.ratio <- Some (De.read reader De.float)
      | Some Field_nickname -> builder.nickname <- Some (De.read reader (De.option De.string))
      | Some Field_status -> builder.status <- Some (De.read reader status_decode)
      | Some Field_pet -> builder.pet <- Some (De.read reader pet_decode)
      | Some Field_marker -> builder.marker <- Some (De.read reader (De.const ()))
      | Some Field_pose -> builder.pose <- Some (De.read reader pose_decode)
      | Some Field_tags -> builder.tags <- Some (De.read reader (De.list De.string))
      | Some Field_scores -> builder.scores <- Some (De.read reader (De.array De.int))
      | Some Field_stops -> builder.stops <- Some (De.read reader (De.list stop_decode))
      | Some Field_mirrors -> builder.mirrors <- Some (De.read reader (De.array stop_decode))
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun (builder: sample_builder) ->
      match (
        builder.title,
        builder.active,
        builder.count,
        builder.small,
        builder.big,
        builder.ratio,
        builder.status,
        builder.pet,
        builder.marker,
        builder.pose,
        builder.tags,
        builder.scores,
        builder.stops,
        builder.mirrors
      ) with
      | (
        Some title,
        Some active,
        Some count,
        Some small,
        Some big,
        Some ratio,
        Some status,
        Some pet,
        Some marker,
        Some pose,
        Some tags,
        Some scores,
        Some stops,
        Some mirrors
      ) ->
          let nickname =
            match builder.nickname with
            | Some nickname -> nickname
            | None -> None
          in
          (({
            title;
            active;
            count;
            small;
            big;
            ratio;
            nickname;
            status;
            pet;
            marker;
            pose;
            tags;
            scores;
            stops;
            mirrors;
          }: sample))
      | _ -> De.missing_field ())

let sample_encode =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "title" Ser.string (fun (value: sample) -> value.title);
          Ser.field "active" Ser.bool (fun (value: sample) -> value.active);
          Ser.field "count" Ser.int (fun (value: sample) -> value.count);
          Ser.field "small" Ser.int32 (fun (value: sample) -> value.small);
          Ser.field "big" Ser.int64 (fun (value: sample) -> value.big);
          Ser.field "ratio" Ser.float (fun (value: sample) -> value.ratio);
          Ser.field "nickname" (Ser.option Ser.string) (fun (value: sample) -> value.nickname);
          Ser.field "status" status_encode (fun (value: sample) -> value.status);
          Ser.field "pet" pet_encode (fun (value: sample) -> value.pet);
          Ser.field "marker" Ser.null (fun (value: sample) -> value.marker);
          Ser.field "pose" pose_encode (fun (value: sample) -> value.pose);
          Ser.field "tags" (Ser.list Ser.string) (fun (value: sample) -> value.tags);
          Ser.field "scores" (Ser.array Ser.int) (fun (value: sample) -> value.scores);
          Ser.field "stops" (Ser.list stop_encode) (fun (value: sample) -> value.stops);
          Ser.field "mirrors" (Ser.array stop_encode) (fun (value: sample) -> value.mirrors);
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

let equal_vec = fun equal left right ->
  let left = vec_to_list left in
  let right = vec_to_list right in
  match List.compare_lengths ~left ~right with
  | 0 ->
      List.zip left right
      |> List.all ~fn:(fun (left, right) -> equal left right)
  | _ -> false

let equal_pose = fun (left: pose) (right: pose) ->
  String.equal left.island right.island && Float.equal left.bearing right.bearing

let equal_stop = fun (left: stop) (right: stop) ->
  String.equal left.island right.island && Int.equal left.supplies right.supplies

let equal_status = fun left right -> left = right

let equal_pet = fun left right ->
  match (left, right) with
  | (NewsCoo, NewsCoo) -> true
  | (Reindeer left, Reindeer right) -> String.equal left right
  | _ -> false

let equal_sample = fun (left: sample) (right: sample) ->
  String.equal left.title right.title
  && Bool.equal left.active right.active
  && Int.equal left.count right.count
  && Int32.equal left.small right.small
  && Int64.equal left.big right.big
  && Float.equal left.ratio right.ratio
  && left.nickname = right.nickname
  && equal_status left.status right.status
  && equal_pet left.pet right.pet
  && equal_pose left.pose right.pose
  && equal_vec String.equal left.tags right.tags
  && left.scores = right.scores
  && equal_vec equal_stop left.stops right.stops
  && Array.to_list left.mirrors = Array.to_list right.mirrors

let print_status = function
  | Active -> "Active"
  | Draft -> "Draft"
  | Archived -> "Archived"

let print_pet = function
  | NewsCoo -> "NewsCoo"
  | Reindeer value -> "Reindeer " ^ Printer.string value

let print_pose = fun (value: pose) ->
  String.concat
    ""
    [
      "{ island = ";
      Printer.string value.island;
      "; bearing = ";
      Printer.float value.bearing;
      " }";
    ]

let print_stop = fun (value: stop) ->
  String.concat
    ""
    [
      "{ island = ";
      Printer.string value.island;
      "; supplies = ";
      Printer.int value.supplies;
      " }";
    ]

let print_sample = fun (value: sample) ->
  String.concat
    ""
    [
      "{ title = ";
      Printer.string value.title;
      "; active = ";
      Printer.bool value.active;
      "; count = ";
      Printer.int value.count;
      "; small = ";
      Printer.int32 value.small;
      "; big = ";
      Printer.int64 value.big;
      "; ratio = ";
      Printer.float value.ratio;
      "; nickname = ";
      Printer.option Printer.string value.nickname;
      "; status = ";
      print_status value.status;
      "; pet = ";
      print_pet value.pet;
      "; pose = ";
      print_pose value.pose;
      "; tags = ";
      Printer.vector Printer.string value.tags;
      "; scores = ";
      Printer.array Printer.int value.scores;
      "; stops = ";
      Printer.vector print_stop value.stops;
      "; mirrors = ";
      Printer.array print_stop value.mirrors;
      " }";
    ]

let finite_float_limit = 1.0e12

let finite_float_gen = Generator.float_range (-.finite_float_limit) finite_float_limit

let finite_float_arb = Arbitrary.make ~shrink:Shrinker.float ~print:Printer.float finite_float_gen

let status_gen =
  Generator.frequency
    [ (1, Generator.return Active); (1, Generator.return Draft); (1, Generator.return Archived); ]

let status_arb = Arbitrary.make ~print:print_status status_gen

let pet_gen =
  Generator.frequency
    [
      (1, Generator.return NewsCoo);
      (3, Generator.map (fun value -> Reindeer value) Generator.string);
    ]

let pet_arb = Arbitrary.make ~print:print_pet pet_gen

let pose_gen =
  Generator.map
    (fun (island, bearing) -> ({ island; bearing }: pose))
    (Generator.pair Generator.string finite_float_gen)

let pose_arb = Arbitrary.make ~print:print_pose pose_gen

let stop_gen =
  Generator.map
    (fun (island, supplies) -> ({ island; supplies }: stop))
    (Generator.pair Generator.string Generator.int)

let stop_arb = Arbitrary.make ~print:print_stop stop_gen

let string_vec_gen = Generator.vector_size (Generator.int_range 0 5) Generator.string

let string_vec_arb = Arbitrary.make ~print:(Printer.vector Printer.string) string_vec_gen

let int_array_gen = Generator.array_size (Generator.int_range 0 5) Generator.int

let int_array_arb = Arbitrary.make ~print:(Printer.array Printer.int) int_array_gen

let stop_vec_gen = Generator.vector_size (Generator.int_range 0 4) stop_gen

let stop_array_gen = Generator.array_size (Generator.int_range 0 4) stop_gen

let sample_gen =
  Generator.map3
    (fun
      (((title, active), count), (small, big, ratio))
      (nickname, (status, (pet, pose)))
      (tags, (scores, (stops, mirrors))) ->
      ({
        title;
        active;
        count;
        small;
        big;
        ratio;
        nickname;
        status;
        pet;
        marker = ();
        pose;
        tags;
        scores;
        stops;
        mirrors;
      }: sample))
    (Generator.pair
      (Generator.pair (Generator.pair Generator.string Generator.bool) Generator.int)
      (Generator.triple Generator.int32 Generator.int64 finite_float_gen))
    (Generator.pair
      (Generator.option Generator.string)
      (Generator.pair status_gen (Generator.pair pet_gen pose_gen)))
    (Generator.pair
      string_vec_gen
      (Generator.pair int_array_gen (Generator.pair stop_vec_gen stop_array_gen)))

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
  match Serde_toml.to_string encode value with
  | Ok encoded -> (
      match Serde_toml.from_string decode encoded with
      | Ok decoded -> equal decoded value
      | Error err -> fail ("decode failed: " ^ Serde.Error.to_string err)
    )
  | Error err -> fail ("encode failed: " ^ Serde.Error.to_string err)

let roundtrip_io = fun encode decode equal value ->
  let buffer = IO.Buffer.create ~size:64 in
  match Serde_toml.to_writer encode (io_writer_of_buffer buffer) value with
  | Ok () -> (
      match Serde_toml.from_reader
        decode
        (String.to_reader ~chunk_size:io_chunk_size (IO.Buffer.contents buffer)) with
      | Ok decoded -> equal decoded value
      | Error err -> fail ("reader decode failed: " ^ Serde.Error.to_string err)
    )
  | Error err -> fail ("writer encode failed: " ^ Serde.Error.to_string err)

let unit_roundtrip_prop =
  run_property
    "serde-toml property empty record roundtrips"
    Arbitrary.bool
    (fun _ ->
      roundtrip_in_memory empty_encode empty_decode (fun () () -> true) ())

let bool_roundtrip_prop =
  run_property
    "serde-toml property bool field roundtrips"
    Arbitrary.bool
    (roundtrip_in_memory
      (single_field_encode "value" Ser.bool)
      (single_field_decode "value" De.bool)
      Bool.equal)

let int_roundtrip_prop =
  run_property
    "serde-toml property int field roundtrips"
    Arbitrary.int
    (roundtrip_in_memory
      (single_field_encode "value" Ser.int)
      (single_field_decode "value" De.int)
      Int.equal)

let int32_roundtrip_prop =
  run_property
    "serde-toml property int32 field roundtrips"
    Arbitrary.int32
    (roundtrip_in_memory
      (single_field_encode "value" Ser.int32)
      (single_field_decode "value" De.int32)
      Int32.equal)

let int64_roundtrip_prop =
  run_property
    "serde-toml property int64 field roundtrips"
    Arbitrary.int64
    (roundtrip_in_memory
      (single_field_encode "value" Ser.int64)
      (single_field_decode "value" De.int64)
      Int64.equal)

let float_roundtrip_prop =
  run_property
    "serde-toml property float field roundtrips"
    finite_float_arb
    (roundtrip_in_memory
      (single_field_encode "value" Ser.float)
      (single_field_decode "value" De.float)
      Float.equal)

let string_roundtrip_prop =
  run_property
    "serde-toml property string field roundtrips"
    Arbitrary.string
    (roundtrip_in_memory
      (single_field_encode "value" Ser.string)
      (single_field_decode "value" De.string)
      String.equal)

let option_string_roundtrip_prop =
  run_property
    "serde-toml property option string field roundtrips"
    Arbitrary.(option string)
    (roundtrip_in_memory
      (single_field_encode "value" (Ser.option Ser.string))
      (optional_field_decode "value" (De.option De.string))
      ( = ))

let string_list_roundtrip_prop =
  run_property
    ~examples:composite_examples
    "serde-toml property string list field roundtrips"
    string_vec_arb
    (roundtrip_in_memory
      (single_field_encode "value" (Ser.list Ser.string))
      (single_field_decode "value" (De.list De.string))
      (equal_vec String.equal))

let int_array_roundtrip_prop =
  run_property
    ~examples:composite_examples
    "serde-toml property int array field roundtrips"
    int_array_arb
    (roundtrip_in_memory
      (single_field_encode "value" (Ser.array Ser.int))
      (single_field_decode "value" (De.array De.int))
      ( = ))

let status_roundtrip_prop =
  run_property
    ~examples:composite_examples
    "serde-toml property unit enum field roundtrips"
    status_arb
    (roundtrip_in_memory
      (single_field_encode "value" status_encode)
      (single_field_decode "value" status_decode)
      equal_status)

let pet_roundtrip_prop =
  run_property
    ~examples:composite_examples
    "serde-toml property newtype enum field roundtrips"
    pet_arb
    (roundtrip_in_memory
      (single_field_encode "value" pet_encode)
      (single_field_decode "value" pet_decode)
      equal_pet)

let pose_roundtrip_prop =
  run_property
    ~examples:composite_examples
    "serde-toml property nested record field roundtrips"
    pose_arb
    (roundtrip_in_memory
      (single_field_encode "value" pose_encode)
      (single_field_decode "value" pose_decode)
      equal_pose)

let stop_array_roundtrip_prop =
  run_property
    ~examples:composite_examples
    "serde-toml property record array field roundtrips"
    (Arbitrary.make ~print:(Printer.array print_stop) stop_array_gen)
    (roundtrip_in_memory
      (single_field_encode "value" (Ser.array stop_encode))
      (single_field_decode "value" (De.array stop_decode))
      ( = ))

let sample_roundtrip_prop =
  run_property
    ~examples:composite_examples
    "serde-toml property sample roundtrips"
    sample_arb
    (roundtrip_in_memory sample_encode sample_decode equal_sample)

let sample_io_roundtrip_prop =
  run_property
    ~examples:composite_examples
    "serde-toml property sample roundtrips over io"
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
  pet_roundtrip_prop;
  pose_roundtrip_prop;
  stop_array_roundtrip_prop;
  sample_roundtrip_prop;
  sample_io_roundtrip_prop;
]

let main ~args = Test.Cli.main ~name:"serde_toml_property_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
