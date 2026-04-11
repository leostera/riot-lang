open Std
module Array = Collections.Array
module Vector = Collections.Vector
module Toml = Data.Toml
module De = Serde.De
module Ser = Serde.Ser

type status =
  | Active
  | Draft
  | Archived

type berth = {
  island: string;
  berth: int;
}

type stop = {
  island: string;
  supplies: int;
}

type manifest = {
  ship: string;
  emergency: bool;
  crew_count: int;
  status: status;
  home: berth;
  tags: string vec;
  scores: int array;
  stops: stop vec;
  mirrors: stop array;
}

type berth_field =
  | Berth_island
  | Berth_berth

type stop_field =
  | Stop_island
  | Stop_supplies

type manifest_field =
  | Field_ship
  | Field_emergency
  | Field_crew_count
  | Field_status
  | Field_home
  | Field_tags
  | Field_scores
  | Field_stops
  | Field_mirrors

type berth_builder = {
  mutable island: string option;
  mutable berth: int option;
}

type stop_builder = {
  mutable island: string option;
  mutable supplies: int option;
}

type manifest_builder = {
  mutable ship: string option;
  mutable emergency: bool option;
  mutable crew_count: int option;
  mutable status: status option;
  mutable home: berth option;
  mutable tags: string vec option;
  mutable scores: int array option;
  mutable stops: stop vec option;
  mutable mirrors: stop array option;
}

type fixture_spec = {
  label: string;
  tag_count: int;
  score_count: int;
  stop_count: int;
  string_repeat: int;
}

type fixture = {
  label: string;
  value: manifest;
  encoded: string;
  std_value: Toml.value;
  std_rendered: string;
}

let small_bench_config: Bench.bench_config = { iterations = 100; warmup = 5 }

let large_bench_config: Bench.bench_config = { iterations = 10; warmup = 1 }

let io_chunk_size = 4_096

let human_size = fun bytes ->
  if bytes >= 1_000_000 then
    Int.to_string (bytes / 1_000_000) ^ "MB"
  else if bytes >= 1_000 then
    Int.to_string (bytes / 1_000) ^ "KB"
  else
    Int.to_string bytes ^ "B"

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

let berth_fields = De.fields [ De.field "island" Berth_island; De.field "berth" Berth_berth ]

let stop_fields = De.fields [ De.field "island" Stop_island; De.field "supplies" Stop_supplies ]

let manifest_fields = De.fields
  [
    De.field "ship" Field_ship;
    De.field "emergency" Field_emergency;
    De.field "crew_count" Field_crew_count;
    De.field "status" Field_status;
    De.field "home" Field_home;
    De.field "tags" Field_tags;
    De.field "scores" Field_scores;
    De.field "stops" Field_stops;
    De.field "mirrors" Field_mirrors;
  ]

let status_decode = De.variant
  [
    De.Variant.unit "Active" Active;
    De.Variant.unit "Draft" Draft;
    De.Variant.unit "Archived" Archived;
  ]

let status_encode = Ser.variant
  [
    Ser.Variant.unit "Active"
      (function
      | Active -> true
      | _ -> false);
    Ser.Variant.unit "Draft"
      (function
      | Draft -> true
      | _ -> false);
    Ser.Variant.unit "Archived"
      (function
      | Archived -> true
      | _ -> false);
  ]

let berth_decode =
  De.record_mut ~fields:berth_fields
    ~create:(fun () : berth_builder -> { island = None; berth = None })
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
    (Ser.fields
      [
        Ser.field "island" Ser.string (fun (value: berth) -> value.island);
        Ser.field "berth" Ser.int (fun (value: berth) -> value.berth);
      ])

let stop_decode =
  De.record_mut ~fields:stop_fields
    ~create:(fun () : stop_builder -> { island = None; supplies = None })
    ~step:(fun reader builder field ->
      match field with
      | Some Stop_island -> builder.island <- Some (De.read reader De.string)
      | Some Stop_supplies -> builder.supplies <- Some (De.read reader De.int)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (builder.island, builder.supplies) with
      | (Some island, Some supplies) -> ({ island; supplies }: stop)
      | _ -> De.missing_field ())

let stop_encode =
  Ser.record
    (Ser.fields
      [
        Ser.field "island" Ser.string (fun (value: stop) -> value.island);
        Ser.field "supplies" Ser.int (fun (value: stop) -> value.supplies);
      ])

let manifest_decode =
  De.record_mut ~fields:manifest_fields
    ~create:(fun () : manifest_builder ->
      {
        ship = None;
        emergency = None;
        crew_count = None;
        status = None;
        home = None;
        tags = None;
        scores = None;
        stops = None;
        mirrors = None;
      })
    ~step:(fun reader builder field ->
      match field with
      | Some Field_ship -> builder.ship <- Some (De.read reader De.string)
      | Some Field_emergency -> builder.emergency <- Some (De.read reader De.bool)
      | Some Field_crew_count -> builder.crew_count <- Some (De.read reader De.int)
      | Some Field_status -> builder.status <- Some (De.read reader status_decode)
      | Some Field_home -> builder.home <- Some (De.read reader berth_decode)
      | Some Field_tags -> builder.tags <- Some (De.read reader (De.list De.string))
      | Some Field_scores -> builder.scores <- Some (De.read reader (De.array De.int))
      | Some Field_stops -> builder.stops <- Some (De.read reader (De.list stop_decode))
      | Some Field_mirrors -> builder.mirrors <- Some (De.read reader (De.array stop_decode))
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun (builder: manifest_builder) ->
      match (
        builder.ship,
        builder.emergency,
        builder.crew_count,
        builder.status,
        builder.home,
        builder.tags,
        builder.scores,
        builder.stops,
        builder.mirrors
      ) with
      | (Some ship, Some emergency, Some crew_count, Some status, Some home, Some tags, Some scores, Some stops, Some mirrors) ->
          ({
              ship;
              emergency;
              crew_count;
              status;
              home;
              tags;
              scores;
              stops;
              mirrors;
            }: manifest)
      | _ -> De.missing_field ())

let manifest_encode =
  Ser.record
    (Ser.fields
      [
        Ser.field "ship" Ser.string (fun (value: manifest) -> value.ship);
        Ser.field "emergency" Ser.bool (fun (value: manifest) -> value.emergency);
        Ser.field "crew_count" Ser.int (fun (value: manifest) -> value.crew_count);
        Ser.field "status" status_encode (fun (value: manifest) -> value.status);
        Ser.field "home" berth_encode (fun (value: manifest) -> value.home);
        Ser.field "tags" (Ser.list Ser.string) (fun (value: manifest) -> value.tags);
        Ser.field "scores" (Ser.array Ser.int) (fun (value: manifest) -> value.scores);
        Ser.field "stops" (Ser.list stop_encode) (fun (value: manifest) -> value.stops);
        Ser.field "mirrors" (Ser.array stop_encode) (fun (value: manifest) -> value.mirrors);
      ])

let repeat = fun text count ->
  let buffer = IO.Buffer.create (String.length text * count) in
  for _index = 1 to count do
    IO.Buffer.add_string buffer text
  done;
  IO.Buffer.contents buffer

let status_to_string = function
  | Active -> "Active"
  | Draft -> "Draft"
  | Archived -> "Archived"

let tags_of_count = fun count ->
  let tags = Vector.with_capacity count in
  for index = 0 to count - 1 do
    Vector.push tags ("grand-line-marker-" ^ Int.to_string index)
  done;
  tags

let scores_of_count = fun count ->
  array__init count (fun index -> (index * 97) mod 1_000_000)

let vec_to_list = fun values ->
  let items = ref [] in
  Vector.iter (fun value -> items := value :: !items) values;
  List.rev !items

let stop_of_index = fun index prefix ->
  ({
      island = prefix ^ "-island-" ^ Int.to_string index;
      supplies = (index * 17) mod 10_000;
    }: stop)

let stops_vec_of_count = fun count prefix ->
  let stops = Vector.with_capacity count in
  for index = 0 to count - 1 do
    Vector.push stops (stop_of_index index prefix)
  done;
  stops

let stops_array_of_count = fun count prefix ->
  array__init count (fun index -> stop_of_index index prefix)

let stop_to_toml = fun (value: stop) ->
  Toml.Table
    [ ("island", Toml.String value.island); ("supplies", Toml.Int value.supplies) ]

let berth_to_toml = fun (value: berth) ->
  Toml.Table
    [ ("island", Toml.String value.island); ("berth", Toml.Int value.berth) ]

let manifest_to_toml = fun (value: manifest) ->
  Toml.Table
    [
      ("ship", Toml.String value.ship);
      ("emergency", Toml.Bool value.emergency);
      ("crew_count", Toml.Int value.crew_count);
      ("status", Toml.String (status_to_string value.status));
      ("home", berth_to_toml value.home);
      ("tags", Toml.Array (vec_to_list value.tags |> List.map (fun item -> Toml.String item)));
      ("scores", Toml.Array (Array.to_list value.scores |> List.map (fun item -> Toml.Int item)));
      ("stops", Toml.Array (vec_to_list value.stops |> List.map stop_to_toml));
      ("mirrors", Toml.Array (Array.to_list value.mirrors |> List.map stop_to_toml));
    ]

let build_fixture = fun ({ label; tag_count; score_count; stop_count; string_repeat }: fixture_spec) ->
  let value: manifest = {
    ship = repeat "thousand-sunny-logbook-" string_repeat;
    emergency = false;
    crew_count = 10;
    status = Active;
    home = ({ island = repeat "water-seven-" string_repeat; berth = 7 }: berth);
    tags = tags_of_count tag_count;
    scores = scores_of_count score_count;
    stops = stops_vec_of_count stop_count "log";
    mirrors = stops_array_of_count stop_count "mirror";
  }
  in
  let encoded =
    Serde_toml.to_string manifest_encode value
    |> Result.expect ~msg:("expected " ^ label ^ " fixture to encode")
  in
  ignore
    (Toml.parse encoded
    |> Result.expect ~msg:("expected " ^ label ^ " fixture to parse with Std.Data.Toml"));
  let std_value = manifest_to_toml value in
  let std_rendered = Toml.to_string std_value in
  { label; value; encoded; std_value; std_rendered }

let small_fixture_spec = {
  label = "small";
  tag_count = 64;
  score_count = 64;
  stop_count = 16;
  string_repeat = 4;
}

let large_fixture_spec = {
  label = "large";
  tag_count = 8_192;
  score_count = 8_192;
  stop_count = 1_024;
  string_repeat = 256;
}

let bench_serde_encode_in_memory = fun fixture () ->
  ignore (Serde_toml.to_string manifest_encode fixture.value)

let bench_serde_encode_writer = fun fixture () ->
  let buffer = IO.Buffer.create (String.length fixture.encoded) in
  ignore (Serde_toml.to_writer manifest_encode (io_writer_of_buffer buffer) fixture.value)

let bench_serde_decode_in_memory = fun fixture () ->
  ignore (Serde_toml.from_string manifest_decode fixture.encoded)

let bench_serde_decode_reader = fun fixture () ->
  ignore
    (Serde_toml.from_reader
      manifest_decode
      (String.to_reader ~chunk_size:io_chunk_size fixture.encoded))

let bench_std_parse = fun fixture () ->
  ignore (Toml.parse fixture.encoded)

let bench_std_render = fun fixture () ->
  ignore (Toml.to_string fixture.std_value)

let benchmark_suite = fun fixture ->
  let serde_size = human_size (String.length fixture.encoded) in
  let std_size = human_size (String.length fixture.std_rendered) in
  let config =
    if String.equal fixture.label "small" then
      small_bench_config
    else
      large_bench_config
  in
  Bench.
    [
      with_config
        ~config
        ("serde-toml encode in-memory " ^ fixture.label ^ " payload (" ^ serde_size ^ ")")
        (bench_serde_encode_in_memory fixture);
      with_config
        ~config
        ("serde-toml encode writer " ^ fixture.label ^ " payload (" ^ serde_size ^ ")")
        (bench_serde_encode_writer fixture);
      with_config
        ~config
        ("serde-toml decode in-memory " ^ fixture.label ^ " payload (" ^ serde_size ^ ")")
        (bench_serde_decode_in_memory fixture);
      with_config
        ~config
        ("serde-toml decode reader " ^ fixture.label ^ " payload (" ^ serde_size ^ ")")
        (bench_serde_decode_reader fixture);
      with_config
        ~config
        ("std-data-toml parse " ^ fixture.label ^ " payload (" ^ serde_size ^ ")")
        (bench_std_parse fixture);
      with_config
        ~config
        ("std-data-toml render " ^ fixture.label ^ " tree (" ^ std_size ^ ")")
        (bench_std_render fixture);
    ]

let () =
  Actors.run
    ~main:(fun ~args ->
      let small_fixture = build_fixture small_fixture_spec in
      let large_fixture = build_fixture large_fixture_spec in
      let benchmarks = benchmark_suite small_fixture @ benchmark_suite large_fixture in
      Bench.Cli.main ~name:"serde-toml benchmarks" ~benchmarks ~args)
    ~args:Env.args
    ()
