open Std

module Array = Collections.Array
module Vector = Collections.Vector
module De = Serde.De
module Ser = Serde.Ser

type rank =
  | Captain
  | Doctor
  | Navigator

type companion =
  | NewsCoo
  | Reindeer of string

type berth = { island: string; berth: int }

type stop = { island: string; supplies: int }

type manifest = {
  ship: string;
  emergency: bool;
  crew_count: int;
  small: int32;
  bounty: int64;
  heading: float;
  nickname: string option;
  rank: rank;
  companion: companion;
  marker: unit;
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
  | Field_small
  | Field_bounty
  | Field_heading
  | Field_nickname
  | Field_rank
  | Field_companion
  | Field_marker
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
  mutable small: int32 option;
  mutable bounty: int64 option;
  mutable heading: float option;
  mutable nickname: string option option;
  mutable rank: rank option;
  mutable companion: companion option;
  mutable marker: unit option;
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

    let write = fun buffer ~from ->
      let written = IO.Buffer.readable_bytes from in
      IO.Buffer.append_slice buffer (IO.Buffer.readable from)
      |> Result.expect ~msg:"serde-yaml bench writer should append buffer contents";
      Ok written

    let write_vectored = fun buffer ~from ->
      let written = ref 0 in
      IO.IoVec.for_each
        from
        ~fn:(fun chunk ->
          IO.Buffer.append_slice buffer chunk
          |> Result.expect ~msg:"serde-yaml bench writer should append slices";
          written := !written + IO.IoSlice.length chunk);
      Ok !written

    let flush = fun _buffer -> Ok ()
  end in
  fun buffer -> IO.Writer.from_sink (module Write) buffer

let berth_fields = De.fields [ De.field "island" Berth_island; De.field "berth" Berth_berth ]

let stop_fields = De.fields [ De.field "island" Stop_island; De.field "supplies" Stop_supplies ]

let manifest_fields =
  De.fields
    [
      De.field "ship" Field_ship;
      De.field "emergency" Field_emergency;
      De.field "crew_count" Field_crew_count;
      De.field "small" Field_small;
      De.field "bounty" Field_bounty;
      De.field "heading" Field_heading;
      De.field "nickname" Field_nickname;
      De.field "rank" Field_rank;
      De.field "companion" Field_companion;
      De.field "marker" Field_marker;
      De.field "home" Field_home;
      De.field "tags" Field_tags;
      De.field "scores" Field_scores;
      De.field "stops" Field_stops;
      De.field "mirrors" Field_mirrors;
    ]

let rank_decode =
  De.variant
    [
      De.Variant.unit "Captain" Captain;
      De.Variant.unit "Doctor" Doctor;
      De.Variant.unit "Navigator" Navigator;
    ]

let rank_encode =
  Ser.variant
    [
      Ser.Variant.unit
        "Captain"
        (fun __tmp1 ->
          match __tmp1 with
          | Captain -> true
          | _ -> false);
      Ser.Variant.unit
        "Doctor"
        (fun __tmp1 ->
          match __tmp1 with
          | Doctor -> true
          | _ -> false);
      Ser.Variant.unit
        "Navigator"
        (fun __tmp1 ->
          match __tmp1 with
          | Navigator -> true
          | _ -> false);
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
        (fun __tmp1 ->
          match __tmp1 with
          | NewsCoo -> true
          | _ -> false);
      Ser.Variant.newtype
        "Reindeer"
        Ser.string
        (fun __tmp1 ->
          match __tmp1 with
          | Reindeer value -> Some value
          | _ -> None);
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
      | (Some island, Some supplies) -> ({ island; supplies }: stop)
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

let manifest_decode =
  De.record_mut
    ~fields:manifest_fields
    ~create:(fun (): manifest_builder ->
      {
        ship = None;
        emergency = None;
        crew_count = None;
        small = None;
        bounty = None;
        heading = None;
        nickname = None;
        rank = None;
        companion = None;
        marker = None;
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
      | Some Field_small -> builder.small <- Some (De.read reader De.int32)
      | Some Field_bounty -> builder.bounty <- Some (De.read reader De.int64)
      | Some Field_heading -> builder.heading <- Some (De.read reader De.float)
      | Some Field_nickname -> builder.nickname <- Some (De.read reader (De.option De.string))
      | Some Field_rank -> builder.rank <- Some (De.read reader rank_decode)
      | Some Field_companion -> builder.companion <- Some (De.read reader companion_decode)
      | Some Field_marker -> builder.marker <- Some (De.read reader (De.const ()))
      | Some Field_home -> builder.home <- Some (De.read reader berth_decode)
      | Some Field_tags -> builder.tags <- Some (De.read reader (De.list De.string))
      | Some Field_scores -> builder.scores <- Some (De.read reader (De.array De.int))
      | Some Field_stops -> builder.stops <- Some (De.read reader (De.list stop_decode))
      | Some Field_mirrors -> builder.mirrors <- Some (De.read reader (De.array stop_decode))
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (
        builder.ship,
        builder.emergency,
        builder.crew_count,
        builder.small,
        builder.bounty,
        builder.heading,
        builder.rank,
        builder.companion,
        builder.marker,
        builder.home,
        builder.tags,
        builder.scores,
        builder.stops,
        builder.mirrors
      ) with
      | (
          Some ship,
          Some emergency,
          Some crew_count,
          Some small,
          Some bounty,
          Some heading,
          Some rank,
          Some companion,
          Some marker,
          Some home,
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
          ({
            ship;
            emergency;
            crew_count;
            small;
            bounty;
            heading;
            nickname;
            rank;
            companion;
            marker;
            home;
            tags;
            scores;
            stops;
            mirrors;
          }: manifest)
      | _ -> De.missing_field ())

let manifest_encode =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "ship" Ser.string (fun (value: manifest) -> value.ship);
          Ser.field "emergency" Ser.bool (fun (value: manifest) -> value.emergency);
          Ser.field "crew_count" Ser.int (fun (value: manifest) -> value.crew_count);
          Ser.field "small" Ser.int32 (fun (value: manifest) -> value.small);
          Ser.field "bounty" Ser.int64 (fun (value: manifest) -> value.bounty);
          Ser.field "heading" Ser.float (fun (value: manifest) -> value.heading);
          Ser.field "nickname" (Ser.option Ser.string) (fun (value: manifest) -> value.nickname);
          Ser.field "rank" rank_encode (fun (value: manifest) -> value.rank);
          Ser.field "companion" companion_encode (fun (value: manifest) -> value.companion);
          Ser.field "marker" Ser.null (fun (value: manifest) -> value.marker);
          Ser.field "home" berth_encode (fun (value: manifest) -> value.home);
          Ser.field "tags" (Ser.list Ser.string) (fun (value: manifest) -> value.tags);
          Ser.field "scores" (Ser.array Ser.int) (fun (value: manifest) -> value.scores);
          Ser.field "stops" (Ser.list stop_encode) (fun (value: manifest) -> value.stops);
          Ser.field "mirrors" (Ser.array stop_encode) (fun (value: manifest) -> value.mirrors);
        ]
    )

let repeat = fun text count ->
  let buffer = IO.Buffer.create ~size:(String.length text * count) in
  for _ = 1 to count do
    IO.Buffer.add_string buffer text
  done;
  IO.Buffer.contents buffer

let tags_of_count = fun count ->
  let tags = Vector.with_capacity ~size:count in
  for index = 0 to count - 1 do
    Vector.push tags ~value:("log-pose-" ^ Int.to_string index)
  done;
  tags

let scores_of_count = fun count -> Array.init ~count ~fn:(fun index -> (index * 97) mod 1_000_000)

let stop_of_index = fun index prefix ->
  ({ island = prefix ^ "-island-" ^ Int.to_string index; supplies = (index * 17) mod 10_000 }: stop)

let stops_vec_of_count = fun count prefix ->
  let stops = Vector.with_capacity ~size:count in
  for index = 0 to count - 1 do
    Vector.push stops ~value:(stop_of_index index prefix)
  done;
  stops

let stops_array_of_count = fun count prefix ->
  Array.init
    ~count
    ~fn:(fun index -> stop_of_index index prefix)

let equal_companion = fun left right ->
  match (left, right) with
  | (NewsCoo, NewsCoo) -> true
  | (Reindeer left_name, Reindeer right_name) -> String.equal left_name right_name
  | _ -> false

let vec_to_list = fun values ->
  let items = ref [] in
  Vector.for_each values ~fn:(fun value -> items := value :: !items);
  List.rev !items

let equal_stop = fun (left: stop) (right: stop) ->
  String.equal left.island right.island && Int.equal left.supplies right.supplies

let equal_berth = fun (left: berth) (right: berth) ->
  String.equal left.island right.island && Int.equal left.berth right.berth

let equal_stop_lists = fun left right ->
  match List.compare_lengths ~left ~right with
  | 0 ->
      List.zip left right
      |> List.all ~fn:(fun (left, right) -> equal_stop left right)
  | _ -> false

let equal_manifest = fun (left: manifest) (right: manifest) ->
  String.equal left.ship right.ship
  && Bool.equal left.emergency right.emergency
  && Int.equal left.crew_count right.crew_count
  && Int32.equal left.small right.small
  && Int64.equal left.bounty right.bounty
  && Float.equal left.heading right.heading
  && left.nickname = right.nickname
  && left.rank = right.rank
  && equal_companion left.companion right.companion
  && equal_berth left.home right.home
  && vec_to_list left.tags = vec_to_list right.tags
  && left.scores = right.scores
  && equal_stop_lists (vec_to_list left.stops) (vec_to_list right.stops)
  && equal_stop_lists (Array.to_list left.mirrors) (Array.to_list right.mirrors)

let build_fixture = fun
  ({
    label;
    tag_count;
    score_count;
    stop_count;
    string_repeat;
  }: fixture_spec) ->
  let value: manifest = {
    ship = repeat "thousand-sunny-logbook-" string_repeat;
    emergency = false;
    crew_count = 10;
    small = Int32.from_int 7;
    bounty = Int64.from_int 3_000_000_000;
    heading = 12.5;
    nickname = Some (repeat "black-leg-" string_repeat);
    rank = Captain;
    companion = Reindeer "Chopper";
    marker = ();
    home = ({ island = repeat "water-seven-" string_repeat; berth = 7 }: berth);
    tags = tags_of_count tag_count;
    scores = scores_of_count score_count;
    stops = stops_vec_of_count stop_count "log";
    mirrors = stops_array_of_count stop_count "mirror";
  }
  in
  let encoded =
    Serde_yaml.to_string manifest_encode value
    |> Result.expect ~msg:("expected " ^ label ^ " fixture to encode")
  in
  let decoded =
    Serde_yaml.from_string manifest_decode encoded
    |> Result.expect ~msg:("expected " ^ label ^ " fixture to decode")
  in
  if not (equal_manifest value decoded) then
    panic ("serde_yaml_bench: fixture roundtrip failed for " ^ label);
  { label; value; encoded }

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

let bench_encode_in_memory = fun fixture () ->
  ignore
    (Serde_yaml.to_string manifest_encode fixture.value)

let bench_encode_writer = fun fixture () ->
  let buffer = IO.Buffer.create ~size:(String.length fixture.encoded) in
  ignore (Serde_yaml.to_writer manifest_encode (io_writer_of_buffer buffer) fixture.value)

let bench_decode_in_memory = fun fixture () ->
  ignore
    (Serde_yaml.from_string manifest_decode fixture.encoded)

let bench_decode_reader = fun fixture () ->
  ignore
    (Serde_yaml.from_reader
      manifest_decode
      (String.to_reader ~chunk_size:io_chunk_size fixture.encoded))

let benchmark_suite = fun fixture ->
  let size = human_size (String.length fixture.encoded) in
  let config =
    if String.equal fixture.label "small" then
      small_bench_config
    else
      large_bench_config
  in
  Bench.[
    with_config
      ~config
      ("serde-yaml encode in-memory " ^ fixture.label ^ " dataset (" ^ size ^ ")")
      (bench_encode_in_memory fixture);
    with_config
      ~config
      ("serde-yaml encode writer " ^ fixture.label ^ " dataset (" ^ size ^ ")")
      (bench_encode_writer fixture);
    with_config
      ~config
      ("serde-yaml decode in-memory " ^ fixture.label ^ " dataset (" ^ size ^ ")")
      (bench_decode_in_memory fixture);
    with_config
      ~config
      ("serde-yaml decode reader " ^ fixture.label ^ " dataset (" ^ size ^ ")")
      (bench_decode_reader fixture);
  ]

let main ~args =
  let small_fixture = build_fixture small_fixture_spec in
  let large_fixture = build_fixture large_fixture_spec in
  let benchmarks = benchmark_suite small_fixture @ benchmark_suite large_fixture in
  Bench.Cli.main ~name:"serde-yaml benchmarks" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
