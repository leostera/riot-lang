open Std

module Array = Collections.Array
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
      |> Result.expect ~msg:"serde-yaml test writer should append buffer contents";
      Ok written

    let write_vectored = fun buffer ~from ->
      let written = ref 0 in
      IO.IoVec.for_each
        from
        ~fn:(fun chunk ->
          IO.Buffer.append_slice buffer chunk
          |> Result.expect ~msg:"serde-yaml test writer should append slices";
          written := !written + IO.IoSlice.length chunk);
      Ok !written

    let flush = fun _buffer -> Ok ()
  end in
  fun buffer -> IO.Writer.from_sink (module Write) buffer

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

let vec_to_list = fun values ->
  let items = ref [] in
  Vector.for_each values ~fn:(fun value -> items := value :: !items);
  List.rev !items

let equal_companion = fun left right ->
  match (left, right) with
  | (NewsCoo, NewsCoo) -> true
  | (Reindeer left_name, Reindeer right_name) -> String.equal left_name right_name
  | _ -> false

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

let fixture: manifest = {
  ship = "Thousand Sunny";
  emergency = false;
  crew_count = 10;
  small = Int32.from_int 7;
  bounty = Int64.from_int 3_000_000_000;
  heading = 12.5;
  nickname = None;
  rank = Captain;
  companion = Reindeer "Chopper";
  marker = ();
  home = ({ island = "Water 7"; berth = 3 }: berth);
  tags = Vector.from_list [ "straw-hat"; "shipwright" ];
  scores = [|7; 9|];
  stops = Vector.from_list
    [
      ({ island = "Water 7"; supplies = 25 }: stop);
      ({ island = "Fish-Man Island"; supplies = 40 }: stop);
    ];
  mirrors = [|
    ({ island = "Dressrosa"; supplies = 12 }: stop);
  |];
}

let record_to_string_test =
  Test.case
    "serde-yaml encodes a stable manifest document"
    (fun _ctx ->
      match Serde_yaml.to_string manifest_encode fixture with
      | Ok encoded ->
          let expected =
            String.concat
              "\n"
              [
                "\"ship\": \"Thousand Sunny\"";
                "\"emergency\": false";
                "\"crew_count\": 10";
                "\"small\": 7";
                "\"bounty\": 3000000000";
                "\"heading\": 12.5";
                "\"nickname\": null";
                "\"rank\": \"Captain\"";
                "\"companion\": !Reindeer \"Chopper\"";
                "\"marker\": null";
                "\"home\":";
                "  \"island\": \"Water 7\"";
                "  \"berth\": 3";
                "\"tags\":";
                "  - \"straw-hat\"";
                "  - \"shipwright\"";
                "\"scores\":";
                "  - 7";
                "  - 9";
                "\"stops\":";
                "  -";
                "    \"island\": \"Water 7\"";
                "    \"supplies\": 25";
                "  -";
                "    \"island\": \"Fish-Man Island\"";
                "    \"supplies\": 40";
                "\"mirrors\":";
                "  -";
                "    \"island\": \"Dressrosa\"";
                "    \"supplies\": 12";
                "";
              ]
          in
          if String.equal encoded expected then
            Ok ()
          else
            Error ("expected:\n" ^ expected ^ "\nactual:\n" ^ encoded)
      | Error err -> Error (Serde.Error.to_string err))

let record_roundtrip_test =
  Test.case
    "serde-yaml roundtrips a manifest from string"
    (fun _ctx ->
      match Serde_yaml.to_string manifest_encode fixture with
      | Ok encoded -> (
          match Serde_yaml.from_string manifest_decode encoded with
          | Ok decoded ->
              if equal_manifest fixture decoded then
                Ok ()
              else
                Error "decoded manifest did not match encoded manifest"
          | Error err -> Error (Serde.Error.to_string err)
        )
      | Error err -> Error (Serde.Error.to_string err))

let scalar_roundtrip_test =
  Test.case
    "serde-yaml roundtrips top-level scalars and sequences"
    (fun _ctx ->
      match Serde_yaml.to_string Ser.string "Road Poneglyph" with
      | Error err -> Error (Serde.Error.to_string err)
      | Ok encoded ->
          if not (String.equal encoded "\"Road Poneglyph\"\n") then
            Error "unexpected string encoding"
          else
            match Serde_yaml.from_string De.string encoded with
            | Error err -> Error (Serde.Error.to_string err)
            | Ok decoded ->
                if not (String.equal decoded "Road Poneglyph") then
                  Error "string roundtrip failed"
                else
                  let numbers = Vector.from_list [ 1; 2; 3 ] in
                  match Serde_yaml.to_string (Ser.list Ser.int) numbers with
                  | Error err -> Error (Serde.Error.to_string err)
                  | Ok seq_encoded -> (
                      match Serde_yaml.from_string (De.list De.int) seq_encoded with
                      | Error err -> Error (Serde.Error.to_string err)
                      | Ok seq_decoded ->
                          if vec_to_list numbers = vec_to_list seq_decoded then
                            Ok ()
                          else
                            Error "sequence roundtrip failed"
                    ))

let tagged_variant_decode_test =
  Test.case
    "serde-yaml decodes tagged variants and empty null fields"
    (fun _ctx ->
      let yaml =
        String.concat
          "\n"
          [
            "\"ship\": \"Going Merry\"";
            "\"emergency\": true";
            "\"crew_count\": 5";
            "\"small\": 2";
            "\"bounty\": 1000";
            "\"heading\": 90.0";
            "\"nickname\":";
            "\"rank\": Captain";
            "\"companion\": !Reindeer \"Chopper\"";
            "\"marker\": null";
            "\"home\":";
            "  \"island\": \"Syrup Village\"";
            "  \"berth\": 1";
            "\"tags\": []";
            "\"scores\": []";
            "\"stops\": []";
            "\"mirrors\": []";
            "";
          ]
      in
      match Serde_yaml.from_string manifest_decode yaml with
      | Ok decoded ->
          if
            String.equal decoded.ship "Going Merry"
            && Bool.equal decoded.emergency true
            && decoded.nickname = None
            && decoded.rank = Captain
            && equal_companion decoded.companion (Reindeer "Chopper")
            && vec_to_list decoded.tags = []
            && Array.to_list decoded.scores = []
          then
            Ok ()
          else
            Error "decoded tagged manifest did not match expected fields"
      | Error err -> Error (Serde.Error.to_string err))

let reader_writer_test =
  Test.case
    "serde-yaml writes to writers and reads from readers"
    (fun _ctx ->
      let buffer = IO.Buffer.create ~size:128 in
      match Serde_yaml.to_writer manifest_encode (io_writer_of_buffer buffer) fixture with
      | Ok () -> (
          match Serde_yaml.from_reader
            manifest_decode
            (String.to_reader ~chunk_size:3 (IO.Buffer.contents buffer)) with
          | Ok decoded ->
              if equal_manifest fixture decoded then
                Ok ()
              else
                Error "reader/writer roundtrip failed"
          | Error err -> Error (Serde.Error.to_string err)
        )
      | Error err -> Error (Serde.Error.to_string err))

let comments_test =
  Test.case
    "serde-yaml ignores comments outside quoted strings"
    (fun _ctx ->
      let yaml =
        String.concat
          "\n"
          [
            "# observation log";
            "\"ship\": \"Sunny # not a comment\" # actual comment";
            "\"emergency\": false";
            "\"crew_count\": 10";
            "\"small\": 7";
            "\"bounty\": 3000000000";
            "\"heading\": 12.5";
            "\"nickname\": null";
            "\"rank\": \"Captain\"";
            "\"companion\": !Reindeer \"Chopper\"";
            "\"marker\": null";
            "\"home\":";
            "  \"island\": \"Water 7\"";
            "  \"berth\": 3";
            "\"tags\": []";
            "\"scores\": []";
            "\"stops\": []";
            "\"mirrors\": []";
            "";
          ]
      in
      match Serde_yaml.from_string manifest_decode yaml with
      | Ok decoded ->
          if String.equal decoded.ship "Sunny # not a comment" then
            Ok ()
          else
            Error "comment stripping damaged quoted string content"
      | Error err -> Error (Serde.Error.to_string err))

let tests = [
  record_to_string_test;
  record_roundtrip_test;
  scalar_roundtrip_test;
  tagged_variant_decode_test;
  reader_writer_test;
  comments_test;
]

let main ~args = Test.Cli.main ~name:"serde_yaml_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
