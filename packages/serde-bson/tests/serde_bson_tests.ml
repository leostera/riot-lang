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
      IO.Buffer.append_slice buffer (IO.Buffer.readable from) |> Result.expect ~msg:"serde-bson test writer should append buffer contents";
      Ok written

    let write_vectored = fun buffer ~from ->
      let written = ref 0 in
      IO.IoVec.for_each
        ~fn:(fun chunk ->
          IO.Buffer.append_slice buffer chunk |> Result.expect ~msg:"serde-bson test writer should append slices";
          written := !written + IO.IoSlice.length chunk)
        from;
      Ok !written

    let flush = fun _buffer -> Ok ()
  end in
  fun buffer ->
    IO.Writer.from_sink (module Write) buffer

type mode =
  | Captain
  | Doctor
  | Navigator of string

type berth = {
  island: string;
  berth: int;
}

type manifest = {
  ship: string;
  emergency: bool;
  crew_count: int;
  small: int32;
  bounty: int64;
  heading: float;
  nickname: string option;
  mode: mode;
  marker: unit;
  home: berth;
  tags: string vec;
  scores: int array;
}

type berth_field =
  | Berth_island
  | Berth_berth

type manifest_field =
  | Field_ship
  | Field_emergency
  | Field_crew_count
  | Field_small
  | Field_bounty
  | Field_heading
  | Field_nickname
  | Field_mode
  | Field_marker
  | Field_home
  | Field_tags
  | Field_scores

type berth_builder = {
  mutable island: string option;
  mutable berth: int option;
}

type manifest_builder = {
  mutable ship: string option;
  mutable emergency: bool option;
  mutable crew_count: int option;
  mutable small: int32 option;
  mutable bounty: int64 option;
  mutable heading: float option;
  mutable nickname: string option option;
  mutable mode: mode option;
  mutable marker: unit option;
  mutable home: berth option;
  mutable tags: string vec option;
  mutable scores: int array option;
}

let berth_fields = De.fields [ De.field "island" Berth_island; De.field "berth" Berth_berth ]

let manifest_fields = De.fields
  [
    De.field "ship" Field_ship;
    De.field "emergency" Field_emergency;
    De.field "crew_count" Field_crew_count;
    De.field "small" Field_small;
    De.field "bounty" Field_bounty;
    De.field "heading" Field_heading;
    De.field "nickname" Field_nickname;
    De.field "mode" Field_mode;
    De.field "marker" Field_marker;
    De.field "home" Field_home;
    De.field "tags" Field_tags;
    De.field "scores" Field_scores;
  ]

let mode_decode = De.variant
  [
    De.Variant.unit "Captain" Captain;
    De.Variant.unit "Doctor" Doctor;
    De.Variant.newtype "Navigator" De.string (fun value -> Navigator value);
  ]

let mode_encode = Ser.variant
  [ Ser.Variant.unit "Captain"
      (
        function
        | Captain -> true
        | _ -> false
      ); Ser.Variant.unit "Doctor"
      (
        function
        | Doctor -> true
        | _ -> false
      ); Ser.Variant.newtype "Navigator" Ser.string
      (
        function
        | Navigator value -> Some value
        | _ -> None
      ); ]

let berth_decode =
  De.record_mut ~fields:berth_fields ~create:(fun () : berth_builder ->
    { island = None; berth = None })
    ~step:(fun reader builder field ->
      match field with
      | Some Berth_island -> builder.island <- Some (De.read reader De.string)
      | Some Berth_berth -> builder.berth <- Some (De.read reader De.int)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (builder.island, builder.berth) with
      | (Some island, Some berth) -> ({ island; berth }: berth)
      | _ -> De.missing_field ())

let berth_encode = Ser.record
  (Ser.fields
    [
      Ser.field "island" Ser.string (fun (value: berth) -> value.island);
      Ser.field "berth" Ser.int (fun (value: berth) -> value.berth);
    ])

let manifest_decode =
  De.record_mut ~fields:manifest_fields
    ~create:(fun () : manifest_builder ->
      {
        ship = None;
        emergency = None;
        crew_count = None;
        small = None;
        bounty = None;
        heading = None;
        nickname = None;
        mode = None;
        marker = None;
        home = None;
        tags = None;
        scores = None;
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
      | Some Field_mode -> builder.mode <- Some (De.read reader mode_decode)
      | Some Field_marker -> builder.marker <- Some (De.read reader (De.const ()))
      | Some Field_home -> builder.home <- Some (De.read reader berth_decode)
      | Some Field_tags -> builder.tags <- Some (De.read reader (De.list De.string))
      | Some Field_scores -> builder.scores <- Some (De.read reader (De.array De.int))
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (
        builder.ship,
        builder.emergency,
        builder.crew_count,
        builder.small,
        builder.bounty,
        builder.heading,
        builder.nickname,
        builder.mode,
        builder.marker,
        builder.home,
        builder.tags,
        builder.scores
      ) with
      | (Some ship, Some emergency, Some crew_count, Some small, Some bounty, Some heading, Some nickname, Some mode, Some marker, Some home, Some tags, Some scores) ->
          ({
              ship;
              emergency;
              crew_count;
              small;
              bounty;
              heading;
              nickname;
              mode;
              marker;
              home;
              tags;
              scores;
            }: manifest)
      | _ -> De.missing_field ())

let manifest_encode = Ser.record
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
        Ser.field "mode" mode_encode (fun (value: manifest) -> value.mode);
        Ser.field "marker" Ser.null (fun (value: manifest) -> value.marker);
        Ser.field "home" berth_encode (fun (value: manifest) -> value.home);
        Ser.field "tags" (Ser.list Ser.string) (fun (value: manifest) -> value.tags);
        Ser.field "scores" (Ser.array Ser.int) (fun (value: manifest) -> value.scores);
      ]
  )

let vec_to_list = fun values ->
  let items = ref [] in
  Vector.for_each values ~fn:(fun value -> items := value :: !items);
  List.rev !items

let equal_manifest = fun (left: manifest) (right: manifest) ->
  String.equal left.ship right.ship
  && Bool.equal left.emergency right.emergency
  && Int.equal left.crew_count right.crew_count
  && Int32.equal left.small right.small
  && Int64.equal left.bounty right.bounty
  && Float.equal left.heading right.heading
  && left.nickname = right.nickname
  && left.mode = right.mode
  && left.home = right.home
  && vec_to_list left.tags = vec_to_list right.tags
  && left.scores = right.scores

let byte_values = fun value ->
  List.init
    ~count:(String.length value)
    ~fn:(fun index -> Char.code (String.get_unchecked value ~at:index))

let sample_manifest: manifest = {
  ship = "Thousand Sunny";
  emergency = false;
  crew_count = 10;
  small = 7l;
  bounty = 3_000_000_000L;
  heading = 12.5;
  nickname = Some "Sunny-go";
  mode = Navigator "Nami";
  marker = ();
  home = ({ island = "Water 7"; berth = 7 }: berth);
  tags = Vector.from_list [ "log-pose"; "cola"; "coup-de-burst" ];
  scores = [|98; 87; 77; 101|];
}

let test_roundtrips_manifest = fun _ctx ->
  let* encoded =
    match Serde_bson.to_string manifest_encode sample_manifest with
    | Ok encoded -> Ok encoded
    | Error err -> Error ("encode failed: " ^ Serde.Error.to_string err)
  in
  match Serde_bson.from_string manifest_decode encoded with
  | Ok decoded when equal_manifest decoded sample_manifest -> Ok ()
  | Ok _ -> Error "expected serde-bson roundtrip to preserve the manifest"
  | Error err -> Error ("decode failed: " ^ Serde.Error.to_string err)

let test_decodes_from_reader = fun _ctx ->
  match Serde_bson.to_string manifest_encode sample_manifest with
  | Ok encoded -> (
      match Serde_bson.from_reader manifest_decode (String.to_reader ~chunk_size:5 encoded) with
      | Ok decoded when equal_manifest decoded sample_manifest -> Ok ()
      | Ok _ -> Error "expected reader decode to preserve the manifest"
      | Error err -> Error ("reader decode failed: " ^ Serde.Error.to_string err)
    )
  | Error err -> Error ("encode failed: " ^ Serde.Error.to_string err)

let test_writes_to_writer = fun _ctx ->
  let buffer = IO.Buffer.create ~size:128 in
  let* () =
    match Serde_bson.to_writer manifest_encode (io_writer_of_buffer buffer) sample_manifest with
    | Ok () -> Ok ()
    | Error err -> Error ("writer encode failed: " ^ Serde.Error.to_string err)
  in
  match Serde_bson.from_string manifest_decode (IO.Buffer.contents buffer) with
  | Ok decoded when equal_manifest decoded sample_manifest -> Ok ()
  | Ok _ -> Error "expected writer output to decode back into the original manifest"
  | Error err -> Error ("writer output decode failed: " ^ Serde.Error.to_string err)

let test_rejects_top_level_scalar_encode = fun _ctx ->
  match Serde_bson.to_string Ser.int 7 with
  | Ok _ -> Error "expected serde-bson to reject top-level scalar encodes"
  | Error _ -> Ok ()

let test_rejects_trailing_bytes = fun _ctx ->
  match Serde_bson.to_string manifest_encode sample_manifest with
  | Ok encoded -> (
      match Serde_bson.from_string manifest_decode (encoded ^ "\x00") with
      | Ok _ -> Error "expected trailing BSON bytes to be rejected"
      | Error _ -> Ok ()
    )
  | Error err -> Error ("encode failed: " ^ Serde.Error.to_string err)

let test_rejects_invalid_bool_tag = fun _ctx ->
  let invalid = "\x0C\x00\x00\x00\x08ok\x00\x02\x00" in
  match Serde_bson.from_string manifest_decode invalid with
  | Ok _ -> Error "expected invalid BSON bool payload to be rejected"
  | Error _ -> Ok ()

let test_variant_payload_uses_singleton_document = fun _ctx ->
  let manifest = { sample_manifest with mode = Navigator "Nami" } in
  match Serde_bson.to_string manifest_encode manifest with
  | Ok encoded ->
      if List.mem 0x03 (byte_values encoded) then
        Ok ()
      else
        Error "expected BSON variant payload to include an embedded document type"
  | Error err -> Error ("encode failed: " ^ Serde.Error.to_string err)

let tests = [
  Test.case "serde-bson roundtrips manifest" test_roundtrips_manifest;
  Test.case "serde-bson decodes from readers" test_decodes_from_reader;
  Test.case "serde-bson writes to writers" test_writes_to_writer;
  Test.case "serde-bson rejects top-level scalar encodes" test_rejects_top_level_scalar_encode;
  Test.case "serde-bson rejects trailing bytes" test_rejects_trailing_bytes;
  Test.case "serde-bson rejects invalid bool payloads" test_rejects_invalid_bool_tag;
  Test.case "serde-bson variants with payloads use singleton documents" test_variant_payload_uses_singleton_document;
]

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"serde_bson_tests" ~tests ~args ())
    ~args:Env.args
    ()
