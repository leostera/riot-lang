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
      |> Result.expect ~msg:"serde-cbor test writer should append buffer contents";
      Ok written

    let write_vectored = fun buffer ~from ->
      let written = ref 0 in
      IO.IoVec.for_each
        ~fn:(fun chunk ->
          IO.Buffer.append_slice buffer chunk
          |> Result.expect ~msg:"serde-cbor test writer should append slices";
          written := !written + IO.IoSlice.length chunk)
        from;
      Ok !written

    let flush = fun _buffer -> Ok ()
  end in
  fun buffer -> IO.Writer.from_sink (module Write) buffer

type mode =
  | Captain
  | Doctor
  | Navigator of string

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
      De.field "marker" Field_marker;
      De.field "home" Field_home;
      De.field "tags" Field_tags;
      De.field "scores" Field_scores;
    ]

let mode_decode =
  De.variant
    [
      De.Variant.unit "Captain" Captain;
      De.Variant.unit "Doctor" Doctor;
      De.Variant.newtype "Navigator" De.string (fun value -> Navigator value);
    ]

let mode_encode =
  Ser.variant
    [
      Ser.Variant.unit
        "Captain"
        (
          function
          | Captain -> true
          | _ -> false
        );
      Ser.Variant.unit
        "Doctor"
        (
          function
          | Doctor -> true
          | _ -> false
        );
      Ser.Variant.newtype
        "Navigator"
        Ser.string
        (
          function
          | Navigator value -> Some value
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

let equal_sample = fun (left: sample) (right: sample) ->
  Bool.equal left.ready right.ready
  && Int.equal left.count right.count
  && Int32.equal left.small right.small
  && Int64.equal left.big right.big
  && Float.equal left.ratio right.ratio
  && String.equal left.label right.label
  && left.alias = right.alias
  && left.mode = right.mode
  && left.home = right.home
  && vec_to_list left.tags = vec_to_list right.tags
  && left.scores = right.scores

let sample_value: sample = {
  ready = true;
  count = 10;
  small = 7l;
  big = 3_000_000_000L;
  ratio = 12.5;
  label = "Thousand Sunny";
  alias = Some "Sunny-go";
  mode = Navigator "Nami";
  marker = ();
  home = ({ island = "Water 7"; berth = 7 }: berth);
  tags = Vector.from_list [ "log-pose"; "cola"; "coup-de-burst" ];
  scores = [|98; 87; 77; 101|];
}

let byte_values = fun value ->
  List.init
    ~count:(String.length value)
    ~fn:(fun index -> Char.code (String.get_unchecked value ~at:index))

let test_small_positive_int_uses_single_byte = fun _ctx ->
  match Serde_cbor.to_string Ser.int 10 with
  | Ok encoded when byte_values encoded = [ 0x0a ] -> Ok ()
  | Ok encoded ->
      Error ("expected CBOR encoding for 10 to be [0x0a], got "
      ^ String.concat "," (List.map (byte_values encoded) ~fn:Int.to_string))
  | Error err -> Error ("encode failed: " ^ Serde.Error.to_string err)

let test_negative_one_uses_single_byte = fun _ctx ->
  match Serde_cbor.to_string Ser.int (-1) with
  | Ok encoded when byte_values encoded = [ 0x20 ] -> Ok ()
  | Ok _ -> Error "expected CBOR encoding for -1 to be [0x20]"
  | Error err -> Error ("encode failed: " ^ Serde.Error.to_string err)

let test_roundtrips_sample = fun _ctx ->
  let* encoded =
    match Serde_cbor.to_string sample_encode sample_value with
    | Ok encoded -> Ok encoded
    | Error err -> Error ("encode failed: " ^ Serde.Error.to_string err)
  in
  match Serde_cbor.from_string sample_decode encoded with
  | Ok decoded when equal_sample decoded sample_value -> Ok ()
  | Ok _ -> Error "expected serde-cbor roundtrip to preserve the sample"
  | Error err -> Error ("decode failed: " ^ Serde.Error.to_string err)

let test_decodes_from_reader = fun _ctx ->
  match Serde_cbor.to_string sample_encode sample_value with
  | Ok encoded -> (
      match Serde_cbor.from_reader sample_decode (String.to_reader ~chunk_size:4 encoded) with
      | Ok decoded when equal_sample decoded sample_value -> Ok ()
      | Ok _ -> Error "expected reader decode to preserve the sample"
      | Error err -> Error ("reader decode failed: " ^ Serde.Error.to_string err)
    )
  | Error err -> Error ("encode failed: " ^ Serde.Error.to_string err)

let test_writes_to_writer = fun _ctx ->
  let buffer = IO.Buffer.create ~size:128 in
  let* () =
    match Serde_cbor.to_writer sample_encode (io_writer_of_buffer buffer) sample_value with
    | Ok () -> Ok ()
    | Error err -> Error ("writer encode failed: " ^ Serde.Error.to_string err)
  in
  match Serde_cbor.from_string sample_decode (IO.Buffer.contents buffer) with
  | Ok decoded when equal_sample decoded sample_value -> Ok ()
  | Ok _ -> Error "expected writer output to decode back into the original sample"
  | Error err -> Error ("writer output decode failed: " ^ Serde.Error.to_string err)

let test_rejects_trailing_bytes = fun _ctx ->
  match Serde_cbor.to_string sample_encode sample_value with
  | Ok encoded -> (
      match Serde_cbor.from_string sample_decode (encoded ^ "\x00") with
      | Ok _ -> Error "expected trailing CBOR bytes to be rejected"
      | Error _ -> Ok ()
    )
  | Error err -> Error ("encode failed: " ^ Serde.Error.to_string err)

let tests = [
  Test.case
    "serde-cbor small positive ints use a single byte"
    test_small_positive_int_uses_single_byte;
  Test.case "serde-cbor -1 uses a single byte" test_negative_one_uses_single_byte;
  Test.case "serde-cbor roundtrips samples" test_roundtrips_sample;
  Test.case "serde-cbor decodes from readers" test_decodes_from_reader;
  Test.case "serde-cbor writes to writers" test_writes_to_writer;
  Test.case "serde-cbor rejects trailing bytes" test_rejects_trailing_bytes;
]

let main ~args = Test.Cli.main ~name:"serde_cbor_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
