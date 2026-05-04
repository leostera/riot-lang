open Std

module Array = Collections.Array
module Vector = Collections.Vector
module De = Serde.De
module Ser = Serde.Ser

type status =
  | Active
  | Draft
  | Archived

type payload = {
  name: string;
  role: string;
  crew: string;
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

type payload_field =
  | Field_name
  | Field_role
  | Field_crew
  | Field_age
  | Field_active
  | Field_small
  | Field_big
  | Field_ratio
  | Field_tags
  | Field_scores
  | Field_nickname
  | Field_status

type payload_builder = {
  mutable name: string option;
  mutable role: string option;
  mutable crew: string option;
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

type fixture_spec = { label: string; tag_count: int; score_count: int; string_repeat: int }

type fixture = {
  label: string;
  value: payload;
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

let repeat = fun text count ->
  let buffer = IO.Buffer.create ~size:(String.length text * count) in
  for _index = 1 to count do
    IO.Buffer.add_string buffer text
  done;
  IO.Buffer.contents buffer

let tags_of_count = fun count ->
  let tags = Vector.with_capacity ~size:count in
  for index = 0 to count - 1 do
    Vector.push tags ~value:("grand-line-log-" ^ Int.to_string index)
  done;
  tags

let scores_of_count = fun count -> Array.init ~count ~fn:(fun index -> (index * 97) mod 1_000_000)

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

let payload_fields =
  De.fields
    [
      De.field "name" Field_name;
      De.field "role" Field_role;
      De.field "crew" Field_crew;
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

let payload_decode =
  De.record_mut
    ~fields:payload_fields
    ~create:(fun (): payload_builder ->
      {
        name = None;
        role = None;
        crew = None;
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
      | Some Field_role -> builder.role <- Some (De.read reader De.string)
      | Some Field_crew -> builder.crew <- Some (De.read reader De.string)
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
    ~finish:(fun (builder: payload_builder) ->
      match (
        builder.name,
        builder.role,
        builder.crew,
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
          Some role,
          Some crew,
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
            role;
            crew;
            age;
            active;
            small;
            big;
            ratio;
            tags;
            scores;
            nickname;
            status;
          }: payload)
      | _ -> De.missing_field ())

let payload_encode =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "name" Ser.string (fun (value: payload) -> value.name);
          Ser.field "role" Ser.string (fun (value: payload) -> value.role);
          Ser.field "crew" Ser.string (fun (value: payload) -> value.crew);
          Ser.field "age" Ser.int (fun (value: payload) -> value.age);
          Ser.field "active" Ser.bool (fun (value: payload) -> value.active);
          Ser.field "small" Ser.int32 (fun (value: payload) -> value.small);
          Ser.field "big" Ser.int64 (fun (value: payload) -> value.big);
          Ser.field "ratio" Ser.float (fun (value: payload) -> value.ratio);
          Ser.field "tags" (Ser.list Ser.string) (fun (value: payload) -> value.tags);
          Ser.field "scores" (Ser.array Ser.int) (fun (value: payload) -> value.scores);
          Ser.field "nickname" (Ser.option Ser.string) (fun (value: payload) -> value.nickname);
          Ser.field "status" status_encode (fun (value: payload) -> value.status);
        ]
    )

let build_fixture = fun ({
  label;
  tag_count;
  score_count;
  string_repeat;
}: fixture_spec) ->
  let value: payload = {
    name = "Monkey D. Luffy";
    role = repeat "captain-of-the-straw-hats-" string_repeat;
    crew = repeat "thousand-sunny-route-to-laugh-tale-" string_repeat;
    age = 19;
    active = true;
    small = 1_337l;
    big = 3_000_000_000L;
    ratio = 42.125;
    tags = tags_of_count tag_count;
    scores = scores_of_count score_count;
    nickname = Some "future-pirate-king";
    status = Active;
  }
  in
  let encoded =
    Serde_urlencoded.to_string payload_encode value
    |> Result.expect ~msg:("expected " ^ label ^ " fixture to encode")
  in
  { label; value; encoded }

let small_fixture_spec = {
  label = "small";
  tag_count = 64;
  score_count = 64;
  string_repeat = 4;
}

let large_fixture_spec = {
  label = "large";
  tag_count = 16_384;
  score_count = 16_384;
  string_repeat = 256;
}

let io_writer_of_buffer =
  let module Write = struct
    type t = IO.Buffer.t

    let write = fun buffer ~from ->
      let written = IO.Buffer.readable_bytes from in
      IO.Buffer.append_slice buffer (IO.Buffer.readable from)
      |> Result.expect ~msg:"serde-urlencoded bench writer should append buffer contents";
      Ok written

    let write_vectored = fun buffer ~from ->
      let written = ref 0 in
      IO.IoVec.for_each
        from
        ~fn:(fun chunk ->
          IO.Buffer.append_slice buffer chunk
          |> Result.expect ~msg:"serde-urlencoded bench writer should append slices";
          written := !written + IO.IoSlice.length chunk);
      Ok !written

    let flush = fun _buffer -> Ok ()
  end in
  fun buffer -> IO.Writer.from_sink (module Write) buffer

let bench_encode_in_memory = fun fixture () ->
  ignore
    (Serde_urlencoded.to_string payload_encode fixture.value)

let bench_encode_writer = fun fixture () ->
  let buffer = IO.Buffer.create ~size:(String.length fixture.encoded) in
  ignore (Serde_urlencoded.to_writer payload_encode (io_writer_of_buffer buffer) fixture.value)

let bench_decode_in_memory = fun fixture () ->
  ignore
    (Serde_urlencoded.from_string payload_decode fixture.encoded)

let bench_decode_reader = fun fixture () ->
  ignore
    (Serde_urlencoded.from_reader
      payload_decode
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
      ("serde-urlencoded encode in-memory " ^ fixture.label ^ " payload (" ^ size ^ ")")
      (bench_encode_in_memory fixture);
    with_config
      ~config
      ("serde-urlencoded encode writer " ^ fixture.label ^ " payload (" ^ size ^ ")")
      (bench_encode_writer fixture);
    with_config
      ~config
      ("serde-urlencoded decode in-memory " ^ fixture.label ^ " payload (" ^ size ^ ")")
      (bench_decode_in_memory fixture);
    with_config
      ~config
      ("serde-urlencoded decode reader " ^ fixture.label ^ " payload (" ^ size ^ ")")
      (bench_decode_reader fixture);
  ]

let main ~args =
  let small_fixture = build_fixture small_fixture_spec in
  let large_fixture = build_fixture large_fixture_spec in
  let benchmarks = benchmark_suite small_fixture @ benchmark_suite large_fixture in
  Bench.Cli.main ~name:"serde-urlencoded benchmarks" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
