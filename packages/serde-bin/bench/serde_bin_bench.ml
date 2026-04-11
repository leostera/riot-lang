open Std
module Vector = Collections.Vector
module De = Serde.De
module Ser = Serde.Ser

type pet =
  | Cat
  | Dog of string

type item = {
  id: int;
  name: string;
  active: bool;
  tags: string vec;
  nickname: string option;
  pet: pet;
  score: int64;
}

type dataset = {
  version: int;
  source: string;
  items: item vec;
}

type item_field =
  | Item_id
  | Item_name
  | Item_active
  | Item_tags
  | Item_nickname
  | Item_pet
  | Item_score

type dataset_field =
  | Dataset_version
  | Dataset_source
  | Dataset_items

type item_builder = {
  mutable id: int option;
  mutable name: string option;
  mutable active: bool option;
  mutable tags: string vec option;
  mutable nickname: string option option;
  mutable pet: pet option;
  mutable score: int64 option;
}

type dataset_builder = {
  mutable version: int option;
  mutable source: string option;
  mutable items: item vec option;
}

let bench_config: Bench.bench_config = { iterations = 50; warmup = 3 }

let human_size = fun bytes ->
  if bytes >= 1_000_000 then
    Int.to_string (bytes / 1_000_000) ^ "MB"
  else if bytes >= 1_000 then
    Int.to_string (bytes / 1_000) ^ "KB"
  else
    Int.to_string bytes ^ "B"

let item_fields = De.fields
  [
    De.field "id" Item_id;
    De.field "name" Item_name;
    De.field "active" Item_active;
    De.field "tags" Item_tags;
    De.field "nickname" Item_nickname;
    De.field "pet" Item_pet;
    De.field "score" Item_score;
  ]

let dataset_fields =
  De.fields
    [
      De.field "version" Dataset_version;
      De.field "source" Dataset_source;
      De.field "items" Dataset_items;
    ]

let pet_decode =
  De.variant
    [ De.Variant.unit "Cat" Cat; De.Variant.newtype "Dog" De.string (fun value -> Dog value) ]

let pet_encode =
  Ser.variant
    [
      Ser.Variant.unit "Cat"
        (function
        | Cat -> true
        | Dog _ -> false);
      Ser.Variant.newtype "Dog" Ser.string
        (function
        | Dog value -> Some value
        | Cat -> None);
    ]

let item_decode =
  De.record_mut ~fields:item_fields
    ~create:(fun () : item_builder ->
      { id = None; name = None; active = None; tags = None; nickname = None; pet = None; score = None })
    ~step:(fun reader builder field ->
      match field with
      | Some Item_id -> builder.id <- Some (De.read reader De.int)
      | Some Item_name -> builder.name <- Some (De.read reader De.string)
      | Some Item_active -> builder.active <- Some (De.read reader De.bool)
      | Some Item_tags -> builder.tags <- Some (De.read reader (De.list De.string))
      | Some Item_nickname -> builder.nickname <- Some (De.read reader (De.option De.string))
      | Some Item_pet -> builder.pet <- Some (De.read reader pet_decode)
      | Some Item_score -> builder.score <- Some (De.read reader De.int64)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun (builder: item_builder) ->
      match (builder.id, builder.name, builder.active, builder.tags, builder.nickname, builder.pet, builder.score) with
      | (Some id, Some name, Some active, Some tags, Some nickname, Some pet, Some score) ->
          ({ id; name; active; tags; nickname; pet; score }: item)
      | _ -> De.missing_field ())

let item_encode =
  Ser.record
    (Ser.fields
      [
        Ser.field "id" Ser.int (fun (value: item) -> value.id);
        Ser.field "name" Ser.string (fun (value: item) -> value.name);
        Ser.field "active" Ser.bool (fun (value: item) -> value.active);
        Ser.field "tags" (Ser.list Ser.string) (fun (value: item) -> value.tags);
        Ser.field "nickname" (Ser.option Ser.string) (fun (value: item) -> value.nickname);
        Ser.field "pet" pet_encode (fun (value: item) -> value.pet);
        Ser.field "score" Ser.int64 (fun (value: item) -> value.score);
      ])

let dataset_decode =
  De.record_mut ~fields:dataset_fields
    ~create:(fun () : dataset_builder -> { version = None; source = None; items = None })
    ~step:(fun reader builder field ->
      match field with
      | Some Dataset_version -> builder.version <- Some (De.read reader De.int)
      | Some Dataset_source -> builder.source <- Some (De.read reader De.string)
      | Some Dataset_items -> builder.items <- Some (De.read reader (De.list item_decode))
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun (builder: dataset_builder) ->
      match (builder.version, builder.source, builder.items) with
      | (Some version, Some source, Some items) -> ({ version; source; items }: dataset)
      | _ -> De.missing_field ())

let dataset_encode =
  Ser.record
    (Ser.fields
      [
        Ser.field "version" Ser.int (fun (value: dataset) -> value.version);
        Ser.field "source" Ser.string (fun (value: dataset) -> value.source);
        Ser.field "items" (Ser.list item_encode) (fun (value: dataset) -> value.items);
      ])

let build_dataset = fun () ->
  let items = Vector.with_capacity 2_000 in
  for index = 0 to 1_999 do
    let tags =
      Vector.of_list
        [
          "riot";
          "serde";
          "bin";
          "batch-" ^ Int.to_string (index land 15);
          "bucket-" ^ Int.to_string (index mod 32);
        ]
    in
    Vector.push items
      ({
          id = index;
          name = "item-" ^ Int.to_string index;
          active = index land 1 = 0;
          tags;
          nickname =
            if index mod 3 = 0 then
              Some ("nick-" ^ Int.to_string index)
            else
              None;
          pet =
            if index mod 5 = 0 then
              Cat
            else
              Dog ("dog-" ^ Int.to_string (index mod 128));
          score = Int64.of_int (index * 17);
        }: item)
  done;
  ({ version = 1; source = "serde-bin bench"; items }: dataset)

type fixture = {
  dataset: dataset;
  serde_bytes: string;
  marshal_bytes: string;
}

let build_fixture = fun () ->
  let dataset = build_dataset () in
  let serde_bytes =
    Serde_bin.to_string dataset_encode dataset
    |> Result.expect ~msg:"expected serde-bin fixture encoding to succeed"
  in
  let marshal_bytes = Stdlib.Marshal.to_string dataset [] in
  let decoded: dataset =
    Serde_bin.of_string dataset_decode serde_bytes
    |> Result.expect ~msg:"expected serde-bin fixture decoding to succeed"
  in
  let _marshal_roundtrip: dataset = Stdlib.Marshal.from_string marshal_bytes 0 in
  ignore decoded;
  { dataset; serde_bytes; marshal_bytes }

let bench_encode_serde = fun fixture () ->
  ignore (Serde_bin.to_string dataset_encode fixture.dataset)

let bench_encode_marshal = fun fixture () ->
  ignore (Stdlib.Marshal.to_string fixture.dataset [])

let bench_decode_serde = fun fixture () ->
  ignore (Serde_bin.of_string dataset_decode fixture.serde_bytes)

let bench_decode_marshal = fun fixture () ->
  let _decoded: dataset = Stdlib.Marshal.from_string fixture.marshal_bytes 0 in
  ()

let benchmark_suite = fun fixture ->
  let serde_size = human_size (String.length fixture.serde_bytes) in
  let marshal_size = human_size (String.length fixture.marshal_bytes) in
  Bench.[
    with_config
      ~config:bench_config
      ("serde-bin encode dataset (" ^ serde_size ^ ")")
      (bench_encode_serde fixture);
    with_config
      ~config:bench_config
      ("Stdlib.Marshal encode dataset (" ^ marshal_size ^ ")")
      (bench_encode_marshal fixture);
    with_config
      ~config:bench_config
      ("serde-bin decode dataset (" ^ serde_size ^ ")")
      (bench_decode_serde fixture);
    with_config
      ~config:bench_config
      ("Stdlib.Marshal decode dataset (" ^ marshal_size ^ ")")
      (bench_decode_marshal fixture);
  ]

let () =
  Actors.run
    ~main:(fun ~args ->
      let fixture = build_fixture () in
      Bench.Cli.main ~name:"serde-bin benchmarks" ~benchmarks:(benchmark_suite fixture) ~args)
    ~args:Env.args
    ()
