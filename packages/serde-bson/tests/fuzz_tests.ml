open Std

module Test = Std.Test
module De = Serde.De

let accept_rejection = fun fn ->
  try
    fn ()
    |> ignore
  with
  | _ -> ()

type sample_field =
  | Field_name
  | Field_count
  | Field_active
  | Field_tags
  | Field_nickname

let sample_fields =
  De.fields
    [
      De.field "name" Field_name;
      De.field "count" Field_count;
      De.field "active" Field_active;
      De.field "tags" Field_tags;
      De.field "nickname" Field_nickname;
    ]

let sample_decode =
  De.record
    ~fields:sample_fields
    ~init:()
    ~step:(fun reader () field ->
      (
        match field with
        | Some Field_name ->
            De.read reader De.string
            |> ignore
        | Some Field_count ->
            De.read reader De.int
            |> ignore
        | Some Field_active ->
            De.read reader De.bool
            |> ignore
        | Some Field_tags ->
            De.read reader (De.list De.string)
            |> ignore
        | Some Field_nickname ->
            De.read reader (De.option De.string)
            |> ignore
        | None ->
            De.read reader De.skip_any
            |> ignore
      );
      ())
    ~finish:(fun () -> ())

let mutator =
  Test.Fuzz.Mutator.(bytes
  |> with_max_len 2_048
  |> with_dictionary [ ""; "\x05\x00\x00\x00\x00"; "\x00"; "\xff"; "name"; "riot"; ])

let test_decode_fuzz = fun _ctx input ->
  accept_rejection (fun () -> Serde_bson.from_string De.skip_any input);
  accept_rejection (fun () -> Serde_bson.from_string De.bool input);
  accept_rejection (fun () -> Serde_bson.from_string De.string input);
  accept_rejection (fun () -> Serde_bson.from_string De.int input);
  accept_rejection (fun () -> Serde_bson.from_string (De.list De.string) input);
  accept_rejection (fun () -> Serde_bson.from_string (De.dict De.skip_any) input);
  accept_rejection (fun () -> Serde_bson.from_string sample_decode input);
  accept_rejection (fun () -> Serde_bson.from_reader sample_decode (IO.Reader.from_string input));
  Ok ()

let tests =
  Test.[
    fuzz
      "serde-bson decoders accept arbitrary bytes"
      ~seeds:[ ""; "\x05\x00\x00\x00\x00"; "\x10count\x00\x01\x00\x00\x00\x00"; ]
      ~mutator
      test_decode_fuzz;
  ]

let main ~args = Test.Cli.main ~name:"serde_bson_fuzz_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
