open Std

module Test = Std.Test
module Protocol = Postgres.Internal.Protocol
module Bytes = Std.IO.Bytes
module Buffer = Std.StringBuilder

let byte_at = fun text index -> Char.code (String.get_unchecked text ~at:index)

let declared_length = fun input ->
  if String.length input < 5 then
    Int.max 4 (String.length input + 4)
  else
    let raw =
      (byte_at input 1 lsl 24)
      lor (byte_at input 2 lsl 16)
      lor (byte_at input 3 lsl 8)
      lor byte_at input 4
    in
    4 + Int.rem raw 4_096

let body_after_header = fun input ->
  let len = String.length input in
  let offset =
    if len >= 5 then
      5
    else
      Int.min 1 len
  in
  String.sub input ~offset ~len:(len - offset)

let test_backend_message_reader_fuzz = fun _ctx input ->
  let msg_type =
    if String.length input = 0 then
      Char.code '?'
    else
      byte_at input 0
  in
  Protocol.Reader.parse_backend_message_result
    msg_type
    (declared_length input)
    (Bytes.from_string (body_after_header input))
  |> ignore;
  Ok ()

let test_config_parser_fuzz = fun _ctx input ->
  Postgres.Config.from_string input
  |> ignore;
  Ok ()

let test_writer_encoders_fuzz = fun _ctx input ->
  Protocol.Writer.startup_message ~user:input ~database:input ~application_name:(Some input)
  |> ignore;
  Protocol.Writer.password_message input
  |> ignore;
  Protocol.Writer.sasl_initial_response ~mechanism:"SCRAM-SHA-256" ~response:input
  |> ignore;
  Protocol.Writer.sasl_response input
  |> ignore;
  Protocol.Writer.query_message input
  |> ignore;
  Protocol.Writer.parse_message
    ~statement_name:input
    ~query:input
    ~param_types:[ String.length input; byte_at (input ^ "\x00") 0; ]
  |> ignore;
  Protocol.Writer.bind_message
    ~portal_name:input
    ~statement_name:input
    ~params:[ Some input; None; Some ""; ]
  |> ignore;
  Protocol.Writer.execute_message ~portal_name:input ~max_rows:(String.length input)
  |> ignore;
  Protocol.Writer.describe_message ~what:'S' ~name:input
  |> ignore;
  Protocol.Writer.close_message ~what:'S' ~name:input
  |> ignore;
  ignore (Buffer.create ~size:(String.length input + 1));
  Ok ()

let byte_mutator =
  Test.Fuzz.Mutator.(bytes
  |> with_max_len 4_096
  |> with_dictionary
    [
      "";
      "\x52\x00\x00\x00\x08\x00\x00\x00\x00";
      "\x5a\x00\x00\x00\x05I";
      "\x45\x00\x00\x00\x08SERROR\x00\x00";
      "postgresql://user:pass@localhost:5432/app";
      "localhost:5432:app:user:pass";
      "SELECT $1";
    ])

let tests =
  Test.[
    fuzz
      "backend message reader accepts arbitrary framed bytes"
      ~seeds:[
        "";
        "\x52\x00\x00\x00\x08\x00\x00\x00\x00";
        "\x5a\x00\x00\x00\x05I";
        "\x3f\x00\x00\x00\x04";
      ]
      ~mutator:byte_mutator
      test_backend_message_reader_fuzz;
    fuzz
      "connection string parser accepts arbitrary text"
      ~seeds:[
        "";
        "postgresql://alice:secret@localhost:5432/app";
        "localhost:5432:prod:bob:s3cr3t";
      ]
      ~mutator:byte_mutator
      test_config_parser_fuzz;
    fuzz
      "frontend message writers accept arbitrary text fields"
      ~seeds:[ ""; "select 1"; "stmt"; "\x00inside"; ]
      ~mutator:byte_mutator
      test_writer_encoders_fuzz;
  ]

let main ~args = Test.Cli.main ~name:"postgres_fuzz_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
