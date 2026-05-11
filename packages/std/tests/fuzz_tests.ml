open Std

module Test = Std.Test

let printable_text = fun input ->
  String.map
    input
    ~fn:(fun ch ->
      let code = Char.code ch in
      if code = 9 || code = 10 || code = 13 || (code >= 32 && code <= 126) then
        ch
      else
        ' ')

let accept_rejection = fun fn ->
  try
    fn ()
    |> ignore
  with
  | _ -> ()

let drain_csv = fun input ->
  let iter = Data.Csv.from_string input in
  let rec loop remaining =
    if remaining <= 0 then
      ()
    else
      match Iter.MutIterator.next iter with
      | None -> ()
      | Some _ -> loop (remaining - 1)
  in
  loop 64

let text_mutator =
  Test.Fuzz.Mutator.(text
  |> with_max_len 4_096
  |> with_dictionary
    [
      "";
      "{}";
      "[]";
      "{\"name\":\"riot\"}";
      "key = \"value\"";
      "<root><item>value</item></root>";
      "(root (item value))";
      "a,b,c\n1,2,3";
      "https://example.test/path?q=1#frag";
      "*.ml";
      "0.0.34";
      ">= 0.0.1";
      "550e8400-e29b-41d4-a716-446655440000";
    ])

let test_data_parsers_fuzz = fun _ctx input ->
  let input = printable_text input in
  accept_rejection (fun () -> Data.Json.from_string input);
  accept_rejection (fun () -> Data.JsonStream.from_string input);
  accept_rejection (fun () -> Data.Toml.parse input);
  accept_rejection (fun () -> Data.Xml.from_string input);
  accept_rejection (fun () -> Data.Sexp.from_string input);
  accept_rejection (fun () -> Data.Sexp.parse_many input);
  accept_rejection (fun () -> drain_csv input);
  Ok ()

let test_uri_glob_path_fuzz = fun _ctx input ->
  let input = printable_text input in
  accept_rejection (fun () -> Net.Uri.from_string input);
  accept_rejection
    (fun () ->
      let buffer = IO.Buffer.from_string input in
      Net.Uri.from_slice (IO.Buffer.readable buffer));
  accept_rejection (fun () -> Net.Uri.Query.parse input);
  accept_rejection (fun () -> Net.Uri.percent_decode input);
  accept_rejection (fun () -> Net.Uri.form_decode input);
  accept_rejection (fun () -> Path.from_string input);
  accept_rejection
    (fun () ->
      match Glob.create [ input ] with
      | Ok glob -> Glob.matches glob ~str:input
      | Error _ -> Ok false);
  Ok ()

let test_encoding_decoders_fuzz = fun _ctx input ->
  let input = printable_text input in
  accept_rejection (fun () -> Encoding.Base16.decode input);
  accept_rejection (fun () -> Encoding.Base16.decode_bytes input);
  accept_rejection (fun () -> Encoding.Base32.decode input);
  accept_rejection (fun () -> Encoding.Base32.decode_bytes input);
  accept_rejection (fun () -> Encoding.Base64.decode input);
  accept_rejection (fun () -> Encoding.Base85.decode input);
  accept_rejection (fun () -> Encoding.Base85.decode_bytes input);
  accept_rejection (fun () -> Encoding.Octal.decode_int input);
  accept_rejection (fun () -> Encoding.Octal.decode_int32 input);
  accept_rejection (fun () -> Encoding.Octal.decode_int64 input);
  Ok ()

let test_scalar_parsers_fuzz = fun _ctx input ->
  let input = printable_text input in
  accept_rejection (fun () -> DateTime.parse input);
  accept_rejection (fun () -> Version.parse input);
  accept_rejection (fun () -> Version.parse_requirement input);
  accept_rejection (fun () -> UUID.from_string input);
  accept_rejection (fun () -> Net.Addr.parse input);
  accept_rejection (fun () -> Net.Addr.parse_datagram input);
  Ok ()

let tests =
  Test.[
    fuzz
      "std structured data parsers accept arbitrary text"
      ~seeds:[ ""; "{}"; "key = \"value\""; "<root/>"; "(a b)"; "a,b\n1,2"; ]
      ~mutator:text_mutator
      test_data_parsers_fuzz;
    fuzz
      "std uri glob and path parsers accept arbitrary text"
      ~seeds:[ ""; "/tmp"; "https://example.test/a?b=c"; "*.ml"; "[bad"; ]
      ~mutator:text_mutator
      test_uri_glob_path_fuzz;
    fuzz
      "std encoding decoders accept arbitrary text"
      ~seeds:[ ""; "SGVsbG8="; "48656c6c6f"; "755"; "0o644"; ]
      ~mutator:text_mutator
      test_encoding_decoders_fuzz;
    fuzz
      "std scalar parsers accept arbitrary text"
      ~seeds:[
        "";
        "2026-05-11T12:00:00Z";
        "0.0.34";
        ">= 0.0.1";
        "550e8400-e29b-41d4-a716-446655440000";
        "127.0.0.1:8080";
      ]
      ~mutator:text_mutator
      test_scalar_parsers_fuzz;
  ]

let main ~args = Test.Cli.main ~name:"std_fuzz_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
