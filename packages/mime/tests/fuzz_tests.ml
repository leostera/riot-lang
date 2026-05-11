open Std

module Test = Std.Test

let printable_text = fun input ->
  let bytes = IO.Bytes.from_string input in
  let len = IO.Bytes.length bytes in
  let rec loop index =
    if index >= len then
      IO.Bytes.unsafe_to_string bytes
    else
      let ch = IO.Bytes.get_unchecked bytes ~at:index in
      let code = Char.code ch in
      if code < 32 || code > 126 then
        IO.Bytes.set_unchecked bytes ~at:index ~char:' ';
    loop (index + 1)
  in
  loop 0

let mutator =
  Test.Fuzz.Mutator.(text
  |> with_max_len 4_096
  |> with_dictionary
    [
      "";
      "text/plain";
      "multipart/mixed; boundary=abc";
      "--abc\r\nContent-Type: text/plain\r\n\r\nhello\r\n--abc--";
      "attachment; filename=\"a.txt\"";
      "base64";
    ])

let test_mime_fuzz = fun _ctx input ->
  let input = printable_text input in
  let headers = [
    ("content-type", input);
    ("content-disposition", input);
    ("content-transfer-encoding", input);
    ("content-id", input);
  ]
  in
  match Mime.parse ~headers ~body:input with
  | Error _ -> Ok ()
  | Ok entity ->
      Mime.attachments entity
      |> ignore;
      Ok ()

let tests =
  Test.[
    fuzz
      "mime header and multipart parser accepts arbitrary text"
      ~seeds:[ ""; "text/plain"; "multipart/mixed; boundary=abc"; ]
      ~mutator
      test_mime_fuzz;
  ]

let main ~args = Test.Cli.main ~name:"mime_fuzz_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
