open Std

module Test = Std.Test

let feed_chunks = fun parser chunks ->
  let rec prepend_reversed values acc =
    match values with
    | [] -> acc
    | value :: rest -> prepend_reversed rest (value :: acc)
  in
  let rec loop parser chunks acc =
    match chunks with
    | [] -> (parser, List.reverse acc)
    | chunk :: rest ->
        let parser, events = Tty.Input.Parser.feed parser chunk in
        loop parser rest (prepend_reversed events acc)
  in
  loop parser chunks []

let byte_chunks = fun value ->
  List.init
    ~count:(String.length value)
    ~fn:(fun index -> String.sub value ~offset:index ~len:1)

let test_tokenizer_parses_csi = fun _ctx ->
  let _, tokens = Tty.Input.Tokenizer.feed (Tty.Input.Tokenizer.create ()) "\x1b[A" in
  match tokens with
  | [Tty.Input.Token.Control (Tty.Input.Token.Csi { raw; body })] when raw = "\x1b[A" && body = "A" ->
      Ok ()
  | _ ->
      Error "Expected tokenizer to emit one CSI token for up-arrow"

let test_parser_chunked_arrow = fun _ctx ->
  let parser = Tty.Input.Parser.create () in
  let _, events = feed_chunks parser [ "\x1b"; "["; "A" ] in
  match events with
  | [event] when Tty.Input.event_to_string event = "up" -> Ok ()
  | _ -> Error "Expected chunked up-arrow sequence to parse to one up event"

let test_parser_chunked_mouse = fun _ctx ->
  let parser = Tty.Input.Parser.create () in
  let _, events = feed_chunks parser (byte_chunks "\x1b[<0;10;20M") in
  match events with
  | [ `Mouse { button = Tty.Input.Left; action = Tty.Input.Mouse_press; x = 10; y = 20; modifiers = [] } ] ->
      Ok ()
  | _ ->
      Error "Expected chunked SGR mouse press to parse correctly"

let test_parser_bracketed_paste_one_byte_at_a_time = fun _ctx ->
  let parser = Tty.Input.Parser.create () in
  let _, events = feed_chunks parser (byte_chunks "\x1b[200~hello\nworld\x1b[201~") in
  match events with
  | [ `Paste "hello\nworld" ] ->
      Ok ()
  | _ ->
      Error "Expected bracketed paste to be reassembled into one paste event"

let test_parser_lone_escape_flushes = fun _ctx ->
  let parser, events = Tty.Input.Parser.feed (Tty.Input.Parser.create ()) "\x1b" in
  let _, flushed = Tty.Input.Parser.flush parser in
  if events = [] then
    match flushed with
    | [event] when Tty.Input.event_to_string event = "escape" -> Ok ()
    | _ -> Error "Expected flush to turn a lone escape into one escape event"
  else
    Error "Expected lone escape to stay pending until flush"

let test_parser_incomplete_escape_stays_pending = fun _ctx ->
  let parser, events = Tty.Input.Parser.feed (Tty.Input.Parser.create ()) "\x1b[" in
  if events != [] then
    Error "Expected incomplete CSI sequence to stay pending"
  else
    let _, flushed = Tty.Input.Parser.flush parser in
    match flushed with
    | [ `Unknown "\x1b[" ] -> Ok ()
    | _ -> Error "Expected flush to surface incomplete CSI as one unknown event"

let test_parser_alt_key = fun _ctx ->
  let parser = Tty.Input.Parser.create () in
  let _, events = feed_chunks parser [ "\x1b"; "x" ] in
  match events with
  | [event] when Tty.Input.event_to_string event = "alt+x" -> Ok ()
  | _ -> Error "Expected escape-prefixed x to parse as alt+x"

let tests =
  Test.[
    case "tokenizer_parses_csi" test_tokenizer_parses_csi;
    case "parser_chunked_arrow" test_parser_chunked_arrow;
    case "parser_chunked_mouse" test_parser_chunked_mouse;
    case "parser_bracketed_paste_one_byte_at_a_time" test_parser_bracketed_paste_one_byte_at_a_time;
    case "parser_lone_escape_flushes" test_parser_lone_escape_flushes;
    case "parser_incomplete_escape_stays_pending" test_parser_incomplete_escape_stays_pending;
    case "parser_alt_key" test_parser_alt_key;
  ]

let () =
  Actors.run ~main:(fun ~args -> Test.Cli.main ~name:"tty_input_parser" ~tests ~args) ~args:Env.args ()
