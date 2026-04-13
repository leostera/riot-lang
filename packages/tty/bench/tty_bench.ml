open Std

let ansi_rich_text =
  let chunk = "\x1b[1;31merror\x1b[0m " in
  String.concat "" (List.init ~count:256 ~fn:(fun _ -> chunk))

let osc_rich_text =
  let chunk = "\x1b]2;tty\x07\x1b[38;5;196mwarn\x1b[0m " in
  String.concat "" (List.init ~count:256 ~fn:(fun _ -> chunk))

let styled_text = String.concat "" (List.init ~count:128 ~fn:(fun _ -> "tty benchmark payload "))

let styled_fragments = List.init ~count:1_024 ~fn:(fun _ -> "tty")

let unicode_text = String.concat "" (List.init ~count:256 ~fn:(fun _ -> "Cafe\u{0301} 🙂 "))

let mixed_trace = "\x1b[200~hello\x1b[201~\x1b[A\x1b[<0;10;20Mplain-text\x1b[I"

let mixed_trace_chunks =
  List.init
    ~count:(String.length mixed_trace)
    ~fn:(fun index -> String.sub mixed_trace ~offset:index ~len:1)

let paste_trace = "\x1b[200~hello\nworld\x1b[201~"

let paste_trace_chunks =
  List.init
    ~count:(String.length paste_trace)
    ~fn:(fun index -> String.sub paste_trace ~offset:index ~len:1)

let style =
  Tty.Style.default
  |> Tty.Style.bold
  |> Tty.Style.underline
  |> Tty.Style.fg (Tty.Color.make "#FF8800")
  |> Tty.Style.bg (Tty.Color.ansi 4)

let bench_cursor_position_seq = fun () ->
  let _ = Tty.Escape_seq.cursor_position_seq 24 80 in
  ()

let bench_strip_ansi = fun () ->
  let _ = Tty.Escape_seq.strip ansi_rich_text in
  ()

let bench_width_ansi = fun () ->
  let _ = Tty.Escape_seq.width ansi_rich_text in
  ()

let bench_strip_osc_ansi = fun () ->
  let _ = Tty.Escape_seq.strip osc_rich_text in
  ()

let bench_width_unicode = fun () ->
  let _ = Tty.Escape_seq.width unicode_text in
  ()

let bench_style_styled = fun () ->
  let _ = Tty.Style.styled style styled_text in
  ()

let bench_style_short_fragments = fun () ->
  let _ =
    List.map styled_fragments ~fn:(fun fragment -> Tty.Style.styled style fragment)
  in
  ()

let bench_input_parse_escape = fun () ->
  let _ = Tty.Input.parse_escape "\x1b[<20;7;9m" in
  ()

let bench_parser_chunked_trace = fun () ->
  let rec loop parser =
    function
    | [] ->
        let _ = Tty.Input.Parser.flush parser in
        ()
    | chunk :: rest ->
        let parser, _ = Tty.Input.Parser.feed parser chunk in
        loop parser rest
  in
  loop (Tty.Input.Parser.create ()) mixed_trace_chunks

let bench_parser_whole_trace = fun () ->
  let parser, _ = Tty.Input.Parser.feed (Tty.Input.Parser.create ()) mixed_trace in
  let _ = Tty.Input.Parser.flush parser in
  ()

let bench_parser_chunked_paste = fun () ->
  let rec loop parser =
    function
    | [] ->
        let _ = Tty.Input.Parser.flush parser in
        ()
    | chunk :: rest ->
        let parser, _ = Tty.Input.Parser.feed parser chunk in
        loop parser rest
  in
  loop (Tty.Input.Parser.create ()) paste_trace_chunks

let benchmarks =
  Bench.[
    with_config ~config:{ iterations = 500; warmup = 50 } "tty cursor_position_seq" bench_cursor_position_seq;
    with_config ~config:{ iterations = 100; warmup = 10 } "tty strip ansi: 256 chunks" bench_strip_ansi;
    with_config ~config:{ iterations = 100; warmup = 10 } "tty width ansi: 256 chunks" bench_width_ansi;
    with_config ~config:{ iterations = 100; warmup = 10 } "tty strip osc+ansi: 256 chunks" bench_strip_osc_ansi;
    with_config ~config:{ iterations = 100; warmup = 10 } "tty width unicode: 256 chunks" bench_width_unicode;
    with_config ~config:{ iterations = 200; warmup = 20 } "tty style.styled" bench_style_styled;
    with_config ~config:{ iterations = 100; warmup = 10 } "tty style.short_fragments" bench_style_short_fragments;
    with_config ~config:{ iterations = 500; warmup = 50 } "tty input.parse_escape mouse" bench_input_parse_escape;
    with_config ~config:{ iterations = 200; warmup = 20 } "tty parser.chunked mixed trace" bench_parser_chunked_trace;
    with_config ~config:{ iterations = 500; warmup = 50 } "tty parser.whole mixed trace" bench_parser_whole_trace;
    with_config ~config:{ iterations = 200; warmup = 20 } "tty parser.chunked paste" bench_parser_chunked_paste;
  ]

let () =
  Runtime.run ~main:(fun ~args -> Bench.Cli.main ~name:"tty benchmarks" ~benchmarks ~args) ~args:Env.args ()
