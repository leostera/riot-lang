open Std
open Propane

module Test = Std.Test

let examples = 100_000

let property_config = {
  Property.default_config with
  test_count = examples;
}

let assert_property = fun name property ->
  match Property.check ~config:property_config property with
  | Property.Success ->
      Ok ()
  | Property.Failure { counter_example; shrink_steps } ->
      Error (
        name
        ^ " failed\nCounter-example:\n"
        ^ counter_example
        ^ "\nShrink steps: "
        ^ Int.to_string shrink_steps
      )
  | Property.Error { exception_; backtrace } ->
      Error (
        name
        ^ " raised "
        ^ Kernel.Exception.to_string exception_
        ^ "\n"
        ^ backtrace
      )
  | Property.Assumption_violated ->
      Error (name ^ " exhausted assumptions")

let byte_chunks = fun value ->
  List.init
    ~count:(String.length value)
    ~fn:(fun index -> String.sub value ~offset:index ~len:1)

let event_strings = fun events ->
  List.map events ~fn:Tty.Input.event_to_string

let rec string_lists_equal = fun left right ->
  match (left, right) with
  | [], [] -> true
  | left :: left_rest, right :: right_rest ->
      String.equal left right && string_lists_equal left_rest right_rest
  | _ -> false

let parse_whole = fun trace ->
  let parser, events = Tty.Input.Parser.feed (Tty.Input.Parser.create ()) trace in
  let _, flushed = Tty.Input.Parser.flush parser in
  event_strings (events @ flushed)

let parse_bytewise = fun trace ->
  let rec loop parser chunks acc =
    match chunks with
    | [] ->
        let _, flushed = Tty.Input.Parser.flush parser in
        List.reverse_append (List.reverse (event_strings flushed)) acc |> List.reverse
    | chunk :: rest ->
        let parser, events = Tty.Input.Parser.feed parser chunk in
        loop parser rest (List.reverse_append (List.reverse (event_strings events)) acc)
  in
  loop (Tty.Input.Parser.create ()) (byte_chunks trace) []

let short_printable =
  Arbitrary.make
    ~print:String.escaped
    Generator.(string_size (int_range 0 24) char_printable)

let ansiish_string =
  let text_fragment = Generator.string_size (Generator.int_range 0 8) Generator.char_printable in
  let control_fragment =
    Generator.one_of
      [
        Generator.return "\x1b[31m";
        Generator.return "\x1b[0m";
        Generator.return "\x1b[A";
        Generator.return "\x1b]2;title\x07";
        Generator.return "\x1b[?25l";
        Generator.return "\x1b[?25h";
      ]
  in
  Arbitrary.make
    ~print:String.escaped
    Generator.(map (String.concat "") (list_size (int_range 0 12) (one_of [ text_fragment; control_fragment ])))

let parser_trace =
  let text_fragment = Generator.string_size (Generator.int_range 0 4) Generator.char_lowercase in
  let token_fragment =
    Generator.one_of
      [
        Generator.return "\x1b[A";
        Generator.return "\x1b[B";
        Generator.return "\x1b[1;5D";
        Generator.return "\x1b[24~";
        Generator.return "\x1b[Z";
        Generator.return "\x1b[I";
        Generator.return "\x1b[O";
        Generator.return "\x1b[<0;10;20M";
        Generator.return "\x1b]2;tty\x07";
        Generator.return "\x1bOP";
      ]
  in
  Arbitrary.make
    ~print:String.escaped
    Generator.(map (String.concat "") (list_size (int_range 0 10) (one_of [ text_fragment; token_fragment ])))

let strip_is_idempotent =
  Property.for_all ansiish_string
    (fun value ->
      let stripped = Tty.Escape_seq.strip value in
      String.equal (Tty.Escape_seq.strip stripped) stripped)

let strip_removes_escape_bytes =
  Property.for_all ansiish_string
    (fun value ->
      let stripped = Tty.Escape_seq.strip value in
      not (String.contains stripped "\x1b"))

let style_default_is_identity =
  Property.for_all Arbitrary.string
    (fun value -> String.equal (Tty.Style.styled Tty.Style.default value) value)

let styled_width_matches_plain_width =
  let style =
    Tty.Style.default
    |> Tty.Style.bold
    |> Tty.Style.underline
    |> Tty.Style.fg (Tty.Color.make "#FF8800")
  in
  Property.for_all short_printable
    (fun value ->
      Int.equal (Tty.Escape_seq.width (Tty.Style.styled style value)) (String.width value))

let color_of_rgb_clamps_components =
  Property.for_all Arbitrary.(triple int int int)
    (fun (red, green, blue) ->
      match Tty.Color.of_rgb (red, green, blue) with
      | Tty.Color.RGB (red, green, blue) ->
          Int.(red >= 0 && red <= 255 && green >= 0 && green <= 255 && blue >= 0 && blue <= 255)
      | _ ->
          false)

let parser_chunking_is_invariant =
  Property.for_all parser_trace
    (fun trace -> string_lists_equal (parse_whole trace) (parse_bytewise trace))

let tests = [
  Test.property "strip is idempotent" ~examples (fun _ctx -> assert_property "strip is idempotent" strip_is_idempotent);
  Test.property "strip removes escape bytes" ~examples (fun _ctx -> assert_property "strip removes escape bytes" strip_removes_escape_bytes);
  Test.property "style default is identity" ~examples (fun _ctx -> assert_property "style default is identity" style_default_is_identity);
  Test.property "styled width matches plain width" ~examples (fun _ctx ->
    assert_property "styled width matches plain width" styled_width_matches_plain_width);
  Test.property "color.of_rgb clamps components" ~examples (fun _ctx ->
    assert_property "color.of_rgb clamps components" color_of_rgb_clamps_components);
  Test.property "parser chunking is invariant" ~examples (fun _ctx ->
    assert_property "parser chunking is invariant" parser_chunking_is_invariant);
]

let () =
  Actors.run ~main:(fun ~args -> Test.Cli.main ~name:"tty_property" ~tests ~args) ~args:Env.args ()
