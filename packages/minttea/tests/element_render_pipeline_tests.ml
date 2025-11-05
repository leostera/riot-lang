open Std

(** Test Element → Scene → Matrix → ANSI pipeline *)
let test_simple_text_element () =
  let element = Minttea.Element.text "Hello" in
  let output = Minttea.Render.Pipeline.to_string ~width:5 ~height:1 element in
  
  (* Should contain "Hello" *)
  if String.contains output 'H' && String.contains output 'o' then Ok ()
  else Error (format "Expected 'Hello' in output, got: %s" output)

let test_box_with_padding () =
  let element = 
    Minttea.Element.box 
      ~style:(Minttea.Style.default
        |> Minttea.Style.padding_left 1
        |> Minttea.Style.padding_right 1)
      (Minttea.Element.text "Hi")
  in
  let output = Minttea.Render.Pipeline.to_string ~width:10 ~height:1 element in
  
  (* Should contain "Hi" with padding *)
  if String.contains output 'H' && String.contains output 'i' then Ok ()
  else Error (format "Expected 'Hi' in output, got: %s" output)

let test_row_layout () =
  let element =
    Minttea.Element.row [
      Minttea.Element.text ~style:(Minttea.Style.default |> Minttea.Style.width_fixed 3) "AAA";
      Minttea.Element.text ~style:(Minttea.Style.default |> Minttea.Style.width_fixed 3) "BBB";
    ]
  in
  let output = Minttea.Render.Pipeline.to_string ~width:6 ~height:1 element in
  
  (* Should contain both texts *)
  if String.contains output 'A' && String.contains output 'B' then Ok ()
  else Error (format "Expected 'AAA' and 'BBB' in output, got: %s" output)

let test_column_layout () =
  let element =
    Minttea.Element.column [
      Minttea.Element.text ~style:(Minttea.Style.default |> Minttea.Style.height_fixed 1) "Top";
      Minttea.Element.text ~style:(Minttea.Style.default |> Minttea.Style.height_fixed 1) "Bot";
    ]
  in
  let output = Minttea.Render.Pipeline.to_string ~width:10 ~height:2 element in
  
  (* Should contain both texts *)
  if String.contains output 'T' && String.contains output 'B' then Ok ()
  else Error (format "Expected 'Top' and 'Bot' in output, got: %s" output)

let test_flex_distribution () =
  let element =
    Minttea.Element.row [
      Minttea.Element.text ~style:(Minttea.Style.default |> Minttea.Style.width_flex 1.0) "A";
      Minttea.Element.text ~style:(Minttea.Style.default |> Minttea.Style.width_flex 2.0) "BB";
    ]
  in
  let output = Minttea.Render.Pipeline.to_string ~width:9 ~height:1 element in
  
  (* Should contain both texts with proportional space *)
  if String.contains output 'A' && String.contains output 'B' then Ok ()
  else Error (format "Expected 'A' and 'BB' with flex distribution, got: %s" output)

let test_styled_text () =
  let element =
    Minttea.Element.text 
      ~style:(Minttea.Style.default 
        |> Minttea.Style.bold true
        |> Minttea.Style.fg (Minttea.Style.color "#FF0000"))
      "Bold"
  in
  let output = Minttea.Render.Pipeline.to_string ~width:10 ~height:1 element in
  
  (* Should contain "Bold" and ANSI escape codes *)
  if String.contains output 'B' && String.contains output '\x1b' then Ok ()
  else Error (format "Expected styled 'Bold' text, got: %s" output)

let tests =
  Test.[
    case "simple text element" test_simple_text_element;
    case "box with padding" test_box_with_padding;
    case "row layout" test_row_layout;
    case "column layout" test_column_layout;
    case "flex distribution" test_flex_distribution;
    case "styled text" test_styled_text;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"element-render-pipeline" ~tests ~args)
    ~args:Env.args ()
