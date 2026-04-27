open Std

module Component = Suri.Component

let test_component_text_is_escaped = fun _ctx ->
  let html =
    Component.div [ Component.text "<script>alert('x') & \"y\"</script>"; ]
    |> Component.to_html
  in
  Test.assert_equal
    ~expected:"<div>&lt;script&gt;alert(&#39;x&#39;) &amp; &quot;y&quot;&lt;/script&gt;</div>"
    ~actual:html;
  Ok ()

let test_component_attrs_are_escaped = fun _ctx ->
  let html =
    Component.div
      ~attrs:[ Component.attr "title" "\"<&>'"; Component.attr "data-user" "alice&bob"; ]
      []
    |> Component.to_html
  in
  Test.assert_equal
    ~expected:"<div title=\"&quot;&lt;&amp;&gt;&#39;\" data-user=\"alice&amp;bob\"></div>"
    ~actual:html;
  Ok ()

let test_component_invalid_attr_name_is_omitted = fun _ctx ->
  let html =
    Component.div
      ~attrs:[ Component.attr "title" "safe"; Component.attr "onload x" "alert(1)"; ]
      [ Component.text "ok"; ]
    |> Component.to_html
  in
  Test.assert_equal ~expected:"<div title=\"safe\">ok</div>" ~actual:html;
  Ok ()

let test_component_invalid_tag_name_renders_children_safely = fun _ctx ->
  let html =
    Component.el "img src=x onerror=alert(1)" [ Component.text "fallback <b>text</b>"; ]
    |> Component.to_html
  in
  Test.assert_equal ~expected:"fallback &lt;b&gt;text&lt;/b&gt;" ~actual:html;
  Ok ()

let test_component_script_and_style_are_raw_text = fun _ctx ->
  let html =
    Component.fragment
      [
        Component.script "const ok = value => value < 3 && value > 0;";
        Component.style ".icon::before { content: \"<\"; }";
      ]
    |> Component.to_html
  in
  Test.assert_equal
    ~expected:"<script>const ok = value => value < 3 && value > 0;</script><style>.icon::before { content: \"<\"; }</style>"
    ~actual:html;
  Ok ()

let tests =
  Test.[
    case "component text is escaped" test_component_text_is_escaped;
    case "component attributes are escaped" test_component_attrs_are_escaped;
    case "component invalid attributes are omitted" test_component_invalid_attr_name_is_omitted;
    case
      "component invalid tags render children safely"
      test_component_invalid_tag_name_renders_children_safely;
    case "component script and style remain raw text" test_component_script_and_style_are_raw_text;
  ]

let main ~args = Test.Cli.main ~name:"suri_hardening_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
