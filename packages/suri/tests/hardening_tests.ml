open Std

module Component = Suri.Component
module Static = Suri.Middleware.Static

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

let test_static_mount_matching_respects_segment_boundaries = fun _ctx ->
  Test.assert_true (Static.For_testing.matches_mount ~at:"/assets" ~request_path:"/assets");
  Test.assert_true (Static.For_testing.matches_mount ~at:"/assets" ~request_path:"/assets/app.css");
  Test.assert_false
    (Static.For_testing.matches_mount ~at:"/assets" ~request_path:"/assets2/app.css");
  Test.assert_false (Static.For_testing.matches_mount ~at:"/assets" ~request_path:"/asset");
  Ok ()

let test_static_root_boundary_is_component_based = fun _ctx ->
  Test.assert_true
    (Static.For_testing.path_is_within_root
      ~root:(Path.v "/var/www")
      (Path.v "/var/www/images/logo.png"));
  Test.assert_true
    (Static.For_testing.path_is_within_root ~root:(Path.v "/var/www") (Path.v "/var/www"));
  Test.assert_false
    (Static.For_testing.path_is_within_root ~root:(Path.v "/var/www") (Path.v "/var/www2/file"));
  Ok ()

let test_static_dotfile_detection_checks_all_segments = fun _ctx ->
  Test.assert_true (Static.For_testing.path_has_dot_segment (Path.v ".env"));
  Test.assert_true (Static.For_testing.path_has_dot_segment (Path.v "public/.git/config"));
  Test.assert_true (Static.For_testing.path_has_dot_segment (Path.v "nested/.well-known/token"));
  Test.assert_false (Static.For_testing.path_has_dot_segment (Path.v "public/assets/app.css"));
  Ok ()

let test_static_directory_listing_escapes_displayed_values = fun _ctx ->
  let html =
    Static.For_testing.directory_listing_html
      ~request_path:"/files/<root>"
      ~path:(Path.v "/tmp/<root>")
      ~entries:[ ("<script>alert(1)</script>", false, 12, 0.0); ]
  in
  Test.assert_true (String.contains html "Index of /tmp/&lt;root&gt;");
  Test.assert_true (String.contains html "&lt;script&gt;alert(1)&lt;/script&gt;");
  Test.assert_false (String.contains html "<script>alert(1)</script>");
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
    case
      "static mount matching respects segment boundaries"
      test_static_mount_matching_respects_segment_boundaries;
    case "static root boundary is component based" test_static_root_boundary_is_component_based;
    case
      "static dotfile detection checks all segments"
      test_static_dotfile_detection_checks_all_segments;
    case
      "static directory listing escapes displayed values"
      test_static_directory_listing_escapes_displayed_values;
  ]

let main ~args = Test.Cli.main ~name:"suri_hardening_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
