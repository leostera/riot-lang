open Std

module Html_tests = struct
  open Liveview.Html

  type test_msg = Click | Input of string | Submit

  let test_text_node () =
    Test.case "HTML: text node" (fun () ->
        let html = string "Hello" in
        let output = to_string html in
        if output = "Hello" then Ok ()
        else Error (format "Expected 'Hello', got '%s'" output))

  let test_int_node () =
    Test.case "HTML: int node" (fun () ->
        let html = int 42 in
        let output = to_string html in
        if output = "42" then Ok ()
        else Error (format "Expected '42', got '%s'" output))

  let test_empty_div () =
    Test.case "HTML: empty div" (fun () ->
        let html = div () in
        let output = to_string html in
        if output = "<div ></div>" then Ok ()
        else Error (format "Expected '<div ></div>', got '%s'" output))

  let test_div_with_id () =
    Test.case "HTML: div with id" (fun () ->
        let html = div ~id:"main" () in
        let output = to_string html in
        if output = "<div id=\"main\"></div>" then Ok ()
        else
          Error (format "Expected '<div id=\"main\"></div>', got '%s'" output))

  let test_div_with_attrs () =
    Test.case "HTML: div with multiple attrs" (fun () ->
        let html = div ~id:"main" ~attrs:[ ("class", "container") ] () in
        let output = to_string html in
        if
          output = "<div class=\"container\" id=\"main\"></div>"
          || output = "<div id=\"main\" class=\"container\"></div>"
        then Ok ()
        else
          Error
            (format
               "Expected '<div id=\"main\" class=\"container\"></div>', got \
                '%s'"
               output))

  let test_div_with_children () =
    Test.case "HTML: div with children" (fun () ->
        let html = div ~children:[ string "Hello" ] () in
        let output = to_string html in
        if output = "<div >Hello</div>" then Ok ()
        else Error (format "Expected '<div >Hello</div>', got '%s'" output))

  let test_nested_divs () =
    Test.case "HTML: nested divs" (fun () ->
        let html = div ~children:[ div ~children:[ string "Inner" ] () ] () in
        let output = to_string html in
        if output = "<div ><div >Inner</div></div>" then Ok ()
        else
          Error
            (format "Expected '<div ><div >Inner</div></div>', got '%s'" output))

  let test_h1 () =
    Test.case "HTML: h1 element" (fun () ->
        let html = h1 ~children:[ string "Title" ] () in
        let output = to_string html in
        if output = "<h1 >Title</h1>" then Ok ()
        else Error (format "Expected '<h1 >Title</h1>', got '%s'" output))

  let test_multiple_headings () =
    Test.case "HTML: multiple heading levels" (fun () ->
        let tests =
          [
            (h1 ~children:[ string "H1" ] (), "<h1 >H1</h1>");
            (h2 ~children:[ string "H2" ] (), "<h2 >H2</h2>");
            (h3 ~children:[ string "H3" ] (), "<h3 >H3</h3>");
            (h4 ~children:[ string "H4" ] (), "<h4 >H4</h4>");
            (h5 ~children:[ string "H5" ] (), "<h5 >H5</h5>");
            (h6 ~children:[ string "H6" ] (), "<h6 >H6</h6>");
          ]
        in
        let errors =
          List.filter_map
            (fun (html, expected) ->
              let output = to_string html in
              if output = expected then None
              else Some (format "Expected '%s', got '%s'" expected output))
            tests
        in
        if List.length errors = 0 then Ok ()
        else Error (String.concat "; " errors))

  let test_paragraph () =
    Test.case "HTML: paragraph element" (fun () ->
        let html = p ~children:[ string "Paragraph text" ] () in
        let output = to_string html in
        if output = "<p >Paragraph text</p>" then Ok ()
        else Error (format "Expected '<p >Paragraph text</p>', got '%s'" output))

  let test_span () =
    Test.case "HTML: span element" (fun () ->
        let html = span ~children:[ string "Inline text" ] () in
        let output = to_string html in
        if output = "<span >Inline text</span>" then Ok ()
        else
          Error (format "Expected '<span >Inline text</span>', got '%s'" output))

  let test_button_with_text () =
    Test.case "HTML: button with text" (fun () ->
        let html =
          button
            ~on_click:(on_click (fun _ -> Click))
            ~children:[ string "Click me" ]
            ()
        in
        let output = to_string html in
        if output = "<button >Click me</button>" then Ok ()
        else
          Error
            (format "Expected '<button >Click me</button>', got '%s'" output))

  let test_list_splat () =
    Test.case "HTML: list splat" (fun () ->
        let html = list [ string "A"; string "B"; string "C" ] in
        let output = to_string html in
        if output = "A\nB\nC" then Ok ()
        else Error (format "Expected 'A\\nB\\nC', got '%s'" output))

  let test_complex_tree () =
    Test.case "HTML: complex tree" (fun () ->
        let html =
          div ~id:"app"
            ~children:
              [
                h1 ~children:[ string "Counter" ] ();
                div
                  ~children:
                    [
                      span ~children:[ string "Count: "; int 10 ] ();
                      button
                        ~on_click:(on_click (fun _ -> Click))
                        ~children:[ string "Increment" ]
                        ();
                    ]
                  ();
              ]
            ()
        in
        let output = to_string html in
        let expected =
          "<div id=\"app\"><h1 >Counter</h1>\n\
           <div ><span >Count: \n\
           10</span>\n\
           <button >Increment</button></div></div>"
        in
        if output = expected then Ok ()
        else Error (format "Expected '%s', got '%s'" expected output))

  let test_script_with_src () =
    Test.case "HTML: script with src" (fun () ->
        let html = script ~src:"/app.js" ~type_:"text/javascript" () in
        let output = to_string html in
        let valid_outputs =
          [
            "<script src=\"/app.js\" type=\"text/javascript\"></script>";
            "<script type=\"text/javascript\" src=\"/app.js\"></script>";
          ]
        in
        if List.mem output valid_outputs then Ok ()
        else
          Error
            (format "Expected one of ['%s'], got '%s'"
               (String.concat "', '" valid_outputs)
               output))

  let test_script_with_inline () =
    Test.case "HTML: script with inline code" (fun () ->
        let html = script ~children:[ string "console.log('hi');" ] () in
        let output = to_string html in
        if output = "<script >console.log('hi');</script>" then Ok ()
        else
          Error
            (format "Expected '<script >console.log('hi');</script>', got '%s'"
               output))

  let test_attrs_to_string_empty () =
    Test.case "HTML: attrs_to_string with empty list" (fun () ->
        let output = attrs_to_string [] in
        if output = "" then Ok ()
        else Error (format "Expected empty string, got '%s'" output))

  let test_attrs_to_string_single () =
    Test.case "HTML: attrs_to_string with single attr" (fun () ->
        let output = attrs_to_string [ `attr ("id", "main") ] in
        if output = "id=\"main\"" then Ok ()
        else Error (format "Expected 'id=\"main\"', got '%s'" output))

  let test_attrs_to_string_multiple () =
    Test.case "HTML: attrs_to_string with multiple attrs" (fun () ->
        let output =
          attrs_to_string [ `attr ("id", "main"); `attr ("class", "container") ]
        in
        let valid_outputs =
          [
            "id=\"main\" class=\"container\""; "class=\"container\" id=\"main\"";
          ]
        in
        if List.mem output valid_outputs then Ok ()
        else
          Error
            (format "Expected one of ['%s'], got '%s'"
               (String.concat "', '" valid_outputs)
               output))

  let test_event_handlers_extraction () =
    Test.case "HTML: event handlers extraction" (fun () ->
        let handlers =
          event_handlers
            [
              `attr ("id", "btn");
              `event ("click", fun _ -> Click);
              `event ("input", fun s -> Input s);
            ]
        in
        if List.length handlers = 2 then
          let names = List.map fst handlers in
          if List.mem "click" names && List.mem "input" names then Ok ()
          else
            Error
              (format "Expected ['click', 'input'], got ['%s']"
                 (String.concat "', '" names))
        else Error (format "Expected 2 handlers, got %d" (List.length handlers)))

  let test_on_click_attr () =
    Test.case "HTML: on_click creates correct event attr" (fun () ->
        let handler = on_click (fun _ -> Click) in
        match handler with
        | `event (name, _) ->
            if name = "click" then Ok ()
            else Error (format "Expected event name 'click', got '%s'" name)
        | _ -> Error "Expected event attribute")

  type child_msg = ChildClick
  type parent_msg = ParentClick | ChildMsg of child_msg

  let test_map_action_text () =
    Test.case "HTML: map_action on text node" (fun () ->
        let html = string "Hello" in
        let mapped = map_action (fun m -> ChildMsg m) html in
        let output = to_string mapped in
        if output = "Hello" then Ok ()
        else Error (format "Expected 'Hello', got '%s'" output))

  let test_map_action_element () =
    Test.case "HTML: map_action on element" (fun () ->
        let html = div ~children:[ string "Test" ] () in
        let mapped = map_action (fun m -> ChildMsg m) html in
        let output = to_string mapped in
        if output = "<div >Test</div>" then Ok ()
        else Error (format "Expected '<div >Test</div>', got '%s'" output))

  let test_map_action_with_handler () =
    Test.case "HTML: map_action transforms event handlers" (fun () ->
        let html =
          button
            ~on_click:(on_click (fun _ -> Click))
            ~children:[ string "Click" ]
            ()
        in
        let mapped : parent_msg t = map_action (fun _ -> ParentClick) html in
        let handlers =
          match mapped with El { attrs; _ } -> event_handlers attrs | _ -> []
        in
        if List.length handlers = 1 then
          let _, handler = List.hd handlers in
          let msg = handler "" in
          match msg with
          | ParentClick -> Ok ()
          | _ -> Error "Expected ParentClick message"
        else Error (format "Expected 1 handler, got %d" (List.length handlers)))

  let test_map_action_nested () =
    Test.case "HTML: map_action on nested elements" (fun () ->
        let html =
          div
            ~children:
              [
                button
                  ~on_click:(on_click (fun _ -> Click))
                  ~children:[ string "1" ]
                  ();
                button
                  ~on_click:(on_click (fun _ -> Click))
                  ~children:[ string "2" ]
                  ();
              ]
            ()
        in
        let mapped : parent_msg t = map_action (fun _ -> ParentClick) html in
        let rec count_handlers = function
          | Text _ -> 0
          | Splat els ->
              List.fold_left (fun acc el -> acc + count_handlers el) 0 els
          | El { attrs; children; _ } ->
              List.length (event_handlers attrs)
              + List.fold_left
                  (fun acc el -> acc + count_handlers el)
                  0 children
        in
        let handler_count = count_handlers mapped in
        if handler_count = 2 then Ok ()
        else Error (format "Expected 2 handlers, got %d" handler_count))

  let tests =
    [
      test_text_node ();
      test_int_node ();
      test_empty_div ();
      test_div_with_id ();
      test_div_with_attrs ();
      test_div_with_children ();
      test_nested_divs ();
      test_h1 ();
      test_multiple_headings ();
      test_paragraph ();
      test_span ();
      test_button_with_text ();
      test_list_splat ();
      test_complex_tree ();
      test_script_with_src ();
      test_script_with_inline ();
      test_attrs_to_string_empty ();
      test_attrs_to_string_single ();
      test_attrs_to_string_multiple ();
      test_event_handlers_extraction ();
      test_on_click_attr ();
      test_map_action_text ();
      test_map_action_element ();
      test_map_action_with_handler ();
      test_map_action_nested ();
    ]
end

let () =
  Miniriot.run
    ~main:(fun ~args ->
      let tests = Html_tests.tests in
      Test.Cli.main ~name:"suri" ~tests ~args ())
    ~args:Env.args
  |> exit
