open Std
open Suri
open Suri.Component

(* Simple static page showcasing component system *)

let welcome_page : unit t = html
  [ head
      [
        title [ text "Welcome to Suri Components" ];
        meta
        ~attrs:[ attr "charset" "UTF-8"; attr "viewport" "width=device-width, initial-scale=1.0" ]
        ();
        style
          {|
        body { 
          font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
          max-width: 800px; 
          margin: 40px auto; 
          padding: 0 20px;
          line-height: 1.6;
          color: #333;
        }
        .hero { 
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          padding: 60px 40px; 
          border-radius: 12px;
          margin-bottom: 40px;
        }
        .hero h1 {
          margin: 0 0 16px 0;
          font-size: 2.5em;
        }
        .hero p {
          margin: 0;
          font-size: 1.2em;
          opacity: 0.9;
        }
        section {
          margin-bottom: 40px;
        }
        h2 {
          color: #667eea;
          border-bottom: 2px solid #667eea;
          padding-bottom: 8px;
        }
        ul {
          list-style: none;
          padding: 0;
        }
        li {
          padding: 8px 0;
          padding-left: 24px;
          position: relative;
        }
        li:before {
          content: "✓";
          color: #667eea;
          font-weight: bold;
          position: absolute;
          left: 0;
        }
        .form-group {
          margin-bottom: 20px;
        }
        label {
          display: block;
          margin-bottom: 8px;
          font-weight: 600;
          color: #555;
        }
        input, textarea {
          width: 100%;
          padding: 12px;
          border: 2px solid #ddd;
          border-radius: 6px;
          font-size: 14px;
          font-family: inherit;
          box-sizing: border-box;
        }
        input:focus, textarea:focus {
          outline: none;
          border-color: #667eea;
        }
        .btn { 
          padding: 12px 24px; 
          background: #667eea;
          color: white; 
          border: none; 
          border-radius: 6px;
          cursor: pointer;
          font-size: 16px;
          font-weight: 600;
          transition: background 0.2s;
        }
        .btn:hover {
          background: #5568d3;
        }
        fieldset {
          border: 2px solid #e0e0e0;
          border-radius: 8px;
          padding: 20px;
        }
        legend {
          padding: 0 8px;
          font-weight: 600;
          color: #667eea;
        }
        footer {
          margin-top: 60px;
          padding-top: 20px;
          border-top: 1px solid #ddd;
          text-align: center;
          color: #999;
        }
        code {
          background: #f4f4f4;
          padding: 2px 6px;
          border-radius: 3px;
          font-family: 'Monaco', 'Courier New', monospace;
        }
      |}
      ]; body
      [
        header
        ~attrs:[ class_ "hero" ]
        [
          h1 [ text "Welcome to Suri Components" ];
          p [ text "Build type-safe, composable HTML with the power of OCaml" ]
        ];
        main
          [
            section
            [
              h2 [ text "Why Components?" ];
              p
              [
                text "Suri Components provide a React-style way of building UIs that work ";
                text "seamlessly with both static HTML generation and LiveView interactive apps."
              ]
            ];
            section
            [
              h2 [ text "Features" ];
              ul
              [
                li [ text "Type-safe HTML generation - catch errors at compile time" ];
                li [ text "React-style component composition" ];
                li [ text "Works with static HTML and LiveView" ];
                li [ text "No inline JavaScript - use LiveView for interactivity" ];
                li [ text "Build reusable design systems" ];
                li [ text "Self-closing tags handled correctly" ]
              ]
            ];
            section
            [
              h2 [ text "Example: Contact Form" ];
              p
              [
                text "This form is rendered entirely with components. ";
                text "Add LiveView handlers to make it interactive!"
              ];
              form
              ~attrs:[ action "/submit"; method_ "POST" ]
              [
                fieldset
                [
                  legend [ text "Contact Information" ];
                  div
                  ~attrs:[ class_ "form-group" ]
                  [
                    label ~attrs:[ for_ "name" ] [ text "Name" ];
                    input
                    ~attrs:[
                      type_ "text";
                      id "name";
                      name "name";
                      placeholder "Enter your name";
                      required
                    ]
                    ()
                  ];
                  div
                  ~attrs:[ class_ "form-group" ]
                  [
                    label ~attrs:[ for_ "email" ] [ text "Email" ];
                    input
                    ~attrs:[
                      type_ "email";
                      id "email";
                      name "email";
                      placeholder "you@example.com";
                      required
                    ]
                    ()
                  ];
                  div
                  ~attrs:[ class_ "form-group" ]
                  [
                    label ~attrs:[ for_ "message" ] [ text "Message" ];
                    textarea
                    ~attrs:[
                      id "message";
                      name "message";
                      placeholder "Your message here...";
                      attr "rows" "4";
                      required
                    ]
                    []
                  ];
                  button ~attrs:[ type_ "submit"; class_ "btn" ] [ text "Send Message" ]
                ]
              ]
            ];
            section
              [
                h2 [ text "Code Example" ];
                p [ text "This entire page is built with components:" ];
                pre
                  [ code
                      [ text
                          {|open Suri.Component

let my_page =
  div ~attrs:[class_ "container"] [
    h1 [text "Welcome"];
    p [text "Hello, Components!"];
    button ~attrs:[class_ "btn"] [
      text "Click me"
    ]
  ]

let html = to_html my_page|} ] ]
              ]
          ];
        footer [ p [ text "Built with "; strong [ text "Suri.Component" ]; text " | © 2025" ] ]
      ] ]

let () =
  let html = to_html welcome_page in
  println html
