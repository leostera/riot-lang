open Std
open Suri
open Suri.Component

(**
   This example shows how to start with static HTML components
   and progressively enhance them with LiveView interactivity.

   The key insight: THE SAME COMPONENT STRUCTURE works for both!
*)

(** Common Styles *)
let page_styles =
  {|
  body { 
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
    max-width: 600px; 
    margin: 40px auto; 
    padding: 0 20px;
    text-align: center;
    background: #f5f5f5;
  }
  .card {
    background: white;
    border-radius: 12px;
    padding: 40px;
    box-shadow: 0 4px 6px rgba(0,0,0,0.1);
    margin-bottom: 20px;
  }
  h1 {
    margin: 0 0 8px 0;
    color: #333;
  }
  .label {
    color: #666;
    font-size: 14px;
    margin-bottom: 20px;
    display: block;
  }
  .count {
    font-size: 72px;
    font-weight: bold;
    color: #007bff;
    margin: 20px 0;
    font-variant-numeric: tabular-nums;
  }
  .buttons {
    display: flex;
    gap: 12px;
    justify-content: center;
    margin-top: 20px;
  }
  .btn { 
    padding: 12px 24px; 
    border: none; 
    border-radius: 8px;
    cursor: pointer;
    font-size: 18px;
    font-weight: 600;
    transition: all 0.2s;
    min-width: 60px;
  }
  .btn-primary { 
    background: #007bff;
    color: white; 
  }
  .btn-primary:hover {
    background: #0056b3;
    transform: translateY(-1px);
    box-shadow: 0 2px 4px rgba(0,123,255,0.3);
  }
  .btn-secondary { 
    background: #6c757d;
    color: white; 
  }
  .btn-secondary:hover {
    background: #545b62;
    transform: translateY(-1px);
    box-shadow: 0 2px 4px rgba(108,117,125,0.3);
  }
  .btn:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }
  .note {
    background: #fff3cd;
    border: 1px solid #ffeaa7;
    color: #856404;
    padding: 12px;
    border-radius: 6px;
    margin-top: 20px;
    font-size: 14px;
  }
  .success {
    background: #d4edda;
    border: 1px solid #c3e6cb;
    color: #155724;
    padding: 12px;
    border-radius: 6px;
    margin-top: 20px;
    font-size: 14px;
  }
  code {
    background: #f4f4f4;
    padding: 2px 6px;
    border-radius: 3px;
    font-family: 'Monaco', 'Courier New', monospace;
  }
|}

(** Step 1: Static Component (No Interactivity) *)
let counter_view_static = fun count ->
  div
    ~attrs:[ class_ "card" ]
    [
      h1 [ text "Counter (Static HTML)" ];
      span ~attrs:[ class_ "label" ] [ text "A static component preview" ];
      div ~attrs:[ class_ "count" ] [ text (Int.to_string count) ];
      div
        ~attrs:[ class_ "buttons" ]
        [
          button ~attrs:[ class_ "btn btn-secondary" ] [ text "-" ];
          button ~attrs:[ class_ "btn btn-primary" ] [ text "+" ];
        ];
      div
        ~attrs:[ class_ "note" ]
        [
          strong [ text "Note: " ];
          text "Buttons are not interactive in static HTML rendering. ";
          text "This is just a preview of what the component looks like.";
        ];
    ]

let static_page: unit t =
  html
    [
      head
        [
          title [ text "Counter - Static Preview" ];
          meta ~attrs:[ attr "charset" "UTF-8" ] ();
          style page_styles;
        ];
      body
        [
          counter_view_static 0;
          div
            ~attrs:[ class_ "card"; style_ "text-align: left" ]
            [
              h2 [ text "What is this?" ];
              p
                [
                  text "This is a ";
                  strong [ text "static HTML component" ];
                  text ". It's rendered server-side to a plain HTML string with ";
                  code [ text "to_html" ];
                  text ".";
                ];
              p [ text "The component tree looks like this:" ];
              pre
                ~attrs:[
                  style_ "background: #f4f4f4; padding: 16px; border-radius: 6px; overflow-x: auto";
                ]
                [
                  code
                    [
                      text
                        {|div ~attrs:[class_ "card"] [
  h1 [text "Counter"];
  div ~attrs:[class_ "count"] [
    text (Int.to_string count)
  ];
  div ~attrs:[class_ "buttons"] [
    button ~attrs:[class_ "btn"] [text "-"];
    button ~attrs:[class_ "btn"] [text "+"];
  ];
]|};
                    ];
                ];
            ];
        ];
    ]

(** Step 2: LiveView Component (With Event Handlers) *)
(* Define our message type for LiveView *)

type msg =
  | Increment
  | Decrement
  | Reset

let counter_view_interactive = fun count -> div
  ~attrs:[ class_ "card" ]
  [
    h1 [ text "Counter (LiveView)" ];
    span ~attrs:[ class_ "label" ] [ text "Interactive component with server-side handlers" ];
    div ~attrs:[ class_ "count" ] [ text (Int.to_string count) ];
    div
      ~attrs:[ class_ "buttons" ]
      [
        button
          ~attrs:[
            class_ "btn btn-secondary";
            on_click (fun _ -> Decrement);
          ]
          [ text "-" ];
        button
          ~attrs:[
            class_ "btn btn-secondary";
            on_click (fun _ -> Reset);
          ]
          [ text "Reset" ];
        button
          ~attrs:[
            class_ "btn btn-primary";
            on_click (fun _ -> Increment);
          ]
          [ text "+" ];
      ];
    div
      ~attrs:[ class_ "success" ]
      [
        strong [ text "✓ Interactive: " ];
        text "Buttons are wired to LiveView event handlers. ";
        text "Clicks are processed on the server and the UI updates automatically.";
      ];
  ]

let interactive_page count =
  html
    [
      head
        [
          title [ text "Counter - LiveView Interactive" ];
          meta ~attrs:[ attr "charset" "UTF-8" ] ();
          style page_styles;
        ];
      body
        [
          counter_view_interactive count;
          div
            ~attrs:[ class_ "card"; style_ "text-align: left" ]
            [
              h2 [ text "How does it work?" ];
              p
                [
                  text "This is the ";
                  strong [ text "same component structure" ];
                  text " as the static version, but with ";
                  code [ text "on_click" ];
                  text " event handlers added!";
                ];
              p [ text "The enhanced component looks like:" ];
              pre
                ~attrs:[
                  style_ "background: #f4f4f4; padding: 16px; border-radius: 6px; overflow-x: auto";
                ]
                [
                  code
                    [
                      text
                        {|type msg = Increment | Decrement | Reset

let counter_view count =
  div ~attrs:[class_ "card"] [
    h1 [text "Counter"];
    div ~attrs:[class_ "count"] [
      text (Int.to_string count)
    ];
    div ~attrs:[class_ "buttons"] [
      button ~attrs:[
        class_ "btn";
        on_click (fun _ -> Decrement)  (* 👈 Add handler! *)
      ] [text "-"];
      button ~attrs:[
        class_ "btn";
        on_click (fun _ -> Increment)  (* 👈 Add handler! *)
      ] [text "+"];
    ];
  ]|};
                    ];
                ];
              p
                [
                  text "In LiveView, these handlers are wired to your ";
                  code [ text "update" ];
                  text " function. In static HTML (";
                  code [ text "to_html" ];
                  text "), they're ignored.";
                ];
            ];
        ];
    ]

(** Step 3: Comparison Page *)
let comparison_page: msg t =
  html
    [
      head
        [
          title [ text "Static vs LiveView Comparison" ];
          meta ~attrs:[ attr "charset" "UTF-8" ] ();
          style page_styles;
          style
            {|
        .comparison {
          display: grid;
          grid-template-columns: 1fr 1fr;
          gap: 20px;
          max-width: 1200px;
          margin: 0 auto;
        }
        @media (max-width: 768px) {
          .comparison { grid-template-columns: 1fr; }
        }
      |};
        ];
      body
        [
          div
            ~attrs:[ class_ "card" ]
            [
              h1 [ text "Progressive Enhancement" ];
              p [ text "Start with static components, add LiveView when you need interactivity." ];
            ];
          div
            ~attrs:[ class_ "comparison" ]
            [ div [ counter_view_static 5 ]; div [ counter_view_interactive 5 ] ];
          div
            ~attrs:[ class_ "card"; style_ "text-align: left; max-width: 800px; margin: 40px auto" ]
            [
              h2 [ text "The Power of Unified Components" ];
              h3 [ text "Same Structure, Different Rendering" ];
              ul
                [
                  li
                    [
                      strong [ text "Static HTML: " ];
                      code [ text "to_html component" ];
                      text " - Event handlers ignored, clean HTML output";
                    ];
                  li
                    [
                      strong [ text "LiveView: " ];
                      text "Event handlers wired to server, automatic UI updates";
                    ];
                ];
              h3 [ text "Benefits" ];
              ul
                [
                  li [ text "Write components once" ];
                  li [ text "Preview statically during development" ];
                  li [ text "Add interactivity incrementally" ];
                  li [ text "Type-safe all the way" ];
                  li [ text "No client-side JavaScript required" ];
                ];
              h3 [ text "Migration Path" ];
              ol
                [
                  li [ text "Build your UI with components and static HTML" ];
                  li [ text "When you need interactivity, add event handlers" ];
                  li [ text "Wire to LiveView - the component structure stays the same!" ];
                ];
            ];
        ];
    ]

(** Demo Output *)
let main ~args:_ =
  println "=== STATIC HTML (No Interactivity) ===";
  println (to_html static_page);
  println "";
  println "=== LIVEVIEW READY (Events Active) ===";
  println (to_html (interactive_page 42));
  println "";
  println "=== COMPARISON PAGE ===";
  println (to_html comparison_page);
  println "";
  println "💡 The same Component.t type works for both static and interactive!";
  println "   Events are ignored in static HTML, active in LiveView.";
  Ok ()

let () = Runtime.run ~main ~args:Env.args ()
