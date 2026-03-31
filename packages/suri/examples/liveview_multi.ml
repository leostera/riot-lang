open Std
open Suri

(** Multiple LiveView Components Example
    
    Demonstrates embedding multiple independent LiveView components
    in a single page with custom layout and styling.
    
    Features:
    - Counter: User-driven interactions (click buttons)
    - Timer: Server-driven updates (ticks every second)
    - Custom page layout with side-by-side components
    
    Run with: tusk run suri:liveview_multi
    Then open: http://localhost:9998 *)

(** Counter LiveView - User-driven interactions *)
module Counter = struct
  open Component

  let id = LiveView.id "counter"

  type state = {
    count: int;
  }

  type msg =
    Increment
    | Decrement
    | Reset

  type args = unit

  let serialize_args = fun () -> Data.Json.Null

  let deserialize_args = fun _ -> Ok ()

  let init = fun _conn () -> {count = 0}

  let update = fun event state ->
    match event with
    | LiveView.App Increment -> {count = state.count + 1}
    | App Decrement -> {count = state.count - 1}
    | App Reset -> {count = 0}
    | _ -> state

  let render = fun ~state () -> div
  ~attrs:[ class_ "component-card" ]
  [
    div
    ~attrs:[ class_ "card-header" ]
    [
      h2 [ text "Counter" ];
      p ~attrs:[ class_ "card-subtitle" ] [ text "Click buttons to change the count" ];

    ];
    div
    ~attrs:[ class_ "card-body" ]
    [
      div ~attrs:[ class_ "display-value" ] [ text (Int.to_string state.count) ];
      div
      ~attrs:[ class_ "button-group" ]
      [
        button ~attrs:[ class_ "btn btn-decrement"; on_click (fun _ -> Decrement);  ] [ text "−" ];
        button ~attrs:[ class_ "btn btn-reset"; on_click (fun _ -> Reset);  ] [ text "Reset" ];
        button ~attrs:[ class_ "btn btn-increment"; on_click (fun _ -> Increment);  ] [ text "+" ];

      ];

    ];

  ]
end

(** Status LiveView - Shows current server timestamp *)
module Status = struct
  let id = LiveView.id "status"

  open LiveView
  open Component

  type state = {
    timer: Timer.id;
    updates: int;
    last_update: string;
  }

  type msg =
    Refresh

  type args = unit

  type Message.t +=
    TimerTick of msg

  let serialize_args = fun () -> Data.Json.Null

  let deserialize_args = fun _ -> Ok ()

  let init = fun _conn () ->
    let timer = Timer.send_interval
    (self ())
    ~interval:(Time.Duration.from_secs 1)
    (TimerTick Refresh) in
    {timer; updates = 0; last_update = "Not refreshed yet"; }

  let update = fun event state ->
    match event with
    | Custom (TimerTick Refresh)
    | App Refresh ->
        let timestamp = Datetime.(now () |> to_iso8601) in
        {state with updates = state.updates + 1; last_update = timestamp; }
    | _ -> state

  let render = fun ~state () -> div
  ~attrs:[ class_ "component-card" ]
  [
    div
    ~attrs:[ class_ "card-header" ]
    [
      h2 [ text "Status" ];
      p ~attrs:[ class_ "card-subtitle" ] [ text "Click to get server timestamp" ];

    ];
    div
    ~attrs:[ class_ "card-body" ]
    [
      div ~attrs:[ class_ "display-value status-display" ] [ text state.last_update;  ];
      div
      ~attrs:[ class_ "status-info" ]
      [ text ("Refreshed " ^ Int.to_string state.updates ^ " times");  ];

    ];

  ]
end

(** Page styles *)
let page_styles = {|
  * {
    box-sizing: border-box;
    margin: 0;
    padding: 0;
  }
  
  body {
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    min-height: 100vh;
    padding: 40px 20px;
  }
  
  .container {
    max-width: 1200px;
    margin: 0 auto;
  }
  
  .header {
    text-align: center;
    color: white;
    margin-bottom: 40px;
  }
  
  .header h1 {
    font-size: 3em;
    margin-bottom: 10px;
  }
  
  .header p {
    font-size: 1.2em;
    opacity: 0.9;
  }
  
  .components-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(400px, 1fr));
    gap: 30px;
    margin-bottom: 40px;
  }
  
  .component-card {
    background: white;
    border-radius: 16px;
    box-shadow: 0 20px 60px rgba(0, 0, 0, 0.3);
    overflow: hidden;
  }
  
  .card-header {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    padding: 30px;
    text-align: center;
  }
  
  .card-header h2 {
    font-size: 2em;
    margin-bottom: 8px;
  }
  
  .card-subtitle {
    font-size: 0.9em;
    opacity: 0.9;
  }
  
  .card-body {
    padding: 40px;
  }
  
  .display-value {
    font-size: 5em;
    font-weight: bold;
    text-align: center;
    color: #333;
    margin-bottom: 30px;
    font-variant-numeric: tabular-nums;
  }
  
  .status-display {
    color: #667eea;
    font-size: 3em;
  }
  
  .button-group {
    display: flex;
    gap: 12px;
    justify-content: center;
  }
  
  .btn {
    font-size: 1.5em;
    font-weight: bold;
    padding: 16px 24px;
    border: none;
    border-radius: 8px;
    cursor: pointer;
    transition: all 0.2s ease;
    min-width: 80px;
  }
  
  .btn:hover {
    transform: translateY(-2px);
    box-shadow: 0 4px 12px rgba(0, 0, 0, 0.2);
  }
  
  .btn:active {
    transform: translateY(0);
  }
  
  .btn-decrement {
    background: #dc3545;
    color: white;
  }
  
  .btn-decrement:hover {
    background: #c82333;
  }
  
  .btn-reset {
    background: #6c757d;
    color: white;
  }
  
  .btn-reset:hover {
    background: #5a6268;
  }
  
  .btn-increment {
    background: #28a745;
    color: white;
  }
  
  .btn-increment:hover {
    background: #218838;
  }
  
  .status-info {
    text-align: center;
    color: #666;
    font-size: 1.1em;
    margin-top: 10px;
  }
  
  .btn-refresh {
    background: #667eea;
    color: white;
  }
  
  .btn-refresh:hover {
    background: #5568d3;
  }
  
  .footer {
    background: white;
    border-radius: 12px;
    padding: 30px;
    text-align: center;
    color: #666;
  }
  
  .footer h3 {
    color: #333;
    margin-bottom: 15px;
  }
  
  .footer p {
    line-height: 1.6;
    margin-bottom: 10px;
  }
  
  .footer strong {
    color: #667eea;
  }
|}

(** Home page handler with both LiveViews embedded *)
let home_page = fun conn _req ->
  let open Component in
    let page = html
    [
      head
      [
        meta ~attrs:[ attr "charset" "UTF-8" ] ();
        meta ~attrs:[ attr "viewport" "width=device-width, initial-scale=1.0" ] ();
        title [ text "Multiple LiveViews - Suri" ];
        LiveView.client_script;
        style page_styles;

      ];
      body
      [
        div
        ~attrs:[ class_ "container" ]
        [
          div
          ~attrs:[ class_ "header" ]
          [
            h1 [ text "Multiple LiveView Components" ];
            p [ text "Each component is independent with its own state and WebSocket connection" ];

          ];
          div
          ~attrs:[ class_ "components-grid" ]
          [ (LiveView.embed (module Counter) ()); (LiveView.embed (module Status) ());  ];
          div
          ~attrs:[ class_ "footer" ]
          [
            h3 [ text "How It Works" ];
            p
            [
              strong [ text "Counter: " ];
              text "User-driven interactions. Click the buttons to update the count. ";
              text "Events are sent to the server, which updates state and sends back HTML patches.";

            ];
            p
            [
              strong [ text "Status: " ];
              text "Server timestamp rendering. Click the button to get the current server time. ";
              text "The timestamp is generated on the server and sent back to the client.";

            ];
            p
            [
              strong [ text "Both components: " ];
              text "Have independent state, separate WebSocket connections, and update without affecting each other!";

            ];

          ];

        ];

      ];

    ] in
    conn
    |> Middleware.Conn.with_status Net.Http.Status.Ok
    |> Middleware.Conn.with_header "Content-Type" "text/html; charset=utf-8"
    |> Middleware.Conn.with_body (Component.to_html page)
    |> Middleware.Conn.send

(* Define routes *)

let routes = Middleware.Router.[get "/" home_page;
LiveView.live (module Counter);
LiveView.live (module Status);]

(* App is just a list of middleware! *)

let app = [ Middleware.router routes;  ]

let () =
  Miniriot.run ~args:Env.args ()
    ~main:(fun ~args:_ ->
      let config = Suri.config ~port:9_998 () in
      match Suri.start_link ~config app with
      | Ok supervisor ->
          Log.info "╔═══════════════════════════════════════════════════╗";
          Log.info "║  Multiple LiveViews running!                     ║";
          Log.info "║  http://localhost:9998                           ║";
          Log.info "║                                                   ║";
          Log.info "║  Counter: User interactions                      ║";
          Log.info "║  Status: Server-side timestamp rendering         ║";
          Log.info "╚═══════════════════════════════════════════════════╝";
          let count = Supervisor.Dynamic.count_children supervisor in
          Log.info ("Started with " ^ Int.to_string count.active ^ " acceptors");
          let rec loop = fun () ->
            sleep (Time.Duration.from_secs 100);
            loop ()
          in
          loop ()
      | Error `Bind_error ->
          Log.error "Failed to bind to port 9998";
          Error (Failure "Failed to start server"))
