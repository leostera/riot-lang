open Std

module Protocol: module type of Protocol

module Session: module type of Session

(**
   {1 LiveView - Server-Rendered Components with Live Updates}

   LiveView enables building interactive web applications where the UI is rendered
   on the server and updates are pushed to the client over WebSocket.

   {2 Quick Start}

   {[
     module Counter = struct
       open Suri.Component

       type state = { count: int }
       type msg = Increment | Decrement

       let init _conn = { count = 0 }

       let update msg state =
         match msg with
         | Increment -> { count = state.count + 1 }
         | Decrement -> { count = state.count - 1 }

       let render ~state () =
         div ~attrs:[class_ "counter"] [
           h1 [text (Int.to_string state.count)];
           button ~attrs:[on_click (fun _ -> Decrement)] [text "-"];
           button ~attrs:[on_click (fun _ -> Increment)] [text "+"];
         ]
     end

     (* Create handler and serve HTML *)
     let routes = [
       LiveView.route "/counter" (module Counter);
     ]
   ]}

   {2 Architecture}

   LiveView follows the Elm Architecture:
   - {b State} - Your application state (immutable)
   - {b Messages} - Events that trigger state changes
   - {b Update} - Pure function: [msg -> state -> state]
   - {b Render} - Pure function: [state -> Component.t]

   The runtime handles:
   - WebSocket connection management (via Channel.Handler)
   - Event routing from browser to server
   - State management per connection (in dedicated process)
   - Automatic re-rendering on state changes
*)

(** {1 Component Interface} *)

val id: string -> string

(**
   Generate a unique LiveView component ID.

   Takes a base name and appends a UUID v7 suffix to ensure uniqueness.
   Each call returns a different ID, preventing conflicts when multiple
   LiveView components are embedded in the same page.

   Example:
   {[
     module Counter = struct
       let id = LiveView.id "counter"  (* Results in: "counter-<uuid>" *)
       (* ... *)
     end
   ]}
*)
type 'msg event =
  | Custom of Message.t
  (** Any Runtime message from other processes *)
  | App of 'msg

(** Component-specific messages from UI events *)
type Channel.Handler.initialization_error +=
  | MissingSessionToken
  | InvalidSessionToken of Session.decode_error
  | InvalidSessionArgs of Data.Json.t
  | MissingSessionArgs of Data.Json.t

val initialization_error_to_string: Channel.Handler.initialization_error -> string

(**
   Event wrapper for component messages.

   - [App msg] wraps UI events (clicks, form submissions, etc.)
   - [Custom msg] wraps any Runtime process message (timers, notifications, etc.)

   This allows components to handle both user interactions and server-driven updates.
*)
module type Component = sig
  val id: string

  (**
     Unique identifier for this LiveView component.
     Use [LiveView.id "name"] to generate a unique ID.
     This is used to create both the WebSocket endpoint path and DOM element ID.
  *)
  type state
  (** Application state *)
  type msg
  (** Messages that can change state *)
  type args

  (** Initialization arguments passed from HTTP handler to WebSocket mount *)
  val serialize_args: args -> Data.Json.t

  (** Serialize args to JSON for embedding in session token *)
  val deserialize_args: Data.Json.t -> (args, Data.Json.t) result

  (** Deserialize args from JSON when mounting component *)
  val init: Middleware.Conn.t -> args -> state

  (** Initialize state when client connects with initialization arguments *)
  val update: msg event -> state -> state

  (**
     Update state based on event (pure function).

     Handle both UI events ([App msg]) and process messages ([Custom msg]):
     {[
       let update event state =
         match event with
         | App Increment -> { state with count = state.count + 1 }
         | Custom (Timer Tick) -> { state with time = current_time () }
         | _ -> state
     ]}
  *)
  val render: state:state -> unit -> msg Component.t

  (** Render state to Component tree (pure function) *)
end

(** {1 LiveView JavaScript Runtime} *)

val serve_runtime: ?prefix:string -> unit -> Middleware.Pipeline.middleware

(**
   Middleware to serve the LiveView JavaScript runtime.

   This automatically serves the LiveView JavaScript at the specified path.
   Add it once to your middleware pipeline.

   @param prefix The path to serve the runtime from (default: "/assets/liveview.js")

   Example:
   {[
     let app = [
       LiveView.serve_runtime ();  (* Serves at /assets/liveview.js *)
       (* or *)
       LiveView.serve_runtime ~prefix:"/suri/live.js" ();
       Middleware.router routes;
     ]
   ]}
*)
(** {1 Mounting LiveViews} *)

val mount:
  (module Component with type state = 's and type msg = 'm) ->
  Middleware.Conn.t ->
  Channel.Handler.upgrade_opts * Channel.Handler.t

(**
   Create a LiveView Channel.Handler.

   Returns a tuple of (upgrade_opts, handler) that can be used with
   WebSocket upgrade mechanisms.

   The handler:
   1. Spawns a component process to manage state
   2. Handles WebSocket frames (Mount, Event messages)
   3. Sends patches back to the client

   Example:
   {[
     let (opts, handler) = LiveView.mount (module Counter) conn in
     (* Use with WebSocket upgrade *)
   ]}
*)
val embed: (module Component with type args = 'args) -> 'args -> 'msg Component.t

(**
   Embed a LiveView component into a page with signed session token.

   The secret is automatically retrieved from Suri.Config via Std.Config.

   @param module Component module
   @param args The initialization arguments to pass to the component

   Returns the HTML for the LiveView mounting div with embedded session token and bootstrap script.
   The component will connect to the WebSocket endpoint defined by its [id] field.

   This allows you to:
   - Control the full page layout and styles
   - Embed multiple LiveViews in one page (each returns a string)
   - Use custom HTML templates

   Example (single component):
   {[
     module Counter = struct
       let id = LiveView.id "counter"
       (* ... *)
     end

     let home_page conn =
       let page = Component.html [
         head [
           LiveView.client_script;
           style my_custom_styles;
         ];
         body [
           h1 [text "My Dashboard"];
           Component.text (LiveView.embed (module Counter) args);
         ];
       ] in
       conn
       |> Conn.respond ~status:Ok ~body:(Component.to_html page)
       |> Conn.send
   ]}

   Example (multiple components):
   {[
     let home_page conn =
       let page = Component.html [
         head [LiveView.client_script; style styles];
         body [
           Component.text (LiveView.embed (module Counter) counter_args);
           Component.text (LiveView.embed (module Timer) timer_args);
         ];
       ] in
       (* ... *)
   ]}
*)
val live: (module Component with type state = 's and type msg = 'm) -> Middleware.Router.route

(**
   Create a LiveView route.

   Reads the path from the module's [path] field and automatically prefixes it
   with "/suri/live/" to create the WebSocket endpoint.

   This creates a route that handles both:
   - Regular HTTP GET: Returns minimal HTML for WebSocket upgrade
   - WebSocket upgrade: Mounts the LiveView component

   Use it directly in your router alongside other routes.

   Example:
   {[
     module Counter = struct
       let path = "/counter"
       type state = { count: int }
       type msg = Increment | Decrement
       (* ... *)
     end

     let routes = Middleware.Router.[
       get "/" home_handler;
       LiveView.live (module Counter);  (* Creates route at /suri/live/counter *)
     ]

     let app = [
       LiveView.serve_runtime ();
       Middleware.router routes;
     ]

     Suri.start_link app
   ]}
*)
(** {1 JavaScript Runtime} *)

val javascript_runtime: string

(**
   Get the LiveView JavaScript runtime code as a string.

   You typically don't need this directly - use [client_script] instead.
   This can be used if you want to serve the runtime separately:
   {[
     get "/assets/liveview.js" (fun _conn _req ->
       Response.ok
         ~headers:[("Content-Type", "application/javascript")]
         ~body:LiveView.javascript_runtime
         ())
   ]}
*)
val client_script: 'msg Component.t

(**
   Script element containing the LiveView JavaScript runtime.

   Include this once in your page's <head> section to load the LiveView client.

   Example:
   {[
     let page = Component.html [
       head [
         LiveView.client_script;  (* Include LiveView JS *)
         style my_styles;
       ];
       body [
         LiveView.embed (module Counter) conn;
       ];
     ]
   ]}
*)
val html_template:
  element_id:string ->
  ws_path:string ->
  ?title:string ->
  ?styles:string ->
  'msg Component.t ->
  string

(**
   Generate HTML template with LiveView bootstrapping.

   @param element_id DOM element ID to mount LiveView
   @param ws_path WebSocket path for LiveView connection
   @param title Optional page title (default: "LiveView App")
   @param styles Optional CSS styles to include in page
   @param initial_content Initial content to show while connecting

   Example:
   {[
     let page = LiveView.html_template
       ~element_id:"app"
       ~ws_path:"/live/counter"
       ~title:"My Counter"
       ~styles:"body { background: blue; }"
       Component.(div [text "Loading..."])
     in
     Response.ok ~body:page ()
   ]}
*)
