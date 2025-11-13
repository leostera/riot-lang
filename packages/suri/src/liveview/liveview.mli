open Std

(** {1 LiveView - Server-Rendered Components with Live Updates}

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
    - Automatic re-rendering on state changes *)

(** {1 Component Interface} *)

module type Component = sig
  type state
  (** Application state *)
  
  type msg
  (** Messages that can change state *)
  
  val init : Middleware.Conn.t -> state
  (** Initialize state when client connects *)
  
  val update : msg -> state -> state
  (** Update state based on message (pure function) *)
  
  val render : state:state -> unit -> msg Component.t
  (** Render state to Component tree (pure function) *)
end

(** {1 Mounting LiveViews} *)

val mount : (module Component with type state = 's and type msg = 'm) -> 
            Middleware.Conn.t ->
            Channel.Handler.upgrade_opts * Channel.Handler.t
(** Create a LiveView Channel.Handler.
    
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
    ]} *)

val live : 
  string -> 
  (module Component with type state = 's and type msg = 'm) ->
  Web_server.Handler.t
(** Create a LiveView handler that serves HTML or upgrades to WebSocket.
    
    This function creates a handler that:
    - On regular HTTP GET: Returns HTML page with LiveView JavaScript
    - On WebSocket upgrade: Mounts the LiveView component
    
    Example:
    {[
      let handler = LiveView.live "/counter" (module Counter)
      
      (* Or combine multiple LiveView handlers *)
      let handler socket_conn req =
        let path = Web_server.Request.uri req in
        match path with
        | "/counter" -> LiveView.live "/counter" (module Counter) socket_conn req
        | "/chat" -> LiveView.live "/chat" (module Chat) socket_conn req
        | _ -> Web_server.Handler.close (Web_server.Response.not_found ())
    ]} *)

(** {1 JavaScript Runtime} *)

val javascript_runtime : string
(** Get the LiveView JavaScript runtime code.
    
    This is embedded in the HTML template, but can also be served separately:
    {[
      get "/assets/liveview.js" (fun _conn _req ->
        Response.ok
          ~headers:[("Content-Type", "application/javascript")]
          ~body:LiveView.javascript_runtime
          ())
    ]} *)

val html_template : element_id:string -> ws_path:string -> 'msg Component.t -> string
(** Generate HTML template with LiveView bootstrapping.
    
    @param element_id DOM element ID to mount LiveView
    @param ws_path WebSocket path for LiveView connection
    @param initial_content Initial content to show while connecting
    
    Example:
    {[
      let page = LiveView.html_template
        ~element_id:"app"
        ~ws_path:"/live/counter"
        Component.(div [text "Loading..."])
      in
      Response.ok ~body:page ()
    ]} *)
