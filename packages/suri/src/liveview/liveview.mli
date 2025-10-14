(** # LiveView - Server-rendered Components with Live Updates

    LiveView enables building interactive web applications where the UI is rendered
    on the server and updates are pushed to the client over WebSocket. Changes to
    state on the server automatically trigger DOM updates in the browser.

    This is inspired by Phoenix LiveView and provides a similar developer experience
    with OCaml's type safety.

    ## Quick Start

    Define a component with state, messages, and rendering:

    ```ocaml
    module Counter = struct
      type state = { count : int }
      type msg = Increment | Decrement | Reset

      let init _conn = { count = 0 }

      let update msg state =
        match msg with
        | Increment -> { count = state.count + 1 }
        | Decrement -> { count = state.count - 1 }
        | Reset -> { count = 0 }

      let render ~state () =
        let open Html in
        div ~id:"counter" [
          h1 [ string "Count: "; int state.count ];
          div [
            button ~on_click:(fun _ -> Increment) [ string "+" ] ();
            button ~on_click:(fun _ -> Reset) [ string "Reset" ] ();
            button ~on_click:(fun _ -> Decrement) [ string "-" ] ();
          ] ()
        ] ()
    end

    let (opts, handler) = LiveView.mount (module Counter)
    ```

    ## Architecture

    LiveView follows the Elm Architecture (Model-View-Update):

    1. **State** - Your application state (the model)
    2. **Messages** - Events that can change state (actions)
    3. **Update** - Pure function: msg -> state -> state
    4. **Render** - Pure function: state -> Html

    The LiveView runtime handles:
    - WebSocket connection management
    - Event routing from browser to server
    - State management
    - DOM diffing and patching
    - Automatic re-rendering on state changes

    ## Component Lifecycle

    1. Client connects via WebSocket
    2. `init` is called with connection info
    3. Initial HTML is rendered and sent to client
    4. User interactions trigger events
    5. Events are sent to server over WebSocket
    6. `update` is called with event message
    7. New state triggers `render`
    8. Diff is computed and sent to client
    9. Client patches DOM with changes

    ## Event Handling

    Events flow from client to server:
    - User clicks button in browser
    - JavaScript runtime sends event over WebSocket
    - Server deserializes event to your message type
    - `update` is called with the message
    - State changes trigger re-render
    - HTML diff is sent back to client
    - Client applies patch to DOM

    ## State Management

    Each LiveView connection runs in its own process with isolated state.
    This provides:
    - No shared mutable state between users
    - Easy reasoning about concurrency
    - Natural horizontal scaling
    - Process isolation (crashes don't affect other users)

    ## Integration with Web Server

    To use LiveView, you need to:
    1. Upgrade HTTP connection to WebSocket
    2. Pass the handler to Channel processing
    3. Serve the JavaScript runtime to clients

    Example with Suri.WebServer:
    ```ocaml
    let route conn =
      match conn.path with
      | "/ws/counter" ->
          let (opts, handler) = LiveView.mount (module Counter) in
          (* Upgrade connection to WebSocket with handler *)
          ...
      | _ -> ...
    ```

    ## Client-Side Setup

    Include the LiveView JavaScript runtime in your HTML:
    ```html
    <div id="counter"></div>
    <script src="/liveview/runtime.js"></script>
    <script>
      window.spawnLiveView('counter', '/ws/counter');
    </script>
    ```

    The runtime automatically:
    - Establishes WebSocket connection
    - Sends Mount message to initialize
    - Handles user interactions
    - Applies server-sent DOM patches
    - Reconnects on disconnect *)

open Std

module Html = Html
(** HTML DSL for building UI - re-exported for convenience *)

module type Component = sig
  (** Component interface - implement this to create a LiveView component.
      
      Components follow the Elm Architecture pattern:
      - State: Your application state
      - Messages: Events that modify state
      - init: Initialize state
      - update: Handle messages and update state
      - render: Convert state to HTML
      
      Example:
      ```ocaml
      module TodoList = struct
        type state = {
          todos : string list;
          input : string;
        }
        
        type msg =
          | AddTodo
          | UpdateInput of string
          | RemoveTodo of int
        
        let init _conn = {
          todos = [];
          input = "";
        }
        
        let update msg state =
          match msg with
          | AddTodo ->
              { state with
                todos = state.input :: state.todos;
                input = ""
              }
          | UpdateInput text ->
              { state with input = text }
          | RemoveTodo idx ->
              let todos = List.filteri (fun i _ -> i <> idx) state.todos in
              { state with todos }
        
        let render ~state () =
          let open Html in
          div [
            input ~on_input:(fun v -> UpdateInput v) ~value:state.input () ();
            button ~on_click:(fun _ -> AddTodo) [ string "Add" ] ();
            ul (List.mapi (fun i todo ->
              li [
                string todo;
                button ~on_click:(fun _ -> RemoveTodo i) [ string "✕" ] ()
              ] ()
            ) state.todos)
          ] ()
      end
      ```*)

  type state
  (** Component state - can be any type that represents your application state.
      
      Keep state as simple as possible. Avoid:
      - Closures or functions in state
      - External resources (file handles, sockets)
      - Circular references
      
      Good state types:
      - Records with primitive fields
      - Lists, options, results
      - Nested records
      - Discriminated unions *)

  type msg
  (** Message type - all events that can modify component state.
      
      Define as a discriminated union with all possible user actions:
      ```ocaml
      type msg =
        | ButtonClicked
        | InputChanged of string
        | ItemSelected of int
        | FormSubmitted
      ```*)

  val init : Middleware.Conn.t -> state
  (** Initialize component state when connection starts.
      
      Called once when the WebSocket connection is established.
      Use the connection to extract route params, query strings, etc.
      
      Example:
      ```ocaml
      let init conn =
        let user_id = Middleware.Conn.get_param conn "user_id" in
        { user_id; data = [] }
      ```*)

  val update : msg -> state -> state
  (** Update state in response to a message.
      
      This should be a pure function with no side effects.
      Given a message and current state, return new state.
      
      Example:
      ```ocaml
      let update msg state =
        match msg with
        | Increment -> { count = state.count + 1 }
        | Decrement -> { count = state.count - 1 }
        | Reset -> { count = 0 }
      ```
      
      The LiveView runtime will automatically re-render after update. *)

  val render : state:state -> unit -> msg Html.t
  (** Render component state to HTML.
      
      This should be a pure function that converts state to HTML.
      Event handlers in the HTML should produce values of type `msg`.
      
      Example:
      ```ocaml
      let render ~state () =
        let open Html in
        div [
          h1 [ string state.title ];
          p [ string state.description ];
          button ~on_click:(fun _ -> Save) [ string "Save" ] ()
        ] ()
      ```
      
      Tips:
      - Keep rendering logic simple
      - Extract complex rendering to helper functions
      - Use Html.list for conditional rendering
      - Use Html.map_action for nested components *)
end

type event = Mount | Event of string * string | Patch of string
(** Internal event types used by LiveView protocol.
    
    - Mount: Initial connection established
    - Event: User interaction from browser
    - Patch: HTML diff sent from server to client
    
    You typically don't need to work with these directly. *)

val serialize_event : event -> (string, string) result
(** Serialize an event to JSON string (internal use).
    
    Used by the runtime to send events over WebSocket. *)

val deserialize_event : string -> (event, string) result
(** Deserialize JSON string to event (internal use).
    
    Used by the runtime to parse incoming WebSocket messages. *)

val mount :
  (module Component with type state = 's and type msg = 'm) ->
  Channel.Handler.upgrade_opts * Channel.Handler.t
(** Mount a LiveView component as a WebSocket handler.
    
    This creates a Channel handler that manages the LiveView lifecycle:
    - Accepts WebSocket connection
    - Initializes component state
    - Routes events to update function
    - Sends HTML patches to client
    
    Use this with your web server's WebSocket upgrade:
    ```ocaml
    let (upgrade_opts, handler) = LiveView.mount (module Counter) in
    (* Pass handler to WebSocket upgrade logic *)
    ```
    
    The returned handler can be used with Suri.Channel or integrated
    with your HTTP server's WebSocket upgrade mechanism. *)
