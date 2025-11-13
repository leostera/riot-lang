open Std

module Protocol = Protocol
module HandlerRegistry = Handler_registry

(** Generate a unique LiveView component ID *)
let id base_name =
  let uuid = Std.UUID.v7 () in
  let uuid_hex = Std.UUID.to_string_nodash uuid in
  base_name ^ "-" ^ uuid_hex

(** Component interface *)
module type Component = sig
  val id : string
  
  type state
  type msg
  
  val init : Middleware.Conn.t -> state
  val update : msg -> state -> state
  val render : state:state -> unit -> msg Component.t
end

(** Message type for sending patches from component to handler *)
type Message.t += RenderPatch of string

(** Component process messages *)
type Message.t += 
  | ComponentMount
  | ComponentEvent of { handler_id : string; event_data : string }

(** Attach handler IDs to component tree and register handlers *)
let rec attach_handler_ids registry (component : 'msg Component.t) : 'msg Component.t =
  match component with
  | Component.Text _ -> component
  | Component.Fragment children ->
      Component.Fragment (List.map (attach_handler_ids registry) children)
  | Component.El { tag; attrs; children } ->
      (* Process attributes and extract event handlers *)
      let (regular_attrs, event_handlers) = 
        List.partition (fun attr ->
          match attr with
          | Component.Attr _ -> true
          | Component.Event _ -> false
        ) attrs
      in
      
      (* Register event handlers and create data attributes *)
      let handler_attrs = List.map (fun attr ->
        match attr with
        | Component.Event (event_name, handler) ->
            let handler_id = HandlerRegistry.register registry handler in
            [
              Component.Attr ("data-lv-handler", handler_id);
              Component.Attr ("data-lv-event", event_name);
            ]
        | _ -> []
      ) event_handlers |> List.flatten in
      
      (* Process children recursively *)
      let new_children = List.map (attach_handler_ids registry) children in
      
      Component.El {
        tag;
        attrs = regular_attrs @ handler_attrs;
        children = new_children;
      }

(** Render component with handler IDs attached *)
let render_with_handlers registry component =
  let component_with_ids = attach_handler_ids registry component in
  Component.to_html component_with_ids

(** Component process - manages state and rendering *)
module ComponentProcess = struct
  type ('state, 'msg) t = {
    mutable state : 'state;
    update : 'msg -> 'state -> 'state;
    render : state:'state -> unit -> 'msg Component.t;
    registry : 'msg HandlerRegistry.t;
    handler_pid : Pid.t;
  }
  
  let rec loop (t : ('state, 'msg) t) =
    match receive_any () with
    | ComponentMount ->
        Log.info "Component mounted";
        (* Clear registry and render *)
        HandlerRegistry.clear t.registry;
        let component = t.render ~state:t.state () in
        let html = render_with_handlers t.registry component in
        let patch = Protocol.serialize_server_msg (Protocol.Patch html) in
        send t.handler_pid (RenderPatch patch);
        loop t
    
    | ComponentEvent { handler_id; event_data } ->
        Log.debug ("Component event: " ^ handler_id ^ " -> " ^ event_data);
        (match HandlerRegistry.find t.registry handler_id with
         | None ->
             Log.warn ("Unknown handler: " ^ handler_id);
             loop t
         | Some handler ->
             (* Process event *)
             let msg = handler event_data in
             let new_state = t.update msg t.state in
             t.state <- new_state;
             
             (* Re-render *)
             HandlerRegistry.clear t.registry;
             let component = t.render ~state:t.state () in
             let html = render_with_handlers t.registry component in
             let patch = Protocol.serialize_server_msg (Protocol.Patch html) in
             send t.handler_pid (RenderPatch patch);
             loop t)
    
    | _ ->
        loop t
  
  let start_link handler_pid (type s m)
      (module C : Component with type state = s and type msg = m) conn =
    spawn_link (fun () ->
      Log.info "Component process started";
      loop {
        state = C.init conn;
        update = C.update;
        render = C.render;
        registry = HandlerRegistry.create ();
        handler_pid;
      };
      Ok ())
end

(** Create a Channel.Handler for a LiveView component *)
module MountHandler (C : Component) = struct
  include Channel.Handler.Default
  
  type args = Middleware.Conn.t
  type state = { component : Pid.t }
  
  let init conn =
    let this = self () in
    let component = ComponentProcess.start_link this (module C) conn in
    `ok { component }
  
  let handle_frame (frame : Http.Ws.Frame.t) _conn state =
    match frame.opcode with
    | Http.Ws.Frame.Text -> (
        match Protocol.deserialize_client_msg frame.payload with
        | Ok Protocol.Mount ->
            Log.info "Received Mount event";
            send state.component ComponentMount;
            `ok state
        | Ok Protocol.Event { handler_id; event_data } ->
            Log.debug ("Received Event: " ^ handler_id);
            send state.component (ComponentEvent { handler_id; event_data });
            `ok state
        | Error err ->
            Log.error ("Failed to deserialize: " ^ err);
            `ok state)
    | Http.Ws.Frame.Ping ->
        `push ([ Http.Ws.Frame.pong () ], state)
    | _ ->
        `ok state
  
  let handle_message msg state =
    match msg with
    | RenderPatch patch ->
        Log.debug ("Sending patch to client (" ^ string_of_int (String.length patch) ^ " bytes)");
        let frame = Http.Ws.Frame.text patch in
        `push ([ frame ], state)
    | _ ->
        `ok state
end

(** JavaScript runtime string - included inline *)
let javascript_runtime = 
  (* Include morphdom library for efficient DOM patching *)
  Morphdom.javascript ^ {|

class LiveView {
  constructor(elementId, wsPath) {
    this.elementId = elementId;
    this.wsPath = wsPath;
    this.element = null;
    this.socket = null;
    this.handlers = new Map();
    this.connected = false;
  }
  
  connect() {
    this.element = document.getElementById(this.elementId);
    if (!this.element) {
      console.error(`Element #${this.elementId} not found`);
      return;
    }
    
    const protocol = window.location.protocol.replace('http', 'ws');
    const url = `${protocol}//${window.location.host}${this.wsPath}`;
    
    this.socket = new WebSocket(url);
    
    this.socket.addEventListener('open', () => {
      this.connected = true;
      console.log('[LiveView] Connected');
      this.mount();
    });
    
    this.socket.addEventListener('message', (event) => {
      this.handleMessage(event.data);
    });
    
    this.socket.addEventListener('close', () => {
      this.connected = false;
      console.log('[LiveView] Disconnected, reconnecting...');
      setTimeout(() => this.connect(), 1000);
    });
    
    this.socket.addEventListener('error', (error) => {
      console.error('[LiveView] Error:', error);
    });
  }
  
  mount() {
    this.send('"Mount"');
  }
  
  handleMessage(data) {
    try {
      const msg = JSON.parse(data);
      
      if (msg.Patch) {
        this.patch(msg.Patch);
      } else if (msg.Error) {
        console.error('[LiveView] Server error:', msg.Error);
      }
    } catch (error) {
      console.error('[LiveView] Failed to handle message:', error);
    }
  }
  
  patch(html) {
    // Use morphdom for efficient DOM patching
    // Parse HTML string into a temporary container
    const template = document.createElement('div');
    template.innerHTML = html;
    const newContent = template.firstElementChild;
    
    if (!newContent) {
      console.error('[LiveView] Failed to parse patch HTML');
      return;
    }
    
    // Get current content element (or use element itself if empty)
    const currentContent = this.element.firstElementChild;
    
    if (!currentContent) {
      // First render - just append
      this.element.appendChild(newContent);
    } else {
      // Use morphdom to efficiently patch the DOM
      morphdom(currentContent, newContent, {
        onBeforeElUpdated: (fromEl, toEl) => {
          // Allow all updates - morphdom will preserve state automatically
          return true;
        }
      });
    }
    
    // Rebind event handlers (only new ones will be bound)
    this.rebindEventHandlers();
  }
  
  rebindEventHandlers() {
    // Find all elements with data-lv-handler
    const elements = this.element.querySelectorAll('[data-lv-handler]');
    const foundHandlers = new Set();
    
    elements.forEach(el => {
      const handlerId = el.getAttribute('data-lv-handler');
      const eventName = el.getAttribute('data-lv-event');
      
      if (!handlerId || !eventName) return;
      
      foundHandlers.add(handlerId);
      
      // Check if this handler is already bound to this element
      const existing = this.handlers.get(handlerId);
      if (existing && existing.element === el) {
        // Handler already bound to this exact element, skip
        return;
      }
      
      // Clean up old binding if it exists
      if (existing) {
        existing.element.removeEventListener(existing.eventName, existing.listener);
      }
      
      // Bind new handler
      const listener = (e) => {
        e.preventDefault();
        this.handleEvent(handlerId, e);
      };
      
      el.addEventListener(eventName, listener);
      this.handlers.set(handlerId, { element: el, eventName, listener });
    });
    
    // Clean up handlers for elements that no longer exist
    for (const [handlerId, { element, eventName, listener }] of this.handlers.entries()) {
      if (!foundHandlers.has(handlerId)) {
        element.removeEventListener(eventName, listener);
        this.handlers.delete(handlerId);
      }
    }
    
    console.log(`[LiveView] Bound ${this.handlers.size} handlers`);
  }
  
  handleEvent(handlerId, event) {
    const eventData = this.serializeEvent(event);
    
    const msg = JSON.stringify({
      Event: [handlerId, eventData]
    });
    
    this.send(msg);
  }
  
  serializeEvent(event) {
    // Extract useful event data
    const data = {
      type: event.type,
    };
    
    if (event.target) {
      if (event.target.value !== undefined) {
        data.value = event.target.value;
      }
      if (event.target.checked !== undefined) {
        data.checked = event.target.checked;
      }
    }
    
    return JSON.stringify(data);
  }
  
  send(data) {
    if (this.connected && this.socket.readyState === WebSocket.OPEN) {
      this.socket.send(data);
    } else {
      console.warn('[LiveView] Not connected, queuing message');
    }
  }
  
  disconnect() {
    if (this.socket) {
      this.socket.close();
    }
  }
}

// Export for use in browser
window.LiveView = LiveView;
|}

(** Client script as a Component element *)
let client_script : 'msg Component.t =
  let open Component in
  script ~attrs:[attr "type" "text/javascript"] [text javascript_runtime]

(** Generate HTML template with LiveView bootstrap *)
let html_template ~element_id ~ws_path ?title ?styles initial_content =
  let title_text = Option.unwrap_or title ~default:"LiveView App" in
  let open Component in
  let head_elements = [
    meta ~attrs:[attr "charset" "UTF-8"] ();
    meta ~attrs:[attr "viewport" "width=device-width, initial-scale=1.0"] ();
    title_ [text title_text];
    script ~attrs:[attr "type" "text/javascript"] [text javascript_runtime];
  ] in
  let head_elements = match styles with
    | Some css -> head_elements @ [style_ [text css]]
    | None -> head_elements
  in
  let page = html [
    head head_elements;
    body [
      div ~attrs:[id element_id] [initial_content];
      script [text (
        "const lv = new LiveView('" ^ element_id ^ "', '" ^ ws_path ^ "');\n" ^
        "lv.connect();"
      )];
    ];
  ] in
  Component.to_html page

(** Serve the LiveView JavaScript runtime as middleware.
    
    This middleware automatically serves the LiveView JavaScript runtime
    at the specified path. Add it to your middleware pipeline once.
    
    Example:
    {[
      let app = [
        LiveView.serve_runtime ();  (* Serves at /assets/liveview.js *)
        Middleware.router routes;
      ]
    ]} *)
let serve_runtime ?(prefix = "/assets/liveview.js") () : Middleware.Pipeline.middleware =
  fun conn ->
    let req_path = Middleware.Conn.uri conn in
    if req_path = prefix then
      conn
      |> Middleware.Conn.with_status Net.Http.Status.Ok
      |> Middleware.Conn.with_header "Content-Type" "application/javascript; charset=utf-8"
      |> Middleware.Conn.with_body javascript_runtime
      |> Middleware.Conn.send
    else
      conn  (* Pass through to next middleware *)

(** Create a LiveView mount handler *)
let mount (type s m) 
    (module C : Component with type state = s and type msg = m)
    (conn : Middleware.Conn.t) =
  let module M = MountHandler (C) in
  let opts = Channel.Handler.{ do_upgrade = true } in
  (opts, Channel.Handler.make (module M) conn)

(** Embed a LiveView component into a page.
    
    Creates a mounting div with LiveView JavaScript bootstrap code.
    The component will connect to its WebSocket endpoint at /suri/live/<id>. *)
let embed (type s m)
    (module C : Component with type state = s and type msg = m)
    (_conn : Middleware.Conn.t) : m Component.t =
  let open Component in
  let element_id = "liveview-" ^ C.id in
  let ws_path = "/suri/live/" ^ C.id in
  (* JavaScript variable names can't contain hyphens, replace with underscores *)
  let js_var_name = "lv_" ^ String.map (fun c -> if c = '-' then '_' else c) element_id in
  
  Fragment [
    div ~attrs:[id element_id] [];
    script [text (
      "const " ^ js_var_name ^ " = new LiveView('" ^ element_id ^ "', '" ^ ws_path ^ "');\n" ^
      js_var_name ^ ".connect();"
    )];
  ]

(** Create a LiveView route.
    
    Reads the unique ID from the module's [id] field and creates the WebSocket endpoint
    at "/suri/live/<id>".
    
    This creates a route that handles WebSocket upgrades only.
    The initial HTML page should be served separately using [embed].
    
    Example:
    {[
      module Counter = struct
        let id = LiveView.id "counter"
        (* ... *)
      end
      
      let routes = Middleware.Router.[
        get "/" home_handler;
        LiveView.live (module Counter);  (* Creates route at /suri/live/counter-<uuid> *)
      ]
    ]} *)
let live (type s m) 
  (module C : Component with type state = s and type msg = m) : Middleware.Router.route =
  (* Read id from module and create WebSocket path *)
  let ws_path = "/suri/live/" ^ C.id in
  
  let handler conn =
    (* This route only handles WebSocket upgrades *)
    let headers = Middleware.Conn.headers conn in
    let is_websocket_upgrade =
      match (
        Net.Http.Header.get headers "upgrade",
        Net.Http.Header.get headers "connection"
      ) with
      | Some upgrade, Some conn_header
        when String.lowercase_ascii upgrade = "websocket"
        && (String.lowercase_ascii conn_header = "upgrade" 
           || String.contains (String.lowercase_ascii conn_header) 'u') ->
          true
      | _ -> false
    in
    
    if is_websocket_upgrade then begin
      Log.info ("LiveView: Mounting component at " ^ ws_path);
      let (opts, handler) = mount (module C) conn in
      Middleware.Conn.upgrade_websocket opts handler conn
    end else begin
      (* Not a WebSocket upgrade - return error *)
      conn
      |> Middleware.Conn.with_status Net.Http.Status.BadRequest
      |> Middleware.Conn.with_body "This endpoint only accepts WebSocket connections"
      |> Middleware.Conn.send
    end
  in
  Middleware.Router.any ws_path handler

