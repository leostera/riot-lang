(** # Channel - WebSocket Communication Layer

    Channel provides WebSocket frame handling and bidirectional real-time
    communication between server and clients. It's the foundation for building
    real-time features like LiveView, chat, notifications, and collaborative editing.

    ## Quick Start

    Define a WebSocket handler:

    ```ocaml
    module EchoHandler = struct
      type args = unit
      type state = unit

      let init () = `ok ()

      let handle_frame frame _conn state =
        match frame with
        | { Http.Ws.Frame.opcode = Text; payload; _ } ->
            let response = Http.Ws.Frame.text payload in
            `push ([response], state)
        | { opcode = Ping; _ } ->
            `push ([Http.Ws.Frame.pong ()], state)
        | _ -> `ok state

      let handle_message _msg state = `ok state
    end

    let handler = Channel.Handler.make (module EchoHandler) ()
    ```

    ## Architecture

    Channel uses an actor-based architecture where each WebSocket connection
    runs in its own process. The Handler interface defines how to process:
    - Incoming WebSocket frames from the client
    - Messages from other processes in the system

    This design allows:
    - Broadcasting messages to multiple connections
    - Isolation between connections (one crash doesn't affect others)
    - Supervision and monitoring of connections
    - Easy horizontal scaling

    ## Frame Types

    Channel uses `Http.Ws.Frame.t` for WebSocket frames following RFC 6455:
    - **Text** - UTF-8 text messages
    - **Binary** - Raw binary data
    - **Ping/Pong** - Keep-alive heartbeat
    - **Close** - Graceful shutdown
    - **Continuation** - Fragmented message continuation *)

open Std

type topic = string

(** Topic identifier for pub/sub channels *)
module Handler: sig
  (** WebSocket handler interface.
      
      Handlers define the behavior of WebSocket connections by implementing
      three key functions:
      - init: Initialize handler state when connection starts
      - handle_frame: Process incoming WebSocket frames
      - handle_message: Process messages from other processes
      
      This design enables building stateful, message-driven WebSocket applications. *)
  type upgrade_opts = {
    do_upgrade: bool;
  }
  (** Options for WebSocket protocol upgrade *)
  type ('state, 'error) handle_result =
  [
    `push of Http.Ws.Frame.t list * 'state
    (** Send frames to client and update state *)
    | `ok of 'state
    (** Continue with updated state *)
    | `error of 'state * 'error
  ]

  (** Handle error with state *)

  (** Result of handling a frame or message *)
  module type Intf = sig
    (** Handler module interface.
        
        Implement this interface to define custom WebSocket behavior.
        
        Example:
        ```ocaml
        module ChatHandler = struct
          type args = { room : string }
          type state = { room : string; user : string option }

          let init args = `ok { room = args.room; user = None }

          let handle_frame frame _conn state =
            match frame with
            | { Http.Ws.Frame.opcode = Text; payload; _ } ->
                broadcast state.room payload;
                `ok state
            | _ -> `ok state

          let handle_message msg state =
            match msg with
            | BroadcastMsg text ->
                let frame = Http.Ws.Frame.text text in
                `push ([frame], state)
            | _ -> `ok state
        end
        ```*)
    type state
    (** Handler state - maintains connection state across frames *)
    type args

    (** Initialization arguments passed when creating the handler *)
    val init: args -> (state, [>
        `Unknown_opcode of int
      ]) handle_result

    (** Initialize the handler when connection starts.
        
        Called once when the WebSocket connection is established.
        Return `ok with initial state or `error if initialization fails. *)
    val handle_frame: Http.Ws.Frame.t -> Net.TcpStream.t -> state -> (state, [>
        `Unknown_opcode of int
      ]) handle_result

    (** Handle incoming WebSocket frame from client.
        
        Called for each frame received from the client.
        Can return:
        - `ok state to continue with updated state
        - `push (frames, state) to send frames and update state
        - `error (state, err) if frame processing fails *)
    val handle_message: Miniriot.Message.t -> state -> (state, [>
        `Unknown_opcode of int
      ]) handle_result

    (** Handle messages from other processes.
        
        Called when another process sends a message to this handler.
        Useful for broadcasting, notifications, or coordinating between handlers. *)
  end

  type t

  (** Opaque handler type wrapping a handler module and its state *)
  val make: (module Intf with type args = 'a and type state = 'b) -> 'a -> t

  (** Create a handler from a module and initialization arguments.
      
      Example:
      ```ocaml
      let handler = Handler.make (module EchoHandler) ()
      ```*)
  val init: t -> Net.TcpStream.t -> [>
      `continue of Net.TcpStream.t * t
      | `error of Net.TcpStream.t * [>
        `Unknown_opcode of int
      ]
    ]

  (** Initialize the handler with a TCP stream *)
  val handle_frame: t -> Http.Ws.Frame.t -> Net.TcpStream.t -> [>
      `continue of Net.TcpStream.t * t
      | `error of Net.TcpStream.t * [>
        `Unknown_opcode of int
      ]
      | `push of Http.Ws.Frame.t list * t
    ]

  (** Handle an incoming frame *)
  val handle_message: t -> Miniriot.Message.t -> 'a -> [>
      `continue of 'a * t
      | `error of 'a * [>
        `Unknown_opcode of int
      ]
      | `push of Http.Ws.Frame.t list * t
    ]

  (** Handle a message from another process *)
  module Default: sig
    (** Default handler implementations.
        
        Provides sensible defaults for common scenarios:
        - Responds to Ping with Pong
        - Ignores other frames
        - Ignores messages
        
        Use as a starting point for custom handlers:
        ```ocaml
        module MyHandler = struct
          include Channel.Handler.Default
          
          type args = unit
          type state = int
          
          let init () = `ok 0
          
          let handle_frame frame conn state =
            match frame with
            | { Http.Ws.Frame.opcode = Text; payload; _ } ->
                let response = Http.Ws.Frame.text (String.uppercase_ascii payload) in
                `push ([response], state + 1)
            | _ -> Default.handle_frame frame conn state
        end
        ```*)
    val handle_frame: Http.Ws.Frame.t -> Net.TcpStream.t -> 'state -> ('state, 'error) handle_result

    (** Default frame handler: responds to Ping with Pong *)
    val handle_message: Miniriot.Message.t -> 'state -> ('state, 'error) handle_result

    (** Default message handler: ignores all messages *)
  end
end
