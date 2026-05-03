(**
   WebSocket communication layer.

   Channel provides WebSocket frame handling and bidirectional real-time
   communication between server and clients. It's the foundation for building
   real-time features like LiveView, chat, notifications, and collaborative editing.

   ## Quick Start

   Define a WebSocket handler:

   ```ocaml
   module EchoHandler = struct
     type args = unit
     type state = unit

     let init () = Continue ()

     let handle_frame frame _conn state =
       match frame with
       | { Http.Ws.Frame.opcode = Text; payload; _ } ->
           let response = Http.Ws.Frame.text payload in
           Push ([response], state)
       | { opcode = Ping; _ } ->
           Push ([Http.Ws.Frame.pong ()], state)
       | _ -> Continue state

     let handle_message _msg state = Continue state
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
   - **Continuation** - Fragmented message continuation
*)
open Std

type topic = string

(** Topic identifier for pub/sub channels *)
module Handler: sig
  (**
     WebSocket handler interface.

     Handlers define the behavior of WebSocket connections by implementing
     three key functions:
     - init: Initialize handler state when connection starts
     - handle_frame: Process incoming WebSocket frames
     - handle_message: Process messages from other processes

     This design enables building stateful, message-driven WebSocket applications.
  *)
  type upgrade_opts = { do_upgrade: bool }
  (** Options for WebSocket protocol upgrade *)
  type initialization_error = ..
  type error =
    | InitializationFailed of initialization_error
    | UnknownOpcode of int
  type reported_error

  val reported_error: reported_error -> error

  val reported_error_to_string: reported_error -> string

  type ('state, 'error) result =
    | Continue of 'state
    (** Continue with updated state *)
    | Push of Http.Ws.Frame.t list * 'state
    (** Send frames to client and update state *)
    | Error of 'error
  type 'state handle_result = ('state, error) result

  module type Intf = sig
    (**
       Handler module interface.

       Implement this interface to define custom WebSocket behavior.

       Example:
       ```ocaml
       module ChatHandler = struct
         type args = { room : string }
         type state = { room : string; user : string option }

         let init args = Continue { room = args.room; user = None }

         let handle_frame frame _conn state =
           match frame with
           | { Http.Ws.Frame.opcode = Text; payload; _ } ->
               broadcast state.room payload;
               Continue state
           | _ -> Continue state

         let handle_message msg state =
           match msg with
           | BroadcastMsg text ->
               let frame = Http.Ws.Frame.text text in
               Push ([frame], state)
           | _ -> Continue state
       end
       ```
    *)
    type state
    (** Handler state - maintains connection state across frames *)
    type args

    (** Initialization arguments passed when creating the handler *)
    val init: args -> state handle_result

    (**
       Initialize the handler when connection starts.

       Called once when the WebSocket connection is established.
       Return [Continue state] with initial state or [Error err] if
       initialization fails.
    *)
    val handle_frame: Http.Ws.Frame.t -> Net.TcpStream.t -> state -> state handle_result

    (**
       Handle incoming WebSocket frame from client.

       Called for each frame received from the client.
       Can return:
       - [Continue state] to continue with updated state
       - [Push (frames, state)] to send frames and update state
       - [Error err] if frame processing fails
    *)
    val handle_message: Message.t -> state -> state handle_result

    (**
       Handle messages from other processes.

       Called when another process sends a message to this handler.
       Useful for broadcasting, notifications, or coordinating between handlers.
    *)
    val error_to_string: error -> string
  end

  type t

  (** Opaque handler type wrapping a handler module and its state *)
  val make: (module Intf with type args = 'a and type state = 'b) -> 'a -> t

  (**
     Create a handler from a module and initialization arguments.

     Example:
     ```ocaml
     let handler = Handler.make (module EchoHandler) ()
     ```
  *)
  val init: t -> Net.TcpStream.t -> (t, reported_error) result

  (** Initialize the handler with a TCP stream *)
  val initialize: t -> (t, reported_error) result

  (** Initialize the handler without a TCP stream. *)
  val handle_frame: t -> Http.Ws.Frame.t -> Net.TcpStream.t -> (t, reported_error) result

  (** Handle an incoming frame *)
  val handle_message: t -> Message.t -> 'a -> (t, reported_error) result

  (** Handle a message from another process *)
  module Default: sig
    (**
       Default handler implementations.

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

         let init () = Continue 0

         let handle_frame frame conn state =
           match frame with
           | { Http.Ws.Frame.opcode = Text; payload; _ } ->
               let response = Http.Ws.Frame.text (String.uppercase_ascii payload) in
               Push ([response], state + 1)
           | _ -> Default.handle_frame frame conn state
       end
       ```
    *)
    val handle_frame: Http.Ws.Frame.t -> Net.TcpStream.t -> 'state -> 'state handle_result

    (** Default frame handler: responds to Ping with Pong *)
    val handle_message: Message.t -> 'state -> 'state handle_result

    (** Default message handler: ignores all messages *)
    val error_to_string: error -> string
  end
end
