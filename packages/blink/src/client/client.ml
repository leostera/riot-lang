open Std

module Request = Super.Request
module Response = Super.Response
module Budget = Super.Budget
module Telemetry = Super.Telemetry
module Config = Super.Config

type transport = Config.transport

type error = {
  class_: Response.error_class;
  message: string;
  telemetry: Telemetry.t;
}

type pooled_connection = {
  key: string;
  conn: Connection.t;
  mutable last_used_at: Time.Instant.t;
}

type connection = {
  key: string;
  mutable conn: Connection.t;
  mutable reusable: bool;
  mutable closed: bool;
}

type message = Connection.message

type t = {
  config: Config.t;
  budget: Budget.t;
  mutable idle: pooled_connection list;
}

type send_result = {
  result: (Response.t, Error.t) result;
  reusable: bool;
}

let blink_error_to_string = Error.to_string

let exception_to_string = fun caught ->
  match caught with
  | Failure message -> "Failure: " ^ message
  | Invalid_argument message -> "Invalid_argument: " ^ message
  | Not_found -> "Not_found"
  | End_of_file -> "End_of_file"
  | Division_by_zero -> "Division_by_zero"
  | exn -> Exception.to_string exn

let method_to_net = fun value ->
  match value with
  | Request.Get -> Net.Http.Method.Get
  | Request.Post -> Net.Http.Method.Post
  | Request.Put -> Net.Http.Method.Put
  | Request.Patch -> Net.Http.Method.Patch
  | Request.Delete -> Net.Http.Method.Delete

let apply_headers = fun request headers ->
  List.fold_left
    headers
    ~init:request
    ~fn:(fun request (name, value) ->
      Net.Http.Request.with_header request name value)

let endpoint_key = fun uri ->
  let scheme =
    Net.Uri.scheme uri
    |> Option.unwrap_or ~default:"http"
  in
  let host =
    Net.Uri.host uri
    |> Option.unwrap_or ~default:"localhost"
  in
  let default_port =
    match scheme with
    | "https"
    | "wss" -> 443
    | _ -> 80
  in
  let port =
    Net.Uri.port uri
    |> Option.unwrap_or ~default:default_port
  in
  scheme ^ "://" ^ host ^ ":" ^ Int.to_string port

let expired = fun ~now ttl pooled ->
  let idle_for = Time.Instant.saturating_duration_since ~earlier:pooled.last_used_at now in
  Time.Duration.compare idle_for ttl != Order.LT

let prune_pool = fun client ->
  match client.config.connection_policy with
  | Config.Pool { idle_ttl = Some ttl; _ } ->
      let now = client.config.now () in
      let rec loop kept values =
        match values with
        | [] -> List.reverse kept
        | pooled :: rest ->
            if expired ~now ttl pooled then (
              Connection.close pooled.conn;
              loop kept rest
            ) else
              loop (pooled :: kept) rest
      in
      client.idle <- loop [] client.idle
  | Config.CloseAfterRequest
  | Config.ReuseConnection
  | Config.Pool { idle_ttl = None; _ } -> ()

let take_idle = fun key (idle: pooled_connection list) ->
  let rec loop (kept: pooled_connection list) (values: pooled_connection list) =
    match values with
    | [] -> (None, List.reverse kept)
    | pooled :: rest ->
        if String.equal pooled.key key then
          (Some pooled, List.append (List.reverse kept) rest)
        else
          loop (pooled :: kept) rest
  in
  loop [] idle

let take_connection = fun client key uri ->
  match client.config.connection_policy with
  | Config.CloseAfterRequest -> Transport.connect uri
  | Config.ReuseConnection
  | Config.Pool _ ->
      prune_pool client;
      match take_idle key client.idle with
      | (Some pooled, idle) ->
          client.idle <- idle;
          Ok pooled.conn
      | (None, idle) ->
          client.idle <- idle;
          match Transport.connect uri with
          | Ok conn -> Ok conn
          | Error error -> Error error

let remove_keyed_connections = fun key (idle: pooled_connection list) ->
  let rec loop (kept: pooled_connection list) (values: pooled_connection list) =
    match values with
    | [] -> List.reverse kept
    | pooled :: rest ->
        if String.equal pooled.key key then (
          Connection.close pooled.conn;
          loop kept rest
        ) else
          loop (pooled :: kept) rest
  in
  loop [] idle

let count_idle_for_key = fun key (idle: pooled_connection list) ->
  List.fold_left
    idle
    ~init:0
    ~fn:(fun count pooled ->
      if String.equal pooled.key key then
        count + 1
      else
        count)

let release_connection = fun client key conn ~reusable ->
  if not reusable then
    Connection.close conn
  else
    let last_used_at = client.config.now () in
    match client.config.connection_policy with
    | Config.CloseAfterRequest -> Connection.close conn
    | Config.ReuseConnection ->
        client.idle <- { key; conn; last_used_at } :: remove_keyed_connections key client.idle
    | Config.Pool pool ->
        prune_pool client;
        if pool.max_idle_per_endpoint <= 0 then
          Connection.close conn
        else if count_idle_for_key key client.idle >= pool.max_idle_per_endpoint then
          Connection.close conn
        else
          client.idle <- { key; conn; last_used_at } :: client.idle

let response_from_net = fun response body ->
  Response.make
    ~status:(Net.Http.Status.to_int (Net.Http.Response.status response))
    ~body
    ~headers:(Net.Http.Header.to_list (Net.Http.Response.headers response))
    ()

let run_with_deadline = fun conn deadline operation ->
  match deadline with
  | None -> operation ()
  | Some deadline ->
      let cancelled = ref false in
      let _ =
        spawn
          (fun () ->
            sleep deadline;
            if not !cancelled then
              Connection.close conn;
            Ok ())
      in
      let result = operation () in
      cancelled := true;
      result

let send_on_connection = fun conn uri (request: Request.t) ->
  run_with_deadline
    conn
    request.deadline
    (fun () ->
      let net_request = Net.Http.Request.create (method_to_net request.method_) uri in
      let net_request = apply_headers net_request request.headers in
      match Connection.request conn net_request ?body:request.body () with
      | Error error -> { result = Error (Error.RequestFailed error); reusable = false }
      | Ok () -> (
          match Connection.await conn with
          | Error error -> { result = Error (Error.ResponseFailed error); reusable = false }
          | Ok (response, body) ->
              { result = Ok (response_from_net response body); reusable = true }
        ))

let low_level_transport = fun client (request: Request.t) ->
  match Net.Uri.from_string request.url with
  | Error _ -> Error (Error.ProtocolError (Error.InvalidRequestUri request.url))
  | Ok uri ->
      let key = endpoint_key uri in
      match take_connection client key uri with
      | Error error -> Error error
      | Ok conn ->
          let sent = send_on_connection conn uri request in
          release_connection client key conn ~reusable:sent.reusable;
          sent.result

let transport = fun client request ->
  try
    match client.config.transport with
    | Some transport -> transport request
    | None -> low_level_transport client request
  with
  | exn -> Error (Error.ProtocolError (Error.TransportRaised (exception_to_string exn)))

let make = fun ?(config = Config.make ()) () ->
  let started = config.now () in
  { config; budget = Budget.create_with_policy config.budget_policy started; idle = [] }

let budget_remaining = fun client -> Budget.remaining client.budget

let make_telemetry = fun
  client request ~started_at ~attempts ?final_status ?final_error_class ~budget () ->
  let completed_at = client.config.now () in
  Telemetry.make
    ~request
    ~started_at
    ~completed_at
    ~attempts:(List.rev attempts)
    ?final_status
    ?final_error_class
    ~connection_policy:(Config.connection_policy_to_string client.config.connection_policy)
    ~close_behavior:(Config.close_behavior client.config.connection_policy)
    ~budget_remaining:(Budget.remaining budget)
    ()

let fail = fun client class_ message telemetry ->
  client.config.telemetry telemetry;
  Error { class_; message; telemetry }

let succeed = fun client response telemetry ->
  client.config.telemetry telemetry;
  Ok (response, telemetry)

let execute = fun client (request: Request.t) ->
  let started_at = client.config.now () in
  let budget = client.budget in
  if not (Budget.allow ~now:started_at budget) then
    let telemetry =
      let attempt =
        Telemetry.attempt
          ~attempt:0
          ~started_at
          ~completed_at:started_at
          ~lifecycle:Telemetry.Blocked
          ~error_class:Response.RateLimitedByBudget
          ~error_message:"request budget exhausted"
          ()
      in
      make_telemetry
        client
        request
        ~started_at
        ~attempts:[ attempt ]
        ~final_error_class:Response.RateLimitedByBudget
        ~budget
        ()
    in
    fail client Response.RateLimitedByBudget "request budget exhausted" telemetry
  else
    let attempt_started_at = client.config.now () in
    match transport client request with
    | Ok response ->
        let completed_at = client.config.now () in
        if Response.is_success response then (
          let attempt_record =
            Telemetry.attempt
              ~attempt:1
              ~started_at:attempt_started_at
              ~completed_at
              ~lifecycle:Telemetry.Completed
              ~status:response.status
              ()
          in
          let telemetry =
            make_telemetry
              client
              request
              ~started_at
              ~attempts:[ attempt_record ]
              ~final_status:response.status
              ~budget
              ()
          in
          succeed client response telemetry
        ) else (
          let class_ =
            match Response.status_class response.status with
            | Response.RateLimited -> Response.RateLimitedResponse
            | _ -> Response.ServerRejected
          in
          let message = "HTTP status " ^ Int.to_string response.status in
          let attempt_record =
            Telemetry.attempt
              ~attempt:1
              ~started_at:attempt_started_at
              ~completed_at
              ~lifecycle:Telemetry.Failed
              ~status:response.status
              ~error_class:class_
              ~error_message:message
              ()
          in
          let telemetry =
            make_telemetry
              client
              request
              ~started_at
              ~attempts:[ attempt_record ]
              ~final_status:response.status
              ~final_error_class:class_
              ~budget
              ()
          in
          fail client class_ message telemetry
        )
    | Error transport_error ->
        let completed_at = client.config.now () in
        let class_ = Response.error_class_from_transport_error transport_error in
        let message = blink_error_to_string transport_error in
        let attempt_record =
          Telemetry.attempt
            ~attempt:1
            ~started_at:attempt_started_at
            ~completed_at
            ~lifecycle:Telemetry.Failed
            ~error_class:class_
            ~error_message:message
            ()
        in
        let telemetry =
          make_telemetry
            client
            request
            ~started_at
            ~attempts:[ attempt_record ]
            ~final_error_class:class_
            ~budget
            ()
        in
        fail client class_ message telemetry

let error_to_string = fun error ->
  Response.error_class_to_string error.class_ ^ ": " ^ error.message

let with_budget = fun client operation ->
  let started_at = client.config.now () in
  let budget = client.budget in
  if not (Budget.allow ~now:started_at budget) then
    Error (Error.ProtocolError Error.RequestBudgetExhausted)
  else
    operation ()

let connect = fun client uri ->
  let key = Net.Uri.to_string uri in
  with_budget client (fun () -> take_connection client key uri)
  |> Result.map
    ~fn:(fun conn ->
      {
        key;
        conn;
        reusable = true;
        closed = false;
      })

let retire_connection = fun (connection: connection) ->
  connection.reusable <- false;
  if not connection.closed then (
    Connection.close connection.conn;
    connection.closed <- true
  )

let with_connection = fun client (connection: connection) operation ->
  if connection.closed then
    Error Error.Closed
  else
    with_budget
      client
      (fun () ->
        match operation connection.conn with
        | Ok value -> Ok value
        | Error error ->
            retire_connection connection;
            Error error)

let request = fun client connection net_request ?body () ->
  with_connection
    client
    connection
    (fun conn ->
      Connection.request conn net_request ?body ())

let stream = fun client connection -> with_connection client connection Connection.stream

let messages = fun ?on_message client connection ->
  with_connection
    client
    connection
    (fun conn -> Connection.messages ?on_message conn)

let await = fun ?on_message client connection ->
  with_connection
    client
    connection
    (fun conn -> Connection.await ?on_message conn)

let close = fun client connection ->
  if not connection.closed then (
    connection.closed <- true;
    release_connection client connection.key connection.conn ~reusable:connection.reusable
  )

module SSE = struct
  type event = Sse.event = {
    data: string;
    event_type: string option;
    id: string option;
  }

  module Iterator = struct
    type state = {
      client: t;
      connection: connection;
      mutable buffer: string;
      mutable done_: bool;
    }

    type item = event

    let rec next = fun state ->
      if state.done_ then
        None
      else
        match Sse.parse_event state.buffer with
        | Some (Sse.Event event, remaining) ->
            state.buffer <- remaining;
            Some event
        | Some (Sse.Skip, remaining) ->
            state.buffer <- remaining;
            next state
        | Some (Sse.Done, remaining) ->
            state.buffer <- remaining;
            state.done_ <- true;
            None
        | None ->
            match stream state.client state.connection with
            | Error _ ->
                state.done_ <- true;
                None
            | Ok messages ->
                List.for_each
                  messages
                  ~fn:(fun message ->
                    match message with
                    | Connection.Data chunk -> state.buffer <- state.buffer ^ chunk
                    | Connection.Done -> state.done_ <- true
                    | Connection.Status _
                    | Connection.Headers _ -> ());
                if state.done_ && String.equal state.buffer "" then
                  None
                else
                  next state

    let size = fun _state -> 0

    let clone = fun state ->
      {
        client = state.client;
        connection = state.connection;
        buffer = state.buffer;
        done_ = state.done_;
      }
  end

  let await = fun client connection ->
    Iter.MutIterator.make
      (module Iterator)
      {
        Iterator.client = client;
        connection;
        buffer = "";
        done_ = false;
      }
end

module WebSocket = struct
  type client = t

  type t = Websocket.t

  type error = Error.t

  type message = Websocket.message =
    | Text of string
    | Binary of string
    | Ping of string
    | Pong of string
    | Close of int option * string

  let connect = fun client uri -> with_budget client (fun () -> Websocket.connect uri)

  let send_text = fun client conn text ->
    with_budget
      client
      (fun () -> Websocket.send_text conn text)

  let send_binary = fun client conn data ->
    with_budget
      client
      (fun () -> Websocket.send_binary conn data)

  let send_ping = fun client conn ?payload () ->
    with_budget
      client
      (fun () ->
        Websocket.send_ping conn ?payload ())

  let send_pong = fun client conn ?payload () ->
    with_budget
      client
      (fun () ->
        Websocket.send_pong conn ?payload ())

  let send_close = fun client conn ?code ?reason () ->
    with_budget
      client
      (fun () ->
        Websocket.send_close conn ?code ?reason ())

  let receive = fun client conn -> with_budget client (fun () -> Websocket.receive conn)

  let close = fun _client conn -> Websocket.close conn
end

let shutdown = fun client ->
  List.for_each client.idle ~fn:(fun pooled -> Connection.close pooled.conn);
  client.idle <- []
