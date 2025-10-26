open Std
open Std.Data
open Miniriot
open Tusk_model
open Tusk_protocol
open Client

type build_target = BuildPackage of string | BuildAll

type streaming_event =
  | BuildStarted of Session_id.t
  | BuildEvent of Telemetry.event
  | BuildCompleted of {
      session_id : Session_id.t;
      completed_at : Datetime.t;
      stats : WireProtocol.build_stats;
      results : WireProtocol.build_result list;
    }
  | BuildFailed of {
      session_id : Session_id.t;
      failed_at : Datetime.t;
      stats : WireProtocol.build_stats;
      built : WireProtocol.build_result list;
      errors : WireProtocol.build_result list;
    }

let rec handle_streaming_events t session_id callback =
  match Jsonrpc.Client.receive_response t.client with
  | Error (Jsonrpc.InternalError { context; details }) ->
      if
        context = "parse_response_result"
        && String.starts_with ~prefix:"Failed to parse result:" details
      then (
        (* Skip BuildEvent responses that can't be deserialized *)
        Log.debug
          "[BUILD_STREAMING] Skipping unparseable event (BuildEvent), \
           continuing...";
        handle_streaming_events t session_id callback)
      else Error (JsonrpcError (Jsonrpc.InternalError { context; details }))
  | Error e -> Error (JsonrpcError e)
  | Ok response_wrapper ->
      handle_response t session_id callback response_wrapper.result

and handle_response t session_id callback response =
  match response with
  | WireProtocol.BuildEvent { session_id; event } ->
      callback (BuildEvent event);
      handle_streaming_events t session_id callback
  | WireProtocol.BuildComplete { session_id; completed_at; stats; results } ->
      let final_event =
        BuildCompleted { session_id; completed_at; stats; results }
      in
      callback final_event;
      Ok final_event
  | WireProtocol.BuildFailed { session_id; failed_at; stats; built; errors } ->
      let final_event =
        BuildFailed { session_id; failed_at; stats; built; errors }
      in
      callback final_event;
      Ok final_event
  | event ->
      Error
        (UnexpectedEvent
           { event; reason = "Unexpected response type during streaming" })

let build_streaming t target callback =
  let method_, params =
    match target with
    | BuildPackage package ->
        ( method_build_package,
          Jsonrpc.Named [ ("package", Json.String package) ] )
    | BuildAll -> (method_build_all, Jsonrpc.NoParams)
  in
  match Jsonrpc.Client.call t.client ~method_ ~params () with
  | Error e -> Error (JsonrpcError e)
  | Ok (WireProtocol.BuildStarted { session_id; started_at = _ }) ->
      callback (BuildStarted session_id);
      handle_streaming_events t session_id callback
  | Ok event ->
      Error
        (UnexpectedEvent { event; reason = "Expected BuildStarted response" })
