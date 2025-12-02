open Std
open Std.Data

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
  | PlanningFailed of {
      session_id : Session_id.t;
      failed_at : Datetime.t;
      reason : string;
    }
  | CycleDetected of {
      session_id : Session_id.t;
      detected_at : Datetime.t;
      cycle_nodes : string list;
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

and handle_response t expected_session_id callback response =
  match response with
  | WireProtocol.BuildEvent { session_id = event_session_id; event } ->
      (* Only process events for OUR session *)
      if Session_id.to_string event_session_id = Session_id.to_string expected_session_id then
        callback (BuildEvent event);
      handle_streaming_events t expected_session_id callback
  | WireProtocol.BuildComplete { session_id = event_session_id; completed_at; stats; results } ->
      if Session_id.to_string event_session_id = Session_id.to_string expected_session_id then
        let final_event =
          BuildCompleted { session_id = event_session_id; completed_at; stats; results }
        in
        callback final_event;
        Ok final_event
      else
        (* Skip events from other sessions and keep listening *)
        handle_streaming_events t expected_session_id callback
  | WireProtocol.BuildFailed { session_id = event_session_id; failed_at; stats; built; errors } ->
      if Session_id.to_string event_session_id = Session_id.to_string expected_session_id then
        let final_event =
          BuildFailed { session_id = event_session_id; failed_at; stats; built; errors }
        in
        callback final_event;
        Ok final_event
      else
        handle_streaming_events t expected_session_id callback
  | WireProtocol.PlanningFailed { session_id = event_session_id; failed_at; reason } ->
      if Session_id.to_string event_session_id = Session_id.to_string expected_session_id then
        let final_event = PlanningFailed { session_id = event_session_id; failed_at; reason } in
        callback final_event;
        Ok final_event
      else
        handle_streaming_events t expected_session_id callback
  | WireProtocol.CycleDetected { session_id = event_session_id; detected_at; cycle_nodes } ->
      if Session_id.to_string event_session_id = Session_id.to_string expected_session_id then
        let final_event = CycleDetected { session_id = event_session_id; detected_at; cycle_nodes } in
        callback final_event;
        Ok final_event
      else
        handle_streaming_events t expected_session_id callback
  | event ->
      Error
        (UnexpectedEvent
           { event; reason = "Unexpected response type during streaming" })

let build_streaming t target ?target_arch callback =
  let method_, params =
    match target with
    | BuildPackage package ->
        ( method_build_package,
          build_package_params package target_arch )
    | BuildAll -> (method_build_all, build_all_params target_arch)
  in
  match Jsonrpc.Client.call t.client ~method_ ~params () with
  | Error e -> Error (JsonrpcError e)
  | Ok (WireProtocol.BuildStarted { session_id; started_at = _ }) ->
      callback (BuildStarted session_id);
      handle_streaming_events t session_id callback
  | Ok event ->
      Error
        (UnexpectedEvent { event; reason = "Expected BuildStarted response" })
