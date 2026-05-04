(** JSON-RPC 2.0 Client Implementation *)
open Std
open Std.Data
open Std.Collections
open Std.Result.Syntax

module type Transport = sig
  type t

  val send: t -> string -> (unit, string) result

  val receive: t -> (string, string) result

  val close: t -> unit
end

type ('request, 'response) t =
  | Client: {
      transport_mod: (module Transport with type t = 'a);
      transport: 'a;
      protocol_mod:
        (module Common.ApplicationProtocol with type request = 'req and type response = 'res);
      mutable next_id: int;
    } -> ('req, 'res) t

let create = fun ~transport:transport_mod ~protocol:protocol_mod transport ->
  Client {
    transport_mod;
    transport;
    protocol_mod;
    next_id = 1;
  }

let send_raw_request = fun (Client { transport_mod; transport; _ }) json_str ->
  let module T = (val transport_mod : Transport with type t = _) in
  T.send transport (json_str ^ "\n")

let receive_raw_response = fun (Client { transport_mod; transport; _ }) ->
  let module T = (val transport_mod : Transport with type t = _) in
  T.receive transport

let send_request: type req res. (req, res) t -> req -> (unit, Common.error) result = fun
  (Client c as client) request ->
  let module P = (val c.protocol_mod : Common.ApplicationProtocol with type request = req and type response = res) in
  let prereq = P.request_to_params request in
  let id = Common.Number c.next_id in
  c.next_id <- c.next_id + 1;
  let jsonrpc_req = Common.request ~method_:prereq.method_ ~params:prereq.params ~id () in
  let json = Common.request_to_json jsonrpc_req in
  let str = Json.to_string json in
  match send_raw_request client str with
  | Ok () -> Ok ()
  | Error e -> Error (Common.InternalError { context = "send_request"; details = e })

let receive_response: type req res. (req, res) t -> (res Common.response, Common.error) result = fun
  (Client c as client) ->
  let module P = (val c.protocol_mod : Common.ApplicationProtocol with type request = req and type response = res) in
  match receive_raw_response client with
  | Error e -> Error (Common.InternalError { context = "receive_response"; details = e })
  | Ok str -> (
      match Json.from_string str with
      | Error e ->
          Error (Common.ParseError { raw_input = str; parse_error = Json.error_to_string e })
      | Ok json -> (
          match json with
          | Json.Object fields -> (
              match (
                Std.Collections.Proplist.get fields ~key:"jsonrpc",
                Std.Collections.Proplist.get fields ~key:"id"
              ) with
              | (Some (Json.String "2.0"), Some id_json) -> (
                  match Common.id_of_json id_json with
                  | Ok id -> (
                      match Std.Collections.Proplist.get fields ~key:"error" with
                      | Some (Json.Object err_fields) ->
                          let code =
                            match Std.Collections.Proplist.get err_fields ~key:"code" with
                            | Some (Json.Int c) -> c
                            | _ -> (-1)
                          in
                          let message =
                            match Std.Collections.Proplist.get err_fields ~key:"message" with
                            | Some (Json.String m) -> m
                            | _ -> "Unknown error"
                          in
                          let data = Std.Collections.Proplist.get err_fields ~key:"data" in
                          Error (Common.UnknownServerError { code; message; data })
                      | Some err_json ->
                          Error (Common.UnknownServerError {
                            code = (-1);
                            message = "Invalid error format";
                            data = Some err_json;
                          })
                      | None -> (
                          match Std.Collections.Proplist.get fields ~key:"result" with
                          | Some result_json -> (
                              match P.response_of_json result_json with
                              | Ok parsed_result ->
                                  Ok { Common.jsonrpc = "2.0"; result = parsed_result; id }
                              | Error err_json ->
                                  Error (Common.InternalError {
                                    context = "parse_response_result";
                                    details = "Failed to parse result: " ^ (Json.to_string err_json);
                                  })
                            )
                          | None ->
                              Error (Common.InvalidRequest {
                                request_json = json;
                                reason = "Response missing both result and error";
                              })
                        )
                    )
                  | Error e ->
                      Error (Common.InvalidRequest {
                        request_json = json;
                        reason = "Invalid ID: " ^ e;
                      })
                )
              | _ ->
                  Error (Common.InvalidRequest {
                    request_json = json;
                    reason = "Missing jsonrpc or id field";
                  })
            )
          | _ ->
              Error (Common.InvalidRequest {
                request_json = json;
                reason = "Response must be an object";
              })
        )
    )

let call (type req res) (client: (req, res) t) ~method_ ?params () =
  let (Client c) = client in
  let module P = (val c.protocol_mod : Common.ApplicationProtocol with type request = req and type response = res) in
  let id = Common.Number c.next_id in
  c.next_id <- c.next_id + 1;
  let jsonrpc_req =
    Common.request
      ~method_
      ~params:(Option.unwrap_or params ~default:Common.NoParams)
      ~id
      ()
  in
  let json = Common.request_to_json jsonrpc_req in
  let str = Json.to_string json in
  match send_raw_request client str with
  | Error e -> Error (Common.InternalError { context = "call_send"; details = e })
  | Ok () -> (
      match receive_raw_response client with
      | Error e -> Error (Common.InternalError { context = "call_receive"; details = e })
      | Ok str -> (
          match Json.from_string str with
          | Error e ->
              Error (Common.ParseError { raw_input = str; parse_error = Json.error_to_string e })
          | Ok json -> (
              match json with
              | Json.Object fields -> (
                  match Std.Collections.Proplist.get fields ~key:"error" with
                  | Some (Json.Object err_fields) ->
                      let code =
                        match Std.Collections.Proplist.get err_fields ~key:"code" with
                        | Some (Json.Int c) -> c
                        | _ -> (-1)
                      in
                      let message =
                        match Std.Collections.Proplist.get err_fields ~key:"message" with
                        | Some (Json.String m) -> m
                        | _ -> "Unknown error"
                      in
                      let data = Std.Collections.Proplist.get err_fields ~key:"data" in
                      Error (Common.UnknownServerError { code; message; data })
                  | Some err_json ->
                      Error (Common.UnknownServerError {
                        code = (-1);
                        message = "Invalid error format";
                        data = Some err_json;
                      })
                  | None -> (
                      match Std.Collections.Proplist.get fields ~key:"result" with
                      | Some result_json -> (
                          match P.response_of_json result_json with
                          | Ok parsed_result -> Ok parsed_result
                          | Error err_json ->
                              Error (Common.InternalError {
                                context = "call_parse_result";
                                details = "Failed to parse result: " ^ (Json.to_string err_json);
                              })
                        )
                      | None ->
                          Error (Common.InvalidRequest {
                            request_json = json;
                            reason = "Response missing result";
                          })
                    )
                )
              | _ ->
                  Error (Common.InvalidRequest {
                    request_json = json;
                    reason = "Response must be an object";
                  })
            )
        )
    )

let notify (type req res) (client: (req, res) t) ~method_ ?params () =
  let jsonrpc_req = Common.notification ~method_ ?params () in
  let json = Common.request_to_json jsonrpc_req in
  let str = Json.to_string json in
  match send_raw_request client str with
  | Ok () -> Ok ()
  | Error e -> Error (Common.InternalError { context = "notify"; details = e })

let call_batch:
  type req res. (req, res) t ->
  req list ->
  (res Common.response list, Common.error) result = fun client requests ->
  let (Client c) = client in
  let module P = (val c.protocol_mod : Common.ApplicationProtocol with type request = req and type response = res) in
  let jsonrpc_requests =
    List.map
      requests
      ~fn:(fun req ->
        let prereq = P.request_to_params req in
        let id = Common.Number c.next_id in
        c.next_id <- c.next_id + 1;
        Common.request ~method_:prereq.method_ ~params:prereq.params ~id ())
  in
  let json_array = Json.Array (List.map jsonrpc_requests ~fn:Common.request_to_json) in
  let str = Json.to_string json_array in
  match send_raw_request client str with
  | Error e -> Error (Common.InternalError { context = "call_batch_send"; details = e })
  | Ok () -> (
      match receive_raw_response client with
      | Error e -> Error (Common.InternalError { context = "call_batch_receive"; details = e })
      | Ok str ->
          let* json =
            Json.from_string str
            |> Result.map_err
              ~fn:(fun e ->
                Common.ParseError { raw_input = str; parse_error = Json.error_to_string e })
          in
          match json with
          | Json.Array responses ->
              let parsed_responses =
                List.fold_left
                  responses
                  ~init:(Ok [])
                  ~fn:(fun acc json_resp ->
                    let* responses = acc in
                    match json_resp with
                    | Json.Object fields -> (
                        match (
                          Std.Collections.Proplist.get fields ~key:"jsonrpc",
                          Std.Collections.Proplist.get fields ~key:"id"
                        ) with
                        | (Some (Json.String "2.0"), Some id_json) -> (
                            match Common.id_of_json id_json with
                            | Ok id -> (
                                match Std.Collections.Proplist.get fields ~key:"result" with
                                | Some result_json -> (
                                    match P.response_of_json result_json with
                                    | Ok parsed_result ->
                                        Ok ({ Common.jsonrpc = "2.0"; result = parsed_result; id }
                                        :: responses)
                                    | Error err_json ->
                                        Error (Common.InternalError {
                                          context = "call_batch_parse_result";
                                          details = "Failed to parse result: "
                                          ^ Json.to_string err_json;
                                        })
                                  )
                                | None ->
                                    Error (Common.InvalidRequest {
                                      request_json = json_resp;
                                      reason = "Response missing result";
                                    })
                              )
                            | Error e ->
                                Error (Common.InvalidRequest {
                                  request_json = json_resp;
                                  reason = "Invalid ID: " ^ e;
                                })
                          )
                        | _ ->
                            Error (Common.InvalidRequest {
                              request_json = json_resp;
                              reason = "Invalid response in batch";
                            })
                      )
                    | _ ->
                        Error (Common.InvalidRequest {
                          request_json = json_resp;
                          reason = "Batch response item must be an object";
                        }))
              in
              Result.map parsed_responses ~fn:List.rev
          | json ->
              Error (Common.InvalidRequest {
                request_json = json;
                reason = "Batch response must be an array";
              })
    )

let close = fun (Client { transport_mod; transport; _ }) ->
  let module T = (val transport_mod : Transport with type t = _) in
  T.close transport
