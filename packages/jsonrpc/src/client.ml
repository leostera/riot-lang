(** JSON-RPC 2.0 Client Implementation *)

open Std
open Std.Data

module type Transport = sig
  type t

  val send : t -> string -> (unit, string) result
  val receive : t -> (string, string) result
  val close : t -> unit
end

type ('request, 'response) t =
  | Client : {
      transport_mod : (module Transport with type t = 'a);
      transport : 'a;
      protocol_mod :
        (module Common.ApplicationProtocol
           with type request = 'req
            and type response = 'res);
      mutable next_id : int;
    }
      -> ('req, 'res) t

let create ~transport:transport_mod ~protocol:protocol_mod transport =
  Client { transport_mod; transport; protocol_mod; next_id = 1 }

let send_raw_request (Client { transport_mod; transport; _ }) json_str =
  let module T = (val transport_mod : Transport with type t = _) in
  T.send transport (json_str ^ "\n")

let receive_raw_response (Client { transport_mod; transport; _ }) =
  let module T = (val transport_mod : Transport with type t = _) in
  T.receive transport

let send_request (type req res) (Client c as client : (req, res) t)
    (request : req) =
  let module P =
    (val c.protocol_mod
        : Common.ApplicationProtocol with type request = req
         and type response = res)
  in
  let prereq = P.request_to_params request in
  let id = Common.Number c.next_id in
  c.next_id <- c.next_id + 1;
  let jsonrpc_req =
    Common.request ~method_:prereq.method_ ~params:prereq.params ~id ()
  in
  let json = Common.request_to_json jsonrpc_req in
  let str = Json.to_string json in
  send_raw_request client str

let receive_response (type req res) (Client c as client : (req, res) t) :
    (res Common.response, string) result =
  let module P =
    (val c.protocol_mod
        : Common.ApplicationProtocol with type request = req
         and type response = res)
  in
  match receive_raw_response client with
  | Error e -> Error e
  | Ok str -> (
      match Json.of_string str with
      | Error e ->
          Error (format "JSON parse error: %s" (Json.error_to_string e))
      | Ok json -> (
          match json with
          | Json.Object fields -> (
              match
                (List.assoc_opt "jsonrpc" fields, List.assoc_opt "id" fields)
              with
              | Some (Json.String "2.0"), Some id_json -> (
                  match Common.id_of_json id_json with
                  | Ok id -> (
                      let error =
                        match List.assoc_opt "error" fields with
                        | None -> Ok None
                        | Some e -> (
                            match Common.error_of_json e with
                            | Ok err -> Ok (Some err)
                            | Error msg -> Error msg)
                      in
                      match error with
                      | Ok (Some error) ->
                          (* Error response *)
                          Ok
                            { Common.jsonrpc = "2.0"; result = Error error; id }
                      | Ok None -> (
                          (* Success response - parse result *)
                          match List.assoc_opt "result" fields with
                          | Some result_json -> (
                              match P.response_of_json result_json with
                              | Ok parsed_result ->
                                  Ok
                                    {
                                      Common.jsonrpc = "2.0";
                                      result = Ok parsed_result;
                                      id;
                                    }
                              | Error err_json ->
                                  Error
                                    (format "Failed to parse result: %s"
                                       (Json.to_string err_json)))
                          | None ->
                              Error "Response missing both result and error")
                      | Error e -> Error e)
                  | Error e -> Error e)
              | _ -> Error "Invalid response: missing jsonrpc or id field")
          | _ -> Error "Response must be an object"))

let call (type req res) (client : (req, res) t) ~method_ ?params () =
  (* This is a simplified call that doesn't use the protocol's request type *)
  let (Client c) = client in
  let module P =
    (val c.protocol_mod
        : Common.ApplicationProtocol with type request = req
         and type response = res)
  in
  let id = Common.Number c.next_id in
  c.next_id <- c.next_id + 1;
  let jsonrpc_req =
    Common.request ~method_
      ~params:(Option.unwrap_or params ~default:Common.NoParams)
      ~id ()
  in
  let json = Common.request_to_json jsonrpc_req in
  let str = Json.to_string json in
  match send_raw_request client str with
  | Error e ->
      Error (Common.make_error ~code:Common.InternalError ~message:e ())
  | Ok () -> (
      match receive_raw_response client with
      | Error e ->
          Error (Common.make_error ~code:Common.InternalError ~message:e ())
      | Ok str -> (
          match Json.of_string str with
          | Error e ->
              Error
                (Common.make_error ~code:Common.ParseError
                   ~message:(Json.error_to_string e) ())
          | Ok json -> (
              match json with
              | Json.Object fields -> (
                  match List.assoc_opt "error" fields with
                  | Some err_json -> (
                      match Common.error_of_json err_json with
                      | Ok err -> Error err
                      | Error e ->
                          Error
                            (Common.make_error ~code:Common.ParseError
                               ~message:e ()))
                  | None -> (
                      match List.assoc_opt "result" fields with
                      | Some result_json -> (
                          (* Parse the result through the protocol *)
                          match P.response_of_json result_json with
                          | Ok parsed_result -> Ok parsed_result
                          | Error err_json ->
                              Error
                                (Common.make_error ~code:Common.ParseError
                                   ~message:
                                     (format "Failed to parse result: %s"
                                        (Json.to_string err_json))
                                   ()))
                      | None ->
                          Error
                            (Common.make_error ~code:Common.InternalError
                               ~message:"Response missing result" ())))
              | _ ->
                  Error
                    (Common.make_error ~code:Common.ParseError
                       ~message:"Response must be an object" ()))))

let notify (type req res) (client : (req, res) t) ~method_ ?params () =
  let jsonrpc_req = Common.notification ~method_ ?params () in
  let json = Common.request_to_json jsonrpc_req in
  let str = Json.to_string json in
  send_raw_request client str

let call_batch (type req res) (client : (req, res) t) (requests : req list) =
  let (Client c) = client in
  let module P =
    (val c.protocol_mod
        : Common.ApplicationProtocol with type request = req
         and type response = res)
  in
  (* Convert each request to JSON-RPC format *)
  let jsonrpc_requests =
    List.map
      (fun req ->
        let prereq = P.request_to_params req in
        let id = Common.Number c.next_id in
        c.next_id <- c.next_id + 1;
        Common.request ~method_:prereq.method_ ~params:prereq.params ~id ())
      requests
  in

  (* Send batch request *)
  let json_array =
    Json.Array (List.map Common.request_to_json jsonrpc_requests)
  in
  let str = Json.to_string json_array in
  match send_raw_request client str with
  | Error e -> Error e
  | Ok () -> (
      (* Receive batch response *)
      match receive_raw_response client with
      | Error e -> Error e
      | Ok str -> (
          match Json.of_string str with
          | Error e ->
              Error (format "JSON parse error: %s" (Json.error_to_string e))
          | Ok (Json.Array responses) -> (
              (* Parse each response *)
              let parsed_responses =
                List.fold_left
                  (fun acc json_resp ->
                    match acc with
                    | Error e -> Error e
                    | Ok responses -> (
                        match json_resp with
                        | Json.Object fields -> (
                            match
                              ( List.assoc_opt "jsonrpc" fields,
                                List.assoc_opt "id" fields )
                            with
                            | Some (Json.String "2.0"), Some id_json -> (
                                match Common.id_of_json id_json with
                                | Ok id -> (
                                    let error =
                                      match List.assoc_opt "error" fields with
                                      | None -> Ok None
                                      | Some e -> (
                                          match Common.error_of_json e with
                                          | Ok err -> Ok (Some err)
                                          | Error msg -> Error msg)
                                    in
                                    match error with
                                    | Ok (Some error) ->
                                        Ok
                                          ({
                                             Common.jsonrpc = "2.0";
                                             result = Error error;
                                             id;
                                           }
                                          :: responses)
                                    | Ok None -> (
                                        match
                                          List.assoc_opt "result" fields
                                        with
                                        | Some result_json -> (
                                            match
                                              P.response_of_json result_json
                                            with
                                            | Ok parsed_result ->
                                                Ok
                                                  ({
                                                     Common.jsonrpc = "2.0";
                                                     result = Ok parsed_result;
                                                     id;
                                                   }
                                                  :: responses)
                                            | Error _ ->
                                                Error "Failed to parse result")
                                        | None ->
                                            Error
                                              "Response missing both result \
                                               and error")
                                    | Error e -> Error e)
                                | Error e -> Error e)
                            | _ -> Error "Invalid response in batch")
                        | _ -> Error "Batch response item must be an object"))
                  (Ok []) responses
              in
              match parsed_responses with
              | Ok responses -> Ok (List.rev responses)
              | Error e -> Error e)
          | _ -> Error "Batch response must be an array"))

let close (Client { transport_mod; transport; _ }) =
  let module T = (val transport_mod : Transport with type t = _) in
  T.close transport
