(** JSON-RPC 2.0 Client Implementation *)

module type Transport = sig
  type t

  val send : t -> string -> (unit, string) result
  val receive : t -> (string, string) result
  val close : t -> unit
end

type t =
  | Client : {
      transport_mod : (module Transport with type t = 'a);
      transport : 'a;
      mutable next_id : int;
    }
      -> t

let create (type a) (transport_mod : (module Transport with type t = a))
    transport =
  Client { transport_mod; transport; next_id = 1 }

let generate_id () = Common.Number (Random.int 1000000)

let send_request (Client { transport_mod; transport; _ }) req =
  let module T = (val transport_mod : Transport with type t = _) in
  let json = Common.request_to_json req in
  let str = Json.to_string json ^ "\n" in
  T.send transport str

let receive_response (Client { transport_mod; transport; _ }) =
  let module T = (val transport_mod : Transport with type t = _) in
  match T.receive transport with
  | Error e -> Error e
  | Ok str -> (
      match Json.of_string str with
      | Error e -> Error (Printf.sprintf "JSON parse error: %s" e)
      | Ok json -> (
          match Common.response_of_json json with
          | Ok resp -> Ok resp
          | Error e -> Error e))

let call (Client c as client) ~method_ ?params () =
  (* Generate ID and increment counter *)
  let id = Common.Number c.next_id in
  c.next_id <- c.next_id + 1;

  (* Create and send request *)
  let req = Common.make_request ~method_ ?params ~id () in
  match send_request client req with
  | Error e ->
      Error (Common.make_error ~code:Common.InternalError ~message:e ())
  | Ok () -> (
      (* Wait for response *)
      match receive_response client with
      | Error e ->
          Error (Common.make_error ~code:Common.InternalError ~message:e ())
      | Ok resp -> (
          if
            (* Check that ID matches *)
            resp.id <> id
          then
            Error
              (Common.make_error ~code:Common.InvalidRequest
                 ~message:"Response ID doesn't match request ID" ())
          else
            match (resp.result, resp.error) with
            | Some result, None -> Ok result
            | None, Some err -> Error err
            | _ ->
                Error
                  (Common.make_error ~code:Common.InternalError
                     ~message:
                       "Invalid response: must have either result or error"
                     ())))

let notify (Client _ as client) ~method_ ?params () =
  let req = Common.make_notification ~method_ ?params () in
  send_request client req

let call_batch (Client _ as client) requests =
  (* Send batch request *)
  let json = Json.Array (List.map Common.request_to_json requests) in
  let str = Json.to_string json ^ "\n" in
  let (Client { transport_mod; transport; _ }) = client in
  let module T = (val transport_mod : Transport with type t = _) in
  match T.send transport str with
  | Error e -> Error e
  | Ok () -> (
      (* Receive batch response *)
      match T.receive transport with
      | Error e -> Error e
      | Ok str -> (
          match Json.of_string str with
          | Error e -> Error (Printf.sprintf "JSON parse error: %s" e)
          | Ok (Json.Array items) ->
              let responses =
                List.filter_map
                  (fun json ->
                    match Common.response_of_json json with
                    | Ok resp -> Some resp
                    | Error _ -> None)
                  items
              in
              Ok responses
          | Ok _ -> Error "Expected array response for batch request"))

let close (Client { transport_mod; transport; _ }) =
  let module T = (val transport_mod : Transport with type t = _) in
  T.close transport
