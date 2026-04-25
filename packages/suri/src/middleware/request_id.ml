open Std

let x_request_id = "x-request-id"

let request_id = fun ~conn ~next ->
  (* Check if x-request-id already exists in the request *)
  let request_id =
    let headers = Conn.headers conn in
    match Net.Http.Header.get headers x_request_id with
    | Some id -> id
    | None ->
        (* Generate a new UUID v7 *)
        let uuid = UUID.v7 () in UUID.to_string uuid
  in
  (* Add x-request-id to the request for downstream handlers *)
  let conn = Conn.with_header x_request_id request_id conn in
  (* Call next middleware/handler *)
  let conn' = next conn in (* Ensure x-request-id is in the response headers too *)
  conn' |> Conn.with_header x_request_id request_id
