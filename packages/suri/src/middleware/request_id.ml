open Std

let x_request_id = "x-request-id"

let max_request_id_length = 128

let is_visible_ascii = fun char ->
  let code = Char.to_int char in
  code >= 0x21 && code <= 0x7e

let is_valid_request_id = fun id ->
  let len = String.length id in
  len > 0 && len <= max_request_id_length && String.for_all id ~fn:is_visible_ascii

let generate_request_id = fun () ->
  let uuid = UUID.v7 () in
  UUID.to_string uuid

let choose_request_id = fun ?(generate = generate_request_id) header_value ->
  match header_value with
  | Some id when is_valid_request_id id -> id
  | _ -> generate ()

let request_id = fun ~conn ~next ->
  (* Check if x-request-id already exists in the request *)
  let request_id =
    let headers = Conn.headers conn in
    choose_request_id (Net.Http.Header.get headers x_request_id)
  in
  (* Add x-request-id to the request for downstream handlers *)
  let conn = Conn.with_header x_request_id request_id conn in
  (* Call next middleware/handler *)
  let conn' = next conn in
  (* Ensure x-request-id is in the response headers too *)
  conn'
  |> Conn.with_header x_request_id request_id

module For_testing = struct
  let max_request_id_length = max_request_id_length

  let is_valid_request_id = is_valid_request_id

  let choose_request_id = choose_request_id
end
