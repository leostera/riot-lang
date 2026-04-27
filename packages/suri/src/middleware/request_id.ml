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
  let request_id =
    let headers = Conn.headers conn in
    choose_request_id (Net.Http.Header.get headers x_request_id)
  in
  let conn = Conn.with_request_header x_request_id request_id conn in
  let conn' = next conn in
  conn'
  |> Conn.set_header x_request_id request_id
