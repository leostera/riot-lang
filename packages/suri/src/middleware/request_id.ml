open Std

let x_request_id = "x-request-id"

let max_request_id_length = 128

type validation_error =
  | EmptyRequestId
  | RequestIdTooLong of { length: int; max_length: int }
  | InvalidRequestIdCharacter of { char: char; index: int }

let validation_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | EmptyRequestId -> "request ID must not be empty"
  | RequestIdTooLong { length; max_length } ->
      "request ID is too long: "
      ^ Int.to_string length
      ^ " bytes, maximum is "
      ^ Int.to_string max_length
  | InvalidRequestIdCharacter { char; index } ->
      "request ID contains invalid character code "
      ^ Int.to_string (Char.to_int char)
      ^ " at index "
      ^ Int.to_string index

let is_visible_ascii = fun char ->
  let code = Char.to_int char in
  code >= 0x21 && code <= 0x7e

let validate_request_id = fun id ->
  let len = String.length id in
  if len = 0 then
    Error EmptyRequestId
  else if len > max_request_id_length then
    Error (RequestIdTooLong { length = len; max_length = max_request_id_length })
  else
    let rec go index =
      if index >= len then
        Ok ()
      else
        let char = String.get_unchecked id ~at:index in
        if is_visible_ascii char then
          go (index + 1)
        else
          Error (InvalidRequestIdCharacter { char; index })
    in
    go 0

let is_valid_request_id = fun id ->
  match validate_request_id id with
  | Ok () -> true
  | Error _ -> false

let generate_request_id = fun () ->
  let uuid = UUID.v7 () in
  UUID.to_string uuid

let choose_request_id = fun ?(generate = generate_request_id) header_value ->
  match header_value with
  | Some id -> (
      match validate_request_id id with
      | Ok () -> id
      | Error _ -> generate ()
    )
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
