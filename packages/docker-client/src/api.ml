open Std

let ( let* ) value fn = Result.and_then value ~fn

module Config = Super.Config

let uri_error_to_string = fun error ->
  match error with
  | Net.Uri.InvalidScheme -> "invalid scheme"
  | Net.Uri.InvalidAuthority -> "invalid authority"
  | Net.Uri.InvalidPath -> "invalid path"
  | Net.Uri.InvalidQuery -> "invalid query"
  | Net.Uri.InvalidFragment -> "invalid fragment"
  | Net.Uri.InvalidFormat -> "invalid format"
  | Net.Uri.TooLong -> "URI too long"

let uri_for = fun (config: Config.t) path ->
  let base =
    match config.transport with
    | Config.Unix _ -> "http://docker"
    | Config.Tcp { host; port } -> "http://" ^ host ^ ":" ^ Int.to_string port
  in
  Net.Uri.from_string (base ^ path)
  |> Result.map_err ~fn:(fun error -> Error.UriError (uri_error_to_string error))

let connect = fun (config: Config.t) uri ->
  match config.transport with
  | Config.Unix path ->
      let* stream =
        Net.UnixStream.connect path
        |> Result.map_err
          ~fn:(fun error ->
            match error with
            | Net.UnixStream.Connection_refused -> Error.ConnectError "connection refused"
            | Net.UnixStream.Closed -> Error.ConnectError "connection closed"
            | Net.UnixStream.System_error error -> Error.ConnectError (IO.error_message error))
      in
      Ok (Blink.Connection.make
        ~reader:(Net.UnixStream.to_reader stream)
        ~writer:(Net.UnixStream.to_writer stream)
        ~on_close:(fun () -> Net.UnixStream.close stream)
        ~uri
        ())
  | Config.Tcp _ ->
      Blink.connect uri
      |> Result.map_err ~fn:(fun error -> Error.ConnectError (Blink.Error.to_string error))

let request = fun config method_ path ?body ?(headers = []) () ->
  let* uri = uri_for config path in
  let* conn = connect config uri in
  let base_req = Net.Http.Request.create method_ uri in
  let req =
    List.fold_left
      headers
      ~init:base_req
      ~fn:(fun req (name, value) ->
        Net.Http.Request.with_header req name value)
    |> fun req -> Net.Http.Request.with_header req "connection" "close"
  in
  let request_result = Blink.Connection.request conn req ?body () in
  let response_result =
    match request_result with
    | Error error -> Error (Error.HttpError (Blink.Error.to_string error))
    | Ok () -> (
        match Blink.Connection.await conn with
        | Error error -> Error (Error.HttpError (Blink.Error.to_string error))
        | Ok (response, body) ->
            let status = Net.Http.Status.to_int (Net.Http.Response.status response) in
            if status >= 200 && status < 300 then
              Ok body
            else
              Error (Error.DockerError { status; body })
      )
  in
  Blink.Connection.close conn;
  response_result

let query = fun params ->
  match params with
  | [] -> ""
  | _ ->
      "?"
      ^ (
        params
        |> List.map
          ~fn:(fun (name, value) -> Net.Uri.form_encode name ^ "=" ^ Net.Uri.form_encode value)
        |> String.concat "&"
      )
