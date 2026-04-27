open Std

type error =
  | StatusMismatch of {
      expected: Net.Http.Status.t;
      actual: Net.Http.Status.t;
    }
  | BodyMismatch of { expected: string; actual: string }
  | HeaderMissing of { name: string }
  | HeaderMismatch of { name: string; expected: string; actual: string }

let status_to_string = fun status ->
  Net.Http.Status.to_string status ^ " " ^ Net.Http.Status.reason_phrase status

let error_to_string = function
  | StatusMismatch { expected; actual } ->
      "expected status " ^ status_to_string expected ^ ", got " ^ status_to_string actual
  | BodyMismatch { expected; actual } -> "expected body " ^ expected ^ ", got " ^ actual
  | HeaderMissing { name } -> "expected response header " ^ name
  | HeaderMismatch { name; expected; actual } ->
      "expected response header " ^ name ^ " to be " ^ expected ^ ", got " ^ actual

let status = fun expected response ->
  if Net.Http.Status.equal response.Web_server.Response.status expected then
    Ok ()
  else
    Error (StatusMismatch { expected; actual = response.status })

let body = fun expected response ->
  if String.equal response.Web_server.Response.body expected then
    Ok ()
  else
    Error (BodyMismatch { expected; actual = response.body })

let header = fun name expected response ->
  match Net.Http.Header.get response.Web_server.Response.headers name with
  | None -> Error (HeaderMissing { name })
  | Some actual ->
      if String.equal actual expected then
        Ok ()
      else
        Error (HeaderMismatch { name; expected; actual })

let no_header = fun name response ->
  if Net.Http.Header.has response.Web_server.Response.headers name then
    Error (HeaderMismatch { name; expected = "<missing>"; actual = "<present>" })
  else
    Ok ()
