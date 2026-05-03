open Std

type method_ =
  | Get
  | Post
  | Put
  | Patch
  | Delete

type t = {
  method_: method_;
  url: string;
  headers: (string * string) list;
  body: string option;
  auth_required: bool;
  deadline: Time.Duration.t option;
}

let make = fun ?(headers = []) ?body ?(auth_required = false) ?deadline ~method_ ~url () ->
  {
    method_;
    url;
    headers;
    body;
    auth_required;
    deadline;
  }

let method_to_string = fun value ->
  match value with
  | Get -> "GET"
  | Post -> "POST"
  | Put -> "PUT"
  | Patch -> "PATCH"
  | Delete -> "DELETE"

let describe = fun request -> method_to_string request.method_ ^ " " ^ request.url
