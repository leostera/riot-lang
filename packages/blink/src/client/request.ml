open Std

type method_ =
  | Get
  | Post
  | Put
  | Patch
  | Delete

type endpoint_class =
  | PublicRead
  | PrivateRead
  | TradingWrite
  | ExternalRead

type t = {
  method_: method_;
  url: string;
  headers: (string * string) list;
  body: string option;
  endpoint_class: endpoint_class;
  auth_required: bool;
  deadline: Time.Duration.t option;
}

let make = fun
  ?(headers = [])
  ?body
  ?(endpoint_class = PublicRead)
  ?(auth_required = false)
  ?deadline
  ~method_
  ~url
  () ->
  {
    method_;
    url;
    headers;
    body;
    endpoint_class;
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

let endpoint_class_to_string = fun value ->
  match value with
  | PublicRead -> "public_read"
  | PrivateRead -> "private_read"
  | TradingWrite -> "trading_write"
  | ExternalRead -> "external_read"

let describe = fun request -> method_to_string request.method_ ^ " " ^ request.url
