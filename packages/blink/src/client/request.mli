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

val make:
  ?headers:(string * string) list ->
  ?body:string ->
  ?endpoint_class:endpoint_class ->
  ?auth_required:bool ->
  ?deadline:Time.Duration.t ->
  method_:method_ ->
  url:string ->
  unit ->
  t

val method_to_string: method_ -> string

val endpoint_class_to_string: endpoint_class -> string

val describe: t -> string
