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

val make:
  ?headers:(string * string) list ->
  ?body:string ->
  ?auth_required:bool ->
  ?deadline:Time.Duration.t ->
  method_:method_ ->
  url:string ->
  unit ->
  t

val method_to_string: method_ -> string

val describe: t -> string
