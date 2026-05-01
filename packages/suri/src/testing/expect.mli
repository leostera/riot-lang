open Std

type error =
  | StatusMismatch of {
      expected: Net.Http.Status.t;
      actual: Net.Http.Status.t;
    }
  | BodyMismatch of { expected: string; actual: string }
  | HeaderMissing of { name: string }
  | HeaderMismatch of { name: string; expected: string; actual: string }

val error_to_string: error -> string

val status: Net.Http.Status.t -> Web_server.Response.t -> (unit, error) Std.result

val body: string -> Web_server.Response.t -> (unit, error) Std.result

val header: string -> string -> Web_server.Response.t -> (unit, error) Std.result

val no_header: string -> Web_server.Response.t -> (unit, error) Std.result
