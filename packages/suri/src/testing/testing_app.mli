module Request = Testing_request

type outcome =
  | Responded of Web_server.Response.t
  | Upgraded
type response_error =
  | InvalidRequest of Request.error
  | ExpectedResponseButUpgraded

val response_error_to_string: response_error -> string

val run_pipeline_response: Middleware.Pipeline.t -> Middleware.Conn.t -> Web_server.Handler.response

val run_conn: Middleware.Pipeline.t -> Middleware.Conn.t -> outcome

val run: Middleware.Pipeline.t -> Request.t -> (outcome, response_error) Std.result

val response:
  Middleware.Pipeline.t ->
  Request.t ->
  (Web_server.Response.t, response_error) Std.result

val get:
  Middleware.Pipeline.t ->
  ?headers:(string * string) list ->
  string ->
  (Web_server.Response.t, response_error) Std.result

val post:
  Middleware.Pipeline.t ->
  ?headers:(string * string) list ->
  ?body:string ->
  string ->
  (Web_server.Response.t, response_error) Std.result

val put:
  Middleware.Pipeline.t ->
  ?headers:(string * string) list ->
  ?body:string ->
  string ->
  (Web_server.Response.t, response_error) Std.result

val patch:
  Middleware.Pipeline.t ->
  ?headers:(string * string) list ->
  ?body:string ->
  string ->
  (Web_server.Response.t, response_error) Std.result

val delete:
  Middleware.Pipeline.t ->
  ?headers:(string * string) list ->
  string ->
  (Web_server.Response.t, response_error) Std.result
