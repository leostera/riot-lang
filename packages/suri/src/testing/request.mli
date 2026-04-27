open Std

type t
val make:
  ?method_:Net.Http.Method.t ->
  ?uri:string ->
  ?headers:(string * string) list ->
  ?body:string ->
  ?peer:Middleware.Conn.peer ->
  ?params:(string * string) list ->
  ?body_params:(string * string) list ->
  unit ->
  t

val get: ?headers:(string * string) list -> ?peer:Middleware.Conn.peer -> string -> t

val post:
  ?headers:(string * string) list ->
  ?body:string ->
  ?peer:Middleware.Conn.peer ->
  string ->
  t

val put:
  ?headers:(string * string) list ->
  ?body:string ->
  ?peer:Middleware.Conn.peer ->
  string ->
  t

val patch:
  ?headers:(string * string) list ->
  ?body:string ->
  ?peer:Middleware.Conn.peer ->
  string ->
  t

val delete:
  ?headers:(string * string) list ->
  ?body:string ->
  ?peer:Middleware.Conn.peer ->
  string ->
  t

val to_http: t -> Net.Http.Request.t

val to_web_request: t -> Web_server.Request.t

val to_conn: t -> Middleware.Conn.t
