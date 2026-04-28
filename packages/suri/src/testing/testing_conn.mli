open Std

val make:
  ?method_:Net.Http.Method.t ->
  ?uri:string ->
  ?headers:(string * string) list ->
  ?body:string ->
  ?peer:Middleware.Conn.peer ->
  ?params:(string * string) list ->
  ?body_params:(string * string) list ->
  unit ->
  (Middleware.Conn.t, Testing_request.error) result

val parse_query_params: string -> (string * string) list
