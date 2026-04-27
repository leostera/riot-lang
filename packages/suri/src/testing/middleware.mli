module Conn = Suri__Middleware__Conn

module Pipeline = Suri__Middleware__Pipeline

type middleware = Pipeline.middleware
type pipeline = Pipeline.t
val run: middleware -> Conn.t -> Conn.t

val run_pipeline: pipeline -> Conn.t -> Conn.t
