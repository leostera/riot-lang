module Conn = Middleware.Conn

module Pipeline = Middleware.Pipeline

type middleware = Pipeline.middleware
type pipeline = Pipeline.t
val run: middleware -> Conn.t -> Conn.t

val run_pipeline: pipeline -> Conn.t -> Conn.t
