module Conn = Middleware.Conn
module Pipeline = Middleware.Pipeline

type middleware = Pipeline.middleware

type pipeline = Pipeline.t

let run = fun middleware conn -> middleware ~conn ~next:(fun conn -> conn)

let run_pipeline = fun pipeline conn -> Pipeline.run conn pipeline
