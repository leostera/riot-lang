open Std

let make = fun ?method_ ?uri ?headers ?body ?peer ?params ?body_params () ->
  Testing_request.make ?method_ ?uri ?headers ?body ?peer ?params ?body_params ()
  |> Testing_request.to_conn

let parse_query_params = Middleware.Conn.parse_query_params
