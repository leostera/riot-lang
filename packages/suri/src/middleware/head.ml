open Std

(**
   HEAD request handler middleware

   Processes HEAD requests through GET routes and strips the response body.

   The key insight: HEAD responses MUST NOT have a body per HTTP spec,
   regardless of how the route is defined.
*)
let middleware = fun ~conn ~next ->
  let original_method = Conn.method_ conn in
  match original_method with
  | Net.Http.Method.Head ->
      conn
      |> Conn.with_method Net.Http.Method.Get
      |> next
      |> Conn.with_body ""
  | _ -> next conn
